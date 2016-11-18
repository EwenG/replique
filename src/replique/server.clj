(ns replique.server
  (:require [clojure.main :as m]
            [replique.utils :as utils]
            [replique.http :as http]
            [replique.tooling-msg :as tooling-msg])
  (:import [java.net InetAddress Socket ServerSocket SocketException]
           [java.util.concurrent.locks ReentrantLock]
           [java.util.concurrent Executors]))

(defonce cljs-server (atom {:state :stopped}))

(def ^:dynamic *session* nil)

;; lock protects servers
(defonce ^:private lock (ReentrantLock.))
(defonce ^:private servers {})

(defmacro ^:private thread
  [^String name daemon & body]
  `(doto (Thread. (fn [] ~@body) ~name)
    (.setDaemon ~daemon)
    (.start)))

(defn- required
  "Throw if opts does not contain prop."
  [opts prop]
  (when (nil? (get opts prop))
    (throw (ex-info (str "Missing required socket server property " prop) opts))))

(defn- validate-opts
  "Validate server config options"
  [{:keys [name port accept] :as opts}]
  (doseq [prop [:name :port :accept]] (required opts prop))
  (when (or (not (integer? port)) (not (< -1 port 65535)))
    (throw (ex-info (str "Invalid socket server port: " port) opts))))

(defn- accept-connection
  "Start accept function, to be invoked on a client thread, given:
    conn - client socket
    name - server name
    client-id - client identifier
    in - in stream
    out - out stream
    err - err stream
    accept - accept fn symbol to invoke
    args - to pass to accept-fn"
  [^Socket conn name client-id in out err accept args]
  (try
    (binding [*in* in
              *out* out
              *err* err
              *session* {:server name :client client-id}]
      (utils/with-lock lock
        (alter-var-root #'servers assoc-in [name :sessions client-id] {}))
      (require (symbol (namespace accept)))
      (let [accept-fn (resolve accept)]
        (apply accept-fn args)))
    (catch SocketException _disconnect)
    (finally
      (utils/with-lock lock
        (alter-var-root #'servers update-in [name :sessions] dissoc client-id))
      (.close conn))))

;; The caller is resposible for catching exceptions
(defn send-response [out {:keys [status body content-type encoding]}]
  (http/send-and-close out status body content-type encoding))

(defn accept-connection-http [in out accept-http-fn]
  (when-let [request (try (http/read-request in)
                          (catch Exception e
                            (try
                              (tooling-msg/uncaught-exception (Thread/currentThread) e)
                              (http/send-and-close out 500 "Could not read request")
                              ;; Socket closed
                              (catch Exception e
                                (tooling-msg/uncaught-exception (Thread/currentThread) e)))
                            nil))]
    (let [callback (partial send-response out)]
      (let [response (accept-http-fn request callback)]
        (if (instance? java.util.concurrent.Future response)
          ;; Response is sent asynchronously
          nil
          (let [{:keys [status body content-type encoding]} response]
            (try
              (http/send-and-close out status body content-type encoding)
              ;; Socket closed
              (catch Exception e
                (tooling-msg/uncaught-exception (Thread/currentThread) e)))))))))

(defn make-http-reader [first-char in]
  (let [arr (byte-array 1)
        _ (aset arr 0 (byte first-char))
        prepend-in (java.io.ByteArrayInputStream. arr)]
    (-> (java.io.SequenceInputStream. prepend-in in)
        (java.io.InputStreamReader.)
        (java.io.BufferedReader.))))

(defn start-server
  "Start a socket server given the specified opts:
    :address Host or address, string, defaults to loopback address
    :port Port, integer, required
    :name Name, required
    :accept Namespaced symbol of the accept function to invoke, required
    :args Vector of args to pass to accept function
    :bind-err Bind *err* to socket out stream?, defaults to true
    :server-daemon Is server thread a daemon?, defaults to true
    :client-daemon Are client threads daemons?, defaults to true
   Returns server socket."
  [opts]
  (validate-opts opts)
  (let [{:keys [address port name accept accept-http
                args bind-err server-daemon client-daemon]
         :or {bind-err true
              server-daemon true
              client-daemon true}} opts
        address (InetAddress/getByName address)  ;; nil returns loopback
        socket (ServerSocket. port 0 address)
        _ (require (symbol (namespace accept-http)))
        accept-http-fn (resolve accept-http)]
    (utils/with-lock lock
      (alter-var-root #'servers assoc name {:name name, :socket socket, :sessions {}}))
    (thread
      (str "Clojure Server " name) server-daemon
      (try
        (loop [client-counter 1]
          (when (not (.isClosed socket))
            (try
              (let [conn (.accept socket)
                    in (.getInputStream conn)
                    out (.getOutputStream conn)
                    client-id (str client-counter)]
                (let [first-char (.read in)]
                  ;; If the first char is \G, we assume the input stream contains an HTTP GET
                  ;; request
                  ;; If the first char is \P, we assume the input stream contains an HTTP POST
                  ;; request
                  (case first-char
                    ;; \G
                    71 (let [;; unread the char because it is part of the HTTP request
                             in (make-http-reader first-char in)]
                         (accept-connection-http in out accept-http-fn))
                    ;; \P
                    80 (let [;; unread the char because it is part of the HTTP request
                             in (make-http-reader first-char in)]
                         (accept-connection-http in out accept-http-fn))
                    (let [in (clojure.lang.LineNumberingPushbackReader.
                              (java.io.InputStreamReader. in))
                          out (java.io.BufferedWriter.
                               (java.io.OutputStreamWriter. out))]
                      ;; Don't unread the char since the client sent a fake init char
                      (thread
                        (str "Clojure Connection " name " " client-id) client-daemon
                        (accept-connection conn name client-id in out
                                           (if bind-err out *err*) accept args))))))
              (catch SocketException _disconnect))
            (recur (inc client-counter))))
        (finally
          (utils/with-lock lock
            (alter-var-root #'servers dissoc name)))))
    socket))

(defn stop-server
  "Stop server with name or use the server-name from *session* if none supplied.
  Returns true if server stopped successfully, nil if not found, or throws if
  there is an error closing the socket."
  ([]
   (stop-server (:server *session*)))
  ([name]
   (utils/with-lock lock
     (let [server-socket ^ServerSocket (get-in servers [name :socket])]
       (when server-socket
         (alter-var-root #'servers dissoc name)
         (.close server-socket)
         true)))))

(defn server-port []
  (let [ss (-> servers (get :replique) :socket)]
    (.getLocalPort ^ServerSocket ss)))

