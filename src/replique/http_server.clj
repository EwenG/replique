(ns replique.http-server
  (:require [replique.utils :as utils]
            [replique.http :as http]
            [replique.tooling-msg :as tooling-msg])
  (:import [java.net InetAddress ServerSocket SocketException]
           [java.io InputStreamReader BufferedReader]))

(defonce cljs-server (atom {:state :stopped}))
(defonce server-socket nil)

(defmacro ^:private thread
  [^String name daemon & body]
  `(doto (Thread. (fn [] ~@body) ~name)
     (.setDaemon ~daemon)
     (.start)))

(defn- required
  "Throw if opts does not contain prop."
  [opts prop]
  (when (nil? (get opts prop))
    (throw (ex-info (str "Missing required http server property " prop) opts))))

(defn- validate-opts
  "Validate server config options"
  [{:keys [name port accept] :as opts}]
  (doseq [prop [:port :accept]] (required opts prop))
  (when (or (not (integer? port)) (not (< -1 port 65535)))
    (throw (ex-info (str "Invalid http server port: " port) opts))))

;; The caller is responsible for catching exceptions
(defn send-response [out {:keys [status body content-type encoding]}]
  (http/send-and-close out status body content-type encoding))

(defn accept-connection-http [in out accept-http-fn]
  (when-let [request (try (http/read-request in)
                          (catch Exception e
                            (try
                              (tooling-msg/uncaught-exception (Thread/currentThread) e)
                              (http/send-and-close out 500 "Could not read request"
                                                   "text/plain" "UTF-8")
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

(defn start-server
  "Start an http-server given the specified opts:
    :address Host or address, string, defaults to loopback address
    :port Port, integer, required
    :accept Namespaced symbol of the accept function to invoke, required
    :bind-err Bind *err* to socket out stream?, defaults to true
    :server-daemon Is server thread a daemon?, defaults to true
    :client-daemon Are client threads daemons?, defaults to true
   Returns http socket."
  [opts]
  (validate-opts opts)
  (let [{:keys [address port accept accept
                bind-err server-daemon client-daemon]
         :or {bind-err true
              server-daemon true
              client-daemon true}} opts
        address (InetAddress/getByName address)  ;; nil returns loopback
        socket (ServerSocket. port 0 address)
        _ (require (symbol (namespace accept)))
        accept-http-fn (resolve accept)]
    (alter-var-root #'server-socket (constantly socket))
    (thread
      (str "Replique HTTP Server") server-daemon
      (try
        (loop []
          (when (not (.isClosed socket))
            (try
              (let [conn (.accept socket)
                    in (.getInputStream conn)
                    out (.getOutputStream conn)]
                (accept-connection-http
                 (-> in (InputStreamReader.) (BufferedReader.))
                 out accept-http-fn))
              (catch SocketException _disconnect))
            (recur)))
        (finally
          (alter-var-root #'server-socket (constantly nil)))))
    socket))

(defn server-port []
  (.getLocalPort ^ServerSocket server-socket))

(defn server-host []
  (.getHostName (.getInetAddress ^ServerSocket server-socket)))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :http-address]
  [msg]
  (tooling-msg/with-tooling-response msg
    {:http-host (server-host)
     :http-port (server-port)}))
