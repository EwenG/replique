(ns replique.server
  "Socket server. Every connection starts with an EDN :hello message which
  carries the role of the connection:

    :control - one message loop per editor session (replique.control)
    :repl    - one real REPL per REPL buffer, reading from its own socket

  Both roles share the same handshake, so that a REPL connection keeps the
  reader the handshake was read from - a REPL needs a real stdin."
  (:require [replique.protocol :as protocol])
  (:import [java.io BufferedWriter InputStreamReader IOException OutputStreamWriter]
           [java.net InetAddress ServerSocket Socket SocketException]
           [java.nio.charset StandardCharsets]
           [java.util.concurrent.atomic AtomicLong]
           [java.util.concurrent.locks ReentrantLock]))

(defn- normalize-role [role]
  (cond
    (keyword? role) role
    (string? role) (keyword role)
    (symbol? role) (keyword (str role))
    :else nil))

(defmethod protocol/accept-role :repl [conn hello]
  (protocol/write-frame!
   conn (protocol/error hello :not-implemented
                        "The :repl role is not implemented yet")))

(defn- handshake! [{:keys [process-id] :as conn}]
  (let [msg (try (protocol/read-message conn)
                 (catch Throwable t t))]
    (cond
      (protocol/eof? msg) nil

      ;; The client is gone, or the connection is being closed
      (instance? java.io.IOException msg) nil

      (instance? Throwable msg)
      (protocol/write-frame!
       conn (protocol/error nil :malformed-message
                            (str "Could not read the :hello message: "
                                 (.getMessage ^Throwable msg))))

      (not (map? msg))
      (protocol/write-frame!
       conn (protocol/error nil :invalid-message "The :hello message must be a map"))

      :else
      (let [msg (assoc msg :op (protocol/normalize-op (:op msg)))
            role (normalize-role (:role msg))]
        (cond
          (not (protocol/valid-id? (:id msg)))
          (protocol/write-frame!
           conn (protocol/error (dissoc msg :id) :invalid-message
                                (str "An :id must be a string or a number, got: "
                                     (pr-str (:id msg)))))

          (not= :hello (:op msg))
          (protocol/write-frame!
           conn (protocol/error msg :expected-hello
                                "The first message of a connection must be :hello"))

          ;; Guards against a client connecting to a process it did not expect,
          ;; typically from a stale port file.
          (and (:process-id msg) (not= (str (:process-id msg)) process-id))
          (protocol/write-frame!
           conn (protocol/error msg :process-id-mismatch
                                (str "This process is " process-id)
                                {:process-id process-id}))

          (nil? role)
          (protocol/write-frame!
           conn (protocol/error msg :invalid-role "The :hello message must have a :role"))

          (not (contains? (methods protocol/accept-role) role))
          (protocol/write-frame!
           conn (protocol/error msg :unsupported-role
                                (str "Unsupported role: " role)
                                {:supported-roles (vec (sort (keys (methods protocol/accept-role))))}))

          :else (protocol/accept-role conn (assoc msg :role role)))))))

(defn- connection [server ^Socket socket client-id]
  {:id client-id
   :socket socket
   :in (clojure.lang.LineNumberingPushbackReader.
        (InputStreamReader. (.getInputStream socket) StandardCharsets/UTF_8))
   :out (BufferedWriter.
         (OutputStreamWriter. (.getOutputStream socket) StandardCharsets/UTF_8))
   :lock (ReentrantLock.)
   :process-id (:process-id server)})

(defn- close-connection! [server {:keys [id ^Socket socket]}]
  (swap! (:connections server) dissoc id)
  (try (.close socket) (catch Exception _)))

(defn- accept-connection! [server ^Socket socket client-id]
  (let [conn (connection server socket client-id)]
    (swap! (:connections server) assoc client-id conn)
    (doto (Thread. (fn []
                     (try
                       (handshake! conn)
                       (catch SocketException _)
                       (catch Throwable t
                         (try (protocol/write-frame! conn (protocol/exception-error nil t))
                              (catch Throwable _)))
                       (finally (close-connection! server conn))))
                   (str "replique-connection-" client-id))
      (.setDaemon true)
      (.start))))

(defn start-server
  "Start the socket server. opts:
    :host       host or address to bind to, defaults to the loopback address
    :port       port, 0 (the default) binds to a free port
    :name       server name, used to name threads
    :process-id the id this process answers to
  Returns the server map, which holds the java.net.ServerSocket."
  [{:keys [host port name process-id] :or {port 0 name "replique"}}]
  (let [address (InetAddress/getByName host)
        socket (ServerSocket. port 0 address)
        server {:name name
                :socket socket
                :process-id process-id
                :connections (atom {})}
        counter (AtomicLong. 0)
        accept-loop
        (fn []
          (loop []
            (when-not (.isClosed socket)
              (let [^Socket conn-socket
                    (try (.accept socket)
                         (catch IOException e
                           (when-not (.isClosed socket)
                             ;; Too many open files, ... Report and keep
                             ;; listening: if this thread dies the process
                             ;; silently stops accepting connections - and
                             ;; exits, every other thread is a daemon.
                             (.println System/err
                                       (str "replique: could not accept a connection: " e))
                             (Thread/sleep 100))
                           nil))]
                (when conn-socket
                  (try
                    (.setTcpNoDelay conn-socket true)
                    (accept-connection! server conn-socket
                                        (str "c" (.incrementAndGet counter)))
                    (catch Throwable _
                      (try (.close conn-socket) (catch Exception _)))))
                (recur)))))]
    ;; Not a daemon: the process stays alive as long as it is listening
    (doto (Thread. accept-loop (str "replique-server-" name))
      (.setDaemon false)
      (.start))
    server))

(defn server-port [server]
  (.getLocalPort ^ServerSocket (:socket server)))

(defn server-host [server]
  (.getHostAddress (.getInetAddress ^ServerSocket (:socket server))))

(defn stop-server! [server]
  (try (.close ^ServerSocket (:socket server)) (catch Exception _))
  (doseq [[_ conn] @(:connections server)]
    (close-connection! server conn))
  nil)
