(ns replique.server
  "Socket server. Every connection starts with an EDN :hello message which
  carries the role of the connection:

    :control - one message loop per editor session (replique.control)
    :repl    - one real REPL per REPL buffer, reading from its own socket

  Both roles share the same handshake, so that a REPL connection keeps the
  reader the handshake was read from - a REPL needs a real stdin."
  (:require [replique.protocol :as protocol]
            [replique.state :as state])
  (:import [java.io BufferedWriter InputStreamReader IOException OutputStreamWriter]
           [java.net InetAddress ServerSocket Socket SocketException]
           [java.nio.charset StandardCharsets]
           [java.util.concurrent.atomic AtomicLong]))

(defn- accept-hello! [{:keys [process-id] :as conn} msg]
  (if-not (map? msg)
    (protocol/write-frame!
     conn (protocol/error nil :invalid-message "The :hello message must be a map"))
    (let [msg (assoc msg :op (protocol/as-keyword (:op msg)))
          role (protocol/as-keyword (:role msg))]
      (cond
        (not (protocol/valid-id? (:id msg)))
        (protocol/write-frame!
         conn (protocol/error (dissoc msg :id) :invalid-message
                              (protocol/invalid-id-message (:id msg))))

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
         conn (protocol/error msg :invalid-role
                              (if (nil? (:role msg))
                                "The :hello message must have a :role"
                                (str "A :role must be a keyword, a string or a symbol, got: "
                                     (pr-str (:role msg))))))

        (not (contains? (methods protocol/accept-role) role))
        (protocol/write-frame!
         conn (protocol/error msg :unsupported-role
                              (str "Unsupported role: " role)
                              {:supported-roles (vec (sort (keys (methods protocol/accept-role))))}))

        :else (protocol/accept-role conn (assoc msg :role role))))))

(defn- handshake!
  "Read the first line of the connection, which must hold the :hello message
  and nothing else: a line holding anything more is rejected. What follows
  the handshake on a :repl connection is code, not a message, so a client
  that batches the two has misunderstood the connection - and losing the
  batched form silently would be the worse answer.

  The handshake is stricter than the control loop: a connection that cannot
  produce a readable :hello is not speaking this protocol and is closed."
  [conn]
  (loop []
    (let [line (protocol/read-line! conn)]
      (when-not (protocol/eof? line)
        (let [[messages error] (protocol/read-messages line)]
          (cond
            error
            (protocol/write-frame!
             conn (protocol/error nil :malformed-message
                                  (str "Could not read the :hello message: "
                                       (protocol/read-error-message error))))

            ;; blank lines before the handshake are ignored
            (empty? messages) (recur)

            (next messages)
            (protocol/write-frame!
             conn (protocol/error nil :invalid-message
                                  "The :hello message must be alone on its line"))

            :else (accept-hello! conn (first messages))))))))

(defn set-role!
  "Record what the connection turned out to be, once the handshake accepted
  it. Whoever broadcasts an event needs to tell the control connections from
  the repls."
  [{:keys [role]} kind]
  (reset! role kind))

(defn evaluating!
  "Mark the calling thread as the one :interrupt targets on this connection."
  [{:keys [eval-thread]}]
  (locking eval-thread (reset! eval-thread (Thread/currentThread))))

(defn done-evaluating! [{:keys [eval-thread]}]
  (locking eval-thread
    (reset! eval-thread nil)
    ;; An interrupt that arrived at the very end of the evaluation must not
    ;; leak into the next read. The interrupter takes the same lock, so it
    ;; either interrupted before this point - and the flag is cleared here -
    ;; or it found the connection idle and did nothing.
    (Thread/interrupted)))

(defn interrupt!
  "Interrupt what the connection is evaluating. Returns false when it is idle.

  This only stops code that blocks or checks the interrupt flag: Thread.stop
  is gone since jdk 20 and the jvm offers nothing else."
  [{:keys [eval-thread]}]
  (locking eval-thread
    (if-let [^Thread thread @eval-thread]
      (do (.interrupt thread) true)
      false)))

(defn- connection [server ^Socket socket client-id]
  (merge
   {:id client-id
    :socket socket
    ;; nil until the handshake accepted the connection
    :role (atom nil)
    ;; the thread evaluating right now, nil when idle
    :eval-thread (atom nil)
    :in (clojure.lang.LineNumberingPushbackReader.
         (InputStreamReader. (.getInputStream socket) StandardCharsets/UTF_8))
    :out (BufferedWriter.
          (OutputStreamWriter. (.getOutputStream socket) StandardCharsets/UTF_8))
    :process-id (:process-id server)}
   (protocol/outbox)))

(defn- close-connection! [server {:keys [id ^Socket socket]}]
  (swap! (:connections server) dissoc id)
  (try (.close socket) (catch Exception _))
  (state/closed! id))

(defn- accept-connection! [server ^Socket socket client-id]
  (let [conn (connection server socket client-id)]
    (swap! (:connections server) assoc client-id conn)
    (doto (Thread. (fn []
                     ;; Before anything is read, and on this thread rather
                     ;; than by inheritance: what a connection loads through
                     ;; is what every other connection loads through, and a
                     ;; repl wraps this one rather than replacing it.
                     (state/adopt-class-loader!)
                     (try
                       (handshake! conn)
                       (catch SocketException _)
                       (catch Throwable t
                         (try (protocol/write-frame! conn (protocol/exception-error nil t))
                              (catch Throwable _)))
                       (finally
                         ;; the last frame is usually the one that says why we
                         ;; close - get it out before the socket goes
                         (try (protocol/try-flush! conn) (catch Throwable _))
                         (try (protocol/drain! conn) (catch Throwable _))
                         (close-connection! server conn))))
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
