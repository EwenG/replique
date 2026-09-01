(ns replique.ops
  "The ops handled by the control connection."
  (:require [replique.protocol :as protocol]
            [replique.server :as server]
            [replique.state :as state]))

(defmethod protocol/handle :process-info [conn _]
  (let [info (state/info)]
    (assoc info
           :connection (:id conn)
           :uptime (when-let [started-at (:started-at info)]
                     (- (System/currentTimeMillis) started-at)))))

;; Protocol smoke test. :value comes back both as JSON - which is lossy, EDN
;; keywords and symbols become strings - and as the EDN the process read, which
;; is not.
(defmethod protocol/handle :echo [_ msg]
  (protocol/frame {:value (:value msg)
                   :printed (pr-str (:value msg))}))

;; The namespaces the process has, for a client to offer a choice of. What
;; has been loaded rather than what is on the classpath: a repl can only be
;; moved into a namespace that exists, and one that exists only as a file is
;; one nothing can be evaluated in yet.
;;
;; Sorted here rather than by the client. It is the same order for every
;; client, it is the order somebody reading a list expects, and the client
;; that asked is about to show it to somebody.
(defmethod protocol/handle :namespaces [_ _]
  {:namespaces (vec (sort (map (comp str ns-name) (all-ns))))})

;; Stopping an evaluation that went wrong. The client names the repl
;; connection it wants interrupted - it knows the id, the handshake reply of
;; every connection it opened carries it.
;;
;; This interrupts the thread. It stops code that blocks or that checks the
;; interrupt flag, and nothing else: Thread.stop is gone since jdk 20 and the
;; jvm offers no other way. An infinite loop that computes has to be waited
;; out, or the process restarted.
(defmethod protocol/handle :interrupt [_ msg]
  (let [id (:connection msg)
        target (get (state/connections) id)]
    (cond
      (not (string? id))
      (throw (ex-info (str "The :interrupt op needs the :connection to interrupt, got: "
                           (pr-str id))
                      {:replique/error :invalid-message}))

      (nil? target)
      (throw (ex-info (str "Unknown connection: " (pr-str id))
                      {:replique/error :unknown-connection}))

      (not (identical? :repl @(:role target)))
      (throw (ex-info (str "Connection " id " is not a repl")
                      {:replique/error :not-a-repl}))

      :else {:connection id :interrupted (server/interrupt! target)})))

;; Stopping the process. A client that started one can signal it; a client
;; that connected to one cannot - it is not a child of that editor, and after
;; the editor restarts none of them are. Asking is the way that works for
;; both, and it is also the graceful one: the process exits through its
;; shutdown hook, which is what deletes the port file.
(defn exit!
  "End the process. A var of its own so that a test can run the op without
  taking the test runner with it - the tests run inside the process they
  test."
  []
  (System/exit 0))

(def exit-delay-ms
  "How long the reply has before the process goes. Long enough for a write to
  a connection on this machine, short enough to be under the time a client
  waits for the process to be gone."
  300)

(defmethod protocol/handle :shutdown [_ _]
  ;; The payload is returned to be framed and written like any other op's,
  ;; and all that is arranged here is that the process does not go first.
  ;;
  ;; The exit is what is delayed, rather than the reply waited for, because a
  ;; write to a client that stopped reading never returns - and a client that
  ;; asked the process to stop is exactly a client about to stop reading. An
  ;; exit that waited for the write would be an exit that never happened, so
  ;; the two are not connected at all: the connection thread writes the reply,
  ;; and this ends the process whether that write got anywhere or not.
  (doto (Thread. (fn [] (Thread/sleep (long exit-delay-ms)) (exit!)) "replique-exit")
    (.setDaemon true)
    (.start))
  {:stopping true})
