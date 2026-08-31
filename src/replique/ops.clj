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

(defmethod protocol/handle :shutdown [conn msg]
  ;; Written and waited on here rather than returned to be written: what
  ;; comes after it is the process going away, and a reply that found the
  ;; connection busy - a thread of the application printing is enough - would
  ;; be parked, and would go with it.
  ;;
  ;; The wait is bounded and its result ignored on purpose. A connection that
  ;; stays busy is one nobody is reading, and the client that is not reading
  ;; is the one that just asked to be rid of this process: it gets what it
  ;; asked for, and losing the reply is the lesser thing to lose
  (protocol/write-frame! conn (protocol/reply msg {:stopping true}))
  (protocol/flush-blocking! conn)
  (exit!)
  protocol/no-reply)
