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
