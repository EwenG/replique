(ns replique.repl
  (:require [replique.utils :as utils]
            [replique.tooling-msg :as tooling-msg]
            [replique.server :as server])
  (:import [clojure.lang LineNumberingPushbackReader]))

(def ^:private dispatch-request
  (utils/dynaload 'replique.repl-cljs/dispatch-request))

(defn accept-http [request callback]
  (try (@dispatch-request request callback)
       (catch Exception e
         (tooling-msg/uncaught-exception (Thread/currentThread) e)
         {:status 500 :body (.getMessage e)})))

(comment
  (server/server-port)
  )

(defn tooling-repl []
  (clojure.main/repl
   :init (fn [] (in-ns 'replique.repl))
   :prompt #()
   :print (fn [result] (tooling-msg/tooling-prn result))))

(defn print-repl-meta []
  (when (and (tooling-msg/tooling-available?) server/*session*)
    (binding [*out* tooling-msg/tooling-out]
      (utils/with-lock tooling-msg/tooling-out-lock
        (tooling-msg/tooling-prn {:type :repl-meta
                                  :process-id tooling-msg/process-id
                                  :session server/*session*
                                  ;; use dynamic vars for repl-type and repl-env in order to
                                  ;; come back to the right repl-type when leaving a repl
                                  :repl-type (utils/repl-type utils/*repl-env*)
                                  :repl-env utils/*repl-env*
                                  :ns (utils/repl-ns utils/*repl-env*)})))))

(defn start-repl-process [project-map {:keys [process-id host port cljs-compile-path version]}]
  (try
    (alter-var-root #'utils/process-out (constantly *out*))
    (alter-var-root #'utils/process-err (constantly *err*))
    (alter-var-root #'utils/project-map (constantly project-map))
    (alter-var-root #'tooling-msg/process-id (constantly process-id))
    (alter-var-root #'utils/cljs-compile-path (constantly cljs-compile-path))
    (alter-var-root #'utils/version (constantly version))
    ;; *file* is set by leiningen to a temporary file
    (set! *file* "NO_SOURCE_PATH")
    ;; Let leiningen :global-vars option propagate to other REPLs
    ;; The tooling REPL printing is a custom one and thus is not affected by those bindings,
    ;; and it must not !!
    (alter-var-root #'tooling-repl bound-fn*)
    (alter-var-root #'accept-http bound-fn*)
    ;; Let the client know about *ns* changes
    (server/start-server {:address host :port port :name :replique
                          :accept `tooling-repl
                          :accept-http `accept-http
                          :server-daemon false})
    (println (str "Replique version " version " listening on port " (server/server-port)))
    (catch Throwable t
      (prn t))))

(comment
  (clojure.main/repl :prompt #())

  (let [o *out*]
    (Thread/setDefaultUncaughtExceptionHandler
     (reify Thread$UncaughtExceptionHandler
       (uncaughtException [_ thread ex]
         (binding [*out* o]
           (prn "e"))))))
  )

(defn shared-tooling-repl
  ([]
   (shared-tooling-repl :edn))
  ([print-format]
   (tooling-msg/set-print-format print-format)
   ;; Only load tooling stuff when it is necessary
   (require '[replique.tooling])
   (utils/with-lock tooling-msg/tooling-out-lock
     (alter-var-root #'tooling-msg/tooling-out (constantly *out*)))
   (utils/with-lock tooling-msg/tooling-out-lock
     (alter-var-root #'tooling-msg/tooling-err (constantly *err*)))
   (Thread/setDefaultUncaughtExceptionHandler
    (reify Thread$UncaughtExceptionHandler
      (uncaughtException [_ thread ex]
        (tooling-msg/uncaught-exception thread ex))))
   (let [init-fn (fn [] (in-ns 'replique.repl))]
     (clojure.main/repl
      :init init-fn
      :prompt #()
      :caught (fn [e] (tooling-msg/uncaught-exception (Thread/currentThread) e))
      :print (fn [result]
               (utils/with-lock tooling-msg/tooling-out-lock
                 (tooling-msg/tooling-prn result)))))))

(comment
  (.start (Thread. (fn [] (throw (Exception. "f")))))
  )

(defmethod utils/repl-type :replique/clj [repl-env]
  :clj)

(defmethod utils/repl-ns :replique/clj [repl-env]
  (ns-name *ns*))

(defn set-source-meta! [file line column]
  (set! *file* file)
  (.setLineNumber ^LineNumberingPushbackReader *in* line))

(defn options-with-repl-meta [{:keys [init caught print]
                               :as options-map}]
  (assert (and init print caught) "init print and caught are required")
  (assoc options-map
         :init (fn [] (init) (print-repl-meta))
         :print (fn [& args] (apply print args) (print-repl-meta))
         :caught (fn [& args] (apply caught args) (print-repl-meta))))

(defn repl []
  (apply clojure.main/repl (->> {:init (fn [] (in-ns 'user))
                                 :print prn :caught clojure.main/repl-caught}
                                options-with-repl-meta
                                (apply concat))))

;; Behavior of the socket REPL on repl closing
;; The read fn returns (end of stream), the parent REPL prints the result of the repl command.
;; At this point the socket is closed, writing to the socket may (or not) throw a socket
;; exception. If no exception is thrown, the REPL terminates and the same thing happens again
;; on the parent REPLs until their is no more REPL.
;; If a socket exception is thrown, the caugh function is triggered and may itself throw a
;; socket exception. The same thing happens on parent threads.
;; The socket server (at the top level) catches the socket exception and closes the session.

;; Behavior of the :init option of clojure repl
;; It seems (requireing) namespaces in the init fn sometimes throws an exception. I am not sure
;; why, maybe we just cannot dynamically (require) a namespace and immediately use its vars
;; without the use of (resolve)

