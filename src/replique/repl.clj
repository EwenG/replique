(ns replique.repl
  (:require [clojure.main]
            [replique.utils :as utils]
            [replique.tooling-msg :as tooling-msg]
            [replique.interactive :as interactive]
            [replique.server :as server]
            [replique.tooling]))

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

(defn on-ns-change [new-ns]
  (when (and (tooling-msg/tooling-available?) server/*session*)
    (binding [*out* tooling-msg/tooling-out]
      (utils/with-lock tooling-msg/tooling-out-lock
        (tooling-msg/tooling-prn {:type :in-ns
                                  :process-id tooling-msg/process-id
                                  :session server/*session*
                                  :repl-type :clj
                                  :ns (ns-name new-ns)}))))
  new-ns)

(defn start-repl-process [project-map {:keys [process-id host port cljs-compile-path version]}]
  (try
    (alter-var-root #'utils/process-out (constantly *out*))
    (alter-var-root #'utils/process-err (constantly *err*))
    (alter-var-root #'interactive/process-out (constantly *out*))
    (alter-var-root #'interactive/process-err (constantly *err*))
    (alter-var-root #'utils/project-map (constantly project-map))
    (alter-var-root #'tooling-msg/process-id (constantly process-id))
    (alter-var-root #'utils/cljs-compile-path (constantly cljs-compile-path))
    (alter-var-root #'utils/version (constantly version))
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
   (let [init-fn (fn []
                   (in-ns 'replique.repl)
                   (alter-var-root #'in-ns (fn [wrapped] (comp on-ns-change wrapped))))]
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

(defmethod utils/repl-ns :clj [repl-type]
  (ns-name *ns*))

(defn repl []
  (clojure.main/repl :init (fn [] (in-ns 'user))))

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

;; Keeping track of the REPL current ns is done by mutating the in-ns (clj) var and customizing
;; the in-ns special fn (cljs)
;; Keeping track of the repl type is done by sending a tooling message on startup of a repl of
;; a new type, and when quitting the REPL

