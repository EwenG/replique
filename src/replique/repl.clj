(ns replique.repl
  (:require [clojure.main]
            [replique.utils :as utils]
            [replique.tooling-msg :as tooling-msg]
            [replique.interactive :as interactive]
            [replique.server :as server])
  (:import [java.io File FileNotFoundException]))

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

(defn start-repl-process [project-map {:keys [process-id port cljs-compile-path version]}]
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
    (server/start-server {:port port :name :replique
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

(defn repl []
  (println "Clojure" (clojure-version))
  (if (tooling-msg/tooling-available?)
    (clojure.main/repl
     :init (fn [] (in-ns 'user))
     :caught (fn [e]
               (binding [*out* tooling-msg/tooling-err]
                 (utils/with-lock tooling-msg/tooling-out-lock
                   (tooling-msg/tooling-prn {:type :eval
                                             :process-id tooling-msg/process-id
                                             :error true
                                             :repl-type :clj
                                             :session server/*session*
                                             :ns (ns-name *ns*)
                                             :value (utils/repl-caught-str e)})))
               (clojure.main/repl-caught e))
     :print (fn [result]
              (binding [*out* tooling-msg/tooling-out]
                (utils/with-lock tooling-msg/tooling-out-lock
                  (tooling-msg/tooling-prn {:type :eval
                                            :process-id tooling-msg/process-id
                                            :repl-type :clj
                                            :session server/*session*
                                            :ns (ns-name *ns*)
                                            :result (pr-str result)})))
              (prn result)))
    (clojure.main/repl
     :init (fn [] (in-ns 'user)))))

(comment
  (tooling-msg/tooling-msg-handle {:type :clj-var-meta
                                   :context nil
                                   :ns 'replique.repl
                                   :symbol 'tooling-msg-handle
                                   :keys '(:column :line :file)})

  (tooling-msg/tooling-msg-handle {:type :clj-var-meta
                                   :context nil
                                   :ns 'replique.compliment.core
                                   :symbol 'all-sources
                                   :keys '(:column :line :file)})

  (tooling-msg/tooling-msg-handle {:type :clj-var-meta
                                   :context nil
                                   :ns 'replique.foo
                                   :symbol 'foo-bar
                                   :keys '(:column :line :file)})

  )

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
