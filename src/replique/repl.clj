(ns replique.repl
  (:require [replique.utils :as utils]
            [replique.tooling-msg :as tooling-msg]
            [clojure.core.server :as server]
            [replique.http-server :as http-server]
            [replique.source-meta]
            [clojure.stacktrace :refer [print-stack-trace]])
  (:import [clojure.lang LineNumberingPushbackReader Compiler]
           [java.net URL]
           [java.io File Reader PushbackReader]
           [java.nio.file Paths]))

(def ^:private dispatch-request
  (utils/dynaload 'replique.repl-cljs/dispatch-request))

(defn accept-http [request callback]
  (try (@dispatch-request request callback)
       (catch Exception e
         (tooling-msg/uncaught-exception (Thread/currentThread) e)
         {:status 500 :body (.getMessage e)})))

(defn tooling-repl []
  (clojure.main/repl
   :init (fn [] (in-ns 'replique.repl))
   :prompt #()
   :print (fn [result]
            (binding [*print-length* nil
                      *print-level* nil
                      *print-meta* nil]
              (tooling-msg/tooling-prn result)))))

(defn print-repl-meta []
  (when (and (tooling-msg/tooling-available?) server/*session*)
    (let [params (utils/repl-params utils/*repl-env*)]
      (binding [*out* tooling-msg/tooling-out
                *print-length* nil
                *print-level* nil
                *print-meta* nil]
        (utils/with-lock tooling-msg/tooling-out-lock
          (tooling-msg/tooling-prn {:type :repl-meta
                                    :process-id tooling-msg/process-id
                                    :session server/*session*
                                    ;; use dynamic vars for repl-type and repl-env in order to
                                    ;; come back to the right repl-type when leaving a repl
                                    :repl-type (utils/repl-type utils/*repl-env*)
                                    :repl-env utils/*repl-env*
                                    :ns (utils/repl-ns utils/*repl-env*)
                                    :params params}))))))

(defn start-repl-process [{:keys [host port process-id
                                  http-host http-port]}]
  (try
    ;; Leiningen specific --- Not needed anymore - kept here for information
    ;; clojure.main/repl sets the context class loader BEFORE the clojure.lang.Compiler/LOADER
    ;; var is bound. Thus when the clojure.lang.Compiler/LOADER var is bound, it shares a same
    ;; DynamicClassLoader ancestor than the context class loader.
    ;; Leiningen starts the process with an init script (clojure.main/repl is NOT directly used by
    ;; leiningen). When starting the process with an init script, the clojure.lang.Compiler/LOADER
    ;; var is bound BEFORE the context class loader is bound. In order for them to share a same
    ;; ancestor, we set the contextClassLoader here
    #_(.setContextClassLoader (Thread/currentThread) (deref Compiler/LOADER))
    (alter-var-root #'utils/process-out (constantly *out*))
    (alter-var-root #'utils/process-err (constantly *err*))
    (alter-var-root #'utils/host (constantly host))
    (alter-var-root #'utils/port (constantly port))
    (alter-var-root #'utils/http-host (constantly http-host))
    (alter-var-root #'utils/http-port (constantly http-port))
    (alter-var-root #'tooling-msg/process-id (constantly process-id))
    (let [user-init-script (File. (System/getProperty "user.home") ".replique/init.clj")
          project-init-script (File. (System/getProperty "user.dir") ".replique/init.clj")]
      (when (.exists user-init-script)
        (load-file (.getAbsolutePath user-init-script)))
      (when (.exists project-init-script)
        (load-file (.getAbsolutePath project-init-script))))
    ;; Make the cljs-compile-path absolute
    (let [cljs-compile-path (Paths/get utils/cljs-compile-path (make-array String 0))]
      (when-not (.isAbsolute cljs-compile-path)
        (let [absolute-path (Paths/get (System/getProperty "user.dir")
                                       (into-array String [utils/cljs-compile-path]))]
          (alter-var-root #'utils/cljs-compile-path (constantly (str absolute-path))))))
    (http-server/start-server {:address utils/http-host
                               :port utils/http-port
                               :accept `accept-http
                               :server-daemon true})
    (server/start-server {:address utils/host
                          :port utils/port
                          :name :replique
                          :accept `tooling-repl
                          :server-daemon false})
    (println (str "Replique listening on host " (pr-str (utils/server-host))
                  " and port " (utils/server-port) "\n"
                  "HTTP server listening on host " (pr-str (http-server/server-host))
                  " and port " (http-server/server-port)))
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

(defonce core-pr @#'pr)

(defn shared-tooling-repl
  ([]
   (shared-tooling-repl :edn))
  ([print-format]
   (tooling-msg/set-print-format print-format)
   ;; Only load tooling stuff when it is necessary
   (utils/with-lock tooling-msg/tooling-out-lock
     (alter-var-root #'tooling-msg/tooling-out (constantly *out*)))
   (utils/with-lock tooling-msg/tooling-out-lock
     (alter-var-root #'tooling-msg/tooling-err (constantly *err*)))
   (Thread/setDefaultUncaughtExceptionHandler
    (reify Thread$UncaughtExceptionHandler
      (uncaughtException [_ thread ex]
        (tooling-msg/uncaught-exception thread ex))))
   (try
     ;; Make clojure.core/pr dynamic in order to be able to hook into clojure.core/pr in order
     ;; to record printed objects as data
     (.setDynamic #'clojure.core/pr true)
     (let [init-fn (fn []
                     (in-ns 'replique.repl)
                     (require '[replique.tooling]))]
       (clojure.main/repl
        :init init-fn
        :prompt #()
        :caught (fn [e] (tooling-msg/uncaught-exception (Thread/currentThread) e))
        :print (fn [result]
                 (binding [*print-length* nil
                           *print-level* nil
                           *print-meta* nil]
                   (utils/with-lock tooling-msg/tooling-out-lock
                     (tooling-msg/tooling-prn result))))))
     (catch Exception ex
       (.println System/err (with-out-str (print-stack-trace ex)))
       (System/exit -1)))))

(comment
  (.start (Thread. (fn [] (throw (Exception. "f")))))
  )

(defmethod utils/repl-type :replique/clj [repl-env]
  :clj)

(defmethod utils/repl-ns :replique/clj [repl-env]
  (ns-name *ns*))

(defmethod utils/repl-params :replique/clj [repl-env]
  {"clojure.core/*print-level*" *print-level*
   "clojure.core/*print-length*" *print-length*
   "clojure.core/*print-meta*" *print-meta*
   "clojure.core/*warn-on-reflection*" *warn-on-reflection*})

(defn set-source-meta! []
  (let [char (.read ^Reader *in*)
        _ (.unread ^PushbackReader *in* char)
        {:keys [url line column]} @replique.source-meta/source-meta
        url (try (URL. url) (catch Exception _ nil))
        path (when url (utils/url->path url))]
    (set! *file* (or path "NO_SOURCE_PATH"))
    (.setLineNumber ^LineNumberingPushbackReader *in* (or line 1))))

(defn repl-read [request-prompt request-exit]
  (set-source-meta!)
  (clojure.main/repl-read request-prompt request-exit))

;; Wrap functions are called with var args because the clj repl and the cljs repl options
;; do not have the same arity
(defn options-with-repl-meta [{:keys [init caught print]
                               :as options-map}]
  (assert (and init print caught) "init print and caught are required")
  (assoc options-map
         :init (fn [] (init) (print-repl-meta))
         :print (fn [& args] (apply print args) (print-repl-meta))
         :caught (fn [& args] (apply caught args) (print-repl-meta))))

(defn repl-print [x]
  (binding [pr core-pr]
    (prn x)))

(defn repl-caught [e]
  (binding [pr core-pr]
    (clojure.main/repl-caught e)))

(defn repl-prompt []
  (binding [pr core-pr]
    (clojure.main/repl-prompt)))

(defn repl
  ([]
   (repl repl-print))
  ([print-result-fn]
   (binding [*file* *file*]
     (apply clojure.main/repl (->> {:init (fn [] (in-ns 'user))
                                    :print print-result-fn
                                    :caught repl-caught
                                    :read repl-read
                                    :prompt repl-prompt}
                                   options-with-repl-meta
                                   (apply concat))))))

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
