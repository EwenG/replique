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

(defn- request-dispatcher [{:keys [headers]} callback]
  (:user-agent headers))

(defmulti dispatch-request request-dispatcher)

(defn accept-http [request callback]
  (try (dispatch-request request callback)
       (catch Exception e
         (tooling-msg/uncaught-exception (Thread/currentThread) e)
         {:status 500 :body (.getMessage e)})))

(defn tooling-repl []
  (.setContextClassLoader (Thread/currentThread) utils/root-dynamic-classloader)
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

;; Modified namespace prefixes during an eval / load-file called, among the
;; namespace prefixes registered in utils/clj-env-hooks
(defonce modified-namespace-prefixes (atom #{}))

(defn update-modified-namespace-prefixes
  ([namespace-prefix]
   (when-not (contains? @modified-namespace-prefixes namespace-prefix)
     (swap! modified-namespace-prefixes conj namespace-prefix)))
  ([namespace-prefix r k o n]
   (when-not (contains? @modified-namespace-prefixes namespace-prefix)
     (swap! modified-namespace-prefixes conj namespace-prefix))))

(defn add-var-watches [namespace-prefix namespace]
  (let [watch-fn (partial update-modified-namespace-prefixes namespace-prefix)
        watch-k (keyword (str *ns*) (str "post-eval-watch-" namespace-prefix))]
    (doseq [[k v] (ns-interns namespace)]
      (when-not (contains? (.getWatches v) watch-k)
        (add-watch v watch-k watch-fn)))))

(defn matching-namespace-prefixes [namespace-prefixes namespace]
  (filter #(.startsWith (str namespace) (str %)) namespace-prefixes))

(add-watch
 utils/clj-env-hooks
 ::update-post-eval-hooks
 (fn [k r o n]
   (let [namespace-prefixes (keys n)]
     (when (seq namespace-prefixes)
       (doseq [namespace (all-ns)]
         (let [namespace-prefixes (matching-namespace-prefixes namespace-prefixes namespace)]
           (when (seq namespace-prefixes)
             (let [mapping (ns-map namespace)]
               (doseq [namespace-prefix namespace-prefixes]
                 (add-var-watches namespace-prefix namespace))
               (alter-meta! namespace assoc ::prev-mapping mapping)))))))))

(defn post-eval-hook []
  (let [env-hooks @utils/clj-env-hooks
        namespace-prefixes (keys env-hooks)]
    (when (seq namespace-prefixes)
      (doseq [namespace (all-ns)]
        (let [namespace-prefixes (matching-namespace-prefixes namespace-prefixes namespace)]
          (when (seq namespace-prefixes)
            (let [prev-mapping (::prev-mapping (meta namespace))
                  mapping (ns-map namespace)]
              (when-not (identical? prev-mapping mapping)
                (doseq [namespace-prefix namespace-prefixes]
                  (update-modified-namespace-prefixes namespace-prefix)
                  (add-var-watches namespace-prefix namespace))
                (alter-meta! namespace assoc ::prev-mapping mapping))))))
      (doseq [namespace-prefix @modified-namespace-prefixes]
        (when-let [f (get env-hooks namespace-prefix)]
          (try (f)
               (catch Exception e
                 (reset! modified-namespace-prefixes #{})
                 (throw e))))))
    (reset! modified-namespace-prefixes #{})))

(defn start-repl-process [{:keys [host port process-id
                                  http-host http-port]}]
  (try
    ;; All REPLs threads should share a common DynamicClassLoader for the classpath updates to be visibile by all REPLs. Also the Compiler/LOADER classloader must be a child of the root DynamicClassLoader for the Clojure compiler to be able to see classpath updates.
    (let [cl (clojure.lang.DynamicClassLoader. (.getContextClassLoader (Thread/currentThread)))]
      (alter-var-root #'utils/root-dynamic-classloader (constantly cl)))
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
    (let [http-server (http-server/start-server {:address utils/http-host
                                                 :port utils/http-port
                                                 :accept `accept-http
                                                 :server-daemon true})
          repl-server (server/start-server {:address utils/host
                                            :port utils/port
                                            :name :replique
                                            :accept `tooling-repl
                                            :server-daemon false})]
      (alter-var-root #'utils/http-server (constantly http-server))
      (alter-var-root #'utils/repl-server (constantly repl-server)))
    (println (str "Replique listening on host " (pr-str (utils/server-host utils/repl-server))
                  " and port " (utils/server-port utils/repl-server) "\n"
                  "HTTP server listening on host " (pr-str (utils/server-host utils/http-server))
                  " and port " (utils/server-port utils/http-server)))
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

(def ^:dynamic *repl-context* nil)

(defn repl
  "Like clojure.main/repl but with repl-meta printing, post-eval-hook and hooks *repl-context*."
  [& options]
  (let [cl (.getContextClassLoader (Thread/currentThread))]
    (.setContextClassLoader (Thread/currentThread) (clojure.lang.DynamicClassLoader. cl)))
  (let [{:keys [init need-prompt prompt flush read eval print caught]
         :or {init        #()
              need-prompt (if (instance? LineNumberingPushbackReader *in*)
                            #(.atLineStart ^LineNumberingPushbackReader *in*)
                            #(identity true))
              prompt      clojure.main/repl-prompt
              flush       flush
              read        clojure.main/repl-read
              eval        eval
              print       prn
              caught      clojure.main/repl-caught}}
        (apply hash-map options)
        request-prompt (Object.)
        request-exit (Object.)
        read-eval-print
        (fn []
          (try
            (let [read-eval *read-eval*
                  input (clojure.main/with-read-known
                          (set-source-meta!)
                          (binding [*repl-context* :read]
                            (read request-prompt request-exit)))]
              (or (#{request-prompt request-exit} input)
                  (let [value (binding [*read-eval* read-eval]
                                (binding [*repl-context* :eval]
                                  (eval input)))]
                    (binding [*repl-context* :print]
                      (print value))
                    (set! *3 *2)
                    (set! *2 *1)
                    (set! *1 value))))
            (catch Throwable e
              (binding [*repl-context* :caught]
                (caught e))
              (set! *e e)))
          (print-repl-meta)
          (post-eval-hook))]
    (clojure.main/with-bindings
      (binding [*file* *file*]
        (try
          (binding [*repl-context* :init]
            (init))
          (catch Throwable e
            (binding [*repl-context* :caught]
              (caught e))
            (set! *e e)))
        (print-repl-meta)
        (binding [*repl-context* :prompt]
          (prompt))
        (binding [*repl-context* :flush]
          (flush))
        (loop []
          (when-not 
              (try (identical? (read-eval-print) request-exit)
                   (catch Throwable e
                     (binding [*repl-context* :caught]
                       (caught e))
                     (set! *e e)
                     nil))
            (when (binding [*repl-context* :need-prompt]
                    (need-prompt))
              (binding [*repl-context* :prompt]
                (prompt))
              (binding [*repl-context* :flush]
                (flush)))
            (recur)))))))


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
