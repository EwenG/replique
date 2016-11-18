(ns replique.repl
  (:require [clojure.main]
            [clojure.java.io :refer [file]]
            [replique.elisp-printer :as elisp]
            [replique.utils :as utils]
            [replique.tooling-msg :as tooling-msg]
            [replique.tooling]
            [replique.interactive :as interactive]
            [replique.server :as server])
  (:import [java.io File FileNotFoundException]))

(defonce ^:dynamic *files-specs* {})

(def watched-bindings
  [#'clojure.core/*data-readers* 
   #'clojure.core/*print-level* #'clojure.core/*default-data-reader-fn*
   #'clojure.core/*print-length* #'clojure.core/*read-eval*
   #'clojure.core/*print-meta* #'clojure.core/*assert*
   #'clojure.core/*unchecked-math* #'clojure.core/*warn-on-reflection*
   #'clojure.core/*compile-path* #'clojure.core/*command-line-args*
   #'clojure.core/*math-context* #'*files-specs*])

(utils/with-1.9+ (alter-var-root
                  #'watched-bindings conj
                  #'clojure.core/*print-namespace-maps*
                  #'clojure.spec/*explain-out*))

(defn normalize-host [address]
  (cond (= "0.0.0.0" address) "localhost"
        (= "0:0:0:0:0:0:0:1" address) "localhost"
        :else address))

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
   :print (fn [result] (elisp/prn result))))

(defn restore-bindings [bindings]
  (doseq [[v-name v-val] bindings]
    (try
      (var-set (resolve (symbol v-name)) (read-string v-val))
      ;; Don't try to restore the binding in case of exception
      (catch Exception e nil))))

(defn init-and-print-session [bindings]
  (restore-bindings bindings)
  server/*session*)

(defn start-repl-process [{:keys [port directory replique-vars] :as opts}]
  (try
    (alter-var-root #'interactive/process-out (constantly *out*))
    (alter-var-root #'interactive/process-err (constantly *err*))
    (alter-var-root #'tooling-msg/directory (constantly directory))
    (let [{:keys [files-specs]} replique-vars]
      (alter-var-root #'*files-specs* (constantly files-specs))
      (alter-var-root #'utils/cljs-compile-path (constantly (:cljs-compile-path replique-vars))))
    (println "Starting Clojure REPL...")
    ;; Let leiningen :global-vars option propagate to other REPLs
    ;; The tooling REPL printing is a custom one and thus is not affected by those bindings,
    ;; and it must not !!
    (alter-var-root #'tooling-repl bound-fn*)
    (server/start-server {:port port :name :replique
                          :accept `tooling-repl
                          :accept-http `accept-http
                          :server-daemon false})
    (elisp/prn {:host (let [ss  (-> @#'server/servers (get :replique) :socket)
                            inet (.getInetAddress ^java.net.ServerSocket ss)
                            ip (.getHostAddress ^java.net.InetAddress inet)]
                        (normalize-host ip))
                :port (server/server-port)
                :directory (.getAbsolutePath (file "."))})
    (catch Throwable t
      (elisp/prn {:error t}))))

(comment
  (clojure.main/repl :prompt #())

  (let [o *out*]
    (Thread/setDefaultUncaughtExceptionHandler
     (reify Thread$UncaughtExceptionHandler
       (uncaughtException [_ thread ex]
         (binding [*out* o]
           (prn "e"))))))
  )

(defn shared-tooling-repl []
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
                (elisp/prn result))))))

(defmethod tooling-msg/tooling-msg-handle :shutdown [msg]
  (tooling-msg/with-tooling-response msg
    (server/stop-server)
    {:shutdown true}))

(comment
  (.start (Thread. (fn [] (throw (Exception. "f")))))
  )

(defn add-file-spec [file-path spec]
  {:pre [(string? file-path) (keyword? spec) (namespace spec)]}
  (let [rel-path (.relativize (.toPath (file ".")) (.toPath (file file-path)))]
    (set! *files-specs* (assoc *files-specs* rel-path spec))))

(defn remove-custom-tooling-file [file-path spec]
  {:pre [(string? file-path) (keyword? spec) (namespace spec)]}
  (let [rel-path (.relativize (.toPath (file ".")) (.toPath (file file-path)))]
    (set! *files-specs* (dissoc *files-specs* rel-path spec))))

(defn repl-eval
  "Enhanced :eval hook for saving bindings"
  [form]
  (let [prev-bindings (mapv deref watched-bindings)
        evaled (eval form)
        next-bindings (mapv deref watched-bindings)]
    (doseq [[prev-b next-b v] (map vector prev-bindings next-bindings watched-bindings)
            :when (not (identical? prev-b next-b))]
      (let [{v-name :name v-ns :ns} (meta v)]
        (when (and v-name v-ns)
          (binding [*out* tooling-msg/tooling-err]
            (utils/with-lock tooling-msg/tooling-out-lock
              (elisp/prn {:type :binding
                          :directory tooling-msg/directory
                          :repl-type :clj
                          :session server/*session*
                          :ns (ns-name *ns*)
                          :var (str (symbol (str v-ns) (str v-name)))
                          :value (pr-str @v)}))))))
    evaled))

(defn repl []
  (println "Clojure" (clojure-version))
  (clojure.main/repl
   :init (fn [] (in-ns 'user))
   :eval repl-eval
   :caught (fn [e]
             (binding [*out* tooling-msg/tooling-err]
               (utils/with-lock tooling-msg/tooling-out-lock
                 (elisp/prn {:type :eval
                             :directory tooling-msg/directory
                             :error true
                             :repl-type :clj
                             :session server/*session*
                             :ns (ns-name *ns*)
                             :value (utils/repl-caught-str e)})))
             (clojure.main/repl-caught e))
   :print (fn [result]
            (binding [*out* tooling-msg/tooling-out]
              (utils/with-lock tooling-msg/tooling-out-lock
                (elisp/prn {:type :eval
                            :directory tooling-msg/directory
                            :repl-type :clj
                            :session server/*session*
                            :ns (ns-name *ns*)
                            :result (pr-str result)})))
            (prn result))))

#_ (defn format-meta [{:keys [file] :as meta} keys]
  (let [f (and file (File. file))]
    (if (and f (.exists f))
      (select-keys (assoc
                    meta :file
                    (.getAbsolutePath f))
                   keys)
      (select-keys meta (disj keys :file :line :column)))))

#_(defmethod tooling-msg/tooling-msg-handle :clj-var-meta
  [{:keys [context ns symbol keys] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [ctx (when context (read-string context))
          ctx (compliment.context/parse-context ctx)
          bindings (bindings-from-context ctx)
          keys (into #{} keys)]
      (cond
        (or (nil? ns) (nil? symbol) (nil? (find-ns ns)))
        {:meta nil}
        (and ctx (contains? (into #{} bindings) (name symbol)))
        {:not-found :local-binding}
        :else
        (let [v (when (symbol? symbol)
                  (try (ns-resolve ns symbol)
                       (catch ClassNotFoundException e
                         nil)))
              meta (when (and v (meta v))
                     (format-meta (meta v) keys))]
          (if (empty? meta)
            {:meta nil}
            {:meta meta}))))))

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
