(ns replique.repl-cljs
  (:refer-clojure :exclude [load-file in-ns])
  (:require [replique.elisp-printer :as elisp]
            [replique.utils :as utils]
            [replique.tooling-msg :as tooling-msg]
            [replique.http :as http]
            [replique.server :refer [*session*] :as server]
            [replique.sourcemap :as sourcemap]
            [clojure.java.io :as io]
            [cljs.closure :as closure]
            [cljs.env :as cljs-env]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.util]
            [cljs.repl]
            [clojure.string :as string]
            [cljs.js-deps :as deps]
            [cljs.closure :as cljsc]
            [cljs.stacktrace :as st]
            [clojure.edn :as edn]
            [replique.environment :refer [->CljsCompilerEnv]])
  (:import [java.io File BufferedReader InputStreamReader]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.concurrent Executors SynchronousQueue
            RejectedExecutionException ExecutorService]
           [clojure.lang IExceptionInfo]
           [java.util.regex Pattern]
           [java.util.concurrent.locks ReentrantLock]))

(declare init-repl-env)
(declare init-compiler-env)

(defonce repl-env (utils/delay (init-repl-env)))
(defonce compiler-env (utils/delay (init-compiler-env @repl-env)))

(defonce cljs-outs (atom #{}))
(def ^:dynamic *stopped-eval-executor?* false)
(defonce cljs-core-bindings #{#'*assert* #'*print-length* #'*print-meta* #'*print-level*
                              #'*flush-on-newline* #'*print-readably* #'*print-dup*})

(def env {:context :expr :locals {}})

(defn dispatcher [{:keys [method path content]} callback]
  (cond (and (= :get method) (some #(.endsWith ^String path %) (keys http/ext->mime-type)))
        :assets
        (and (= :get method) (= path "/"))
        :init
        (and (= :post method) (= :ready (:type content)))
        :ready
        (and (= :post method)
             (not= :ready (:type content))
             (not= (:session content) (:session @server/cljs-server)))
        :session-expired
        (and (= :post method) (= :result (:type content)))
        :result
        (and (= :post method) (= :print (:type content)))
        :print))

(defmulti dispatch-request dispatcher)

(defmethod dispatch-request :session-expired [request callback]
  {:status 500 :body "Session expired" :content-type "text/plain"})

(defmethod dispatch-request :default [request callback]
  {:status 500 :body (format "Cannot handle request %s" (str request))
   :content-type "text/plain"})

(defn shutdown-eval-executor [executor]
  (let [pendingTasks (.shutdownNow ^ExecutorService executor)]
        ;; Tasks are run on this thread
        (binding [*stopped-eval-executor?* true]
          (doseq [task pendingTasks]
            (.run ^Runnable task)))))

(defmethod dispatch-request :init [{{host :host} :headers} callback]
  (let [url (format "http://%s" host)]
    {:status 200 :body (str "<html>
<head></head>
<body>
<script>var CLOSURE_UNCOMPILED_DEFINES = null;</script>
<script src=\"goog/base.js\"></script>
<script src=\"cljs_deps.js\"></script>
<script>
goog.require(\"replique.cljs_env.repl\");
</script>
<script>
goog.require(\"replique.cljs_env.browser\");
</script>
<script>
replique.cljs_env.repl.connect(\"" url "\");
</script>
</body>
</html>")}))

(defmethod dispatch-request :assets [{path :path :as request} callback]
  (if (not= "/favicon.ico" path)
    (let [path (if (= "/" path) "/index.html" path)
          local-path (cond->
                         (seq (for [x [utils/cljs-compile-path]
                                    :when (.exists (io/file (str x path)))]
                                (str x path)))
                       (complement nil?)
                       first)
          local-path (if (nil? local-path)
                       (cond
                         (re-find #".jar" path)
                         (io/resource (second (string/split path #".jar!/")))
                         (re-find (Pattern/compile (System/getProperty "user.dir")) path)
                         (-> (string/replace path (str (System/getProperty "user.dir") "/") "")
                             io/file)
                         :else nil)
                       local-path)]
      (if local-path
        (if-let [ext (some #(if (.endsWith ^String path %) %) (keys http/ext->mime-type))]
          (let [mime-type (http/ext->mime-type ext "text/plain")
                encoding (http/mime-type->encoding mime-type "UTF-8")]
            {:status 200
             :body (slurp local-path :encoding encoding)
             :content-type mime-type
             :encoding encoding})
          {:status 200 :body (slurp local-path) :content-type "text/plain"})
        (http/make-404 path)))
    (http/make-404 path)))

(defn make-eval-task [js]
  (reify Callable
    (call [this]
      (if *stopped-eval-executor?*
        {:status :error :value "Connection broken"}
        (let[{:keys [js-queue result-queue]} @server/cljs-server]
          (try
            (.put ^SynchronousQueue js-queue js)
            (.take ^SynchronousQueue result-queue)
            ;; If the repl-env is shutdown
            (catch InterruptedException e
              {:status :error :value "Connection broken"})))))))

(defn init-core-bindings []
  `(do
     ~@(for [v cljs-core-bindings]
         `(~'set! ~(symbol "cljs.core" (-> v meta :name str)) ~(deref v)))))

(defn f->src [f]
  (cond (cljs.util/url? f) f
        (.exists (io/file f)) (io/file f)
        :else (io/resource f)))

(defn repl-compile-cljs
  ([f opts]
   (repl-compile-cljs f opts true))
  ([f opts reload-macros]
   (let [src (f->src f)
         compiled (binding [ana/*reload-macros* reload-macros]
                    (closure/compile
                     src
                     (assoc opts
                            :output-file
                            (closure/src-file->target-file src)
                            :force true
                            :mode :interactive)))]
     ;; copy over the original source file if source maps enabled
     (when-let [ns (and (:source-map opts) (first (:provides compiled)))]
       (spit
        (io/file (io/file (cljs.util/output-directory opts))
                 (cljs.util/ns->relpath ns (cljs.util/ext (:source-url compiled))))
        (slurp src)))
     compiled)))

(defn foreign->output-file [foreign opts]
  (let [output-path (closure/rel-output-path
                     (assoc foreign :foreign true)
                     opts)]
    (assoc foreign :file output-path)))

(defn refresh-cljs-deps [opts]
  (let [parse-js-fn (fn [js-file]
                      (-> js-file
                          slurp
                          string/split-lines
                          deps/parse-js-ns
                          (assoc :file js-file)))
        is-goog (fn [js-file]
                  (some #(.startsWith ^String % "goog.")
                        (:provides js-file)))
        ups-foreign-libs (:ups-foreign-libs opts)
        js-files (deps/find-js-fs (:output-dir opts))
        js-files (map parse-js-fn js-files)
        js-files (filter #(and (seq (:provides %))
                               (not (is-goog %)))
                         js-files)
        js-files (map closure/map->javascript-file js-files)
        js-files (->> ups-foreign-libs
                      (map #(foreign->output-file % opts))
                      (into js-files))]
    (deps/dependency-order js-files)))

(defn repl-cljs-on-disk [compiled repl-opts opts]
  (let [sources (closure/add-dependencies
                 (merge repl-opts opts)
                 compiled)]
    (doseq [source sources]
      (closure/source-on-disk opts source))))

(defn repl-eval-compiled [compiled repl-env f opts]
  (let [src (f->src f)]
    (cljs.repl/-evaluate
     repl-env "<cljs repl>" 1
     (slurp (str (cljs.util/output-directory opts)
                 File/separator "cljs_deps.js")))
    (cljs.repl/-evaluate
     repl-env f 1 (closure/add-dep-string opts compiled))
    (cljs.repl/-evaluate
     repl-env f 1
     (closure/src-file->goog-require
      src {:wrap true :reload true :macros-ns (:macros-ns compiled)}))))

(defn load-javascript
  "Accepts a REPL environment, a list of namespaces, and a URL for a
  JavaScript file which contains the implementation for the list of
  namespaces. Will load the JavaScript file into the REPL environment
  if any of the namespaces have not already been loaded from the
  ClojureScript REPL."
  [repl-env provides url]
  (cljs.repl/-evaluate repl-env nil nil (slurp url)))

(defn evaluate-form [js]
  (let [port (server/server-port)
        {:keys [state eval-executor]} @server/cljs-server]
    (cond
      (= :stopped state)
      {:status :error
       :value (format "Waiting for browser to connect on port %d ..." port)}
      :else (try (.get (.submit ^ExecutorService eval-executor
                                ^Callable (make-eval-task js)))
                 (catch RejectedExecutionException e
                   {:status :error
                    :value "Connection broken"})))))

;; Defrecord instead of deftype because cljs.repl uses the repl-env as a hashmap. Why ??? 
(defrecord BrowserEnv [repl-opts]
  cljs.repl/IJavaScriptEnv
  (-setup [this opts] nil)
  (-evaluate [this _ _ js]
    (evaluate-form js))
  (-load [this provides url]
    (load-javascript this provides url))
  ;; We don't want the repl-env to be closed on cljs-repl exit
  (-tear-down [this] nil)
  cljs.repl/IReplEnvOptions
  (-repl-options [this] repl-opts)
  cljs.repl/IParseStacktrace
  (-parse-stacktrace [this st err opts]
    (st/parse-stacktrace this st err opts))
  cljs.repl/IGetError
  (-get-error [this e env opts]
    (edn/read-string
     (cljs.repl/evaluate-form this env "<cljs repl>"
                              `(when ~e
                                 (pr-str
                                  {:ua-product (clojure.browser.repl/get-ua-product)
                                   :value (str ~e)
                                   :stacktrace (.-stack ~e)}))))))

(defn init-repl-env []
  ;; Merge repl-opts in browserenv because clojurescript expects this. This is weird
  (let [repl-opts {:analyze-path []
                   :static-dir [utils/cljs-compile-path]}]
    (merge (BrowserEnv. repl-opts) repl-opts
           ;; st/parse-stacktrace expects host host-port and port to be defined on the repl-env
           {:host "localhost" :host-port (server/server-port) :port (server/server-port)})))

(defn init-compiler-env [repl-env]
  (let [comp-opts {:output-to (str (File. ^String utils/cljs-compile-path "main.js"))
                   :output-dir utils/cljs-compile-path
                   :optimizations :none
                   :recompile-dependents false
                   :preloads ['replique.cljs_env.repl
                              'replique.cljs_env.browser]
                   :cache-analysis false}
        compiler-env (-> comp-opts
                         closure/add-implicit-options
                         cljs-env/default-compiler-env)]
    (cljs-env/with-compiler-env compiler-env
      (comp/with-core-cljs nil
        (fn []
          (let [repl-src "replique/cljs_env/repl.cljs"
                benv-src "replique/cljs_env/browser.cljs"
                repl-compiled (repl-compile-cljs repl-src comp-opts false)
                benv-compiled (repl-compile-cljs benv-src comp-opts false)]
            (repl-cljs-on-disk repl-compiled (cljs.repl/-repl-options repl-env) comp-opts)
            (repl-cljs-on-disk benv-compiled (cljs.repl/-repl-options repl-env) comp-opts)
            (->> (refresh-cljs-deps comp-opts)
                 (closure/output-deps-file
                  (assoc comp-opts :output-to
                         (str (cljs.util/output-directory comp-opts)
                              File/separator "cljs_deps.js"))))
            (doto (io/file (cljs.util/output-directory comp-opts) "goog" "deps.js")
              cljs.util/mkdirs (spit (slurp (io/resource "goog/deps.js"))))
            #_(output-main-file comp-opts port)))))
    compiler-env))

;; This must be executed on a single thread (the server thread for example)
(defmethod dispatch-request :ready [request callback]
  (let [compiler-env @compiler-env
        _ (swap! server/cljs-server assoc :state :stopped)
        {:keys [result-executor eval-executor js-queue result-queue session]
         :or {session 0}} @server/cljs-server
        new-eval-executor (Executors/newSingleThreadExecutor)
        new-result-executor (Executors/newSingleThreadExecutor)
        new-js-queue (SynchronousQueue.)
        new-result-queue (SynchronousQueue.)]
    (when eval-executor (shutdown-eval-executor eval-executor))
    (when result-executor (.shutdownNow ^ExecutorService result-executor))
    ;; Init stuff needs to go there and not in the :init method of the REPL, otherwise it
    ;; get lost on browser refresh
    (let [js (cljs-env/with-compiler-env compiler-env
               (cljsc/-compile
                [`(~'ns ~'cljs.user)
                 `(swap! replique.cljs-env.repl/connection
                         assoc :session ~(inc session))
                 '(set! *print-fn* replique.cljs-env.repl/repl-print)
                 '(set! *print-err-fn* replique.cljs-env.repl/repl-print)
                 '(set! *print-newline* true)
                 '(when (pos? (count replique.cljs-env.repl/print-queue))
                    (replique.cljs-env.repl/flush-print-queue!))
                 (init-core-bindings)]
                {}))]
      (.submit ^ExecutorService new-eval-executor
               (reify Callable
                 (call [this]
                   (.take ^SynchronousQueue new-result-queue))))
      (swap! server/cljs-server assoc
             :eval-executor new-eval-executor
             :result-executor new-result-executor
             :js-queue new-js-queue
             :result-queue new-result-queue
             :session (inc session)
             :state :started)
      {:status 200 :body js})))

(defmethod dispatch-request :result [{:keys [content]} callback]
  (let [{:keys [result-queue js-queue result-executor]} @server/cljs-server
        result-task (reify Callable
                      (call [this]
                        (try
                          (.put ^SynchronousQueue result-queue (read-string (:content content)))
                          (try
                            (callback {:status 200 :body (.take ^SynchronousQueue js-queue)})
                            (catch InterruptedException e (throw e))
                            ;; Socket closed ...
                            (catch Exception e
                              (.put ^SynchronousQueue result-queue
                                    {:status :error :value "Connection broken"})))
                          (catch InterruptedException e
                            (try (callback
                                  {:status 500 :body "Connection closed"
                                   :content-type "text/plain"})
                                 (catch Exception e nil))))))]
    (try (.submit ^ExecutorService result-executor result-task)
         (catch RejectedExecutionException e
           {:status 500 :body "Connection closed" :content-type "text/plain"}))))

(defmethod dispatch-request :print [{:keys [content]} callback]
  ;; Maybe we should print only in the currently active REPL instead of all REPLs
  (doseq [[out out-lock] @cljs-outs]
    (binding [*out* out]
      (utils/with-lock out-lock
        (-> (:content content) read-string print)
        (.flush *out*))))
  {:status 200 :body "ignore__" :content-type "text/plain"})

(defn repl-caught [e repl-env opts]
  (binding [*out* tooling-msg/tooling-err]
    (utils/with-lock tooling-msg/tooling-out-lock
      (-> {:type :eval
           :directory tooling-msg/directory
           :error true
           :repl-type :cljs
           :session *session*
           :ns ana/*cljs-ns*
           :value (if (and (instance? IExceptionInfo e)
                           (#{:js-eval-error :js-eval-exception}
                            (:type (ex-data e))))
                    (:value (:error (ex-data e)))
                    (utils/repl-caught-str e))}
          elisp/prn)))
  (cljs.repl/repl-caught e repl-env opts))

(defn cljs-repl []
  (let [repl-env @repl-env
        compiler-env @compiler-env
        out-lock (ReentrantLock.)
        {:keys [state]} @server/cljs-server]
    (swap! cljs-outs conj [*out* out-lock])
    (when (not= :started state)
      (println (format "Waiting for browser to connect on port %d ..." (server/server-port))))
    (apply
     (partial cljs.repl/repl repl-env)
     (->> (merge
           (:options @compiler-env)
           {:compiler-env compiler-env
            :caught repl-caught
            :print (fn [result]
                     (binding [*out* tooling-msg/tooling-out]
                       (utils/with-lock tooling-msg/tooling-out-lock
                         (elisp/prn {:type :eval
                                     :directory tooling-msg/directory
                                     :repl-type :cljs
                                     :session *session*
                                     :ns ana/*cljs-ns*
                                     :result result})))
                     (utils/with-lock out-lock
                       (println result)))
            ;; Code modifying the runtime should not be put in :init, otherise it would be lost
            ;; on browser refresh
            :init (fn []
                    ;; Let the client know that we are entering a cljs repl
                    (binding [*out* tooling-msg/tooling-out]
                      (utils/with-lock tooling-msg/tooling-out-lock
                        (elisp/prn {:type :eval
                                    :directory tooling-msg/directory
                                    :repl-type :cljs
                                    :session *session*
                                    :ns ana/*cljs-ns*
                                    :result "nil"}))))})
          (apply concat)))
    (swap! cljs-outs disj [*out* out-lock])))

(defn stop-cljs-server []
  (let [{:keys [eval-executor result-executor]} @server/cljs-server]
    (swap! server/cljs-server assoc :state :stopped)
    (when eval-executor (shutdown-eval-executor eval-executor))
    (when result-executor (.shutdownNow ^ExecutorService result-executor))))

(defmethod tooling-msg/tooling-msg-handle :shutdown [msg]
  (tooling-msg/with-tooling-response msg
    (stop-cljs-server)
    (server/stop-server)
    {:shutdown true}))

(defn load-file [file-path]
  (cljs-env/with-compiler-env @compiler-env
    (let [opts (:options @@compiler-env)
          compiled (repl-compile-cljs file-path opts)]
      (repl-cljs-on-disk
       compiled (#'cljs.repl/env->opts @repl-env) opts)
      (->> (refresh-cljs-deps opts)
           (closure/output-deps-file
            (assoc opts :output-to
                   (str (cljs.util/output-directory opts)
                        File/separator "cljs_deps.js"))))
      (:value (repl-eval-compiled compiled @repl-env file-path opts)))))

(defn in-ns [ns-quote]
  (let [[quote ns-name] (vec ns-quote)]
    (when-not (and (= 'quote quote) (symbol? ns-name))
      (throw (IllegalArgumentException. "Argument to in-ns must be a symbol.")))
    (when-not (ana/get-namespace ns-name)
      (swap! cljs-env/*compiler*
             assoc-in [::ana/namespaces ns-name]
             {:name ns-name})
      (cljs.repl/-evaluate
       @repl-env "<cljs repl>" 1
       (str "goog.provide('" (comp/munge ns-name) "');")))
    (set! ana/*cljs-ns* ns-name)))

(defn set-repl-verbose [b]
  (set! cljs.repl/*cljs-verbose* b))

(defmethod tooling-msg/tooling-msg-handle :list-cljs-namespaces [msg]
  (tooling-msg/with-tooling-response msg
    (->> (->CljsCompilerEnv @compiler-env)
         replique.environment/all-ns
         (map str)
         (assoc msg :namespaces))))

(defmethod tooling-msg/tooling-msg-handle :list-css [msg]
  (tooling-msg/with-tooling-response msg
    (let [{:keys [status value]} (cljs.repl/-evaluate
                                  @repl-env "<cljs repl>" 1
                                  "replique.cljs_env.browser.list_css_infos();")]
      (if (= :error status)
        (assoc msg :error value)
        (assoc msg :css-infos (read-string value))))))

(comment
  (require '[goog.dom])
  (.appendChild (.-head js/document) (goog.dom/createDom "link" (js-obj "rel" "stylesheet" "type" "text/css" "href" "mystyle.css")))
  
  )

(defn css-infos-process-uri [{:keys [file-path uri scheme] :as css-infos}]
  (if (= "data" scheme)
    (assoc css-infos :uri
           (->> (slurp file-path)
                sourcemap/encode-base-64
                (str "data:text/css;base64,")))
    css-infos))

(defmethod tooling-msg/tooling-msg-handle :load-css [msg]
  (tooling-msg/with-tooling-response
    (let [{:keys [status value]} (->> (css-infos-process-uri msg)
                                      pr-str pr-str
                                      (format "replique.cljs_env.browser.reload_css(%s);")
                                      (cljs.repl/-evaluate @repl-env "<cljs repl>" 1)
                                      :value)]
      (if (= :error status)
        (assoc msg :error value)
        (assoc msg :result value)))))
