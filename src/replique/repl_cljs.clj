(ns replique.repl-cljs
  (:refer-clojure :exclude [load-file])
  (:require [replique.utils :as utils]
            [replique.tooling-msg :as tooling-msg]
            [replique.http :as http]
            [replique.server :refer [*session*] :as server]
            [replique.environment :refer [->CljsCompilerEnv]]
            [clojure.java.io :as io]
            [cljs.closure :as closure]
            [cljs.env :as cljs-env]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.util]
            [cljs.repl]
            [clojure.string :as string]
            [cljs.js-deps :as deps]
            [cljs.stacktrace :as st]
            [clojure.edn :as edn]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as readers]
            [replique.cljs]
            #_[replique.npm-deps :as npm-deps]
            [replique.source-meta])
  (:import [java.io File PushbackReader]
           [java.nio.file Files Paths Path]
           [java.net URL]
           [java.util.concurrent Executors SynchronousQueue TimeUnit
            RejectedExecutionException ExecutorService TimeoutException CancellationException]
           [clojure.lang IExceptionInfo]
           [java.util.regex Pattern]
           [com.google.common.base Throwables]))

(let [{cljs-major :major
       cljs-minor :minor
       cljs-qualifier :qualifier} cljs.util/*clojurescript-version*]
  (assert (or (> cljs-major 1) (> cljs-minor 9)
              (and (= cljs-major 1) (= cljs-minor 9)
                   (>= cljs-qualifier 473)))
          (format "Replique is compatible with clojurescript 1.9.473+, current version is: %s.%s.%s" cljs-major cljs-minor cljs-qualifier)))

(declare init-repl-env)
(declare init-compiler-env)

(defonce repl-env (utils/delay (init-repl-env)))
(defonce compiler-env (utils/delay (init-compiler-env @repl-env)))

(defonce cljs-outs (atom #{}))
(def ^:dynamic *stopped-eval-executor?* false)
(defonce cljs-core-bindings #{#'*assert* #'*print-length* #'*print-meta* #'*print-level*
                              #'*flush-on-newline* #'*print-readably* #'*print-dup*})

;; Used to send runtime REPL params to the Replique client (*print-length*, *print-level* ...)
;; Also used to reset set! these values when the js runtime reconnects to the REPL
;; Unlike with the Clojure REPL these values are thus global to all cljs REPLs !
;; repl-params are initialized to the same value than the ones of the Clojure process
(defonce repl-params (atom {"cljs.core/*assert*" *assert*
                            "cljs.core/*print-length*" *print-length*
                            "cljs.core/*print-meta*" *print-meta*
                            "cljs.core/*print-level*" *print-level*
                            "cljs.core/*flush-on-newline*" *flush-on-newline*
                            "cljs.core/*print-readably*" *print-readably*
                            "cljs.core/*print-dup*" *print-dup*}))

(def env {:context :expr :locals {}})

(defn dispatcher [{:keys [method path content]} callback]
  (cond (and (= :get method) (= path "/"))
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
        :print
        (and (= :post method) (= :print-tooling (:type content)))
        :print-tooling
        (and (= :get method))
        :assets))

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
    {:status 200
     :content-type "text/html"
     :body (str "<html>
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
<script src=\"replique/cljs_env/bootstrap.js\"></script>
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
          (let [mime-type (http/ext->mime-type ext)
                encoding (if (contains? http/text-encoding mime-type) "UTF-8" "ISO-8859-1")]
            {:status 200
             :body (slurp local-path :encoding encoding)
             :content-type mime-type
             :encoding encoding})
          {:status 200 :body (slurp local-path)})
        (http/make-404 path)))
    (http/make-404 path)))

(defprotocol ISubmitted
  (is-submitted? [this]))

(deftype EvalTask [js ^:unsynchronized-mutable submitted?]
  Callable
  (call [this]
    (if *stopped-eval-executor?*
      {:status :error :value "Connection broken"}
      (let[{:keys [js-queue result-queue]} @server/cljs-server]
        (try
          (.put ^SynchronousQueue js-queue js)
          (set! submitted? true)
          (.take ^SynchronousQueue result-queue)
          ;; If the repl-env is shutdown
          (catch InterruptedException e
            {:status :error :value "Connection broken"})))))
  ISubmitted
  (is-submitted? [this] submitted?))

(defn init-core-bindings []
  `(do
     ~@(for [[k v] @repl-params]
         `(~'set! ~(symbol k) ~v))))

(defn compile-dependency? [source ns-info]
  (not= (:ns source) (:ns ns-info)))

(defn replique-compile-dependencies [source repl-opts opts]
  (binding [ana/*reload-macros* replique.cljs/*reload-all*]
    (let [opts (-> repl-opts
                   (merge opts)
                   ;; :force -> force compilation (require-compile? -> true)
                   (assoc ;; :interactive -> do not clear namespace analysis data
                          ;; before reloading
                          :mode :interactive))
          compile-dependency? (partial compile-dependency? source)
          sources (closure/add-dependency-sources #{source} opts)
          dependencies-sources (filter compile-dependency? sources)
          dependencies-sources (deps/dependency-order dependencies-sources)
          compiled (closure/compile-sources dependencies-sources opts)
          compiled (map #'closure/add-core-macros-if-cljs-js compiled)
          compiled (conj compiled source)
          all-sources (-> compiled
                          (closure/add-js-sources opts)
                          deps/dependency-order
                          closure/add-goog-base)]
      (doseq [source all-sources]
        (closure/source-on-disk opts source))
      all-sources)))

(defn f->src [f]
  (cond (cljs.util/url? f) f
        (.exists (io/file f)) (io/file f)
        :else (io/resource f)))

(defn repl-compile-cljs
  ([f repl-opts opts]
   (repl-compile-cljs f repl-opts opts true))
  ([f repl-opts opts reload-macros]
   (let [src (f->src f)
         ;; prior to cljs 1.9.456, closure/src-file->target-file returned a relative path. After
         ;; cljs 1.9.456, closure/src-file->target-file returns an absolute path. Let's normalize
         ;; the path
         output-path (Paths/get ^String (:output-dir opts) (make-array String 0))
         target-path (.toPath (closure/src-file->target-file src))
         target-path (.resolve output-path target-path)
         output-file (.toFile ^Path (.relativize output-path target-path))
         ns-info (ana/parse-ns src output-file opts)
         dependencies-sources (replique-compile-dependencies ns-info repl-opts opts)
         compiled (binding [ana/*reload-macros* (or reload-macros replique.cljs/*reload-all*)
                            ana/*analyze-deps* true]
                    (closure/compile
                     src
                     (assoc opts
                            :output-file output-file
                            :force true
                            :mode :interactive)))]
     ;; copy over the original source file if source maps enabled
     (when-let [ns (and (:source-map opts) (first (:provides ns-info)))]
       (spit
        (io/file (io/file (cljs.util/output-directory opts))
                 (cljs.util/ns->relpath ns (cljs.util/ext (:source-url compiled))))
        (slurp src)))
     ns-info)))

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

(defn repl-eval-compiled [compiled repl-env f opts]
  (let [src (f->src f)]
    (cljs.repl/-evaluate
     repl-env "<cljs repl>" 1
     (slurp (str (cljs.util/output-directory opts)
                 File/separator "cljs_deps.js")))
    (cljs.repl/-evaluate
     repl-env f 1
     (replique.cljs/src-file->goog-require
      src {:wrap true
           :reload true
           :reload-all replique.cljs/*reload-all*
           :macros-ns (:macros-ns compiled)}))))

(defn load-javascript
  "Accepts a REPL environment, a list of namespaces, and a URL for a
  JavaScript file which contains the implementation for the list of
  namespaces. Will load the JavaScript file into the REPL environment
  if any of the namespaces have not already been loaded from the
  ClojureScript REPL."
  [repl-env provides url]
  (cljs.repl/-evaluate repl-env nil nil (slurp url)))

(defn mapped-stacktrace [stacktrace opts]
  (with-out-str
    (doseq [{:keys [function file line column]}
            (cljs.repl/mapped-stacktrace stacktrace opts)]
      (println "\t"
               (str (when function (str function " "))
                    "(" file (when line (str ":" line)) (when column (str ":" column)) ")")))))

;; resolve the stacktrace using sourcemaps when the evaluation result contains a stacktrace
(defn handle-stacktrace [repl-env ret]
  (if (= :success (:status ret))
    (try
      (let [cst (cljs.repl/-parse-stacktrace
                 repl-env (:stacktrace ret) ret cljs.repl/*repl-opts*)]
        (if (vector? cst)
          (->> (mapped-stacktrace cst cljs.repl/*repl-opts*)
               (str (:value ret))
               (assoc ret :value))
          ret))
      (catch Throwable e
        {:status :error
         :value (Throwables/getStackTraceAsString e)}))
    ret))

(defn evaluate-form [repl-env js & {:keys [timeout-before-submitted]}]
  (let [port (server/server-port)
        {:keys [state eval-executor]} @server/cljs-server]
    (cond
      (= :stopped state)
      {:status :error
       :value (format "Waiting for browser to connect on port %d ..." port)}
      :else (try (let [eval-task (->EvalTask js false)
                       eval-future (.submit ^ExecutorService eval-executor
                                            ^Callable eval-task)
                       result (if timeout-before-submitted
                                ;; Timeout before the task gets submitted, useful to implement a
                                ;; timeout on evaluations initiated by a tooling msg
                                ;; (autocompletion, watch ...) in order to avoid blocking the
                                ;; thread when the js runtime is not available
                                (try
                                  (.get eval-future timeout-before-submitted TimeUnit/MILLISECONDS)
                                  (catch TimeoutException e
                                    (when-not (is-submitted? eval-task)
                                      ;; eval tasks must ony be interrupted on
                                      ;; deconnection/reconnection
                                      (.cancel eval-future false))
                                    (.get eval-future)))
                                (.get eval-future))]
                   (reset! repl-params (:params result))
                   (handle-stacktrace repl-env result))
                 (catch RejectedExecutionException e
                   {:status :error
                    :value "Connection broken"})
                 (catch CancellationException e
                   {:status :error
                    :value "Cancelled"})))))

;; Defrecord instead of deftype because cljs.repl uses the repl-env as a hashmap. Why ??? 
(defrecord BrowserEnv [repl-opts]
  cljs.repl/IJavaScriptEnv
  (-setup [this opts] nil)
  (-evaluate [this _ _ js]
    (evaluate-form this js))
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
                                   :stacktrace (.-stack ~e)})))))
  cljs.repl/IPrintStacktrace
  (-print-stacktrace [repl-env stacktrace error build-options]
    ;; (:value error) was already printed by repl.cljc
    ;; we don't want to print the full stracktrace here
    ))

(defn in-ns* [repl-env ns-name]
  (when-not (ana/get-namespace ns-name)
    (swap! cljs-env/*compiler*
           assoc-in [::ana/namespaces ns-name]
           {:name ns-name})
    (cljs.repl/-evaluate
     repl-env "<cljs repl>" 1 (str "goog.provide('" (comp/munge ns-name) "');")))
  (set! ana/*cljs-ns* ns-name))

;; Customize cljs.repl/wrap-fn to be able to print the sourcemapped stacktrace of errors
;; Customize cljs.repl/wrap-fn to add the result of the evaluation to the results
;; watcher atom
(defn- wrap-fn [form]
  (cond
    (and (seq? form)
         (#{'ns 'require 'require-macros
            'use 'use-macros 'import 'refer-clojure} (first form)))
    identity

    ('#{*1 *2 *3 *e} form) (fn [x] `(let [x# ~x]
                                      (if (cljs.core/instance? js/Error x#)
                                        x#
                                        (cljs.core/pr-str x#))))
    :else
    (fn [x]
      `(try
         (let [ret# ~x]
           (set! *3 *2)
           (set! *2 *1)
           (set! *1 ret#)
           (if (cljs.core/instance? js/Error ret#)
             ret#
             (do
               (cljs.core/pr-str ret#)
               (reset! replique.cljs-env.watch/results ret#))))
         (catch :default e#
           (set! *e e#)
           (throw e#))))))

(defn init-repl-env []
  ;; Merge repl-opts in browserenv because clojurescript expects this. This is weird
  (let [repl-opts {:analyze-path []
                   :static-dir [utils/cljs-compile-path]
                   :wrap #'wrap-fn}]
    (merge (BrowserEnv. repl-opts) repl-opts
           ;; st/parse-stacktrace expects host host-port and port to be defined on the repl-env
           {:host "localhost" :host-port (server/server-port) :port (server/server-port)})))

#_(comment
  (cljs-env/with-compiler-env @compiler-env
    (npm-deps/handle-js-modules))

  (keys (:options @@compiler-env))
  )

(defn init-compiler-env [repl-env]
  (let [comp-opts {:output-to (str (File. ^String utils/cljs-compile-path "main.js"))
                   :output-dir utils/cljs-compile-path
                   :optimizations :none
                   :recompile-dependents false
                   ;; We want the analysis data to be cleared on process restart
                   :cache-analysis false
                   ;; Do not automatically install node deps. This must be done explicitly instead
                   :install-deps false}
        repl-opts (cljs.repl/-repl-options repl-env)
        compiler-env (-> comp-opts
                         closure/add-implicit-options
                         cljs-env/default-compiler-env)]
    (closure/load-data-readers! compiler-env)

    (cljs-env/with-compiler-env compiler-env

      ;; process and index :npm-deps
      #_(let [opts (npm-deps/handle-js-modules)]
        (swap! compiler-env update-in [:options] merge opts))

      (comp/with-core-cljs nil
        (fn []
          (let [repl-src "replique/cljs_env/repl.cljs"
                benv-src "replique/cljs_env/browser.cljs"
                jfxenv-src "replique/cljs_env/javafx.cljs"
                repl-compiled (repl-compile-cljs repl-src repl-opts  comp-opts false)
                benv-compiled (repl-compile-cljs benv-src repl-opts comp-opts false)
                jfx-compiled (repl-compile-cljs jfxenv-src repl-opts comp-opts false)]
            (let [cljs-deps-path (str (cljs.util/output-directory comp-opts)
                                      File/separator "cljs_deps.js")]
              (when-not (.exists (File. cljs-deps-path))
                (->> (refresh-cljs-deps comp-opts)
                     (closure/output-deps-file
                      (assoc comp-opts :output-to cljs-deps-path)))))
            (doto (io/file (cljs.util/output-directory comp-opts) "goog" "deps.js")
              cljs.util/mkdirs (spit (slurp (io/resource "goog/deps.js"))))
            (spit (File. ^String utils/cljs-compile-path "replique/cljs_env/bootstrap.js")
                  (slurp (io/resource "bootstrap.js")))))))

    (spit (File. ^String utils/cljs-compile-path ".repliqueignore") "")

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
               (closure/-compile
                [`(~'ns ~'cljs.user)
                 `(swap! replique.cljs-env.repl/connection
                         assoc (keyword "session") ~(inc session))
                 '(set! *print-fn* replique.cljs-env.repl/repl-print)
                 '(set! *print-err-fn* replique.cljs-env.repl/repl-print)
                 '(set! *print-newline* true)
                 '(when (pos? (count replique.cljs-env.repl/print-queue))
                    (replique.cljs-env.repl/flush-print-queue!))
                 `(~'set! ~'replique.cljs-env.repl/*process-id* ~tooling-msg/process-id)
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
      {:status 200 :body js :content-type "text/javascript"})))

(defmethod dispatch-request :result [{:keys [content]} callback]
  (let [{:keys [result-queue js-queue result-executor]} @server/cljs-server
        result-task (reify Callable
                      (call [this]
                        (try
                          (.put ^SynchronousQueue result-queue (read-string (:content content)))
                          (try
                            (callback {:status 200
                                       :content-type "text/javascript"
                                       :body (.take ^SynchronousQueue js-queue)})
                            (catch InterruptedException e (throw e))
                            ;; Socket closed ...
                            (catch Exception e
                              (.put ^SynchronousQueue result-queue
                                    {:status :error :value "Connection broken"})))
                          (catch InterruptedException e
                            (try (callback
                                  {:status 409 :body "Connection closed"
                                   :content-type "text/plain"})
                                 (catch Exception e nil))))))]
    (try (.submit ^ExecutorService result-executor result-task)
         (catch RejectedExecutionException e
           {:status 409 :body "Connection closed" :content-type "text/plain"}))))

(defmethod dispatch-request :print [{:keys [content]} callback]
  ;; Maybe we should print only in the currently active REPL instead of all REPLs
  (doseq [out @cljs-outs]
    (binding [*out* out]
      (print (:content content))
      (.flush *out*)))
  {:status 200 :body "ignore__" :content-type "text/plain"})

(defmethod dispatch-request :print-tooling [{:keys [content]} callback]
  (binding [*out* tooling-msg/tooling-out]
    (utils/with-lock tooling-msg/tooling-out-lock
      (.append *out* ^String (:content content))
      (flush)))
  {:status 200 :body "ignore__" :content-type "text/plain"})

(defn call-post-eval-hooks [repl-env prev-comp-env comp-env]
  (doseq [[ns-sym f] (:replique/ns-watches comp-env)]
    (when-not (identical? (-> prev-comp-env :cljs.analyzer/namespaces (get ns-sym) :defs)
                          (-> comp-env :cljs.analyzer/namespaces (get ns-sym) :defs))
      (f repl-env comp-env)))
  (doseq [[ns-sym vars-map] (:replique/var-watches comp-env)]
    (doseq [[var-sym f] vars-map]
      (when-not (identical? (-> prev-comp-env :cljs.analyzer/namespaces (get ns-sym)
                                :defs (get var-sym))
                            (-> comp-env :cljs.analyzer/namespaces (get ns-sym)
                                :defs (get var-sym)))
        (f repl-env comp-env)))))

;; patch cljs.repl/eval-cljs in order to add the possibility to define post-eval hooks
;; patch cljs.repl/eval-cljs to correctly set the file name
(defn eval-cljs [repl-env env form opts]
  (let [comp-env @@compiler-env
        eval-result (cljs.repl/evaluate-form
                     repl-env
                     (assoc env :ns (ana/get-namespace ana/*cljs-ns*))
                     (get (meta form) :file "NO_SOURCE_FILE")
                     form
                     ;; the pluggability of :wrap is needed for older JS runtimes like Rhino
                     ;; where catching the error will swallow the original trace
                     ((or (:wrap opts) wrap-fn) form)
                     opts)]
    (call-post-eval-hooks repl-env comp-env @@compiler-env)
    eval-result))

(defn eval-cljs-form
  ([repl-env form] (eval-cljs-form repl-env form nil))
  ([repl-env form {:keys [ns warnings]
                   :or {ns ana/*cljs-ns*
                        warnings ana/*cljs-warnings*}}]
   (binding [ana/*cljs-ns* ns
             ana/*cljs-warnings* warnings]
     (cljs-env/with-compiler-env @compiler-env
       (eval-cljs repl-env env form cljs.repl/*repl-opts*)))))

(comment
  (evaluate-form @repl-env "alert(\"e\");" :timeout-before-submitted 1000)
  )

(defn tooling-form->js [ns form]
  (binding [ana/*analyze-deps* false]
    (cljs-env/with-compiler-env @compiler-env
      (let [ast (ana/analyze (assoc env
                                    :ns ns
                                    :def-emits-var true)
                             form nil nil)]
        (comp/emit-str ast)))))

(defmethod utils/repl-ns :replique/cljs [repl-env]
  ana/*cljs-ns*)

(defmethod utils/repl-type :replique/cljs [repl-env]
  :cljs)

(defmethod utils/repl-params :replique/cljs [repl-env]
  (select-keys @repl-params ["cljs.core/*print-length*"
                             "cljs.core/*print-level*"
                             "cljs.core/*print-meta*"]))

;; Customized REPL to allow exiting the REPL with :cljs/quit
;; Also always reset the reader even is :source-map-inline is false in order to set the metadata on
;; code evaluated from a buffer
;; Delay the creation of the new reader after the source meta has been set by the tooling REPL
(defn repl-read-with-exit [exit-keyword]
  (fn repl-read
    ([request-prompt request-exit]
     (repl-read request-prompt request-exit cljs.repl/*repl-opts*))
    ([request-prompt request-exit opts]
     ;; Wait for something to come in in order to delay the creation of the new reader
     (.unread ^PushbackReader *in* (.read ^PushbackReader *in*))
     (let [current-in *in*]
       (binding [*in* ((:reader opts))]
         (or ({:line-start request-prompt :stream-end request-exit}
              (cljs.repl/skip-whitespace *in*))
             (let [input (reader/read {:read-cond :allow :features #{:cljs}} *in*)]
               ;; Transfer 1-char buffer to original *in*
               (readers/unread current-in (readers/read-char *in*))
               (cljs.repl/skip-if-eol current-in)
               (if (= input exit-keyword)
                 request-exit
                 input))))))))

(defprotocol ReplLoadFile
  (-load-file
    [repl-env file-path]
    [repl-env file-path opts]))

(defprotocol IReplEval
  (-evaluate-form [this js & opts]))

(defn compile-file [repl-env file-path opts]
  (cljs-env/with-compiler-env @compiler-env
    (let [repl-opts (cljs.repl/-repl-options repl-env)
          compiled (repl-compile-cljs file-path repl-opts opts)]
      (->> (refresh-cljs-deps opts)
           (closure/output-deps-file
            (assoc opts :output-to
                   (str (cljs.util/output-directory opts)
                        File/separator "cljs_deps.js"))))
      compiled)))

(defn load-file [repl-env file-path]
  (let [opts (:options @@compiler-env)
        compiled (compile-file repl-env file-path opts)]
    (:value (repl-eval-compiled compiled repl-env file-path opts))))

;; Ensure a namespace is loaded in the compiler-env. If not, compiles it but does not load it
(defn ensure-compiled [repl-env namespace]
  (let [namespace (if (string? namespace) (symbol namespace) namespace)
        {:keys [uri]} (closure/cljs-source-for-namespace namespace)]
    (when uri
      (when-not (get-in @@compiler-env [::ana/namespaces namespace])
        (let [opts (:options @@compiler-env)]
          (compile-file repl-env (.getPath ^URL uri) opts))))))

(defn cljs-repl [main-namespace]
  (let [repl-env @repl-env
        compiler-env @compiler-env
        {:keys [state]} @server/cljs-server
        repl-opts (replique.repl/options-with-repl-meta
                   {:compiler-env compiler-env
                    ;; Code modifying the runtime should not be put in :init, otherwise it
                    ;; would be lost on browser refresh
                    :init (fn [] (in-ns* repl-env 'cljs.user))
                    ;; cljs results are strings, so we must not print with prn
                    :print println
                    :caught cljs.repl/repl-caught
                    :read (repl-read-with-exit :cljs/quit)
                    :reader #(let [{:keys [url line column]} @replique.source-meta/source-meta
                                   url (try (URL. url) (catch Exception _ nil))
                                   path (when url (utils/url->path url))]
                               (replique.cljs/source-logging-push-back-reader
                                *in* 1 (or path "NO_SOURCE_FILE") (or line 1) (or column 1)))})]
    (when main-namespace (ensure-compiled repl-env main-namespace))
    (when (not= :started state)
      (println (format "Waiting for browser to connect on port %d ..." (server/server-port))))
    (swap! cljs-outs conj *out*)
    (try
      (binding [utils/*repl-env* :replique/browser]
        (apply
         (partial replique.cljs/repl repl-env)
         (->> (merge (:options @compiler-env) repl-opts {:eval eval-cljs})
              (apply concat))))
      (finally (swap! cljs-outs disj *out*)))))

(defn stop-cljs-server []
  (let [{:keys [eval-executor result-executor]} @server/cljs-server]
    (swap! server/cljs-server assoc :state :stopped)
    (when eval-executor (shutdown-eval-executor eval-executor))
    (when result-executor (.shutdownNow ^ExecutorService result-executor))))

(extend-type BrowserEnv
  ReplLoadFile
  (-load-file [repl-env file-path]
    (load-file repl-env file-path nil))
  (-load-file [repl-env file-path opts]
    (binding [replique.cljs/*reload-all* (boolean (contains? opts :reload-all))]
      (load-file repl-env file-path)))
  IReplEval
  (-evaluate-form [this js & opts]
    (apply evaluate-form this js opts)))

(defn set-repl-verbose [b]
  (set! cljs.repl/*cljs-verbose* b))

#_(defn install-node-deps! []
  (replique.cljs/with-version
    [0 0 0]
    [1 9 494]
    (prn ":npm-deps is not supported by clojurescript version "
         cljs.util/*clojurescript-version*))
  (replique.cljs/with-version
    [1 9 518]
    [nil nil nil]
    (closure/maybe-install-node-deps! (assoc (:options @@compiler-env)
                                             :verbose true))))

(derive :replique/browser :replique/cljs)

(defmethod tooling-msg/tooling-msg-handle [:replique/browser :list-css] [msg]
  (tooling-msg/with-tooling-response msg
    (let [{:keys [status value]} (cljs.repl/-evaluate
                                  @repl-env "<cljs repl>" 1
                                  "replique.cljs_env.browser.list_css_urls();")]
      (if (not (= :success status))
        (assoc msg :error value)
        (assoc msg :css-urls (read-string value))))))

(defmethod tooling-msg/tooling-msg-handle [:replique/browser :load-css] [{:keys [url] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [{:keys [status value]} (->> (pr-str url)
                                      (format "replique.cljs_env.browser.reload_css(%s);")
                                      (cljs.repl/-evaluate @repl-env "<cljs repl>" 1))]
      (if (not (= :success status))
        (assoc msg :error value)
        (assoc msg :result value)))))

(defn load-js [repl-env {:keys [file-path] :as msg}]
  (let [{:keys [status value]} (->> (slurp file-path)
                                    (cljs.repl/-evaluate repl-env "<cljs repl>" 1))]
    (if (not (= :success status))
      (assoc msg :error value)
      (assoc msg :result value))))

(defmethod tooling-msg/tooling-msg-handle [:replique/browser :load-js] [msg]
  (tooling-msg/with-tooling-response msg
    (load-js @repl-env msg)))

(defn tooling-eval-cljs [repl-env {:keys [form] :as msg}]
  (binding [*out* utils/process-out
            *err* utils/process-err]
    (let [form (reader/read-string {:read-cond :allow :features #{:cljs}} form)
          result (cljs-env/with-compiler-env @compiler-env
                   (eval-cljs repl-env env form (:repl-opts repl-env)))]
      (assoc msg :result result))))

(defmethod tooling-msg/tooling-msg-handle [:replique/browser :eval] [msg]
  (tooling-msg/with-tooling-response msg
    (tooling-eval-cljs @repl-env msg)))


;; stacktraces:
;; Sourcemaps are only handled for errors sent via the evaluation result cljs REPL communication
;; channel (ie not for errors printed on the client side)
;; Unlike clojure, cljs errors do not print with their stacktrace
;; It would not be possible to resolve the sourcemaps on the client side anyway
;; Replique do not extend the printing of errors to include their stacktrace because the behavior
;; would be different between cljs code started with/without replique
;; Errors printed as evaluation results are not printed as data. Their printing is REPL specific
;; anyway

;; cljs files are loaded by appending a <script> tag to the page. Errors during a load-file
;; are handled by a global error handler, which prints the error

;; (require 'xxx :reload-all) does not work in cljs. Thus :reload-all must be done through
;; load-file
