(ns replique.cljs
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.tools.reader :as reader]
   [clojure.tools.reader.reader-types :as readers]
   [cljs.tagged-literals :as tags]
   [cljs.util :as util]
   [cljs.compiler :as comp]
   [cljs.analyzer :as ana]
   [cljs.env :as env]
   [cljs.js-deps :as deps]
   [cljs.closure :as cljsc]

   [cljs.repl :refer [repl-caught repl-quit-prompt repl-read repl-prompt known-repl-opts
                      -repl-options read-source-map *cljs-verbose* *repl-opts*
                      default-special-fns -setup evaluate-form analyze-source err-out
                      -tear-down]])
  (:import [java.io FileWriter PrintWriter Closeable]))

;;Patch cljs.closure/output-main-file in order to:
;; - Avoid the need to provide an :asset-path option. :asset-path is
;; computed from the :main namespaces. When using a node.js env,
;; :output-dir is used instead of :asset-path
;; - Allow multiple :main namespaces. This permits leaving HTML markup
;; identical between dev and production even when multiple namespaces
;; are called at startup
#_(alter-var-root
 #'cljsc/output-main-file
 (constantly
  (fn output-main-file [opts]
    (let [closure-defines (json/write-str (:closure-defines opts))]
      (case (:target opts)
        :nodejs
        (let [asset-path (or (:asset-path opts)
                              (util/output-directory opts))]
          (cljsc/output-one-file
           opts
           (cljsc/add-header
            opts
            (str
             "var path = require(\"path\");\n"
             "try {\n"
             "    require(\"source-map-support\").install();\n"
             "} catch(err) {\n"
             "}\n"
             "require(path.join(path.resolve(\".\"),\"" asset-path "\",\"goog\",\"bootstrap\",\"nodejs.js\"));\n"
             "require(path.join(path.resolve(\".\"),\"" asset-path "\",\"cljs_deps.js\"));\n"
             "goog.global.CLOSURE_UNCOMPILED_DEFINES = " closure-defines ";\n"
             (when (:main opts)
               (str "goog.require(\"" (comp/munge (:main opts)) "\");\n"))
             "goog.require(\"cljs.nodejscli\");\n"))))
        (let [output-dir-uri (-> (:output-dir opts) (File.) (.toURI))
              output-to-uri (-> (:output-to opts) (File.) (.toURI))
              output-dir-path (-> (.normalize output-dir-uri)
                                  (.toString))
              output-to-path (-> (.normalize output-to-uri)
                                 (.toString))
              ;; If output-dir is not a parent dir of output-to, then
              ;; we don't try to infer the asset path because it may not
              ;; be possible.
              rel-path (if (and (.startsWith output-to-path
                                             output-dir-path)
                                (not= output-dir-path output-to-path))
                         (-> (.relativize output-dir-uri output-to-uri)
                             (.toString))
                         nil)]
          (cljsc/output-one-file
           opts
           (str "(function() {\n"
                "var assetPath = " (compute-asset-path (:asset-path opts) (util/output-directory opts) rel-path)
                "var CLOSURE_UNCOMPILED_DEFINES = " closure-defines ";\n"
                "if(typeof goog == \"undefined\") document.write('<script src=\"'+ assetPath +'/goog/base.js\"></script>');\n"
                "document.write('<script src=\"'+ assetPath +'/cljs_deps.js\"></script>');\n"
                (when (:main opts)
                  (str "document.write('<script>if (typeof goog != \"undefined\") { goog.require(\"" (comp/munge (:main opts)) "\"); } else { console.warn(\"ClojureScript could not load :main, did you forget to specify :asset-path?\"); };</script>');"))
                  "})();\n"))))))))

;; Patch output-unoptimzed to always output cljs-deps into cljs_deps.js
;; If output-to is defined and main is not defined, a main file is written
;; which does not goog.require any namespace
#_(alter-var-root
 #'cljsc/output-unoptimized
 (constantly
  (fn output-unoptimized
    [opts & sources]
    (let [disk-sources (remove #(= (:group %) :goog)
                               (map #(cljsc/source-on-disk opts %) sources))
          goog-deps    (io/file (util/output-directory opts)
                                "goog" "deps.js")
          main         (:main opts)]
      (util/mkdirs goog-deps)
      (spit goog-deps (slurp (io/resource "goog/deps.js")))
      (cljsc/output-deps-file
       (assoc opts :output-to
              (str (util/output-directory opts)
                   File/separator "cljs_deps.js"))
       disk-sources)
      (cljsc/output-main-file opts)))))

(defmacro with-version [[min-major min-minor min-qualifier]
                        [max-major max-minor max-qualifier] & body]
  (let [{cljs-major :major
         cljs-minor :minor
         cljs-qualifier :qualifier} cljs.util/*clojurescript-version*]
    (when
        (and
         (or (> cljs-major min-major) (> cljs-minor min-minor)
             (and (= cljs-major min-major) (= cljs-minor min-minor)
                  (>= cljs-qualifier min-qualifier)))
         (or (nil? max-major) (nil? max-minor) (nil? max-qualifier)
             (< cljs-major max-major) (< cljs-minor max-minor)
             (and (= cljs-major max-major) (= cljs-minor max-minor)
                  (<= cljs-qualifier max-qualifier))))
      `(do ~@body))))

;; patch repl* to remove the binding of *print-namespace-maps*, which is broken
;; (*print-namespace-maps* is set whatever the value of the init parameter). A clojurescript
;; patch would not be accepted because cljs devs think this is fine ...
;; Also put the :cljs/quit "magic keyword" handling logic out of repl*

(with-version
  [1 9 854]
  [nil nil nil]
  (defn repl*
    [repl-env {:keys [init need-prompt quit-prompt prompt flush read eval print caught reader
                      print-no-newline source-map-inline wrap repl-requires
                      compiler-env bind-err]
               :or {need-prompt #(if (readers/indexing-reader? *in*)
                                   (== (readers/get-column-number *in*) 1)
                                   (identity true))
                    quit-prompt repl-quit-prompt
                    prompt repl-prompt
                    flush flush
                    read repl-read
                    eval #'cljs.repl/eval-cljs
                    print println
                    caught repl-caught
                    reader #(readers/source-logging-push-back-reader
                             *in*
                             1 "NO_SOURCE_FILE")
                    print-no-newline print
                    source-map-inline true
                    repl-requires '[[cljs.repl :refer-macros [source doc find-doc apropos dir pst]]
                                    [cljs.pprint :refer [pprint] :refer-macros [pp]]]
                    bind-err true}
               :as opts}]
    (doseq [[unknown-opt suggested-opt] (util/unknown-opts (set (keys opts)) (set/union known-repl-opts cljsc/known-opts))]
      (when suggested-opt
        (println (str "WARNING: Unknown option '" unknown-opt "'. Did you mean '" suggested-opt "'?"))))
    (let [repl-opts (-repl-options repl-env)
          repl-requires (into repl-requires (:repl-requires repl-opts))
          {:keys [analyze-path repl-verbose warn-on-undeclared special-fns
                  checked-arrays static-fns fn-invoke-direct]
           :as opts
           :or   {warn-on-undeclared true}}
          (merge
           {:cache-analysis true :source-map true :def-emits-var true}
           (cljsc/add-implicit-options
            (merge-with (fn [a b] (if (nil? b) a b))
                        repl-opts
                        opts
                        {:prompt prompt
                         :need-prompt need-prompt
                         :flush flush
                         :read read
                         :print print
                         :caught caught
                         :reader reader
                         :print-no-newline print-no-newline
                         :source-map-inline source-map-inline})))
          done? (atom false)]
      (env/with-compiler-env (or compiler-env (env/default-compiler-env opts))
        (when (:source-map opts)
          (.start (Thread. (bound-fn [] (read-source-map "cljs/core.aot.js")))))
        (binding [ana/*unchecked-if* false
                  ana/*unchecked-arrays* false
                  *err* (if bind-err
                          (cond-> *out*
                            (not (instance? PrintWriter *out*)) (PrintWriter.))
                          *err*)
                  ana/*cljs-ns* ana/*cljs-ns*
                  *cljs-verbose* repl-verbose
                  ana/*cljs-warnings*
                  (let [warnings (opts :warnings)]
                    (merge
                     ana/*cljs-warnings*
                     (if (or (true? warnings)
                             (false? warnings))
                       (zipmap (keys ana/*cljs-warnings*) (repeat warnings))
                       warnings)
                     (zipmap
                      [:unprovided :undeclared-var
                       :undeclared-ns :undeclared-ns-form]
                      (repeat (if (false? warnings)
                                false
                                warn-on-undeclared)))
                     {:infer-warning false}))
                  ana/*checked-arrays* checked-arrays
                  ana/*cljs-static-fns* static-fns
                  ana/*fn-invoke-direct* (and static-fns fn-invoke-direct)
                  *repl-opts* opts]
          (let [env {:context :expr :locals {}}
                special-fns (merge default-special-fns special-fns)
                is-special-fn? (set (keys special-fns))
                request-prompt (Object.)
                request-exit (Object.)
                opts (comp/with-core-cljs opts
                       (fn []
                         (try
                           (if-let [merge-opts (:merge-opts (-setup repl-env opts))]
                             (merge opts merge-opts)
                             opts)
                           (catch Throwable e
                             (caught e repl-env opts)
                             opts))))
                init (do
                       ;; ??? executing this as a side effect of binding init ???
                       #_(evaluate-form repl-env env "<cljs repl>"
                                      `(~'set! ~'cljs.core/*print-namespace-maps* true)
                                      identity opts)
                       (or init
                           #(evaluate-form repl-env env "<cljs repl>"
                                           (with-meta
                                             `(~'ns ~'cljs.user
                                               (:require ~@repl-requires))
                                             {:line 1 :column 1})
                                           identity opts)))
                read-eval-print
                (fn []
                  (let [input (binding [*ns* (create-ns ana/*cljs-ns*)
                                        reader/resolve-symbol ana/resolve-symbol
                                        reader/*data-readers* tags/*cljs-data-readers*
                                        reader/*alias-map*
                                        (apply merge
                                               ((juxt :requires :require-macros)
                                                (ana/get-namespace ana/*cljs-ns*)))]
                                (read request-prompt request-exit))]
                    (or ({request-exit request-exit
                          #_:cljs/quit #_request-exit
                          request-prompt request-prompt} input)
                        (if (and (seq? input) (is-special-fn? (first input)))
                          (do
                            ((get special-fns (first input)) repl-env env input opts)
                            (print nil))
                          (let [value (eval repl-env env input opts)]
                            (print value))))))]
            (when (:install-deps opts)
              (cljsc/check-npm-deps opts)
              (swap! env/*compiler* update-in [:npm-deps-installed?]
                     (fn [installed?]
                       (when-not installed?
                         (cljsc/maybe-install-node-deps! opts)))))
            (comp/with-core-cljs opts
              (fn []
                (binding [*repl-opts* opts]
                  (try
                    (when analyze-path
                      (if (vector? analyze-path)
                        (run! #(analyze-source % opts) analyze-path)
                        (analyze-source analyze-path opts)))
                    (init)
                    (catch Throwable e
                      (caught e repl-env opts)))
                  (when-let [src (:watch opts)]
                    (.start
                     (Thread.
                      ((ns-resolve 'clojure.core 'binding-conveyor-fn)
                       (fn []
                         (let [log-file (io/file (util/output-directory opts) "watch.log")]
                           (err-out (println "Watch compilation log available at:" (str log-file)))
                           (try
                             (let [log-out (FileWriter. log-file)]
                               (binding [*err* log-out
                                         *out* log-out]
                                 (cljsc/watch src (dissoc opts :watch)
                                              env/*compiler* done?)))
                             (catch Throwable e
                               (caught e repl-env opts)))))))))
                  ;; let any setup async messages flush
                  (Thread/sleep 50)
                  (binding [*in* (if (true? (:source-map-inline opts))
                                   *in*
                                   (reader))]
                    (quit-prompt)
                    (prompt)
                    (flush)
                    (loop []
                      (when-not
                          (try
                            (identical? (read-eval-print) request-exit)
                            (catch Throwable e
                              (caught e repl-env opts)
                              nil))
                        (when (need-prompt)
                          (prompt)
                          (flush))
                        (recur))))))))
          (reset! done? true)
          (-tear-down repl-env))))))

(with-version
  [1 9 655]
  [1 9 671]
  (defn repl*
    [repl-env {:keys [init need-prompt quit-prompt prompt flush read eval print caught reader
                      print-no-newline source-map-inline wrap repl-requires
                      compiler-env bind-err]
               :or {need-prompt #(if (readers/indexing-reader? *in*)
                                   (== (readers/get-column-number *in*) 1)
                                   (identity true))
                    quit-prompt repl-quit-prompt
                    prompt repl-prompt
                    flush flush
                    read repl-read
                    eval #'cljs.repl/eval-cljs
                    print println
                    caught repl-caught
                    reader #(readers/source-logging-push-back-reader
                             *in*
                             1 "NO_SOURCE_FILE")
                    print-no-newline print
                    source-map-inline true
                    repl-requires '[[cljs.repl :refer-macros [source doc find-doc apropos dir pst]]
                                    [cljs.pprint :refer [pprint] :refer-macros [pp]]]
                    bind-err true}
               :as opts}]
    (doseq [[unknown-opt suggested-opt] (util/unknown-opts (set (keys opts)) (set/union known-repl-opts cljsc/known-opts))]
      (when suggested-opt
        (println (str "WARNING: Unknown option '" unknown-opt "'. Did you mean '" suggested-opt "'?"))))
    (let [repl-opts (-repl-options repl-env)
          repl-requires (into repl-requires (:repl-requires repl-opts))
          {:keys [analyze-path repl-verbose warn-on-undeclared special-fns
                  static-fns fn-invoke-direct]
           :as opts
           :or   {warn-on-undeclared true}}
          (merge
           {:cache-analysis true :source-map true :def-emits-var true}
           (cljsc/add-implicit-options
            (merge-with (fn [a b] (if (nil? b) a b))
                        repl-opts
                        opts
                        {:prompt prompt
                         :need-prompt need-prompt
                         :flush flush
                         :read read
                         :print print
                         :caught caught
                         :reader reader
                         :print-no-newline print-no-newline
                         :source-map-inline source-map-inline})))
          done? (atom false)]
      (env/with-compiler-env (or compiler-env (env/default-compiler-env opts))
        (when (:source-map opts)
          (.start (Thread. (bound-fn [] (read-source-map "cljs/core.aot.js")))))
        (binding [ana/*unchecked-if* false
                  *err* (if bind-err
                          (cond-> *out*
                            (not (instance? PrintWriter *out*)) (PrintWriter.))
                          *err*)
                  ana/*cljs-ns* ana/*cljs-ns*
                  *cljs-verbose* repl-verbose
                  ana/*cljs-warnings*
                  (let [warnings (opts :warnings true)]
                    (merge
                     ana/*cljs-warnings*
                     (if (or (true? warnings)
                             (false? warnings))
                       (zipmap (keys ana/*cljs-warnings*) (repeat warnings))
                       warnings)
                     (zipmap
                      [:unprovided :undeclared-var
                       :undeclared-ns :undeclared-ns-form]
                      (repeat (if (false? warnings)
                                false
                                warn-on-undeclared)))
                     {:infer-warning false}))
                  ana/*cljs-static-fns* static-fns
                  ana/*fn-invoke-direct* (and static-fns fn-invoke-direct)
                  *repl-opts* opts]
          (let [env {:context :expr :locals {}}
                special-fns (merge default-special-fns special-fns)
                is-special-fn? (set (keys special-fns))
                request-prompt (Object.)
                request-exit (Object.)
                opts (comp/with-core-cljs opts
                       (fn []
                         (try
                           (if-let [merge-opts (:merge-opts (-setup repl-env opts))]
                             (merge opts merge-opts)
                             opts)
                           (catch Throwable e
                             (caught e repl-env opts)
                             opts))))
                ;; TODO: consider alternative ways to deal with JS module processing at REPL
                opts' opts ;; need to save opts prior to JS module processing for watch
                opts (if (or (:libs opts) (:foreign-libs opts))
                       (let [opts (cljsc/process-js-modules opts)]
                         (swap! env/*compiler* assoc :js-dependency-index (deps/js-dependency-index opts))
                         opts)
                       opts)
                init (do
                       ;; ??? executing this as a side effect of binding init ???
                       #_(evaluate-form repl-env env "<cljs repl>"
                                        `(~'set! ~'cljs.core/*print-namespace-maps* true)
                                        identity opts)
                       (or init
                           #(evaluate-form repl-env env "<cljs repl>"
                                           (with-meta
                                             `(~'ns ~'cljs.user
                                               (:require ~@repl-requires))
                                             {:line 1 :column 1})
                                           identity opts)))
                read-eval-print
                (fn []
                  (let [input (binding [*ns* (create-ns ana/*cljs-ns*)
                                        reader/resolve-symbol ana/resolve-symbol
                                        reader/*data-readers* tags/*cljs-data-readers*
                                        reader/*alias-map*
                                        (apply merge
                                               ((juxt :requires :require-macros)
                                                (ana/get-namespace ana/*cljs-ns*)))]
                                (read request-prompt request-exit))]
                    (or ({request-exit request-exit
                          request-prompt request-prompt} input)
                        (if (and (seq? input) (is-special-fn? (first input)))
                          (do
                            ((get special-fns (first input)) repl-env env input opts)
                            (print nil))
                          (let [value (eval repl-env env input opts)]
                            (print value))))))]
            (comp/with-core-cljs opts
              (fn []
                (binding [*repl-opts* opts]
                  (try
                    (when analyze-path
                      (if (vector? analyze-path)
                        (run! #(analyze-source % opts) analyze-path)
                        (analyze-source analyze-path opts)))
                    (init)
                    (catch Throwable e
                      (caught e repl-env opts)))
                  ;; TODO: consider alternative ways to deal with JS module processing at REPL
                  (let [opts opts'] ;; use opts prior to JS module processing
                    (when-let [src (:watch opts)]
                      (.start
                       (Thread.
                        ((ns-resolve 'clojure.core 'binding-conveyor-fn)
                         (fn []
                           (let [log-file (io/file (util/output-directory opts) "watch.log")]
                             (err-out (println "Watch compilation log available at:" (str log-file)))
                             (try
                               (let [log-out (FileWriter. log-file)]
                                 (binding [*err* log-out
                                           *out* log-out]
                                   (cljsc/watch src (dissoc opts :watch)
                                                env/*compiler* done?)))
                               (catch Throwable e
                                 (caught e repl-env opts))))))))))
                  ;; let any setup async messages flush
                  (Thread/sleep 50)
                  (binding [*in* (if (true? (:source-map-inline opts))
                                   *in*
                                   (reader))]
                    (quit-prompt)
                    (prompt)
                    (flush)
                    (loop []
                      (when-not
                          (try
                            (identical? (read-eval-print) request-exit)
                            (catch Throwable e
                              (caught e repl-env opts)
                              nil))
                        (when (need-prompt)
                          (prompt)
                          (flush))
                        (recur))))))))
          (reset! done? true)
          (-tear-down repl-env))))))

(with-version
 [0 0 0]
 [1 9 562]
 (defn repl*
   [repl-env {:keys [init need-prompt quit-prompt prompt flush read eval print caught reader
                     print-no-newline source-map-inline wrap repl-requires
                     compiler-env bind-err]
              :or {need-prompt #(if (readers/indexing-reader? *in*)
                                  (== (readers/get-column-number *in*) 1)
                                  (identity true))
                   quit-prompt repl-quit-prompt
                   prompt repl-prompt
                   flush flush
                   read repl-read
                   eval #'cljs.repl/eval-cljs
                   print println
                   caught repl-caught
                   reader #(readers/source-logging-push-back-reader
                            *in*
                            1 "NO_SOURCE_FILE")
                   print-no-newline print
                   source-map-inline true
                   repl-requires '[[cljs.repl :refer-macros [source doc find-doc apropos dir pst]]
                                   [cljs.pprint :refer [pprint] :refer-macros [pp]]]
                   bind-err true}
              :as opts}]
   (doseq [[unknown-opt suggested-opt] (util/unknown-opts (set (keys opts)) (set/union known-repl-opts cljsc/known-opts))]
     (when suggested-opt
       (println (str "WARNING: Unknown option '" unknown-opt "'. Did you mean '" suggested-opt "'?"))))
   (let [repl-opts (-repl-options repl-env)
         repl-requires (into repl-requires (:repl-requires repl-opts))
         {:keys [analyze-path repl-verbose warn-on-undeclared special-fns static-fns] :as opts
          :or   {warn-on-undeclared true}}
         (merge
          {:cache-analysis true :source-map true :def-emits-var true}
          (cljsc/add-implicit-options
           (merge-with (fn [a b] (if (nil? b) a b))
                       repl-opts
                       opts
                       {:prompt prompt
                        :need-prompt need-prompt
                        :flush flush
                        :read read
                        :print print
                        :caught caught
                        :reader reader
                        :print-no-newline print-no-newline
                        :source-map-inline source-map-inline})))
         done? (atom false)]
     (env/with-compiler-env (or compiler-env (env/default-compiler-env opts))
       (when (:source-map opts)
         (.start (Thread. (bound-fn [] (read-source-map "cljs/core.aot.js")))))
       (binding [ana/*unchecked-if* false
                 *err* (if bind-err
                         (cond-> *out*
                           (not (instance? PrintWriter *out*)) (PrintWriter.))
                         *err*)
                 ana/*cljs-ns* ana/*cljs-ns*
                 *cljs-verbose* repl-verbose
                 ana/*cljs-warnings*
                 (let [warnings (opts :warnings true)]
                   (merge
                    ana/*cljs-warnings*
                    (if (or (true? warnings)
                            (false? warnings))
                      (zipmap (keys ana/*cljs-warnings*) (repeat warnings))
                      warnings)
                    (zipmap
                     [:unprovided :undeclared-var
                      :undeclared-ns :undeclared-ns-form]
                     (repeat (if (false? warnings)
                               false
                               warn-on-undeclared)))
                    {:infer-warning false}))
                 ana/*cljs-static-fns* static-fns
                 *repl-opts* opts]
         (let [env {:context :expr :locals {}}
               special-fns (merge default-special-fns special-fns)
               is-special-fn? (set (keys special-fns))
               request-prompt (Object.)
               request-exit (Object.)
               opts (comp/with-core-cljs opts
                      (fn []
                        (try
                          (if-let [merge-opts (:merge-opts (-setup repl-env opts))]
                            (merge opts merge-opts)
                            opts)
                          (catch Throwable e
                            (caught e repl-env opts)
                            opts))))
               ;; TODO: consider alternative ways to deal with JS module processing at REPL
               opts' opts ;; need to save opts prior to JS module processing for watch
               opts (if (or (:libs opts) (:foreign-libs opts))
                      (let [opts (cljsc/process-js-modules opts)]
                        (swap! env/*compiler* assoc :js-dependency-index (deps/js-dependency-index opts))
                        opts)
                      opts)
               init (do
                      ;; ??? executing this as a side effect of binding init ???
                      #_(evaluate-form repl-env env "<cljs repl>"
                                       `(~'set! ~'cljs.core/*print-namespace-maps* true)
                                       identity opts)
                      (or init
                          #(evaluate-form repl-env env "<cljs repl>"
                                          (with-meta
                                            `(~'ns ~'cljs.user
                                              (:require ~@repl-requires))
                                            {:line 1 :column 1})
                                          identity opts)))
               read-eval-print
               (fn []
                 (let [input (binding [*ns* (create-ns ana/*cljs-ns*)
                                       reader/resolve-symbol ana/resolve-symbol
                                       reader/*data-readers* tags/*cljs-data-readers*
                                       reader/*alias-map*
                                       (apply merge
                                              ((juxt :requires :require-macros)
                                               (ana/get-namespace ana/*cljs-ns*)))]
                               (read request-prompt request-exit))]
                   (or ({request-exit request-exit
                         #_:cljs/quit #_request-exit
                         request-prompt request-prompt} input)
                       (if (and (seq? input) (is-special-fn? (first input)))
                         (do
                           ((get special-fns (first input)) repl-env env input opts)
                           (print nil))
                         (let [value (eval repl-env env input opts)]
                           (print value))))))]
           (comp/with-core-cljs opts
             (fn []
               (binding [*repl-opts* opts]
                 (try
                   (when analyze-path
                     (if (vector? analyze-path)
                       (run! #(analyze-source % opts) analyze-path)
                       (analyze-source analyze-path opts)))
                   (init)
                   (catch Throwable e
                     (caught e repl-env opts)))
                 ;; TODO: consider alternative ways to deal with JS module processing at REPL
                 (let [opts opts'] ;; use opts prior to JS module processing
                   (when-let [src (:watch opts)]
                     (.start
                      (Thread.
                       ((ns-resolve 'clojure.core 'binding-conveyor-fn)
                        (fn []
                          (let [log-file (io/file (util/output-directory opts) "watch.log")]
                            (err-out (println "Watch compilation log available at:" (str log-file)))
                            (try
                              (let [log-out (FileWriter. log-file)]
                                (binding [*err* log-out
                                          *out* log-out]
                                  (cljsc/watch src (dissoc opts :watch)
                                               env/*compiler* done?)))
                              (catch Throwable e
                                (caught e repl-env opts))))))))))
                 ;; let any setup async messages flush
                 (Thread/sleep 50)
                 (binding [*in* (if (true? (:source-map-inline opts))
                                  *in*
                                  (reader))]
                   (quit-prompt)
                   (prompt)
                   (flush)
                   (loop []
                     (when-not
                         (try
                           (identical? (read-eval-print) request-exit)
                           (catch Throwable e
                             (caught e repl-env opts)
                             nil))
                       (when (need-prompt)
                         (prompt)
                         (flush))
                       (recur))))))))
         (reset! done? true)
         (-tear-down repl-env))))))

(defn repl
  "Generic, reusable, read-eval-print loop. By default, reads from *in* using
  a c.t.r.reader-types/source-logging-push-back-reader,
  writes to *out*, and prints exception summaries to *err*. If you use the
  default :read hook, *in* must either be an instance of
  c.t.r.reader-types/PushbackReader or duplicate its behavior of both supporting
  unread and collapsing CR, LF, and CRLF into a single \\newline. Options
  are sequential keyword-value pairs. The first argument is the JavaScript
  evaluation environment, the second argument is an extended version of the
  standard ClojureScript compiler options. In addition to ClojureScript compiler
  build options it also take a set of options similar to clojure.main/repl with
  adjustments for ClojureScript evalution and compilation model:
  Available clojure.main/repl style options and their defaults:
     - :init, function of no arguments, initialization hook called with
       bindings for set!-able vars in place.
       default: #()
     - :need-prompt, function of no arguments, called before each
       read-eval-print except the first, the user will be prompted if it
       returns true.
       default: #(if (c.t.r.readers-types/indexing-reader? *in*)
                   (== (c.t.r.reader-types/get-column-number *in*) 1)
                   (identity true))
     - :prompt, function of no arguments, prompts for more input.
       default: repl-prompt
     - :flush, function of no arguments, flushes output
       default: flush
     - :read, function of two arguments, reads from *in*:
         - returns its first argument to request a fresh prompt
           - depending on need-prompt, this may cause the repl to prompt
             before reading again
         - returns its second argument to request an exit from the repl
         - else returns the next object read from the input stream
       default: repl-read
     - :eval, function of one argument, returns the evaluation of its
       argument. The eval function must take repl-env, the JavaScript evaluation
       environment, env, the ClojureScript analysis environment, the form
       and opts, the standard ClojureScript REPL/compiler options.
       default: eval
     - :print, function of one argument, prints its argument to the output
       default: println
     - :caught, function of three arguments, a throwable, called when
       read, eval, or print throws an exception or error default. The second
       argument is the JavaScript evaluation environment this permits context
       sensitive handling if necessary. The third argument is opts, the standard
       ClojureScript REPL/compiler options. In the case of errors or exception
       in the JavaScript target, these will be thrown as
       clojure.lang.IExceptionInfo instances.
       default: repl-caught
     - :reader, the c.t.r reader to use.
       default: c.t.r.reader-types/source-logging-push-back-reader
     - :print-no-newline, print without a newline.
       default: print
     - :source-map-inline, whether inline source maps should be enabled. Most
       useful in browser context. Implies using a fresh reader for each form.
       default: true"
  [repl-env & opts]
  (assert (even? (count opts))
    "Arguments after repl-env must be interleaved key value pairs")
  (repl* repl-env (apply hash-map opts)))

;; Support for :reload-all
(def ^:dynamic *reload-all* false)

;; Alternative to cljs.closure/src-file->goog-require in order to support goog.require
;; with :reload-all
(defn ^String src-file->goog-require
  ([src] (src-file->goog-require src {:wrap true}))
  ([src {:keys [wrap all-provides macros-ns] :as options}]
    (let [goog-ns
          (case (util/ext src)
            ("cljs" "cljc") (let [ns-str (str (comp/munge (:ns (ana/parse-ns src))))]
                              (cond-> ns-str
                                (and macros-ns (not (.endsWith ns-str "$macros")))
                                (str "$macros")))
            "js" (cond-> (:provides (cljsc/parse-js-ns src))
                   (not all-provides) first)
            (throw
              (IllegalArgumentException.
               (str "Can't create goog.require expression for " src))))]
      (if (and (not all-provides) wrap)
        (cond
          (:reload-all options) (str "goog.require(\"" goog-ns "\", \"reload-all\");")
          (:reload options) (str "goog.require(\"" goog-ns "\", true);")
          :else (str "goog.require(\"" goog-ns "\");"))
        (if (vector? goog-ns)
          goog-ns
          (str goog-ns))))))

;; Patch parse 'def to allow :const redefinition
;; Remove constants from the environment before parsing 'def expressions

(defonce parse-def-o (get (methods cljs.analyzer/parse) 'def))

(defn maybe-dissoc-const [env form]
  (if-let [sym (second form)]
    (let [maybe-const-var (get-in env [:ns :defs sym])]
      (if (:const maybe-const-var)
        (do
          (swap! cljs.env/*compiler* update-in
                 [:cljs.analyzer/namespaces (-> env :ns :name) :defs] dissoc sym)
          (update-in env [:ns :defs] dissoc sym))
        env))
    env))

(defmethod cljs.analyzer/parse 'def
  [op env form _1 _2]
  (parse-def-o op (maybe-dissoc-const env form) form _1 _2))


;; patch parse 'ns to allow :reload-all
;; parse 'ns* is not patched since (require ... :reload-all) doesn't really work

(defonce parse-ns-o (get (methods cljs.analyzer/parse) 'ns))

(defmethod cljs.analyzer/parse 'ns
  [_1 env [_2 name & args :as form] _3 opts]
  (let [ns-infos (parse-ns-o _1 env form _3 opts)]
    (if *reload-all*
      (-> ns-infos
          (assoc-in [:reload :user-macros] :reload)
          (assoc-in [:reload :require-macros] :reload))
      ns-infos)))

;; patch cljs.util/changed? to only check for modified file when using :reload-all

(defonce changed?-o @#'cljs.util/changed?)

(defn changed? [a b]
  (if *reload-all* (changed?-o a b) false))

(alter-var-root #'cljs.util/changed? (constantly changed?))

;; Custom constructor to be able to set :file :line and :column when evaluating
;; code from a source file at the REPL

(defn ^Closeable source-logging-push-back-reader
  "Creates a SourceLoggingPushbackReader from a given string or PushbackReader"
  [s-or-rdr buf-len file-name line column]
  (readers/->SourceLoggingPushbackReader
   (readers/to-pbr s-or-rdr buf-len)
   line column
   true
   nil
   0
   file-name
   (doto (clojure.tools.reader.impl.utils/make-var)
     (alter-var-root (constantly {:buffer (StringBuilder.)
                                  :offset 0})))))
