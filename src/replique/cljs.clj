(ns replique.cljs)

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

;; patch repl* to remove the binding of *print-namespace-maps*, which is broken
;; (*print-namespace-maps* is set whatever the value of the init parameter). A clojurescript
;; patch would not be accepted because cljs devs think this is fine ...
(defn repl*
  [repl-env {:keys [init need-prompt quit-prompt prompt flush read eval print caught reader
                    print-no-newline source-map-inline wrap repl-requires
                    compiler-env bind-err]
             :or {need-prompt #(if (clojure.tools.reader.reader-types/indexing-reader? *in*)
                                 (== (clojure.tools.reader.reader-types/get-column-number *in*) 1)
                                 (identity true))
                  quit-prompt cljs.repl/repl-quit-prompt
                  prompt cljs.repl/repl-prompt
                  flush flush
                  read cljs.repl/repl-read
                  eval @#'cljs.repl/eval-cljs
                  print println
                  caught cljs.repl/repl-caught
                  reader #(clojure.tools.reader.reader-types/source-logging-push-back-reader
                           (java.io.PushbackReader. (clojure.java.io/reader *in*))
                           1 "NO_SOURCE_FILE")
                  print-no-newline print
                  source-map-inline true
                  repl-requires '[[cljs.repl :refer-macros [source doc find-doc apropos dir pst]]
                                  [cljs.pprint :refer [pprint] :refer-macros [pp]]]
                  bind-err true}
             :as opts}]
  (doseq [[unknown-opt suggested-opt] (cljs.util/unknown-opts (set (keys opts)) (clojure.set/union cljs.repl/known-repl-opts cljs.closure/known-opts))]
    (when suggested-opt
      (println (str "WARNING: Unknown option '" unknown-opt "'. Did you mean '" suggested-opt "'?"))))
  (let [repl-opts (cljs.repl/-repl-options repl-env)
        repl-requires (into repl-requires (:repl-requires repl-opts))
        {:keys [analyze-path repl-verbose warn-on-undeclared special-fns static-fns] :as opts
         :or   {warn-on-undeclared true}}
        (merge
         {:cache-analysis true :source-map true :def-emits-var true}
         (cljs.closure/add-implicit-options
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
    (cljs.env/with-compiler-env (or compiler-env (cljs.env/default-compiler-env opts))
      (when (:source-map opts)
        (.start (Thread. (bound-fn [] (cljs.repl/read-source-map "cljs/core.aot.js")))))
      (binding [cljs.analyzer/*unchecked-if* false
                *err* (if bind-err
                        (cond-> *out*
                          (not (instance? java.io.PrintWriter *out*)) (java.io.PrintWriter.))
                        *err*)
                cljs.analyzer/*cljs-ns* cljs.analyzer/*cljs-ns*
                cljs.repl/*cljs-verbose* repl-verbose
                cljs.analyzer/*cljs-warnings*
                (let [warnings (opts :warnings true)]
                  (merge
                   cljs.analyzer/*cljs-warnings*
                   (if (or (true? warnings)
                           (false? warnings))
                     (zipmap (keys cljs.analyzer/*cljs-warnings*) (repeat warnings))
                     warnings)
                   (zipmap
                    [:unprovided :undeclared-var
                     :undeclared-ns :undeclared-ns-form]
                    (repeat (if (false? warnings)
                              false
                              warn-on-undeclared)))
                   {:infer-warning false}))
                cljs.analyzer/*cljs-static-fns* static-fns
                cljs.repl/*repl-opts* opts]
        (let [env {:context :expr :locals {}}
              special-fns (merge cljs.repl/default-special-fns special-fns)
              is-special-fn? (set (keys special-fns))
              request-prompt (Object.)
              request-exit (Object.)
              opts (cljs.compiler/with-core-cljs opts
                     (fn []
                       (try
                         (if-let [merge-opts (:merge-opts (cljs.repl/-setup repl-env opts))]
                           (merge opts merge-opts)
                           opts)
                         (catch Throwable e
                           (caught e repl-env opts)
                           opts))))
              ;; TODO: consider alternative ways to deal with JS module processing at REPL
              opts' opts ;; need to save opts prior to JS module processing for watch
              opts (if (or (:libs opts) (:foreign-libs opts))
                     (let [opts (cljs.closure/process-js-modules opts)]
                       (swap! cljs.env/*compiler* assoc :js-dependency-index (cljs.js-deps/js-dependency-index opts))
                       opts)
                     opts)
              init (do
                     ;; ??? executing this as a side effect of binding init ???
                     #_(cljs.repl/evaluate-form repl-env env "<cljs repl>"
                                    `(~'set! ~'cljs.core/*print-namespace-maps* true)
                                    identity opts)
                     (or init
                         #(cljs.repl/evaluate-form repl-env env "<cljs repl>"
                                         (with-meta
                                           `(~'ns ~'cljs.user
                                             (:require ~@repl-requires))
                                           {:line 1 :column 1})
                                         identity opts)))
              read-eval-print
              (fn []
                (let [input (binding [*ns* (create-ns cljs.analyzer/*cljs-ns*)
                                      clojure.tools.reader/resolve-symbol cljs.analyzer/resolve-symbol
                                      clojure.tools.reader/*data-readers* cljs.tagged-literals/*cljs-data-readers*
                                      clojure.tools.reader/*alias-map*
                                      (apply merge
                                             ((juxt :requires :require-macros)
                                              (cljs.analyzer/get-namespace cljs.analyzer/*cljs-ns*)))]
                              (read request-prompt request-exit))]
                  (or ({request-exit request-exit
                        :cljs/quit request-exit
                        request-prompt request-prompt} input)
                      (if (and (seq? input) (is-special-fn? (first input)))
                        (do
                          ((get special-fns (first input)) repl-env env input opts)
                          (print nil))
                        (let [value (eval repl-env env input opts)]
                          (print value))))))]
          (cljs.compiler/with-core-cljs opts
            (fn []
              (binding [cljs.repl/*repl-opts* opts]
                (try
                  (when analyze-path
                    (if (vector? analyze-path)
                      (run! #(cljs.repl/analyze-source % opts) analyze-path)
                      (cljs.repl/analyze-source analyze-path opts)))
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
                         (let [log-file (clojure.java.io/file (cljs.util/output-directory opts) "watch.log")]
                           (cljs.repl/err-out (println "Watch compilation log available at:" (str log-file)))
                           (try
                             (let [log-out (java.io.FileWriter. log-file)]
                               (binding [*err* log-out
                                         *out* log-out]
                                 (cljs.closure/watch src (dissoc opts :watch)
                                              cljs.env/*compiler* done?)))
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
        (cljs.repl/-tear-down repl-env)))))

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

