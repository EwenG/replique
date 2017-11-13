(ns replique.nashorn
  (:require [replique.repl-cljs]
            [cljs.compiler :as comp]
            [cljs.stacktrace :as st]
            [cljs.util]
            [cljs.analyzer :as ana]
            [clojure.string :as string]
            [replique.utils :as utils]
            [replique.server :refer [*session*] :as server]
            [replique.tooling-msg :as tooling-msg]
            [clojure.java.io :as io]
            [clojure.stacktrace :refer [print-stack-trace]]
            [cljs.closure :as closure]
            [replique.cljs])
  (:import [java.io File]
           [javax.script ScriptEngine ScriptEngineManager ScriptException ScriptEngineFactory]
           [jdk.nashorn.api.scripting NashornException ScriptObjectMirror]
           [java.util.concurrent.locks ReentrantLock]))

(declare create-engine init-engine init-repl-env)

(defonce engine (utils/delay (init-engine (create-engine)
                                          (cljs.util/output-directory
                                           (:options @@replique.repl-cljs/compiler-env)))))
(defonce repl-env (utils/delay (init-repl-env)))

(defn create-engine []
  (let [factories (.getEngineFactories (ScriptEngineManager.))
        factory (get (zipmap (map #(.getEngineName ^ScriptEngineFactory %) factories) factories) "Oracle Nashorn")]
    (if-let [engine (.getScriptEngine ^ScriptEngineFactory factory)]
      (let [context (.getContext engine)]
        (.setWriter context *out*)
        (.setErrorWriter context *err*)
        engine)
      (throw (IllegalArgumentException.
              "Cannot find the Nashorn script engine, use a JDK version 8 or higher.")))))

(defn eval-str [^ScriptEngine engine ^String s]
  (.eval engine s))

(defn load-ns [engine ns]
  (eval-str engine (format "goog.require(\"%s\");" (comp/munge (first ns)))))

(defn eval-resource
  "Evaluate a file on the classpath in the engine."
  [engine path]
  (let [r (io/resource path)]
    (eval-str engine (slurp r))))

(defn load-js-file [engine file]
  (eval-str engine (format "nashorn_load(\"%s\");" file)))

(defn init-engine [engine output-dir]
  (eval-resource engine "goog/base.js")
  (eval-resource engine "goog/deps.js")
  (eval-str engine
            (format
             (str "var nashorn_load = function(path) {"
                  "  var outputPath = \"%s\" + \"/\" + path;"
                  "  try {"
                  "    load(outputPath);"
                  "} catch(e) {"
                  "  cljs.core._STAR_e = e;"
                  "  throw e;"
                  "}"
                  "};")
             output-dir))
  (eval-str engine
            (str "goog.global.CLOSURE_IMPORT_SCRIPT = function(path) {"
                 " nashorn_load(\"goog/\" + path);"
                 " return true;"
                 "};"))
  (eval-str engine "goog.global.isProvided_ = function(name) { return false; };")
  (load-js-file engine "cljs_deps.js")
  (load-js-file engine "replique/cljs_env/javafx.js")
  engine)

(defn evaluate-form [repl-env js]
  (try
    {:status :success
     :value (if-let [r (eval-str @engine js)]
              (if (and (instance? ScriptObjectMirror r)
                       (.containsKey ^ScriptObjectMirror r "stack"))
                (let [cst (cljs.repl/-parse-stacktrace
                           repl-env (.get ^ScriptObjectMirror r "stack")
                           {} cljs.repl/*repl-opts*)]
                  (if (vector? cst)
                    (->> (replique.repl-cljs/mapped-stacktrace cst cljs.repl/*repl-opts*)
                         (str (.toString ^ScriptObjectMirror r)))
                    (.toString ^ScriptObjectMirror r)))
                (.toString r))
              "")}
    (catch ScriptException e
      (let [^Throwable root-cause (clojure.stacktrace/root-cause e)]
        {:status :exception
         :value (.getMessage root-cause)
         :stacktrace (NashornException/getScriptStackString root-cause)}))
    (catch Throwable e
      (let [^Throwable root-cause (clojure.stacktrace/root-cause e)]
        {:status :exception
         :value (.getMessage root-cause)
         :stacktrace
         (apply str
                (interpose "\n"
                           (map str
                                (.getStackTrace root-cause))))}))))

(defn repl-eval-compiled [compiled repl-env f opts]
  (let [src (replique.repl-cljs/f->src f)]
    (cljs.repl/-evaluate
     repl-env "<cljs repl>" 1
     (slurp (str (cljs.util/output-directory opts)
                 File/separator "cljs_deps.js")))
    `(~'js* ~(replique.cljs/src-file->goog-require
              src {:wrap true
                   :reload true
                   :reload-all replique.cljs/*reload-all*
                   :macros-ns (:macros-ns compiled)}))))

;; Defrecord instead of deftype because cljs.repl uses the repl-env as a hashmap. Why ??? 
(defrecord NashornEnv [repl-opts]
  cljs.repl/IJavaScriptEnv
  (-setup [this opts]
    (cljs.repl/evaluate-form this replique.repl-cljs/env "<cljs repl>"
                             '(do
                                (.require js/goog "cljs.core")
                                (set! *print-newline* false)
                                (set! *print-fn* js/print)
                                (set! *print-err-fn* js/print)))
    ;; monkey-patch goog.isProvided_ to suppress useless errors
    (cljs.repl/evaluate-form
     this replique.repl-cljs/env "<cljs repl>"
     '(set! js/goog.isProvided_ (fn [ns] false)))
    ;; monkey-patch goog.require to be more sensible
    (cljs.repl/evaluate-form
     this replique.repl-cljs/env "<cljs repl>"
     '(do
        (set! *loaded-libs* #{"cljs.core"})
        (set! (.-require js/goog)
              (fn [name reload]
                (when (or (not (contains? *loaded-libs* name)) reload)
                  (set! *loaded-libs* (conj (or *loaded-libs* #{}) name))
                  (js/CLOSURE_IMPORT_SCRIPT
                   (aget (.. js/goog -dependencies_ -nameToPath) name)))))))
    nil)
  (-evaluate [this _ _ js]
    (evaluate-form this js))
  (-load [this ns url]
    (load-ns @engine ns))
  (-tear-down [this] nil)
  cljs.repl/IReplEnvOptions
  (-repl-options [this] repl-opts)
  cljs.repl/IParseStacktrace
  (-parse-stacktrace [this frames-str ret opts]
    (st/parse-stacktrace this frames-str (assoc ret :ua-product :nashorn) opts))
  cljs.repl/IParseError
  (-parse-error [_ err _]
    (update-in err [:stacktrace]
               (fn [st]
                 (string/join "\n" (drop 1 (string/split st #"\n"))))))
  cljs.repl/IPrintStacktrace
  (-print-stacktrace [repl-env stacktrace error build-options]
    ;; (:value error) was already printed by repl.cljc
    ;; we don't want to print the full stracktrace here
    )
  replique.repl-cljs/ReplLoadFile
  (replique.repl-cljs/-load-file [repl-env file-path]
    (replique.repl-cljs/-load-file repl-env file-path nil))
  (replique.repl-cljs/-load-file [repl-env file-path opts]
    (binding [replique.cljs/*reload-all* (boolean (contains? opts :reload-all))]
      (let [comp-opts (:options @@replique.repl-cljs/compiler-env)
            compiled (replique.repl-cljs/compile-file repl-env file-path comp-opts)]
        (repl-eval-compiled compiled repl-env file-path comp-opts)))))

(defn init-repl-env []
  (NashornEnv. {:wrap #'replique.repl-cljs/wrap-fn}))

(defn cljs-repl []
  (let [repl-env @repl-env
        compiler-env @replique.repl-cljs/compiler-env
        repl-opts (replique.repl/options-with-repl-meta
                   {:compiler-env compiler-env
                    :init (fn [] (replique.repl-cljs/in-ns* repl-env 'cljs.user))
                    :print println
                    :caught cljs.repl/repl-caught
                    :read (replique.repl-cljs/repl-read-with-exit :cljs/quit)
                    :need-prompt replique.repl-cljs/repl-need-prompt})]
    (swap! replique.repl-cljs/cljs-outs conj *out*)
    (binding [utils/*repl-env* :replique/nashorn
              replique.repl-cljs/*file* nil
              replique.repl-cljs/*line* nil
              replique.repl-cljs/*column* nil
              replique.repl-cljs/*ignored-form* false]
      (apply
       (partial replique.cljs/repl repl-env)
       (->> (merge (:options @compiler-env) repl-opts {:eval replique.repl-cljs/eval-cljs})
            (apply concat))))
    (swap! replique.repl-cljs/cljs-outs disj *out*)))

(derive :replique/nashorn :replique/cljs)

(defmethod tooling-msg/tooling-msg-handle [:replique/nashorn :list-css] [msg]
  (tooling-msg/with-tooling-response msg
    (let [{:keys [status value]} (cljs.repl/-evaluate
                                  @repl-env "<cljs repl>" 1
                                  "replique.cljs_env.javafx.list_css_urls();")]
      (if (not (= :success status))
        (assoc msg :error value)
        (assoc msg :css-urls (read-string value))))))

(defmethod tooling-msg/tooling-msg-handle [:replique/nashorn :load-css]
  [{:keys [url] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [{:keys [status value]} (->> (pr-str url)
                                      (format "replique.cljs_env.javafx.reload_css(%s);")
                                      (cljs.repl/-evaluate @repl-env "<cljs repl>" 1))]
      (if (not (= :success status))
        (assoc msg :error value)
        (assoc msg :result value)))))

(defmethod tooling-msg/tooling-msg-handle [:replique/nashorn :load-js] [msg]
  (tooling-msg/with-tooling-response msg
    (replique.repl-cljs/load-js @repl-env msg)))

(defmethod tooling-msg/tooling-msg-handle [:replique/nashorn :eval] [msg]
  (tooling-msg/with-tooling-response msg
    (replique.repl-cljs/tooling-eval-cljs @repl-env msg)))

;; unlike with a browser env, load-file is called synchronously (see nashorn_load)
