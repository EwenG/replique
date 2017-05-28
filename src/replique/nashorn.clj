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
            [clojure.java.io :as io])
  (:import [java.io File]
           [javax.script ScriptEngine ScriptEngineManager ScriptException ScriptEngineFactory]
           [jdk.nashorn.api.scripting NashornException]
           [java.util.concurrent.locks ReentrantLock]))

(declare create-engine init-engine ->NashornEnv)

(defonce engine (utils/delay (init-engine (create-engine)
                                          (cljs.util/output-directory
                                           (:options @@replique.repl-cljs/compiler-env)))))

(defonce repl-env (utils/delay (->NashornEnv)))

(defn create-engine []
  (let [factories (.getEngineFactories (ScriptEngineManager.))
        factory (get (zipmap (map #(.getEngineName %) factories) factories) "Oracle Nashorn")]
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
  (eval-str engine
            (format "goog.require(\"%s\");" (comp/munge (first ns)))))

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
                  "  load(outputPath);"
                  "};")
             output-dir))
  (eval-str engine
            (str "goog.global.CLOSURE_IMPORT_SCRIPT = function(path) {"
                 " nashorn_load(\"goog/\" + path);"
                 " return true;"
                 "};"))
  (eval-str engine "goog.global.isProvided_ = function(name) { return false; };")
  (load-js-file engine "cljs_deps.js")
  engine)

;; Defrecord instead of deftype because cljs.repl uses the repl-env as a hashmap. Why ??? 
(defrecord NashornEnv []
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
                   (aget (.. js/goog -dependencies_ -nameToPath) name))))))))
  (-evaluate [this _ _ js]
    (try
      {:status :success
       :value (if-let [r (eval-str @engine js)] (.toString r) "")}
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
  (-load [this ns url]
    (load-ns @engine ns))
  (-tear-down [this] nil)
  cljs.repl/IParseStacktrace
  (-parse-stacktrace [this frames-str ret opts]
    (st/parse-stacktrace this frames-str
                         (assoc ret :ua-product :nashorn) opts))
  cljs.repl/IParseError
  (-parse-error [_ err _]
    (update-in err [:stacktrace]
               (fn [st]
                 (string/join "\n" (drop 1 (string/split st #"\n")))))))

(defn cljs-repl []
  (let [repl-env @repl-env
        compiler-env @replique.repl-cljs/compiler-env
        out-lock (ReentrantLock.)
        repl-opts (if (tooling-msg/tooling-available?)
                    {:compiler-env compiler-env
                     :caught replique.repl-cljs/repl-caught
                     :print (fn [result]
                              (binding [*out* tooling-msg/tooling-out]
                                (utils/with-lock tooling-msg/tooling-out-lock
                                  (tooling-msg/tooling-prn {:type :eval
                                                            :process-id tooling-msg/process-id
                                                            :repl-type :cljs
                                                            :session *session*
                                                            :ns ana/*cljs-ns*
                                                            :result result})))
                              (utils/with-lock out-lock
                                (println result)))
                     ;; Code modifying the runtime should not be put in :init, otherise it would
                     ;; be lost on browser refresh
                     :init (fn []
                             ;; Let the client know that we are entering a cljs repl
                             (binding [*out* tooling-msg/tooling-out]
                               (utils/with-lock tooling-msg/tooling-out-lock
                                 (tooling-msg/tooling-prn {:type :eval
                                                           :process-id tooling-msg/process-id
                                                           :repl-type :cljs
                                                           :session *session*
                                                           :ns ana/*cljs-ns*
                                                           :result "nil"}))))}
                    {:compiler-env compiler-env})]
    (swap! replique.repl-cljs/cljs-outs conj [*out* out-lock])
    (apply
     (partial replique.cljs/repl repl-env)
     (->> (merge (:options @compiler-env) repl-opts {:eval replique.repl-cljs/eval-cljs})
          (apply concat)))
    (swap! replique.repl-cljs/cljs-outs disj [*out* out-lock])))
