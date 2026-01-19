(ns replique.interactive
  "Replique REPL public API"
  (:refer-clojure :exclude [load-file load])
  (:require [replique.repl]
            [replique.repl-protocols :as repl-protocols]
            [replique.utils :as utils]
            [clojure.core.server :as server]
            [replique.environment :as env]
            [clojure.java.io :as io]
            [replique.omniscient :as omniscient]
            [replique.watch :as watch])
  (:import [java.net URL]
           [java.io File]))

(def ^:private cljs-repl* (utils/dynaload 'replique.repl-cljs/cljs-repl))
(def ^:private cljs-repl-nashorn* (utils/dynaload 'replique.nashorn/cljs-repl))
(def ^:private cljs-in-ns* (utils/dynaload 'replique.repl-cljs/in-ns*))
(def ^:private cljs-compiler-env (utils/dynaload 'replique.repl-cljs/compiler-env))
(def ^:private cljs-set-repl-verbose (utils/dynaload 'replique.repl-cljs/set-repl-verbose))
(def ^:private cljs-eval-cljs-form (utils/dynaload 'replique.repl-cljs/eval-cljs-form))
(def ^:private cljs-custom-compiler-opts (utils/dynaload 'replique.repl-cljs/custom-compiler-opts))
(def ^:private cljs-munge (utils/dynaload 'cljs.compiler/munge))
(def ^:private logback-reload* (utils/dynaload 'replique.logback/logback-reload))
(def ^:private log4j2-reload* (utils/dynaload 'replique.log4j2/log4j2-reload))

#_(def ^:private cljs-install-node-deps!
  (utils/dynaload 'replique.repl-cljs/install-node-deps!))

(defn repl [& options]
  (apply replique.repl/repl options))

(defn cljs-repl
  "Start a Clojurescript REPL. When a main-namescape is provided, the namespace is compiled if
  it is not already loaded in the compiler environment."
  ([] (@cljs-repl* nil))
  ([main-namespace] (@cljs-repl* main-namespace)))

(defn cljs-repl-nashorn
  "Start a Clojurescript Nashorn REPL"
  []
  (@cljs-repl-nashorn*))

;; At the moment, load file does not intern macros in the cljs-env, making dynamically loaded
;; macros unavailable to autocompletion/repliquedoc
(defmacro load-file
  "Sequentially read and evaluate the set of forms contained in the file.
  Works both for Clojure and Clojurescript"
  [file-path & opts]
  (let [opts (into #{} opts)]
    (if (utils/cljs-env? &env)
      ;; (:repl-env &env) can be a browser/nashorn/whatever... env
      (repl-protocols/-load-file (:repl-env &env) file-path opts)
      `(clojure.core/load-file ~file-path))))

(defmulti load (fn [protocol env url opts] protocol))

(defmethod load "file" [protocol env url opts]
  (if (utils/cljs-env? env)
    ;; (:repl-env &env) can be a browser/nashorn/whatever... env
    (repl-protocols/-load-file (:repl-env env) url opts)
    `(utils/maybe-locking
      clojure.lang.RT/REQUIRE_LOCK
      (clojure.core/load-file ~(utils/file-url->path url)))))

(defmethod load "jar" [protocol env url opts]
  (let [path (utils/jar-url->path url)
        file (when path (.getName (File. ^String path)))]
    (assert path (str "Cannot load url: " (str url)))
    (if (utils/cljs-env? env)
      (repl-protocols/-load-file (:repl-env env) url opts)
      `(utils/maybe-locking
        clojure.lang.RT/REQUIRE_LOCK
        (Compiler/load (io/reader (URL. ~(str url))) ~path ~file)))))

(defmacro load-url
  "Sequentially read and evaluate the set of forms contained at the URL. Works both for Clojure and Clojurescript"
  [url-str & opts]
  (let [url (URL. url-str)
        protocol (.getProtocol url)
        opts (into #{} opts)]
    (load protocol &env url opts)))

;; It seems that naming this macro "in-ns" make the cljs compiler to crash
(defmacro cljs-in-ns
  "Change the Clojurescript namespace to the namespace named by the symbol"
  [ns-quote]
  (let [quote (and (seq? ns-quote) (first ns-quote))
        ns-name (and (seq? ns-quote) (second ns-quote))]
    (when-not (and (= 'quote quote) (symbol? ns-name))
      (throw (IllegalArgumentException. "Argument to in-ns must be a symbol.")))
    (list 'quote (@cljs-in-ns* (:repl-env &env) ns-name))))

(defmacro set-cljs-repl-verbose
  "Switch the clojurescript REPL into verbose mode"
  [b]
  (@cljs-set-repl-verbose b)
  b)

;; :parallel-build does not seem to make much difference
(def compiler-opts
  "Clojurescript compiler options that can be set at the REPL"
  #{:verbose :warnings :compiler-stats :language-in :language-out
    :closure-warnings :checked-arrays :global-goog-object&array})

(defmacro set-cljs-compiler-opt
  "Set the value of the Clojurescript compiler option named by the key"
  [opt-key opt-val]
  {:pre [(contains? compiler-opts opt-key)]}
  (swap! @cljs-custom-compiler-opts assoc opt-key opt-val)
  (when utils/http-server
    (swap! @@cljs-compiler-env assoc-in [:options opt-key] opt-val))
  opt-val)

#_(defn remote-repl
    "Start a REPL on a remote machine"
    [host port]
    {:pre [(string? host) (number? port)]}
    (let [s (java.net.Socket. host port)
          s-in (.getInputStream s)
          s-out (java.io.BufferedWriter. (java.io.OutputStreamWriter. (.getOutputStream s)))]
      (future
        (try
          (loop []
            (let [input (.read s-in)]
              (when (not (= -1 input))
                (.write *out* input)
                (.flush *out*)
                (recur))))
          (catch Exception _ nil)
          (finally (.close s))))
      (try
        (loop []
          (let [input (read {:read-cond :allow} *in*)]
            (binding [*out* s-out] (prn input))
            (recur)))
        (finally (.close s)))))

(comment
  (require '[clojure.core.server :as core-s])

  (defn remote-repl-accept []
    (clojure.main/repl :prompt (fn [] (printf "<remote> %s=> " (ns-name *ns*)))
                       :read server/repl-read))

  

  (core-s/start-server {:port 9000 :name :test
                        :accept `remote-repl-accept
                        :server-daemon false})

  (core-s/stop-server :test)

  (remote-repl "localhost" 9000)
  )

(defmacro remove-var [var-sym]
  (assert (symbol? var-sym))
  (let [comp-env (when (utils/cljs-env? &env)
                   (env/->CljsCompilerEnv @@cljs-compiler-env))
        var-ns-sym (env/safe-symbol (namespace var-sym))
        var-ns (when var-ns-sym (env/find-ns comp-env var-ns-sym))
        the-var (when var-ns (env/ns-resolve comp-env var-ns var-sym))]
    (assert (and the-var var-sym var-ns))
    (env/ns-unmap comp-env var-ns (-> var-sym name symbol))
    (doseq [n (env/all-ns comp-env)]
      (if (utils/cljs-env? &env)
        (do (doseq [[m-s m-ns-sym] (:uses n)
                    :when (= m-ns-sym var-ns-sym)]
              (env/ns-unmap comp-env n m-s))
            (doseq [[m-s m-qualified-sym] (:renames n)
                    :when (= m-qualified-sym var-sym)]
              (env/ns-unmap comp-env n m-s))
            (@cljs-eval-cljs-form
             (:repl-env &env)
             `(try
                (cljs.core/js-delete ~(symbol "js" (str var-ns-sym))
                                     ~(@cljs-munge (-> var-sym name)))
                (catch js/Error _# nil))))
        (do (doseq [[m-s m-var] (ns-refers n)
                    :when (identical? m-var the-var)]
              (ns-unmap n m-s))
            (when (and (find-ns 'replique.repl-cljs)
                       (realized? @cljs-compiler-env))
              (doseq [n (env/all-ns (env/->CljsCompilerEnv @@cljs-compiler-env))]
                (doseq [[m-s m-ns-sym] (:use-macros n)
                        :when (= m-ns-sym var-ns-sym)]
                  (env/ns-unmap comp-env n m-s))
                (doseq [[m-s m-qualified-sym] (:rename-macros n)
                        :when (= m-qualified-sym var-sym)]
                  (env/ns-unmap comp-env n m-s)))))))))

(defmacro capture-env [capture-atom & body]
  (omniscient/capture-env &env &form capture-atom body))

(defmacro capture-child-env [& body]
  (omniscient/capture-child-env &env &form body))

(defmacro with-env [captured-env-var & body]
  (omniscient/with-env &env captured-env-var body))

(defn logback-reload [file-url]
  (@logback-reload* file-url))

(defn log4j2-reload [file-url]
  (@log4j2-reload* file-url))

(defn eval-js [repl-env js]
  (let [ret ((resolve 'cljs.repl/-evaluate)
             repl-env "<cljs repl>" 1
             js)]
    (case (:status ret)
      :error (throw
              (ex-info (:value ret)
                       {:type :js-eval-error
                        :error ret
                        :repl-env repl-env}))
      :exception (throw
                  (ex-info (:value ret)
                           {:type :js-eval-exception
                            :error ret
                            :repl-env repl-env}))
      :success (:value ret))))

#_(defmacro install-node-deps! []
    (boolean (@cljs-install-node-deps!)))
