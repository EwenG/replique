(ns replique.omniscient
  (:require [replique.utils :as utils]
            [replique.environment :as env]
            [replique.omniscient-runtime]
            [replique.elisp-printer :as elisp]
            [replique.meta]
            [replique.watch :as watch]))

(def ^:private cljs-compiler-env (utils/dynaload 'replique.repl-cljs/compiler-env))
(def ^:private cljs-eval-cljs-form (utils/dynaload 'replique.repl-cljs/eval-cljs-form))
(def ^:private cljs-evaluate-form (utils/dynaload 'replique.repl-cljs/-evaluate-form))
(def ^:private cljs-munged (utils/dynaload 'cljs.compiler/munge))

;; clojure.core/*data-readers* clojure.core/*default-data-reader-fn*

;; Prevent dynamic vars used by the REPL/system from beeing captured
(def excluded-dyn-vars #{'clojure.core/*3 'clojure.core/*print-meta* 'clojure.core/*print-namespace-maps* 'clojure.core/*file* 'clojure.core/*command-line-args* 'clojure.core/*2 'clojure.core/*err* 'clojure.core/*print-length* 'clojure.core/*math-context* 'clojure.core/*e 'clojure.core/*1 'clojure.core/*source-path* 'clojure.core/*unchecked-math* 'clojure.spec/*explain-out* 'clojure.core/*in* 'clojure.core/*print-level* 'replique.server/*session* 'clojure.core/*warn-on-reflection* 'clojure.core/*out* 'clojure.core/*assert* 'clojure.core/*read-eval* 'clojure.core/*ns* 'clojure.core/*compile-path* 'clojure.core.server/*session* 'clojure.spec.alpha/*explain-out* 'clojure.core/pr 'replique.watch/*printed* 'replique.watch/*results* 'replique.omniscient-runtime/*captured-envs*})

(defn generated-local? [local]
  (re-matches #"^(vec__|map__|seq__|first__|p__)[0-9]+$" (str local)))

(defn env->locals [env]
  (let [locals (if (utils/cljs-env? env)
                 (:locals env) env)]
    (->> (keys locals)
         (remove generated-local?)
         (into []))))

(defn locals-map [locals]
  (zipmap (map (partial list 'quote) locals) locals))

(defn dynamic-bindings-clj []
  `(reduce-kv
    (partial replique.omniscient-runtime/dynamic-bindings-clj-reducer excluded-dyn-vars)
    {} (get-thread-bindings)))

(defn ns-map-filter-dynamic [ns-map]
  (filter (fn [[k v]] (:dynamic v)) ns-map))

(defn dyn-vars [comp-env ns]
  (let [uses (->> (select-keys ns [:uses :renames])
                  vals
                  (map (partial env/cljs-ns-map-resolve comp-env))
                  (map ns-map-filter-dynamic))
        defs (->> (select-keys ns [:defs])
                  vals
                  (map ns-map-filter-dynamic))]
    (->> (concat uses defs)
         (into {}))))

(defn dynamic-bindings-cljs [comp-env ns]
  (let [dyn-vars (->> (dyn-vars comp-env ns)
                      (map (comp :name second))
                      (remove (partial contains? excluded-dyn-vars)))]
    (zipmap (map (fn [x] `(quote ~x)) dyn-vars) dyn-vars)))

(defn dynamic-bindings [env]
  (if (utils/cljs-env? env)
    (dynamic-bindings-cljs (env/->CljsCompilerEnv @@cljs-compiler-env) (:ns env))
    (dynamic-bindings-clj)))

(defn safe-ns-resolve [comp-env ns sym]
  (try (env/ns-resolve comp-env ns sym)
       (catch ClassNotFoundException e nil)))

(defn capture-env [env form capture-atom body]
  (let [{:keys [file line column] :or {file *file*}} (meta form)
        position-str (str file ":" line ":" column)]
    `(let [captured-env# ~{:locals (locals-map (env->locals env))
                           ;; exclude dynamic vars that are used by the REPL/system
                           :bindings (dynamic-bindings env)
                           :position position-str}
           [result# captured-envs#] (binding [replique.omniscient-runtime/*captured-envs* []]
                                          [(do ~@body)
                                           replique.omniscient-runtime/*captured-envs*])]
       (let [captured-env# (if (seq captured-envs#)
                             (assoc captured-env# :child-envs captured-envs#)
                             captured-env#)]
         (reset! ~capture-atom captured-env#))
       result#)))

(defn capture-child-env [env form body]
  (let [{:keys [file line column] :or {file *file*}} (meta form)
        position-str (str file ":" line ":" column)]
    `(let [captured-env# ~{:locals (locals-map (env->locals env))
                           ;; exclude dynamic vars that are used by the REPL/system
                           :bindings (dynamic-bindings env)
                           :position position-str}
           [result# captured-envs#] (binding [replique.omniscient-runtime/*captured-envs* []]
                                          [(do ~@body)
                                           replique.omniscient-runtime/*captured-envs*])]
       (when (some? replique.omniscient-runtime/*captured-envs*)
         (let [captured-env# (if (seq captured-envs#)
                               (assoc captured-env# :child-envs captured-envs#)
                               captured-env#)]
           (set! replique.omniscient-runtime/*captured-envs*
                 (conj replique.omniscient-runtime/*captured-envs* captured-env#))))
       result#)))

(defn get-binding-syms [env capture-atom]
  (if (utils/cljs-env? env)
    (let [res (@cljs-eval-cljs-form (:repl-env env)
               `(replique.omniscient-runtime/get-binding-syms ~capture-atom))]
      (read-string (read-string res)))
    (replique.omniscient-runtime/get-binding-syms capture-atom)))

(defn bindings-reducer [capture-atom acc binding-sym]
  (conj acc binding-sym
        `(-> (replique.omniscient-runtime/capture-env-var-value ~capture-atom)
             :bindings (get (quote ~binding-sym)))))

(defn locals-reducer [capture-atom acc local-sym]
  (conj acc local-sym
        `(-> (replique.omniscient-runtime/capture-env-var-value ~capture-atom)
             :locals (get (quote ~local-sym)))))

(defn with-env [env capture-atom body]
  ;; :repl-env may be nil when compiling files
  (when-not (and (utils/cljs-env? env) (nil? (:repl-env env)))
    (let [syms (get-binding-syms env capture-atom)
          locals-syms (:locals syms)
          binding-syms (:bindings syms)]
      `(binding ~(reduce (partial bindings-reducer capture-atom)
                         [] binding-syms)
         (let ~(reduce (partial locals-reducer capture-atom)
                       [] locals-syms)
           ~@body)))))

(defn captured-env-locals [comp-env ns captured-env]
  (let [ns (or (and ns (env/find-ns comp-env (symbol ns)))
               (env/find-ns comp-env (env/default-ns comp-env)))
        resolved (when capture-env
                   (safe-ns-resolve comp-env ns (symbol captured-env)))]
    (when resolved
      (replique.omniscient-runtime/get-locals @resolved))))

(defn captured-env-locals-cljs [comp-env repl-env ns captured-env]
  (let [ns (or (and ns (env/find-ns comp-env (symbol ns)))
               (env/find-ns comp-env (env/default-ns comp-env)))
        resolved (when capture-env
                   (safe-ns-resolve comp-env ns (symbol captured-env)))]
    (when (:name resolved)
      (let [{:keys [status value]} (@cljs-evaluate-form
                                    repl-env
                                    (format "replique.omniscient_runtime.get_locals(%s);"
                                            (@cljs-munged (str (:name resolved))))
                                    :timeout-before-submitted 100)]
        (when (= :success status)
          (elisp/->ElispString value))))))

(comment
  (def env (atom nil))
  
  (let [eeee 44]
    (replique.interactive/capture-env env
     44))
  
  (replique.interactive/with-env env
    eeee)
  )
