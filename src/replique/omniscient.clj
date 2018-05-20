(ns replique.omniscient
  (:require [replique.utils :as utils]
            [replique.environment :as env]
            [replique.omniscient-runtime]))

(def ^:private cljs-compiler-env (utils/dynaload 'replique.repl-cljs/compiler-env))
(def ^:private cljs-eval-cljs-form (utils/dynaload 'replique.repl-cljs/eval-cljs-form))

;; clojure.core/*data-readers* clojure.core/*default-data-reader-fn*

;; Prevent dynamic vars used by the REPL/system from beeing captured
(def excluded-dyn-vars #{'clojure.core/*3 'clojure.core/*print-meta* 'clojure.core/*print-namespace-maps* 'clojure.core/*file* 'clojure.core/*command-line-args* 'clojure.core/*2 'clojure.core/*err* 'clojure.core/*print-length* 'clojure.core/*math-context* 'clojure.core/*e 'clojure.core/*1 'clojure.core/*source-path* 'clojure.core/*unchecked-math* 'clojure.spec/*explain-out* 'clojure.core/*in* 'clojure.core/*print-level* 'replique.server/*session* 'clojure.core/*warn-on-reflection* 'clojure.core/*out* 'clojure.core/*assert* 'clojure.core/*read-eval* 'clojure.core/*ns* 'clojure.core/*compile-path* 'clojure.core.server/*session* 'clojure.spec.alpha/*explain-out* 'clojure.core/pr 'replique.watch/*printed* 'replique.watch/*results* 'replique.omniscient-runtime/*captured-envs*})

(defn generated-local? [local]
  (re-matches #"^(vec__|map__|seq__|first__)[0-9]+$" (str local)))

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

(defn position [env form]
  (let [{:keys [line column file]} (meta form)]
    (if (utils/cljs-env? env)
      (str file ":" line ":" column)
      (str *file* ":" line ":" column))))

(defmacro capture-env [& body]
  `(let [captured-env# ~{:locals (locals-map (env->locals &env))
                         ;; exclude dynamic vars that are used by the REPL/system
                         :bindings (dynamic-bindings &env)
                         :position (position &env &form)}
         [result# captured-envs#] (binding [replique.omniscient-runtime/*captured-envs* []]
                                    [(do ~@body) replique.omniscient-runtime/*captured-envs*])]
     (cond (some? replique.omniscient-runtime/*captured-envs*)
           (set! replique.omniscient-runtime/*captured-envs*
                 (conj replique.omniscient-runtime/*captured-envs*
                       (assoc captured-env# :child-envs captured-envs#)))
           (seq captured-envs#)
           (reset! replique.omniscient-runtime/captured-env
                   (assoc captured-env# :child-envs captured-envs#))
           :else (reset! replique.omniscient-runtime/captured-env captured-env#))
     result#))

(defn get-binding-syms [env captured-env]
  (if (utils/cljs-env? env)
    (let [res (@cljs-eval-cljs-form (:repl-env env)
               `(binding [*print-length* nil
                          *print-level* nil]
                  (pr-str
                   (reduce-kv replique.omniscient-runtime/get-binding-syms-reducer
                              {} ~captured-env))))]
      (read-string (read-string res)))
    (reduce-kv replique.omniscient-runtime/get-binding-syms-reducer {} (eval captured-env))))

(defn locals-reducer [captured-env acc local-sym]
  (conj acc local-sym `(get-in ~captured-env [:locals (quote ~local-sym)])))

(defn bindings-reducer [captured-env acc binding-sym]
  (conj acc binding-sym `(get-in ~captured-env [:bindings (quote ~binding-sym)])))

(defmacro with-env [& body]
  (let [captured-env `(:replique.watch/value (meta replique.omniscient-runtime/captured-env))
        syms (get-binding-syms &env captured-env)
        locals-syms (:locals syms)
        binding-syms (:bindings syms)]
    `(binding ~(reduce (partial bindings-reducer captured-env) [] binding-syms)
       (let ~(reduce (partial locals-reducer captured-env) [] locals-syms)
         ~@body))))

(comment
  (defn rrr2 [x y]
    (capture-env)
    y)

  (defn rrr []
    (capture-env (rrr2 1 2)))
  
  (capture-env (rrr))
  (capture-env 33)

  (with-env
    y)
  )
