(ns replique.omniscient-runtime
  (:require [replique.cljs-env.elisp-printer :as elisp])
  #?(:clj (:import [clojure.lang IDeref])))

#?(:clj
   (defn var->sym [v]
     (let [{:keys [ns name]} (meta v)]
       (when (and ns name)
         (symbol (str (ns-name ns)) (str name))))))

#?(:clj
   (defn dynamic-bindings-clj-reducer [excluded-dyn-vars m k v]
     (let [sym (var->sym k)]
       (if (and sym (not (contains? excluded-dyn-vars sym)))
         (assoc m sym v)
         m))))

(defn get-binding-syms-reducer [m k v]
  (if (or (= k :locals) (= k :bindings))
    (assoc m k (keys v))
    m))

(def ^:dynamic *captured-envs* nil)

(defn capture-env-var-value [capture-atom]
  (let [captured-env-val (-> capture-atom meta :replique.watch/value)
        captured-env-val (if (nil? captured-env-val)
                           (deref capture-atom)
                           captured-env-val)]
    captured-env-val))

#?(:cljs
   (defn get-binding-syms [capture-atom]
     (binding [*print-length* nil
               *print-level* nil]
       (let [captured-env-val (capture-env-var-value capture-atom)]
         (pr-str
          (reduce-kv get-binding-syms-reducer {} captured-env-val))))))

#?(:clj
   (defn get-binding-syms [capture-atom]
     (let [captured-env-val (capture-env-var-value (eval capture-atom))]
       (reduce-kv get-binding-syms-reducer {} captured-env-val))))

#?(:cljs
   (defn get-locals [captured-env]
     (binding [*print-length* nil
               *print-level* nil]
       (let [captured-env-val (-> captured-env meta :replique.watch/value)
             captured-env-val (if (and captured-env
                                       (nil? captured-env-val)
                                       (satisfies? IDeref captured-env))
                                @captured-env
                                captured-env-val)]
         (elisp/pr-str
          (-> captured-env-val :locals keys))))))

#?(:clj
   (defn get-locals [captured-env]
     (let [captured-env-val (-> captured-env meta :replique.watch/value)
           captured-env-val (if (and captured-env
                                     (nil? captured-env-val)
                                     (instance? IDeref captured-env))
                              @captured-env
                              captured-env-val)]
       (-> captured-env-val :locals keys))))

(comment

  (defonce env (atom nil))
  
  (let [[eee rrrr {rrr :ttt} & tttt :as fff] '{"rrrr" 55}]
    (replique.interactive/capture-env env)
    nil)

  (replique.interactive/with-env env
    eee)
  )

