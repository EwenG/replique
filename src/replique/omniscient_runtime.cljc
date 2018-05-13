(ns replique.omniscient-runtime)

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

(comment

  (#?(:clj require :cljs require-macros) '[replique.omniscient :as omniscient])

  (defonce captured (atom nil))

  (def ^:dynamic *dd* [1 2])

  (binding [*dd* {:e "e"}]
    (let [[rrrr {rrr :ttt} :as fff] [1 2]]
      (reset! captured (omniscient/capture-env))))

  (fn [eee]
    (let [[eee rrrr {rrr :ttt} & tttt :as fff] '{"rrrr" 55}]
      (omniscient/capture-env)
      nil))

  (omniscient/with-env @captured
    rrrr)
  )
