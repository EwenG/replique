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

(def ^:dynamic *captured-envs* nil)
(defonce captured-env (atom nil))

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

(comment
  (defn rrr2 [x y]
    (replique.interactive/capture-env)
    y)

  (defn rrr []
    (replique.interactive/capture-env (rrr2 1 2)))

  (dotimes [x 10000]
    (rrr))

  (let [rrr "rrr"]
    (replique.interactive/capture-env 33))
  
  (replique.interactive/with-env
    rrr)

  )
