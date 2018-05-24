(ns replique.omniscient-runtime
  (:require [replique.cljs-env.elisp-printer :as elisp])))

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

#?(:cljs
   (defn get-binding-syms []
     (binding [*print-length* nil
               *print-level* nil]
       (pr-str
        (reduce-kv get-binding-syms-reducer {} (:replique.watch/value (meta captured-env)))))))

#?(:clj
   (defn get-binding-syms []
     (reduce-kv get-binding-syms-reducer {} (:replique.watch/value (meta captured-env)))))

#?(:cljs
   (defn get-locals []
     (binding [*print-length* nil
               *print-level* nil]
       (elisp/pr-str
        (-> captured-env
            meta
            :replique.watch/value
            :locals
            keys)))))

#?(:clj
   (defn get-locals []
     (-> captured-env
         meta
         :replique.watch/value
         :locals
         keys)))

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
