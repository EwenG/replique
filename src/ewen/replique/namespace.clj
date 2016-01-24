(ns ewen.replique.namespace
  (:refer-clojure :exclude [find-ns ns-publics ns-map ns-aliases all-ns]))

(defn all-ns
  ([]
   (all-ns nil))
  ([cljs-comp-env]
   (if-not cljs-comp-env
     (clojure.core/all-ns)
     (keys (:cljs.analyzer/namespaces cljs-comp-env)))))

(defrecord CljsNamespace [name doc excludes use-macros require-macros uses
                          requires imports defs])

(defn find-ns
  ([ns]
   (find-ns ns nil))
  ([ns cljs-comp-env]
   (if-not cljs-comp-env
     (clojure.core/find-ns ns)
     (as-> (:cljs.analyzer/namespaces @cljs-comp-env) $
       (get $ ns)
       (when $ (map->CljsNamespace $))))))

(defprotocol Namespace
  (ns-publics [ns comp-env])
  (ns-map [ns comp-env])
  (ns-aliases [ns comp-env]))

(defn ns-core-refers
  "Returns a list of cljs.core vars visible to the ns."
  [ns cljs-comp-env]
  (let [vars (ns-publics 'cljs.core cljs-comp-env)
        excludes (:excludes (find-ns ns cljs-comp-env))]
    (apply dissoc vars excludes)))

(extend-protocol Namespace
  CljsNamespace
  (ns-publics [ns cljs-comp-env]
    (->> (merge
          (:defs ns)
          (:macros ns))
         (remove (fn [[k v]] (:private v)))
         (into {})))
  (ns-map [ns cljs-comp-env]
    (->> (select-keys ns [:imports :uses :defs])
         (map second)
         (apply merge)
         (merge (ns-core-refers ns cljs-comp-env))))
  (ns-aliases [ns cljs-comp-env]
    (:requires ns))
  clojure.lang.Symbol
  (ns-publics [ns cljs-comp-env]
    (if-not cljs-comp-env
      (clojure.core/ns-publics ns)
      (ns-publics (find-ns ns cljs-comp-env)
                  cljs-comp-env)))
  (ns-map [ns cljs-comp-env]
    (if-not cljs-comp-env
      (clojure.core/ns-map ns)
      (ns-map (find-ns ns cljs-comp-env)
              cljs-comp-env)))
  (ns-aliases [ns cljs-comp-env]
    (if-not cljs-comp-env
      (clojure.core/ns-aliases ns)
      (ns-aliases (find-ns cljs-comp-env)
                  cljs-comp-env)))
  clojure.lang.Namespace
  (ns-publics [ns cljs-comp-env]
    (clojure.core/ns-publics ns))
  (ns-map [ns cljs-comp-env]
    (clojure.core/ns-map ns))
  (ns-aliases [ns cljs-comp-env]
    (clojure.core/ns-aliases ns)))
