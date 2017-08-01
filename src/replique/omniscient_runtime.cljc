(ns replique.omniscient-runtime
  (:refer-clojure :exclude [time]))

(defonce registry (atom {}))

;; the env currently beeing debugged.
;; ie the one that is bound in the omniscient REPL
(defonce ^:dynamic *omniscient-env* nil)
(defonce time (atom nil))

(defn append-env [envs new-env]
  (if (nil? envs) [(assoc new-env :index 0)]
      (->> (count envs) (assoc new-env :index) (conj envs))))

(defn get-env [sym index]
  (get-in @registry [sym index]))

;; time => global var, mute the value on select
;; *omniscient-env* => cljs: set! at the start of the repl, set! to null at the end (in a finally)
;; *omniscient-repl?* bound in the clojure process for both clj and cljs
;; *omniscient-sym* -> idem
;; form-with-locals / form-with-bindings -> keys must be known in the clojure process, vals in the cljs process
;; before starting the repl -> query local keys and binding keys
;; cljs capture-env -> method

(comment
  (require '[replique.environment :as env])
  (require '[replique.repl-cljs :refer [compiler-env]])
  
  (filter (fn [[k v]] (not= "clojure.core" (str (:ns (meta v))))) (ns-map *ns*))

  (defn ns-map-filter-dynamic [ns-map]
    (filter (fn [[k v]] (:dynamic v)) ns-map))

  (defn dyn-vars [comp-env ns]
    (when-let [ns (if (symbol? ns) (env/find-ns comp-env ns) ns)]
      (let [uses (->> (select-keys ns [:uses :renames])
                      vals
                      (map (partial env/cljs-ns-map-resolve comp-env))
                      (map ns-map-filter-dynamique))
            defs (->> (select-keys ns [:defs])
                      vals
                      (map ns-map-filter-dynamique))]
        (->> (concat uses defs)
             (into {})))))

  (require '[cljs.pprint :as pp :refer [pprint]])

  (map (comp :name second) (dyn-vars (env/->CljsCompilerEnv @compiler-env) 'replique.omniscient-runtime))

  (map (comp :name second) (filter (fn [[k v]] (:dynamic v))
                    (env/ns-map (env/->CljsCompilerEnv @compiler-env) 'replique.omniscient-runtime)))
  
  )
