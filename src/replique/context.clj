(ns replique.context
  (:require [replique.environment :as env :refer [->CljsCompilerEnv]]))

(def categories->vars {:in-ns {'clojure.core/in-ns nil}
                       :binding {'cljs.core/let 0
                                 'clojure.core/let 0
                                 'clojure.core/for 0}})

(defn categories->vars-namespaces-reducer [namespaces category vars]
  (reduce #(conj %1 (-> %2 key namespace)) namespaces vars))

(def categories-namespaces (reduce-kv categories->vars-namespaces-reducer
                                            #{} categories->vars))

(defn reverse-aliases-reducer [reversed-aliases alias-sym ns]
  (let [ns-str (str (env/ns-name ns))]
    (if (contains? categories-namespaces ns-str)
      (assoc reversed-aliases ns-str alias-sym)
      reversed-aliases)))

(defn compute-category->syms [comp-env the-ns category->syms aliases vars]
  (if-let [[v v-data] (first vars)]
    (let [v-name (name v)
          v-sym (symbol v-name)
          v-ns-str (namespace v)
          resolved (env/ns-resolve comp-env the-ns v-sym)
          referred? (= (-> (env/meta comp-env resolved) :ns str) v-ns-str)
          category->syms (if referred?
                           (assoc category->syms v-sym v-data)
                           category->syms)
          category->syms (if-let [alias-sym (get aliases v-ns-str)]
                           (assoc category->syms (symbol (str alias-sym) v-name) v-data)
                           category->syms)
          fully-resolved (env/ns-resolve comp-env (symbol v-ns-str) v-sym)
          category->syms (if fully-resolved
                           (assoc category->syms v v-data)
                           category->syms)]
      (recur comp-env the-ns category->syms aliases (rest vars)))
    category->syms))

;; We assume (:require :rename) is not used
(defn compute-categories->syms [comp-env ns contexts]
  (let [ns (symbol ns)
        the-ns (env/find-ns comp-env ns)]
    (when the-ns
      (let [aliases (reduce-kv reverse-aliases-reducer {}
                               (env/ns-aliases comp-env the-ns))]
        (loop [contexts (seq contexts)
               categories->syms {}]
          (if-let [category (first contexts)]
            (let [vars (get categories->vars category)
                  category->syms (compute-category->syms comp-env the-ns {} aliases vars)]
              (recur (rest contexts) (assoc categories->syms category category->syms)))
            categories->syms))))))

;; force additional entries for cljs special fns
(defn compute-categories->syms-cljs [comp-env ns contexts]
  (-> (compute-categories->syms comp-env ns contexts)
      (update :in-ns merge {'in-ns nil 'clojure.core/in-ns nil})))
