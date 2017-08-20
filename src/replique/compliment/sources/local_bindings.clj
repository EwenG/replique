(ns replique.compliment.sources.local-bindings
  "Completion source for local bindings introduced by defn, let and the like."
  (:require [replique.compliment.sources :refer [defsource]]
            [replique.compliment.sources.ns-mappings :refer [var-symbol? dash-matches?]]))

(def let-like-forms '#{let if-let when-let if-some when-some loop with-open
                       dotimes with-local-vars})

(def defn-like-forms '#{defn defn- fn defmacro})

(def doseq-like-forms '#{doseq for})

(def letfn-like-forms '#{letfn})

(defn tree-leaves [t]
  (filter #(not (coll? %)) (tree-seq coll? seq t)))

(defn even-seq [s]
  (if (even? (count s))
    s
    (take (dec (count s)) s)))

(defn parse-binding
  "Given a binding node returns the list of local bindings introduced by that
  node. Handles vector and map destructuring."
  [binding-node]
  (cond (vector? binding-node)
        (mapcat parse-binding binding-node)

        (map? binding-node)
        (let [normal-binds (->> (keys binding-node)
                                (remove keyword?)
                                (mapcat parse-binding))
              keys-binds (if-let [ks (:keys binding-node)]
                           (mapv str ks) ())
              as-binds (if-let [as (:as binding-node)]
                        [(str as)] ())]
          (concat normal-binds keys-binds as-binds))

        (not (#{'& '_} binding-node))
        [(str binding-node)]))

(defn parse-fn-body
  "Extract function name and arglists from the function body, return list of all
  completable variables."
  [fn-body]
  (let [fn-name (when (symbol? (first fn-body))
                  (name (first fn-body)))
        fn-body (if fn-name (rest fn-body) fn-body)]
    (let [params-bindings
          (mapcat parse-binding
                  (loop [[c & r] fn-body, bnodes []]
                    (cond (nil? c) bnodes
                          (list? c) (recur r (conj bnodes (first c))) ;; multi-arity case
                          (vector? c) c                               ;; single-arity case
                          :else (recur r bnodes))))
          all-bindings (if fn-name (conj params-bindings fn-name) params-bindings)]
      (if (first (filter #(= "__prefix__" %) all-bindings))
        '()
        all-bindings))))

(defn has-prefix? [s]
  (first (filter #(= '__prefix__ %) (tree-leaves s))))

(defn extract-local-bindings
  "When given a form that has a binding vector traverses that binding vector and
  returns the list of all local bindings."
  [form]
  (when (list? form)
    (cond (let-like-forms (first form))
          (->> (second form)
               (take-while (comp not has-prefix?))
               even-seq
               (take-nth 2)
               (mapcat parse-binding))

          (defn-like-forms (first form)) (parse-fn-body (rest form))

          (letfn-like-forms (first form))
          (mapcat parse-fn-body (second form))

          (doseq-like-forms (first form))
          (->> (partition 2 (second form))
               (mapcat (fn [[left right]]
                         (case left
                           :let right
                           :while []
                           :when []
                           [left right])))
               (take-while (comp not has-prefix?))
               even-seq
               (take-nth 2)
               (mapcat parse-binding))
          
          (= 'as-> (first form)) (let [b (nth form 2)]
                                   (if (= '__prefix__ b)
                                     []
                                     [(name b)])))))

(comment
  (extract-local-bindings '(let [a b [x x2] [y __prefix__] z zb]))
  (extract-local-bindings '(let [a b c]
                             c))
  
  (extract-local-bindings '(defn ff [x y z __prefix__]
                             (let [e 33] e)))
  (extract-local-bindings '(defn __prefix__))
  (extract-local-bindings '(fn [a b]))

  (extract-local-bindings '(doseq [a b
                                   :when [c d]
                                   :let [f g]] a))
  
  (extract-local-bindings '(as-> 0 __prefix__))
  )

(defn bindings-from-context
  "Returns all local bindings that are established inside the given context."
  [ctx]
  (try (distinct (mapcat (comp extract-local-bindings :form) ctx))
       (catch Exception ex ())))

(defn candidates
  "Returns a list of local bindings inside the context that match prefix."
  ([prefix ns context]
   (candidates nil prefix ns context))
  ([comp-env prefix _ context]
   (when (var-symbol? prefix)
        (for [binding (bindings-from-context context)
              :when (dash-matches? prefix binding)]
          {:candidate binding, :type :local}))))

(defsource ::local-bindings
  :candidates #'candidates
  :doc (constantly nil))
