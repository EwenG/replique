(ns replique.compliment.sources.special-forms
  "Completion for Clojure's special forms."
  (:require [clojure.repl :as repl]
            [replique.compliment.sources :refer [defsource]]
            [replique.compliment.sources.ns-mappings :as vars]
            [replique.environment :refer [special-forms]]))

(defn first-item-in-list?
  "If context is not nil, check if prefix is the first item in a list form."
  [ctx]
  (if ctx
    (when-let [expr (first ctx)]
      (and (list? (:form expr)) (= (:idx expr) 0)))
    true))

(defn candidates
  "Returns list of completions for special forms."
  ([prefix ns context]
   (candidates prefix ns context))
  ([comp-env prefix _ context]
   (when (and (vars/var-symbol? prefix) (first-item-in-list? context))
     (for [form (special-forms comp-env)
           :when (vars/dash-matches? prefix form)]
       {:candidate form
        :type :special-form}))))

#_(defn doc
  "Documentation function for special forms."
  [symbol-str _]
  (when (and (vars/var-symbol? symbol-str) (special-forms symbol-str))
    (vars/generate-docstring (#'repl/special-doc (symbol symbol-str)))))

(defsource ::special-forms
  :candidates #'candidates
  :doc (constantly nil))

(defn literal-candidates
  "We define `true`, `false`, and `nil` in a separate source because they are
  not context-dependent (don't have to be first items in the list)."
  ([prefix ns context]
   (literal-candidates nil prefix ns context))
  ([comp-env prefix ns context]
   (->> ["true" "false" "nil"]
        (filter #(.startsWith ^String % prefix))
        (map (fn [c] {:candidate c, :type :special-form})))))

(defsource ::literals
  :candidates #'literal-candidates
  :doc (constantly nil))
