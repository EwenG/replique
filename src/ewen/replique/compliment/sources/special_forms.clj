(ns ewen.replique.compliment.sources.special-forms
  "Completion for Clojure's special forms."
  (:require [clojure.repl :as repl]
            [ewen.replique.compliment.sources :refer [defsource]]
            [ewen.replique.compliment.sources.ns-mappings :as vars]))

(def ^:private special-forms
  (set (map name '[def if do quote var recur throw try catch
                   monitor-enter monitor-exit new set!])))

(defn first-item-in-list?
  "If context is not nil, check if prefix is the first item in a list form."
  [ctx]
  (if ctx
    (when-let [expr (first ctx)]
      (and (list? (:form expr)) (= (:idx expr) 0)))
    true))

(defn candidates
  "Returns list of completions for special forms."
  [prefix _ context]
  (when (and (vars/var-symbol? prefix) (first-item-in-list? context))
    (for [form special-forms
          :when (vars/dash-matches? prefix form)]
      form)))

#_(defn doc
  "Documentation function for special forms."
  [symbol-str _]
  (when (and (vars/var-symbol? symbol-str) (special-forms symbol-str))
    (vars/generate-docstring (#'repl/special-doc (symbol symbol-str)))))

(defsource ::special-forms
  :candidates #'candidates
  :doc (constantly nil)
  :tag-fn (fn [m _] (assoc m :type :special-form)))

(defn literal-candidates
  "We define `true`, `false`, and `nil` in a separate source because they are
  not context-dependent (don't have to be first items in the list)."
  [prefix _ __]
  (filter #(.startsWith ^String % prefix) ["true" "false" "nil"]))

(defsource ::literals
  :candidates #'literal-candidates
  :doc (constantly nil)
  :tag-fn (fn [m _] (assoc m :type :special-form)))
