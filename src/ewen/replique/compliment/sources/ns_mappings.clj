(ns ewen.replique.compliment.sources.ns-mappings
  "Completion for vars and classes in the current namespace."
  (:require [ewen.replique.compliment.sources :refer [defsource]]
            [ewen.replique.compliment.utils
             :refer [fuzzy-matches? resolve-namespace]]
            [ewen.replique.namespace :as replique-ns])
  (:import java.io.StringWriter))

(defn var-symbol?
  "Test if prefix resembles a var name."
  [x]
  (re-matches #"[^\.\/\:]*([^\/\:]+\/[^\.\/\:]*)?" x))

(defn dash-matches?
  "Tests if prefix partially matches a var name with dashes as
  separators."
  [prefix var]
  (fuzzy-matches? prefix var \-))

(defn get-scope-and-prefix
  "Tries to get take apart scope namespace and prefix in prefixes like
  `scope/var`."
  [^String s ns cljs-comp-env]
  (let [[scope-name sym] (if (> (.indexOf s "/") -1)
                           (.split s "/") ())
        scope (when scope-name
                (resolve-namespace (symbol scope-name) ns cljs-comp-env))
        prefix (if scope (or sym "") s)]
    [scope-name scope prefix]))

(defn try-get-ns-from-context
  "Tries to extract a namespace name if context is a `ns` definition."
  [context]
  (let [[var-list ns-def use-def top-form] context]
    (when (and (sequential? (:form var-list))
               (= (first (:form top-form)) 'ns)
               (or (and (= (first (:form use-def)) :use)
                        (= (second (:form ns-def)) :only))
                   (and (= (first (:form use-def)) :require)
                        (= (second (:form ns-def)) :refer))))
      (find-ns (first (:form ns-def))))))

(defn candidates
  "Returns a list of namespace-bound candidates, with namespace being
  either the scope (if prefix is scoped), `ns` arg or the namespace
  extracted from context if inside `ns` declaration."
  ([^String prefix ns context]
   (candidates prefix ns context nil))
  ([^String prefix ns context cljs-comp-env]
   (if (var-symbol? prefix)
     (let [[scope-name scope ^String prefix]
           (get-scope-and-prefix prefix ns cljs-comp-env)
           ns-form-namespace (try-get-ns-from-context context)
           vars (cond
                  scope (replique-ns/ns-publics scope cljs-comp-env)
                  ns-form-namespace
                  (replique-ns/ns-publics ns-form-namespace cljs-comp-env)
                  :else (replique-ns/ns-map ns cljs-comp-env))]
       (for [[var _] vars
             :let [var-name (name var)]
             :when (dash-matches? prefix var-name)]
         (if scope
           (str scope-name "/" var-name)
           var-name))))))

(defsource ::ns-mappings
  :candidates #'candidates
  :doc (constantly nil))

(comment
  (get (:cljs.analyzer/namespaces @compiler-env) 'ewen.replique.test3)

  {:name ewen.replique.test3, :doc nil, :excludes #{}, :use-macros nil, :require-macros nil, :uses {pr cljs.core, prn cljs.core}, :requires {cc cljs.core, cljs.core cljs.core}, :imports nil, :defs {rr {:name ewen.replique.test3/rr, :file "/home/egr/replique.el/clojure/ewen/replique/out/ewen/replique/test3.cljs", :line 4, :column 1, :end-line 4, :end-column 8, :meta {:file "/home/egr/replique.el/clojure/ewen/replique/out/ewen/replique/test3.cljs", :line 4, :column 6, :end-line 4, :end-column 8}}, tt {:protocol-inline nil, :meta {:file "/home/egr/replique.el/clojure/ewen/replique/out/ewen/replique/test3.cljs", :line 6, :column 8, :end-line 6, :end-column 10, :private true, :arglists (quote ([]))}, :private true, :name ewen.replique.test3/tt, :variadic false, :file "/home/egr/replique.el/clojure/ewen/replique/out/ewen/replique/test3.cljs", :end-column 10, :method-params ([]), :protocol-impl nil, :arglists-meta (nil nil), :column 1, :line 6, :end-line 6, :max-fixed-arity 0, :fn-var true, :arglists (quote ([]))}}}

  (ewen.replique.reflection/find-ns 'ewen.replique.test3 @compiler-env)
  (ewen.replique.reflection/find-ns 'blabla @compiler-env)
  (ewen.replique.reflection/ns-publics 'ewen.replique.test3 @compiler-env)
  (count (ewen.replique.reflection/ns-core-refers 'ewen.replique.test3 @compiler-env))
  (first (ewen.replique.reflection/ns-core-refers 'ewen.replique.test3 @compiler-env))
  (count (ewen.replique.reflection/ns-map 'ewen.replique.test3 @compiler-env))
  )
