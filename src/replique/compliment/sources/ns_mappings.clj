(ns replique.compliment.sources.ns-mappings
  "Completion for vars and classes in the current namespace."
  (:refer-clojure :exclude [ns-publics ns-map meta find-ns])
  (:require [replique.compliment.sources :refer [defsource]]
            [replique.compliment.utils :refer [fuzzy-matches? *extra-metadata*]]
            [replique.environment :refer [ns-publics ns-map meta resolve-namespace find-ns]])
  (:import java.io.StringWriter))

(defn var-symbol?
  "Test if prefix resembles a var name."
  [x]
  (re-matches #"([^\/\:][^\.\/]*([^\/\:]*\/[^\.\/]*)?)?" x))

(defn dash-matches?
  "Tests if prefix partially matches a var name with dashes as
  separators."
  [prefix var]
  (fuzzy-matches? prefix var \-))

(defn get-scope-and-prefix
  "Tries to get take apart scope namespace and prefix in prefixes like
  `scope/var`."
  [comp-env ^String s, ns]
  (let [[scope-name sym] (if (> (.indexOf s "/") -1)
                           (.split s "/") ())
        scope (when scope-name
                (resolve-namespace comp-env (symbol scope-name) ns))
        prefix (if scope
                 (or sym "") s)]
    [scope-name scope prefix]))

(defn try-get-ns-from-context
  "Tries to extract a namespace name if context is a `ns` definition."
  [comp-env context]
  (let [[var-list ns-def use-def top-form] context]
    (when (and (sequential? (:form var-list))
               (= (first (:form top-form)) 'ns)
               (or (and (= (first (:form use-def)) :use)
                        (= (second (:form ns-def)) :only))
                   (and (= (first (:form use-def)) :require)
                        (= (second (:form ns-def)) :refer))))
      (find-ns comp-env (first (:form ns-def))))))

(defn generate-docstring
  "Generates a docstring from a given var metadata. Copied from
  `clojure.repl` with some minor modifications."
  [m]
  (binding [*out* (StringWriter.)]
    (cond
      (:forms m) (doseq [f (:forms m)]
                   (print "  ")
                   (prn f))
      (:arglists m) (prn (:arglists m)))
    (if (:special-form m)
      (do
        (println "Special Form")
        (println " " (:doc m))
        (if (contains? m :url)
          (when (:url m)
            (println (str "\n  Please see http://clojure.org/" (:url m))))
          (println (str "\n  Please see http://clojure.org/special_forms#"
                        (:name m)))))
      (do
        (when (:macro m)
          (println "Macro"))
        (println " " (:doc m))))
    (str *out*)))

(defn candidates
  "Returns a list of namespace-bound candidates, with namespace being
  either the scope (if prefix is scoped), `ns` arg or the namespace
  extracted from context if inside `ns` declaration."
  ([^String prefix, ns context]
   (candidates nil prefix ns context))
  ([comp-env ^String prefix, ns context]
   (when (var-symbol? prefix)
     (let [[scope-name scope ^String prefix] (get-scope-and-prefix comp-env prefix ns)
           ns-form-namespace (try-get-ns-from-context comp-env context)
           vars (cond
                  scope (ns-publics comp-env scope)
                  ns-form-namespace (ns-publics comp-env ns-form-namespace)
                  :else (ns-map comp-env ns))]
       (for [[var-sym var] vars
             :let [var-name (name var-sym)
                   {:keys [arglists doc] :as var-meta} (meta comp-env var)]
             :when (dash-matches? prefix var-name)]
         (if (= (type var) Class)
           {:candidate var-name, :type :class,
            :package (when-let [pkg (.getPackage ^Class var)]
                       ;; Some classes don't have a package
                       (.getName ^Package pkg))}

           (cond-> {:candidate (if scope
                                 (str scope-name "/" var-name)
                                 var-name)
                    :type (cond (:macro var-meta) :macro
                                arglists :function
                                :else :var)
                    :ns (str (or (:ns var-meta) ns))}
             (and arglists (:arglists *extra-metadata*))
             (assoc :arglists (apply list (map pr-str arglists)))

             )))))))

#_(defn doc
  "Documentation function for this sources' completions."
  [symbol-str ns]
  (if (var-symbol? symbol-str)
    (when-let [var (ns-resolve ns (symbol symbol-str))]
      (when (meta var)
        (generate-docstring (meta var))))))

(defsource ::ns-mappings
  :candidates #'candidates
  :doc (constantly nil))

(comment

  (require '[replique.repl-cljs :refer [compiler-env]])
  (require '[replique.environment :refer [->CljsCompilerEnv]])
  (def comp-env (->CljsCompilerEnv @compiler-env))
  
  (type (replique.environment/find-ns comp-env 'cljs.core))
  (count (replique.environment/ns-publics comp-env 'cljs.core))
  (count (replique.environment/ns-core-refers comp-env 'cljs.core))
  (count (replique.environment/ns-map comp-env 'cljs.core))

  (get (:cljs.analyzer/namespaces @@compiler-env) 'cljs.user)

  {:rename-macros {}, :renames {}, :use-macros {doc cljs.repl, find-doc cljs.repl, dir cljs.repl, pst cljs.repl, pp cljs.pprint, source cljs.repl, apropos cljs.repl}, :excludes #{}, :name cljs.user, :imports nil, :requires {cljs.repl cljs.repl, cljs.pprint cljs.pprint}, :uses {pprint cljs.pprint}, :require-macros {cljs.repl cljs.repl, cljs.pprint cljs.pprint}, :doc nil}

  (replique-ns/ns-publics 'replique.compliment.ns-mappings-cljs-test @compiler-env)
  (replique-ns/ns-publics 'replique.compliment.ns-mappings-clj-test nil)

  (get (replique.environment/ns-publics nil 'replique.compliment.ns-mappings-clj-test)
       'my-fn)
  (get (replique.environment/ns-publics comp-env 'replique.compliment.ns-mappings-cljs-test)
       'my-fn)

  (candidates comp-env "clj" (find-ns comp-env 'replique.compliment.ns-mappings-cljs-test) nil)
  (candidates "cloj" (find-ns nil 'replique.compliment.ns-mappings-clj-test) nil)

  )
