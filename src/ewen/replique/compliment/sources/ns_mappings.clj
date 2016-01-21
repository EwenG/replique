(ns ewen.replique.compliment.sources.ns-mappings
  "Completion for vars and classes in the current namespace."
  (:require [ewen.replique.compliment.sources :refer [defsource]]
            [ewen.replique.compliment.utils
             :refer [fuzzy-matches? resolve-namespace]])
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
  [^String s, ns]
  (let [[scope-name sym] (if (> (.indexOf s "/") -1)
                           (.split s "/") ())
        scope (when scope-name
                (resolve-namespace (symbol scope-name) ns))
        prefix (if scope
                 (or sym "") s)]
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
  [^String prefix, ns context]
  (if (var-symbol? prefix)
    (let [[scope-name scope ^String prefix] (get-scope-and-prefix prefix ns)
          ns-form-namespace (try-get-ns-from-context context)
          vars (cond
                scope (ns-publics scope)
                ns-form-namespace (ns-publics ns-form-namespace)
                :else (ns-map ns))]
      (for [[var _] vars
            :let [var-name (name var)]
            :when (dash-matches? prefix var-name)]
        (if scope
          (str scope-name "/" var-name)
          var-name)))))

(defn generate-docstring
  "Generates a docstring from a given var metadata. Copied from
  `clojure.repl` with some minor modifications."
  [m]
  (binding [*out* (StringWriter.)]
    (println (str (when-let [ns (:ns m)] (str (ns-name ns) "/")) (:name m)))
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

(defn doc
  "Documentation function for this sources' completions."
  [symbol-str ns]
  (if (var-symbol? symbol-str)
    (when-let [var (ns-resolve ns (symbol symbol-str))]
      (when (meta var)
        (generate-docstring (meta var))))))

(defn- infer-type [var-meta]
  (cond (:macro var-meta) :macro
        (:arglists var-meta) :function
        :else :var))

(defsource ::ns-mappings
  :candidates #'candidates
  :doc #'doc
  :tag-fn (fn [m {:keys [ns extra-metadata]}]
            (let [c (:candidate m)
                  var (when (var-symbol? c)
                        (ns-resolve ns (symbol c)))
                  var-meta (meta var)]

              (cond (= (type var) Class)
                    (assoc m :type :class
                           :package (.getName (.getPackage ^Class var)))

                    var-meta
                    (cond-> (assoc m :type (infer-type var-meta)
                                   :ns (str (or (:ns var-meta) ns)))
                      (and (:arglists extra-metadata) (:arglists var-meta))
                      (assoc :arglists (apply list (map pr-str (:arglists var-meta))))

                      (and (:doc extra-metadata) (:doc var-meta))
                      (assoc :doc (generate-docstring var-meta)))

                    :else m))))
