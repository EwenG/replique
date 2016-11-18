(ns replique.compliment.sources.namespaces-and-classes
  "Completion for namespace and class names."
  (:refer-clojure :exclude [all-ns ns-aliases find-ns])
  (:require [replique.compliment.sources :refer [defsource]]
            [replique.compliment.utils :refer [fuzzy-matches? defmemoized] :as utils]
            [replique.compliment.sources.class-members :refer [classname-doc]]
            [replique.environment :refer [all-ns ns-aliases namespaces-on-classpath
                                          provides-from-js-dependency-index find-ns]])
  (:import java.io.File replique.environment.CljsCompilerEnv))

(defn nscl-symbol?
  "Tests if prefix looks like a namespace or classname."
  [x]
  (re-matches #"[^\/\:\.][^\/\:]+" x))

(defn nscl-matches?
  "Tests if prefix partially matches a var name with periods as
  separators."
  [prefix namespace]
  (fuzzy-matches? prefix namespace \.))

;;; Obtaining the list of classes

(defn imported-classes
  "Returns names of all classes imported into a given namespace."
  [ns]
  (for [[_ ^Class val] (ns-map ns) :when (class? val)]
    (.getName val)))

(defmemoized all-classes-short-names
  "Returns a map where short classnames are matched with vectors with
  package-qualified classnames."
  []
  (group-by #(-> (re-matches #"([^\.]+\.)*([^\.]+)" %)
                 (nth 2))
            (reduce into [] (vals (utils/classes-on-classpath)))))

(defn- analyze-import-context
  "Checks if the completion is called from ns import declaration. If so, and the
  prefix is inside import vector, return that package name, otherwise return
  `:root`. If not inside :import, return nil."
  [ctx]
  (let [ns-decl (:form (last ctx))
        import-list (:form (last (butlast ctx)))
        prefix-form (:form (first ctx))]
    (when (and (sequential? ns-decl)
               (= (first ns-decl) 'ns)
               (sequential? import-list)
               (= (first import-list) :import))
      (if (= prefix-form import-list)
        :root
        (str (first prefix-form))))))

(defn- get-all-full-names
  "Returns a list of package-qualified classnames given a short classname."
  [prefix]
  (reduce-kv (fn [l, ^String short-name, full-names]
               (if (.startsWith short-name prefix)
                 (concat l (map (fn [c] {:candidate c, :type :class})
                                full-names))
                 l))
             ()
             (all-classes-short-names)))

(defn- get-classes-by-package-name
  "Returns simple classnames that match the `prefix` and belong to `pkg-name`."
  [prefix pkg-name]
  (reduce-kv (fn [l, ^String short-name, full-names]
               (if (and (.startsWith short-name prefix)
                        (some #(.startsWith ^String % pkg-name) full-names))
                 (conj l {:candidate short-name, :type :class})
                 l))
             ()
             (all-classes-short-names)))

(defn candidates
  "Returns a list of namespace and classname completions."
  ([^String prefix, ns context]
   (candidates nil prefix ns context))
  ([comp-env ^String prefix, ns context]
   (when (nscl-symbol? prefix)
     (let [has-dot (> (.indexOf prefix ".") -1)
           import-ctx (analyze-import-context context)]
       ((comp distinct concat)
        (for [ns-str (concat (map (comp name ns-name) (all-ns comp-env))
                             (when-not has-dot
                               (map name (keys (ns-aliases comp-env ns)))))
              :when (nscl-matches? prefix ns-str)]
          {:candidate ns-str, :type :namespace})
        (when (not comp-env)
          (for [class-str (imported-classes ns)
                :when (nscl-matches? prefix class-str)]
            {:candidate class-str, :type :class}))
        (cond (and (not comp-env) (= import-ctx :root)) (get-all-full-names prefix)
              (and (not comp-env) import-ctx) (get-classes-by-package-name prefix import-ctx))
        ;; Fuzziness is too slow for all classes, so just startsWith.
        ;; Also have to do clever tricks to keep the performance high.
        (if has-dot
          (concat (when (not comp-env)
                    (for [[root-pkg classes] (utils/classes-on-classpath)
                          :when (.startsWith prefix root-pkg)
                          ^String cl-str classes
                          :when (.startsWith cl-str prefix)]
                      {:candidate cl-str, :type :class}))
                  (for [ns-str (namespaces-on-classpath comp-env)
                        :when (nscl-matches? prefix ns-str)]
                    {:candidate ns-str, :type :namespace})
                  (when (= CljsCompilerEnv (type comp-env))
                    (for [ns-str (provides-from-js-dependency-index comp-env)
                          :when (nscl-matches? prefix ns-str)]
                      {:candidate ns-str, :type :namespace})))
          (concat (when (not comp-env)
                    (for [[^String root-pkg _] (utils/classes-on-classpath)
                          :when (.startsWith root-pkg prefix)]
                      {:candidate (str root-pkg "."), :type :class}))
                  (for [^String ns-str (namespaces-on-classpath comp-env)
                        :when (.startsWith ns-str prefix)]
                    {:candidate ns-str, :type :namespace})
                  (when (= CljsCompilerEnv (type comp-env))
                    (for [ns-str (provides-from-js-dependency-index comp-env)
                        :when (nscl-matches? prefix ns-str)]
                    {:candidate ns-str, :type :namespace})))))))))

#_(defn doc [ns-or-class-str curr-ns]
  (when (nscl-symbol? ns-or-class-str)
    (if-let [ns (find-ns (symbol ns-or-class-str))]
      (str ns "\n" (:doc (meta ns)) "\n")
      (when-let [class (try (ns-resolve curr-ns (symbol ns-or-class-str))
                            (catch Exception ex nil))]
        (when (= (type class) Class)
          (classname-doc class))))))

(defsource ::namespaces-and-classes
  :candidates #'candidates
  :doc (constantly nil))

(comment
  (require '[replique.repl-cljs :refer [compiler-env]])
  (require '[replique.environment :refer [->CljsCompilerEnv]])
  (def comp-env (->CljsCompilerEnv @compiler-env))
  
  (candidates comp-env "cljs.c" (find-ns comp-env 'replique.compliment.ns-mappings-cljs-test) nil)
  (candidates "cljs.c" (find-ns nil 'replique.compliment.ns-mappings-cljs-test) nil)
  (candidates comp-env "goog.ed" (find-ns comp-env 'replique.compliment.ns-mappings-cljs-test) nil)
  )
