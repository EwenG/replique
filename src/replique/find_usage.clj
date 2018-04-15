(ns replique.find-usage
  (:require [replique.environment :as env]
            [replique.context :as context])
  (:import [replique.environment CljsCompilerEnv]))

(defn var-symbols-in-namespaces-reducer
  [comp-env var-ns var-sym var m ns]
  (let [var-symbols (context/var-symbols-in-namespace comp-env var-ns var-sym var ns)]
    (assoc! m (str ns) var-symbols)))

(defn var-symbols-in-namespaces [comp-env var-ns var-sym var namespaces]
  (->> namespaces
       (reduce (partial var-symbols-in-namespaces-reducer comp-env 
                        var-ns var-sym var)
               (transient {}))
       persistent!))

(defn scoped-keywords-in-namespace [comp-env k-ns k-name the-ns]
  (if (= k-ns the-ns)
    (list (symbol (str "::" k-name)))
    (doall
     (for [[alias ns] (env/ns-aliases comp-env the-ns)
           :when (= ns k-ns)]
       (symbol (str "::" alias) k-name)))))

(defn scoped-keywords-in-namespaces-reducer [comp-env k-ns k-name m the-ns]
  (let [keywords (scoped-keywords-in-namespace comp-env k-ns k-name the-ns)]
    (assoc! m (str the-ns) keywords)))

(defn scoped-keywords-in-namespaces [comp-env k-ns k-name namespaces]
  (->> namespaces
       (reduce (partial scoped-keywords-in-namespaces-reducer comp-env k-ns k-name)
               (transient {}))
       persistent!))

(defn namespaces-empty-map-reducer [m the-ns]
  (assoc! m (str the-ns) '()))

(defn namespaces-empty-map [namespaces]
  (->> namespaces
       (reduce namespaces-empty-map-reducer (transient {}))
       persistent!))

(defn symbols-in-namespaces-keyword [comp-env ns context ^String prefix]
  (let [double-colon? (.startsWith prefix "::")
        prefix (.replaceFirst prefix "(^::?)" "")
        scope-split-index (.lastIndexOf prefix "/")
        scope-name (when (> scope-split-index -1) (subs prefix 0 scope-split-index))
        prefix (if (> scope-split-index -1)
                 (subs prefix (inc scope-split-index))
                 prefix)]
    (when (> (count prefix) 0)
      (cond (and double-colon? scope-name)
            (when-let [scope (env/resolve-namespace comp-env (symbol scope-name) ns)]
              {:symbols-in-namespaces (scoped-keywords-in-namespaces
                                       comp-env scope prefix (env/all-ns comp-env))
               :find-usage-type :keyword
               :keyword (keyword (str scope) prefix)})
            double-colon?
            {:symbols-in-namespaces (scoped-keywords-in-namespaces
                                     comp-env ns prefix (env/all-ns comp-env))
             :find-usage-type :keyword
             :keyword (keyword (str ns) prefix)}
            scope-name (if-let [scope (env/find-ns comp-env (symbol scope-name))]
                         {:symbols-in-namespaces (scoped-keywords-in-namespaces
                                                  comp-env scope prefix (env/all-ns comp-env))
                          :find-usage-type :keyword
                          :keyword (keyword scope-name prefix)}
                         {:symbols-in-namespaces (namespaces-empty-map (env/all-ns comp-env))
                          :find-usage-type :keyword
                          :keyword (keyword scope-name prefix)})
            :else {:symbols-in-namespaces (namespaces-empty-map (env/all-ns comp-env))
                   :find-usage-type :keyword
                   :keyword (keyword prefix)}))))

(defn safe-ns-resolve [comp-env ns sym]
  (try (env/ns-resolve comp-env ns sym)
       (catch ClassNotFoundException e nil)))

(defn ns-imports-symbols [comp-env ns sym]
  (doall (for [[k v] (env/ns-imports comp-env ns)
               :when (= v sym)]
           k)))

(defn symbols-in-namespace-imports [comp-env sym symbols ns]
  (assoc! symbols (str ns) (ns-imports-symbols comp-env ns sym)))

(defn symbols-in-namespaces-imports [comp-env sym]
  (->> (env/all-ns comp-env)
       (reduce (partial symbols-in-namespace-imports comp-env sym) (transient {}))
       persistent!))

(defn ns-aliases-symbols [comp-env ns target-ns]
  (doall (for [[k v] (env/ns-aliases comp-env ns)
               :when (= v target-ns)]
           k)))

(defn symbols-in-namespace-alias [comp-env target-ns symbols ns]
  (assoc! symbols (str ns) (ns-aliases-symbols comp-env ns target-ns)))

(defn symbols-in-namespaces-aliases [comp-env target-ns]
  (->> (env/all-ns comp-env)
       (reduce (partial symbols-in-namespace-alias comp-env target-ns) (transient {}))
       persistent!))

(defn symbols-in-namespaces
  [comp-env ns
   {:keys [at-local-binding-position? in-comment? in-string? locals] :as context}
   ^String prefix]
  (let [ns (or (and ns (env/find-ns comp-env (symbol ns)))
               (env/find-ns comp-env (env/default-ns comp-env)))]
    (when (and prefix (not at-local-binding-position?) (not in-comment?) (not in-string?))
      (let [prefix (.replaceFirst prefix "^#_" "")
            keyword? (.startsWith prefix ":")]
        (cond keyword?
              (symbols-in-namespaces-keyword comp-env ns context prefix)
              (contains? (env/special-forms comp-env) prefix)
              {:find-usage-type :special-form
               :special-form prefix}
              :else
              (when (not (contains? locals prefix))
                (let [prefix-sym (symbol prefix)
                      resolved (safe-ns-resolve comp-env ns prefix-sym)
                      resolved-ns (when (nil? resolved) (env/find-ns comp-env prefix-sym))
                      resolved-ns (if (and (nil? resolved) (nil? resolved-ns))
                                    (get (env/ns-aliases comp-env ns) prefix-sym)
                                    (or resolved resolved-ns))]
                  (cond (env/looks-like-var? comp-env resolved)
                        (let [m (env/meta comp-env resolved)
                              var-ns (:ns m)
                              var-sym (:name m)]
                          (when (and resolved var-ns var-sym)
                            {:symbols-in-namespaces
                             (var-symbols-in-namespaces
                              comp-env var-ns var-sym resolved
                              (env/all-ns comp-env))
                             :find-usage-type :var
                             :var (symbol (str var-ns) (str var-sym))}))
                        ;; A JVM class or 
                        (class? resolved)
                        (let [class-symbol (symbol (.getName ^Class resolved))]
                          {:symbols-in-namespaces (symbols-in-namespaces-imports
                                                   comp-env class-symbol)
                           :find-usage-type :class
                           :class class-symbol})
                        ;; a cljs import
                        ;; only imported symbols are resolved, not the fully qualified symbol
                        ;; of a js-dependency-index provide. This is different from the way it
                        ;; works for jvm classes
                        (and (instance? CljsCompilerEnv comp-env) (symbol? resolved))
                        {:symbols-in-namespaces (symbols-in-namespaces-imports
                                                 comp-env resolved)
                         :find-usage-type :class
                         :class resolved}
                        (some? resolved-ns)
                        {:symbols-in-namespaces (symbols-in-namespaces-aliases
                                                 comp-env resolved-ns)
                         :find-usage-type :namespace
                         :namespace (env/ns-name resolved-ns)}))))))))

(comment
  (let [v #'replique.watch/browse-get]
    (context/var-symbols-in-namespace nil (.-ns v) (.-sym v) v *ns*))
  (let [v #'prn]
    (var-symbols-in-namespaces nil (.-ns v) (.-sym v) v (all-ns)))

  (def cenv (env/->CljsCompilerEnv @replique.repl-cljs/compiler-env))
  (do
    (def vv 'replique.compliment.ns-mappings-clj-test/my-macro)
    (def nn (env/find-ns cenv 'replique.compliment.ns-mappings-cljs-test))
    (def v-sym (symbol (name vv)))
    (def v-ns (env/find-ns cenv (symbol (namespace vv))))
    (def v (env/ns-resolve cenv v-ns v-sym)))
  
  (var-symbols-in-namespaces cenv v-ns v-sym v [nn])

  (env/meta cenv (env/ns-resolve cenv (env/find-ns cenv 'cljs.core) 'prn))
  (env/meta cenv nil)
  )
