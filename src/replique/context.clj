(ns replique.context
  (:require [replique.utils :as utils]
            [replique.environment :as env])
  (:import [clojure.lang Keyword]))

(defn try-get-object-class [comp-env ns object-str]
  (let [object-str (try (read-string object-str)
                        (catch Exception e nil))]
    (cond (string? object-str) String
          (keyword? object-str) Keyword
          (symbol? object-str)
          (let [resolved (try (env/ns-resolve comp-env ns object-str)
                              (catch Exception e nil))
                resolved-type-hint (when (var? resolved) (:tag (meta resolved)))
                resolved-type-hint (when (and resolved-type-hint
                                              (class? resolved-type-hint))
                                     resolved-type-hint)
                value (try (binding [*ns* ns] (eval object-str))
                           (catch Exception e nil))]
            (when-let [^Class class (or resolved-type-hint (type value))]
              class)))))

(defn resolve-class [ns class]
  (when (not= (str class) "")
    (let [maybe-class (try (ns-resolve ns class)
                           (catch ClassNotFoundException ex nil))]
      (when (class? maybe-class) maybe-class))))

(defn try-get-meta-class [comp-env ns meta-str]
  (when-let [meta (try (read-string meta-str)
                       (catch Exception e nil))]
    (cond (symbol? meta)
          (when-let [^Class class (resolve-class ns meta)]
            class)
          (map? meta)
          (when-let [tag (get meta :tag)]
            (when-let [^Class class (resolve-class ns tag)]
              class)))))

(def context-forms-clj {'clojure.core/let [:binding-context :let-like]
                        'clojure.core/if-let [:binding-context :let-like]
                        'clojure.core/when-let [:binding-context :let-like]
                        'clojure.core/if-some [:binding-context :let-like]
                        'clojure.core/when-some [:binding-context :let-like]
                        'clojure.core/loop [:binding-context :let-like]
                        'clojure.core/with-open [:binding-context :let-like]
                        'clojure.core/dotimes [:binding-context :let-like]
                        'clojure.core/with-local-vars [:binding-context :let-like]
                        
                        'clojure.core/defn [:binding-context :fn-like]
                        'clojure.core/defn- [:binding-context :fn-like]
                        'clojure.core/fn [:binding-context :fn-like]
                        'clojure.core/defmacro [:binding-context :fn-like]

                        'clojure.core/deftype [:binding-context :deftype-like]
                        'clojure.core/defrecord [:binding-context :deftype-like]

                        'clojure.core/extend-type [:binding-context :extend-type-like]
                        'clojure.core/extend-protocol [:binding-context :extend-type-like]
                        'clojure.core/reify [:binding-context :extend-type-like]
                        'clojure.core/proxy [:binding-context :extend-type-like]
                        
                        'clojure.core/defmethod [:binding-context :defmethod-like]

                        'clojure.core/for [:binding-context :for-like]
                        'clojure.core/doseq [:binding-context :for-like]

                        'clojure.core/letfn [:binding-context :letfn-like]

                        'replique.interactive/with-env [:binding-context :with-env-like]

                        'clojure.core/require [:dependency-context :require-like]

                        'clojure.core/use [:dependency-context :use-like]

                        'clojure.core/import [:dependency-context :import-like]

                        'clojure.core/refer [:dependency-context :refer-like]

                        'clojure.core/refer-clojure [:dependency-context :refer-clojure-like]

                        'clojure.core/load [:dependency-context :load-like]})

(def context-forms-cljs {'cljs.core/let [:binding-context :let-like]
                         'cljs.core/if-let [:binding-context :let-like]
                         'cljs.core/when-let [:binding-context :let-like]
                         'cljs.core/if-some [:binding-context :let-like]
                         'cljs.core/when-some [:binding-context :let-like]
                         'cljs.core/loop [:binding-context :let-like]
                         'cljs.core/dotimes [:binding-context :let-like]

                         'cljs.core/defn [:binding-context :fn-like]
                         'cljs.core/defn- [:binding-context :fn-like]
                         'cljs.core/fn [:binding-context :fn-like]
                         'cljs.core/defmacro [:binding-context :fn-like]

                         'cljs.core/deftype [:binding-context :deftype-like]
                         'cljs.core/defrecord [:binding-context :deftype-like]

                         'cljs.core/extend-type [:binding-context :extend-type-like]
                         'cljs.core/extend-protocol [:binding-context :extend-type-like]
                         'cljs.core/reify [:binding-context :extend-type-like]
                         'cljs.core/specify [:binding-context :extend-type-like]
                         'cljs.core/specify! [:binding-context :extend-type-like]

                         'cljs.core/defmethod [:binding-context :defmethod-like]

                         'cljs.core/for [:binding-context :for-like]
                         'cljs.core/doseq [:binding-context :for-like]

                         'cljs.core/letfn [:binding-context :letfn-like]

                         'replique.interactive/with-env [:binding-context :with-env-like]

                         'cljs.core/require [:dependency-context :require-like]

                         'cljs.core/use [:dependency-context :use-like]

                         'cljs.core/require-macros [:dependency-context :require-macros-like]

                         'cljs.core/import [:dependency-context :import-like]

                         'cljs.core/refer-clojure [:dependency-context :refer-clojure-like]})


(defn ns-map-reducer [comp-env var syms sym v]
  (if (= var v)
    (conj syms sym)
    syms))

(defn ns-aliases-reducer
  [comp-env var-ns var-sym var syms alias n]
  (if (= var-ns n)
    (conj syms (symbol (str alias) (str var-sym)))
    syms))

(defn var-symbols-in-namespace [comp-env var-ns var-sym var ns]
  (let [var-ns-sym (env/ns-name var-ns)
        the-ns-map (env/ns-map comp-env ns)]
    (let [mapping-symbols (reduce-kv (partial ns-map-reducer comp-env var)
                                     '() the-ns-map)
          alias-symbols (reduce-kv (partial ns-aliases-reducer
                                            comp-env
                                            var-ns var-sym var)
                                   '() (env/ns-aliases comp-env ns))]
      (doall (concat mapping-symbols alias-symbols)))))

(defn context-forms-reducer [form-context context-forms sym]
  (assoc! context-forms (str sym) form-context))

(defn context-forms [comp-env repl-env ns context-forms]
  (let [ns (symbol ns)
        the-ns (env/find-ns comp-env ns)]
    (when the-ns
      (loop [binding-context (transient {})
             dependency-context (transient {})
             context-forms (seq context-forms)]
        (if-let [[v [context form-context]] (first context-forms)]
          (let [resolved (env/ns-resolve comp-env (symbol (namespace v)) v)
                m (env/meta comp-env resolved)
                var-ns (:ns m)
                var-sym (:name m)]
            (if (and var-ns var-sym)
              (let [syms (var-symbols-in-namespace comp-env var-ns var-sym resolved the-ns)]
                (case context
                  :binding-context
                  (recur (reduce (partial context-forms-reducer form-context)
                                 binding-context (cons v syms))
                         dependency-context
                         (rest context-forms))
                  :dependency-context
                  (recur binding-context
                         (reduce (partial context-forms-reducer form-context)
                                 dependency-context (cons v syms))
                         (rest context-forms))
                  (recur binding-context dependency-context (rest context-forms))))
              (recur binding-context dependency-context (rest context-forms))))
          {:binding-context (persistent! binding-context)
           :dependency-context (persistent! dependency-context)
           :ns-context {"ns" :ns-like
                        "clojure.core/ns" :ns-like}
           :repl-type (utils/repl-type repl-env)})))))

(comment
  (require '[replique.environment :as env])
  (def cenv (env/->CljsCompilerEnv @replique.repl-cljs/compiler-env))
  )
