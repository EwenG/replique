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
  (let [maybe-class (try (ns-resolve ns class)
                         (catch ClassNotFoundException ex nil))]
    (when (class? maybe-class) maybe-class)))

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

                         'cljs.core/require [:dependency-context :require-like]

                         'cljs.core/use [:dependency-context :use-like]

                         'cljs.core/require-macros [:dependency-context :require-macros-like]

                         'cljs.core/import [:dependency-context :import-like]

                         'cljs.core/refer-clojure [:dependency-context :refer-clojure-like]})


(def context-forms-by-namespaces-clj {'clojure.core context-forms-clj})
(def context-forms-by-namespaces-cljs {'cljs.core context-forms-cljs})

(def ^:dynamic *binding-context* nil)
(def ^:dynamic *dependency-context* nil)

(defn var->sym [comp-env v]
  (let [{:keys [ns name]} (env/meta comp-env v)]
    (when (and ns name)
      (symbol (str (env/ns-name ns)) (str name)))))

(defn override-default-require-symbol [comp-env context-forms the-ns-map]
  (doseq [[v [context form-context]] context-forms]
    (when-let [v-name (name v)]
      (let [v-sym (symbol v-name)
            ns-map-v (var->sym comp-env (get the-ns-map v-sym))]
        (when-not (= v ns-map-v)
          (case context
            :binding-context (->> (assoc! *binding-context* (str v-sym) nil)
                                  (set! *binding-context*))
            :dependency-context (->> (assoc! *dependency-context* (str v-sym) nil)
                                     (set! *dependency-context*))))))))

(defn additional-symbols-from-mapping [comp-env context-forms sym v]
  (when-let [v (var->sym comp-env v)]
    (when-let [[context form-context] (get context-forms v)]
      (let [v-sym (symbol (name v))]
        (when-not (= sym v-sym)
          (case context
            :binding-context (->> form-context
                                  (assoc! *binding-context* (str sym))
                                  (set! *binding-context*))
            :dependency-context (->> form-context
                                     (assoc! *dependency-context* (str sym))
                                     (set! *dependency-context*))))))))

(defn additional-symbols-from-aliases
  [comp-env context-forms context-forms-by-namespaces alias n]
  (when-let [context-forms (get context-forms-by-namespaces (env/ns-name n))]
    (doseq [[v [context form-context]] context-forms]
      (let [v-name (name v)]
        (let [sym (symbol (str alias) (str v-name))]
          (case context
            :binding-context (->> form-context
                                  (assoc! *binding-context* (str sym))
                                  (set! *binding-context*))
            :dependency-context (->> form-context
                                     (assoc! *dependency-context* (str sym))
                                     (set! *dependency-context*))))))))

(defn context-forms-overrides [comp-env repl-env ns context-forms context-forms-by-namespaces]
  (let [ns (symbol ns)
        the-ns (env/find-ns comp-env ns)]
    (when the-ns
      (binding [*binding-context* (transient {})
                *dependency-context* (transient {})]
        (let [the-ns-map (env/ns-map comp-env the-ns)
              the-ns-aliases (env/ns-aliases comp-env the-ns)]
          (override-default-require-symbol comp-env context-forms the-ns-map)
          (doseq [[sym v] the-ns-map]
            (additional-symbols-from-mapping comp-env context-forms sym v))
          (doseq [[alias n] the-ns-aliases]
            (additional-symbols-from-aliases comp-env context-forms context-forms-by-namespaces
                                             alias n)))
        {:binding-context (persistent! *binding-context*)
         :dependency-context (persistent! *dependency-context*)
         :repl-type (utils/repl-type repl-env)}))))

(comment
  (context-forms-overrides nil (str *ns*) context-forms-clj context-forms-by-namespaces-clj)

  (require '[replique.environment :as env])
  (def cenv (env/->CljsCompilerEnv @replique.repl-cljs/compiler-env))

  (context-forms-overrides cenv
                           "replique.compliment.ns-mappings-cljs-test"
                           context-forms-cljs context-forms-by-namespaces-cljs)
  )
