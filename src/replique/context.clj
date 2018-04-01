(ns replique.context
  (:require [replique.environment :as env :refer [->CljsCompilerEnv]]
            [replique.utils :as utils])
  (:import [java.lang.reflect Modifier Method]
           [clojure.lang Keyword]))

(def binding-context->vars {:let-like ['clojure.core/let
                                       'cljs.core/let
                                       'clojure.core/if-let
                                       'cljs.core/if-let
                                       'clojure.core/when-let
                                       'cljs.core/when-let
                                       'clojure.core/if-some
                                       'cljs.core/if-some
                                       'clojure.core/when-some
                                       'cljs.core/when-some
                                       'clojure.core/loop
                                       'cljs.core/loop
                                       'clojure.core/with-open
                                       'clojure.core/dotimes
                                       'cljs.core/dotimes
                                       'clojure.core/with-local-vars]
                            :fn-like ['clojure.core/defn
                                      'cljs.core/defn
                                      'clojure.core/defn-
                                      'cljs.core/defn-
                                      'clojure.core/fn
                                      'cljs.core/fn
                                      'clojure.core/defmacro
                                      'cljs.core/defmacro
                                      'clojure.core/defmethod
                                      'cljs.core/defmethod]
                            :deftype-like ['clojure.core/deftype
                                           'clojure.core/defrecord
                                           'cljs.core/deftype
                                           'cljs.core/defrecord]
                            :for-like ['clojure.core/for
                                       'cljs.core/for
                                       'clojure.core/doseq
                                       'cljs.core/doseq]
                            :letfn-like ['clojure.core/letfn
                                         'cljs.core/letfn]})

(def ns-context->vars {:ns-like ['clojure.core/ns]})

(def dependency-context->vars {:require-like ['clojure.core/require
                                              'cljs.core/require]
                               :use-like ['clojure.core/use
                                          'cljs.core/use]
                               :require-macros-like ['cljs.core/require-macros]
                               :import-like ['clojure.core/import
                                             'cljs.core/import]
                               :refer-like ['clojure.core/refer]
                               :refer-clojure-like ['clojure.core/refer-clojure
                                                    'cljs.core/refer-clojure]
                               :load-like ['clojure.core/load]})

(defn categories->vars-namespaces-reducer [namespaces category vars]
  (reduce #(conj %1 (-> %2 namespace)) namespaces vars))

(def categories-namespaces (reduce-kv categories->vars-namespaces-reducer
                                      #{} (merge binding-context->vars
                                                 ns-context->vars
                                                 dependency-context->vars)))

(defn reverse-aliases-reducer [reversed-aliases alias-sym ns]
  (let [ns-str (str (env/ns-name ns))]
    (if (contains? categories-namespaces ns-str)
      (assoc reversed-aliases ns-str alias-sym)
      reversed-aliases)))

(defn sym->category-reducer [comp-env category aliases the-ns sym->category v]
  (let [v-name (name v)
        v-sym (symbol v-name)
        v-ns-str (namespace v)
        resolved (env/ns-resolve comp-env the-ns v-sym)
        referred? (-> (env/meta comp-env resolved) :ns str (= v-ns-str))
        fully-resolved? (env/ns-resolve comp-env the-ns v)
        alias-sym (get aliases v-ns-str)]
    (cond-> sym->category
      referred? (assoc v-name category)
      fully-resolved? (assoc (str v) category)
      alias-sym (assoc (str alias-sym "/" v-name) category))))

(defn compute-categories->syms [comp-env ns aliases category->vars]
  (loop [categories (keys category->vars)
         sym->category {}]
    (if-let [category (first categories)]
      (let [sym->category (reduce (partial sym->category-reducer comp-env
                                           category aliases ns)
                                  sym->category (get category->vars category))]
        (recur (rest categories) sym->category))
      sym->category)))

;; We assume (:require :rename) is not used
(defn compute-context->categories->syms [comp-env repl-env ns]
  (let [ns (symbol ns)
        the-ns (env/find-ns comp-env ns)]
    (when the-ns
      (let [aliases (reduce-kv reverse-aliases-reducer {} (env/ns-aliases comp-env the-ns))
            binding-syms (compute-categories->syms comp-env ns aliases binding-context->vars)
            ns-syms (compute-categories->syms comp-env ns aliases ns-context->vars)
            dependency-syms (compute-categories->syms
                             comp-env ns aliases dependency-context->vars)]
        {:binding-context binding-syms
         :dependency-context dependency-syms
         :ns-context ns-syms
         :repl-type (utils/repl-type repl-env)}))))

(defn compute-context->categories->syms-cljs [comp-env repl-env ns]
  (let [result (compute-context->categories->syms comp-env repl-env ns)]
    (assoc-in result [:ns-context "ns"] :ns-like)))

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
