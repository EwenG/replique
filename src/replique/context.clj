(ns replique.context
  (:require [replique.environment :as env :refer [->CljsCompilerEnv]]))

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
                                      'cljs.core/defmacro]
                            :for-like ['clojure.core/for
                                       'cljs.core/for
                                       'clojure.core/doseq
                                       'cljs.core/doseq]})

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
(defn compute-context->categories->syms [comp-env ns]
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
         :ns-context ns-syms}))))

(defn compute-context->categories->syms-cljs [comp-env ns]
  (let [result (compute-context->categories->syms comp-env ns)]
    (assoc-in result [:ns-context "ns"] :ns-like)))
