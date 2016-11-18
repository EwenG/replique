(ns replique.repliquedoc
  (:refer-clojure :exclude [find-ns meta ns-resolve])
  (:require [replique.tooling-msg :as tooling-msg]
            [clojure.set]
            [replique.compliment.core :as compliment]
            [replique.compliment.context :as context]
            [replique.compliment.sources.local-bindings
             :refer [bindings-from-context]]
            [replique.environment :refer [->CljsCompilerEnv find-ns meta
                                          looks-like-var? ns-resolve]]
            [replique.compliment.context :as context]
            [replique.compliment.utils :refer [resolve-class]]
            [replique.compliment.sources.class-members :refer [class-member-symbol?
                                                      static-member-symbol?
                                                      try-get-object-class
                                                      get-all-members
                                                      static-members]]
            [replique.compliment.sources.local-bindings :refer [bindings-from-context]]
            [replique.compliment.sources.ns-mappings :refer [var-symbol?]])
  (:import [replique.environment CljsCompilerEnv]
           [java.lang.reflect Method Member Modifier]))

(defmulti format-call (fn [{:keys [type]} index] type))

(defmethod format-call :var [{:keys [var comp-env]} index]
  (let [{:keys [ns name arglists]} (meta comp-env var)]
    (cond (and name arglists)
          {:name (format "%s/%s" (str ns) name)
           :arglists arglists
           :index index}
          name
          {:name (format "%s/%s" (str ns) name)}
          :else {})))

(defn format-method [{:keys [^Method method]} index]
  (let [klass (.getName ^Class (.getDeclaringClass method))
        parameter-types (->> (.getParameterTypes method)
                             (mapv #(-> (.getName ^Class %) symbol)))
        return-type (-> (.getName ^Class (.getReturnType method)) symbol)]
    {:name (format "%s.%s" klass (.getName ^Member method))
     :arglists (list parameter-types)
     :index index
     :return return-type}))

(defmethod format-call :method [call index]
  (format-method call index))

(defmethod format-call :static-method [call index]
  (format-method call index))

(comment
  (tooling-msg/tooling-msg-handle {:type :repliquedoc-clj
                                            :context "(__prefix__)
"
                                            :ns "replique.repliquedoc"
                                            :symbol "."})

  (tooling-msg/tooling-msg-handle {:type :repliquedoc-clj
                                            :context "(clojure.core __prefix__)
"
                                            :ns "replique.repliquedoc"
                                            :symbol "ee"})

  (tooling-msg/tooling-msg-handle {:type :repliquedoc-clj
                                            :context "(prn __prefix__)
"
                                            :ns "replique.repliquedoc"
                                            :symbol "ee"})
  )
(comment
  (def ee1 Class)
  (def ee2 "e")
  (.getName ee1)
  (.codePointAt ee2)

  (defn ee
    ([^String e])
    ([^Double rr & {:keys [ee rr] :or {ee "e" rr "t"}}])
    ([]))
  (ee r r g e n)
  )

(defmethod format-call :special-form [{:keys [sym arglists]} index]
  {:name (str sym) :arglists arglists :index index})

(def special-forms #{'def 'if 'do 'quote 'recur 'throw 'try 'catch 'new 'set!})
(def special-forms-clj (clojure.set/union
                        special-forms
                        #{'var 'monitor-enter 'monitor-exit}))

(def special-forms-arglists
  {'def '([symbol doc-string? init?])
   'if '([test then else])
   'do '([exprs*])
   'quote '([form])
   'recur '([exprs*])
   'throw '([expr])
   'try '([expr* catch-clause* finally-clause?])
   'catch '([classname name expr*])
   'new '([classname args*])
   'set! '([symbol expr])
   'var '([symbol])
   'monitor-enter '([x])
   'monitor-exit '([x])})

(defn function-call [compiler-env ns [fn-sym & _] bindings sym-at-point]
  (let [fn-sym (if (= '__prefix__ fn-sym)
                 sym-at-point
                 fn-sym)]
    ;; ns-resolve on something that is not a var may fail
    (when (var-symbol? (str fn-sym))
      (cond (get bindings (str fn-sym))
            nil
            ;; TODO cljs special-forms
            (get special-forms-clj fn-sym)
            {:type :special-form :sym fn-sym :arglists (get special-forms-arglists fn-sym)}
            :else (let [resolved (ns-resolve compiler-env ns fn-sym)]
                    (when (looks-like-var? compiler-env resolved)
                      {:type :var :var resolved :comp-env compiler-env}))))))

(comment
  (let [ee ee]
    (ee "e" 3))
  )

(defn method-with-class? [klass member]
  (when (and (instance? Method member)
         (or (not klass) (= klass (.getDeclaringClass ^Member member))))
    member))

(defn method-call [ns [method-sym object & _] bindings sym-at-point]
  (let [method-sym (if (= '__prefix__ method-sym)
                     sym-at-point
                     method-sym)]
    (when (class-member-symbol? (str method-sym))
      (let [;; Remove the starting "."
            method-name (subs (str method-sym) 1)
            object (if (= '__prefix__ object)
                     sym-at-point
                     object)
            object (when (and (symbol? object)
                              (var-symbol? (str object))
                              (not (get bindings (str object))))
                     (clojure.core/ns-resolve ns object))
            klass (when (= (type object) clojure.lang.Var)
                    (type (deref object)))
            members (get (get-all-members ns) method-name)
            method (some (partial method-with-class? klass) members)]
        (when method
          {:type :method :method method})))))

(defn static-method-call [ns [method-sym object & _] bindings sym-at-point]
  (let [method-sym (if (= '__prefix__ method-sym)
                     sym-at-point
                     method-sym)]
    (when (static-member-symbol? (str method-sym))
      (let [;; Remove the starting "."
            [cl-name method-name] (.split (str method-sym) "/")
            cl (resolve-class ns (symbol cl-name))]
        (when cl
          (let [members (get (static-members cl) method-name)]
            (when (instance? Method (first members))
              {:type :static-method :method (first members)})))))))

(comment
  (tooling-msg/tooling-msg-handle {:type :repliquedoc-clj
                              :context "(__prefix__)
"
                              :ns "replique.tooling"
                              :symbol "Integer/compare"})

  (tooling-msg/tooling-msg-handle
   {:type :repliquedoc-cljs
    :context "(__prefix__)
"
    :ns "replique.compliment.ns-mappings-cljs-test"
    :symbol "my-macro"})
  )

(defprotocol Repliquedoc
  (handle-repliquedoc* [comp-env ns context sym-at-point]))

(extend-protocol Repliquedoc
  CljsCompilerEnv
  (handle-repliquedoc* [comp-env ns context sym-at-point]
    (let [[{:keys [form idx]} & _] context]
      (when (and (list? form) (first form) (symbol? (first form)))
        (let [bindings (set (bindings-from-context context))
              fn (delay (function-call comp-env ns form bindings sym-at-point))]
          (when @fn (format-call @fn idx))))))
  nil
  (handle-repliquedoc* [comp-env ns context sym-at-point]
    (let [[{:keys [form idx]} & _] context]
      (when (and (list? form) (first form) (symbol? (first form)))
        (let [bindings (set (bindings-from-context context))
              method (delay (method-call ns form bindings sym-at-point))
              static-method (delay (static-method-call ns form bindings sym-at-point))
              fn (delay (function-call comp-env ns form bindings sym-at-point))]
          (cond
            @method
            (format-call @method idx)
            @static-method
            (format-call @static-method idx)
            @fn
            (format-call @fn idx)
            :else nil))))))

(defn handle-repliquedoc [comp-env ns context sym-at-point]
  (let [{:keys [context]} (context/cache-context comp-env (when ns (symbol ns)) context)
        context (reverse context)
        ns (compliment/ensure-ns comp-env (when ns (symbol ns)))
        sym-at-point (and sym-at-point (symbol sym-at-point))]
    (handle-repliquedoc* comp-env ns context sym-at-point)))

(defn handle-repliquedoc-cljc [comp-env ns context sym-at-point]
  (let [{:keys [reader-conditionals context]} (context/cache-context
                                               comp-env (when ns (symbol ns)) context)
        context (reverse context)
        sym-at-point (and sym-at-point (symbol sym-at-point))]
    (if (= #{:cljs} reader-conditionals)
      (let [ns (compliment/ensure-ns comp-env (when ns (symbol ns)))]
        (handle-repliquedoc* comp-env ns context sym-at-point))
      (let [ns (compliment/ensure-ns nil (when ns (symbol ns)))]
        (handle-repliquedoc* nil ns context sym-at-point)))))
