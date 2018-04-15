(ns replique.repliquedoc
  (:require [replique.environment :as env]
            [replique.context :as context])
  (:import [java.lang.reflect Method Member Modifier]
           [java.lang Math]
           [replique.environment CljsCompilerEnv]))

(defn safe-ns-resolve [comp-env ns sym]
  (try (env/ns-resolve comp-env ns sym)
       (catch ClassNotFoundException e nil)))

(defn format-method [^Class fn-param-class ^Method method fn-context-position]
  (let [parameter-types (->> (.getParameterTypes method)
                             (mapv #(-> (.getName ^Class %) symbol)))
        return-type (-> (.getName ^Class (.getReturnType method)) symbol)]
    {:name (format "%s.%s" (.getName fn-param-class) (.getName method))
     :index fn-context-position
     :arglists (->> (.getParameterTypes method)
                    (mapv #(-> (.getName ^Class %) symbol))
                    list)
     :return (-> (.getName ^Class (.getReturnType method)) symbol)}))

;; We don't print all the methods signatures
(defn format-overloaded-method [^Class fn-param-class ^Method method fn-context-position]
  (let [parameter-types (->> (.getParameterTypes method)
                             (mapv #(-> (.getName ^Class %) symbol)))
        return-type (-> (.getName ^Class (.getReturnType method)) symbol)]
    {:name (format "%s.%s" (.getName fn-param-class) (.getName method))
     :index fn-context-position
     :arglists '([& args])
     :return (-> (.getName ^Class (.getReturnType method)) symbol)}))

(defn method-call [comp-env ns locals
                   ^String fn-context fn-context-position
                   fn-param fn-param-meta]
  (when (and fn-param fn-context (.startsWith fn-context "."))
    (let [fn-context (subs fn-context 1)
          [_ _ local-meta :as local-param] (get locals fn-param)
          fn-param-meta (or fn-param-meta local-meta)
          ^Class fn-param-class (cond fn-param-meta
                                      (context/try-get-meta-class comp-env ns fn-param-meta)
                                      (nil? local-param)
                                      (context/try-get-object-class comp-env ns fn-param))]
      (when fn-param-class 
        (let [methods (for [^Method method (.getMethods fn-param-class)
                              :when (and (= fn-context (.getName method))
                                         (not (Modifier/isStatic (.getModifiers method))))]
                        method)]
          (when-let [^Method method (first methods)]
            (if (> (count methods) 1)
              (format-overloaded-method fn-param-class method
                                        (Math/max 0 ^long (dec fn-context-position)))
              (format-method fn-param-class method
                             (Math/max 0 ^long (dec fn-context-position))))))))))

(defn static-method-call [comp-env ns ^String fn-context fn-context-position]
  (when fn-context
    (let [scope-split-index (.lastIndexOf fn-context "/")
          scope-name (when (> scope-split-index -1) (subs fn-context 0 scope-split-index))
          suffix (when (> scope-split-index -1) (subs fn-context (inc scope-split-index)))]
      (when (and (> (count scope-name) 0) (> (count suffix) 0))
        (when-let [^Class class (context/resolve-class ns (symbol scope-name))]
          (let [methods (for [^Method method (.getMethods class)
                              :when (and (= suffix (.getName method))
                                         (Modifier/isStatic (.getModifiers method)))]
                          method)]
            (when-let [^Method method (first methods)]
              (if (> (count methods) 1)
                (format-overloaded-method class method fn-context-position)
                (format-method class method fn-context-position)))))))))

(def special-forms #{"def" "if" "do" "quote" "recur" "throw" "try" "catch" "new" "set!"
                     "var" "monitor-enter" "monitor-exit"})

(def special-forms-arglists
  {"def" '([symbol doc-string? init?])
   "if" '([test then else])
   "do" '([exprs*])
   "quote" '([form])
   "recur" '([exprs*])
   "throw" '([expr])
   "try" '([expr* catch-clause* finally-clause?])
   "catch" '([classname name expr*])
   "new" '([classname args*])
   "set!" '([symbol expr])
   "var" '([symbol])
   "monitor-enter" '([x])
   "monitor-exit" '([x])})

(defn function-call [comp-env ns locals fn-context fn-context-position]
  (when-not (contains? locals fn-context)
    (if-let [special-form (get (env/special-forms comp-env) fn-context)]
      {:name special-form
       :arglists (get special-forms-arglists fn-context)
       :index fn-context-position}
      (when-let [resolved (safe-ns-resolve comp-env ns (symbol fn-context))]
        (when (env/looks-like-var? comp-env resolved)
          (let [{:keys [ns name arglists]} (env/meta comp-env resolved)]
            (cond (and name arglists)
                  {:name (format "%s/%s" (str ns) name)
                   :arglists arglists
                   :index fn-context-position}
                  name
                  {:name (format "%s/%s" (str ns) name)}
                  :else {})))))))

(defprotocol Repliquedoc
  (handle-repliquedoc* [comp-env ns context]))

(extend-protocol Repliquedoc
  CljsCompilerEnv
  (handle-repliquedoc* [comp-env ns {:keys [locals in-string? in-comment?
                                            fn-param fn-param-meta
                                            fn-context fn-context-position] :as context}]
    (when (and (not in-comment?) fn-context fn-context-position)
      (function-call comp-env ns locals fn-context fn-context-position)))
  nil
  (handle-repliquedoc* [comp-env ns {:keys [locals in-string? in-comment?
                                            fn-param fn-param-meta
                                            fn-context fn-context-position] :as context}]
    (when (and (not in-comment?) fn-context fn-context-position)
      (let [method-doc (method-call comp-env ns locals fn-context fn-context-position
                                    fn-param fn-param-meta)
            static-doc (and
                        (nil? method-doc)
                        (static-method-call comp-env ns fn-context fn-context-position))
            function-doc (and (nil? method-doc)
                              (nil? static-doc)
                              (function-call comp-env ns locals fn-context fn-context-position))]
        (or method-doc static-doc function-doc)))))

(defn handle-repliquedoc [comp-env ns context]
  (let [ns (or (and ns (env/find-ns comp-env (symbol ns)))
               (env/find-ns comp-env (env/default-ns comp-env)))]
    (handle-repliquedoc* comp-env ns context)))

