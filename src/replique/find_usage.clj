(ns replique.find-usage
  (:require [replique.environment :as env]
            [replique.watch :as w :rename {browse-get bb2}]
            [replique.context :as context])
  (:import [java.io FileReader BufferedReader PushbackReader]))

(comment
  (def ^:const c-newline (int \newline))
  (def ^:const c-newline (int \newline))
  (def ^:const c-eof (int -1))

  (defn make-reader [^String path]
    (PushbackReader. (BufferedReader. (FileReader. path))))

  (defn read-until-newline [^PushbackReader r]
    (let [c (.read r)]
      (cond
        (.equals c-eof c) c
        (.equals c-newline c) c
        :else (recur r))))

  (defn forward-comment [^PushbackReader r]
    (let [c (.read r)]
      (cond
        (.equals c-eof c) c
        (or (.equals c-newline c) #_(.equals c-space c) #_(.equals c-tab c)) (recur r)
        :else (.unread r c))))

  (comment
    (with-open [r (make-reader "/home/ewen/clojure/replique/src/replique/watch.clj")]
      (dotimes [i 20]
        (let [c (.read r)]
          (prn (.equals c-newline c)))))

    (with-open [r (make-reader "/home/ewen/clojure/replique/src/replique/watch.clj")]
      (read-until-newline r)
      (prn (char (.read r))))
    ))

;; -----------------------------

(def ^:dynamic *context-forms-overrides* nil)

(defn ns-map-reducer [comp-env context-forms var syms sym v]
  (context/additional-symbols-from-mapping comp-env context-forms sym v)
  (if (= var v)
    (conj syms sym)
    syms))

(defn ns-aliases-reducer
  [comp-env context-forms context-forms-by-namespaces var-ns var-sym var syms alias n]
  (context/additional-symbols-from-aliases
   comp-env context-forms context-forms-by-namespaces alias n)
  (if (= var-ns n)
    (conj syms (symbol (str alias) (str var-sym)))
    syms))

(defn var-symbols-in-namespace
  ([comp-env var-ns var-sym var ns]
   (var-symbols-in-namespace comp-env var-ns var-sym var ns nil nil))
  ([comp-env var-ns var-sym var ns context-forms context-forms-by-namespaces]
   (binding [context/*binding-context* (transient {})
             context/*dependency-context* (transient {})]
     (let [var-ns-sym (env/ns-name var-ns)
           the-ns-map (env/ns-map comp-env ns)]
       (context/override-default-require-symbol comp-env context-forms the-ns-map)
       (let [mapping-symbols (reduce-kv (partial ns-map-reducer comp-env context-forms var)
                                        '() the-ns-map)
             alias-symbols (reduce-kv (partial ns-aliases-reducer
                                               comp-env
                                               context-forms context-forms-by-namespaces
                                               var-ns var-sym var)
                                      '() (env/ns-aliases comp-env ns))
             binding-context (persistent! context/*binding-context*)
             dependency-context (persistent! context/*dependency-context*)]
         (when (or (seq binding-context) (seq dependency-context))
           (set! *context-forms-overrides*
                 (assoc! *context-forms-overrides*
                         (str ns) {:binding-context binding-context
                                   :dependency-context dependency-context})))
         (doall (concat mapping-symbols alias-symbols)))))))

(defn var-symbols-in-namespaces-reducer
  [comp-env context-forms context-forms-by-namespaces var-ns var-sym var m ns]
  (let [var-symbols (var-symbols-in-namespace
                     comp-env var-ns var-sym var ns context-forms context-forms-by-namespaces)]
    (if (seq var-symbols)
      (assoc! m (str ns) var-symbols)
      m)))

(defn var-symbols-in-namespaces
  ([comp-env var-ns var-sym var namespaces]
   (var-symbols-in-namespaces comp-env var-ns var-sym var namespaces nil nil))
  ([comp-env var-ns var-sym var namespaces context-forms context-forms-by-namespaces]
   (->> namespaces
        (reduce (partial var-symbols-in-namespaces-reducer
                         comp-env context-forms context-forms-by-namespaces
                         var-ns var-sym var)
                (transient {}))
        persistent!)))

(defn safe-ns-resolve [comp-env ns sym]
  (try (env/ns-resolve comp-env ns sym)
       (catch ClassNotFoundException e nil)))

(defn symbols-in-namespaces
  [comp-env ns
   {:keys [at-local-binding-position? in-comment? in-string? locals] :as context}
   ^String prefix context-forms context-forms-by-namespaces]
  (let [ns (or (and ns (env/find-ns comp-env (symbol ns)))
               (env/find-ns comp-env (env/default-ns comp-env)))]
    (when (and prefix
               (not at-local-binding-position?) (not in-comment?) (not in-string?))
      (let [prefix (.replaceFirst prefix "^#_" "")]
        (when (and (not (contains? locals prefix))
                   (not (contains? (env/special-forms comp-env) prefix)))
          (let [prefix-sym (symbol prefix)
                resolved (safe-ns-resolve comp-env ns prefix-sym)
                resolved (when (env/looks-like-var? comp-env resolved) resolved)
                m (env/meta comp-env resolved)
                var-ns (:ns m)
                var-sym (:name m)]
            (when (and resolved var-ns var-sym)
              (binding [*context-forms-overrides* (transient {})]
                (var-symbols-in-namespaces comp-env var-ns var-sym resolved
                                           (env/all-ns comp-env)
                                           context-forms context-forms-by-namespaces)))))))))

(comment
  (let [v #'replique.watch/browse-get]
    (var-symbols-in-namespace nil (.-ns v) (.-sym v) v *ns*))
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
