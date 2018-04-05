(ns replique.find-usage
  (:require [replique.environment :as env]
            [replique.watch :as w :refer :all :rename {browse-get bb2}])
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

(defn var-symbols-in-namespace [comp-env var-ns var-sym var ns]
  (let [var-ns-sym (env/ns-name var-ns)
        mapping-symbols (for [[sym v] (env/ns-map comp-env ns)
                              :when (= var v)]
                          sym)
        alias-symbols (for [[alias n] (env/ns-aliases comp-env ns)
                            :when (= var-ns n)]
                        (symbol (str alias) (str var-sym)))]
    (doall (concat mapping-symbols alias-symbols))))

(defn var-symbols-in-namespaces-reducer [comp-env var-ns var-sym var m ns]
  (let [var-symbols (var-symbols-in-namespace comp-env var-ns var-sym var ns)]
    (if (seq var-symbols)
      (assoc! m (str ns) var-symbols)
      m)))

(defn var-symbols-in-namespaces [comp-env var-ns var-sym var namespaces]
  (->> namespaces
       (reduce (partial var-symbols-in-namespaces-reducer comp-env var-ns var-sym var)
               (transient {}))
       persistent!))

(defn safe-ns-resolve [comp-env ns sym]
  (try (env/ns-resolve comp-env ns sym)
       (catch ClassNotFoundException e nil)))

(defn symbols-in-namespaces
  [comp-env ns
   {:keys [at-binding-position? in-comment? in-string? locals] :as context}
   ^String prefix]
  (let [ns (or (and ns (env/find-ns comp-env (symbol ns)))
               (env/find-ns comp-env (env/default-ns comp-env)))]
    (when (and prefix
               #_(not at-binding-position?) (not in-comment?) (not in-string?)
               (not (get locals prefix)))
      (let [prefix (.replaceFirst prefix "^#_" "")]
        (when (not (contains? locals prefix))
          (let [prefix-sym (symbol prefix)
                resolved (safe-ns-resolve comp-env ns prefix-sym)
                resolved (when (env/looks-like-var? comp-env resolved) resolved)
                m (env/meta comp-env resolved)
                var-ns (:ns m)
                var-sym (:name m)]
            (when (and resolved var-ns var-sym)
              (var-symbols-in-namespaces comp-env var-ns var-sym resolved
                                         (env/all-ns comp-env)))))))))

(comment
  (ns-name (.-ns #'var-symbols-in-namespace))
  (.-sym #'var-symbols-in-namespace)

  (meta #'var-symbols-in-namespace)

  (filter #(identical? (val %) #'var-symbols-in-namespace) (ns-map *ns*))
  (filter #(identical? (val %) #'clojure.core/proxy-mappings) (ns-map *ns*))
  proxy-mappings
  
  (filter #(identical? (val %) #'replique.watch/browse-get) (ns-map *ns*))
  (ns-unmap *ns* 'browse-get)

  (ns-aliases *ns*)

  (symbol (str "rrn") "rr")
  
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
  
  (var-symbols-in-namespace cenv v-ns v-sym v nn)

  (for [[k2 v2] (env/ns-map cenv nn)
        :when (= 'cljs-ns-m k2) #_(identical? v2 v)]
    v2)

  (env/resolve-namespace cenv 'cljs-ns-m nn)
  
  (env/ns-aliases cenv nn)
  
  'replique.compliment.ns-mappings-cljs-test2/ff2
  (env/cljs-ns-map-resolve* cenv (first {'ff3 'replique.compliment.ns-mappings-cljs-test2/ff2}))


  (let [ns (if (symbol? ns) (find-ns comp-env ns) ns)
          aliases-candidates (merge (:requires ns)
                                    (:require-macros ns))
          imports (:imports ns)]
      (apply dissoc aliases-candidates (keys imports)))
  )

;; at-binding-position? for var definition -> add a binding-position type
