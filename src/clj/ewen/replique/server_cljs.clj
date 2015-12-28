(ns ewen.replique.server-cljs)

(defonce compiler-env (atom nil))
(defonce repl-env (atom nil))
(defonce env {:context :expr :locals {}})

(defn init-class-loader []
  (let [cl (.getContextClassLoader (Thread/currentThread))]
    (.setContextClassLoader (Thread/currentThread)
                            (clojure.lang.DynamicClassLoader. cl))))

(defmulti init-opts :cljs-env)

(defmethod init-opts :browser [out-file main-file]
  )

(defn init-tooling-msg-handle [tooling-msg-handle]
  nil)
