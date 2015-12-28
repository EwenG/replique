(ns ewen.replique.server-cljs)

(defonce compiler-env (atom nil))
(defonce repl-env (atom nil))
(defonce env {:context :expr :locals {}})

(defn init-class-loader []
  (let [cl (.getContextClassLoader (Thread/currentThread))]
    (.setContextClassLoader (Thread/currentThread)
                            (clojure.lang.DynamicClassLoader. cl))))

(defn init-common []
  (init-class-loader))

(defn init-tooling-msg-handle [tooling-msg-handle]
  nil)
