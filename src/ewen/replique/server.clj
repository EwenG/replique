(ns ewen.replique.server
  (:require [clojure.main]
            [clojure.core.server :refer [start-server]]
            [clojure.java.io :refer [file]]))

(require '[ewen.replique.server-clj :as server-clj])

(def ^:const init-requires
  '[])

(defmulti tooling-msg-handle :type)

(defn repl []
  (clojure.main/repl :init clojure.core.server/repl-init
                     :read clojure.core.server/repl-read))

(defmulti repl identity)

(defmethod repl :clj [type]
  (println "Clojure" (clojure-version))
  (clojure.main/repl :init clojure.core.server/repl-init
                     :read clojure.core.server/repl-read))

(defmethod repl :cljs [type]
  (println "Clojure" (clojure-version))
  (clojure.main/repl :init clojure.core.server/repl-init
                     :read clojure.core.server/repl-read))

(defn tooling-repl []
  (let [init-fn (fn [] (in-ns 'ewen.replique.server))]
    (clojure.main/repl
     :init init-fn
     :read clojure.core.server/repl-read
     :prompt #())))

(defn init-repl [{:keys [port type cljs-env]}]
  (start-server {:port port :name :replique-tooling-repl
                 :accept 'ewen.replique.server/tooling-repl
                 :server-daemon false})
  (start-server {:port 0 :name :replique-repl
                 :accept 'ewen.replique.server/repl
                 :server-daemon false
                 :args [type]})
  (println "REPL started"))

(defmulti repl-dispatch (fn [{:keys [type cljs-env]}]
                          [type cljs-env]))

(defmethod repl-dispatch [:clj nil] [opts]
  (server-clj/init-tooling-msg-handle tooling-msg-handle)
  (init-repl opts))

(defmethod repl-dispatch [:cljs :browser] [opts]
  (require 'ewen.replique.server-cljs)
  (let [init-class-loader (ns-resolve 'ewen.replique.server-cljs
                                      'init-class-loader)
        init-tooling-msg-handle (ns-resolve 'ewen.replique.server-cljs
                                            'init-tooling-msg-handle)
        init-opts (ns-resolve 'ewen.replique.server-cljs 'init-opts)
        init-browser-env (ns-resolve 'ewen.replique.server-cljs
                                     'init-browser-env)
        output-index-html (ns-resolve 'ewen.replique.server-cljs
                                      'output-index-html)
        repl-env (ns-resolve 'ewen.replique.server-cljs 'repl-env)]
    (init-class-loader)
    (let [{:keys [comp-opts repl-opts]} (init-opts opts)]
      (init-browser-env comp-opts repl-opts)
      (output-index-html comp-opts)
      (init-tooling-msg-handle tooling-msg-handle)
      (init-repl opts)
      (doto (file ".replique-port")
        (spit (str {:tooling-repl (-> @#'clojure.core.server/servers
                                      (get :replique-tooling-repl)
                                      :socket
                                      (.getLocalPort))
                    :repl (-> @#'clojure.core.server/servers
                              (get :replique-repl)
                              :socket
                              (.getLocalPort))
                    :cljs-env (-> @(:server-state @@repl-env)
                                  :socket
                                  (.getLocalPort))}))
        (.deleteOnExit)))))

(comment
  (-> @(:server-state @ewen.replique.server-cljs/repl-env)
      :socket
      (.getLocalPort))
  )

(defn -main [opts]
  (repl-dispatch (read-string opts)))

(defmethod tooling-msg-handle :repl-infos [msg]
  (let [{:keys [replique-tooling-repl replique-repl]}
        @#'clojure.core.server/servers]
    {:replique-tooling-repl
     {:host (-> (:socket replique-tooling-repl)
                (.getInetAddress) (.getHostAddress))
      :port (-> (:socket replique-tooling-repl) (.getLocalPort))}
     :replique-repl
     {:host (-> (:socket replique-tooling-repl)
                (.getInetAddress) (.getHostAddress))
      :port (-> (:socket replique-repl) (.getLocalPort))}}))
