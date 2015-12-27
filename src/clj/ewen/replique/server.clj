(ns ewen.replique.server
  (:require [clojure.main]
            [clojure.core.server :refer [start-server]]))

(def ^:const init-requires
  '[])

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

(defn init-repl [port type cljs-env]
  (start-server {:port port :name :replique-tooling-repl
                 :accept 'ewen.replique.server/tooling-repl
                 :server-daemon false})
  (start-server {:port 0 :name :replique-repl
                 :accept 'ewen.replique.server/repl
                 :server-daemon false
                 :args [type]})
  (-> @#'clojure.core.server/servers
      (get :replique-tooling-repl)
      :socket
      (.getLocalPort)
      prn))

(defmulti -main (fn [port type cljs-env] (read-string type)))

(defmethod -main :clj [port type cljs-env]
  (require '[ewen.replique.server-clj])
  (init-repl (read-string port) (read-string type) (read-string cljs-env)))

(defmethod -main :cljs [port type cljs-env]
  (require '[ewen.replique.server-cljs])
  (init-repl (read-string port) (read-string type) (read-string cljs-env)))

(defmulti tooling-msg-handle :type)

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
