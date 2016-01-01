(ns ewen.replique.server-clj
  (:require [ewen.replique.server :as server]
            [clojure.core.server :refer [start-server]]
            [clojure.java.io :as io :refer [file]]))

(defmethod server/repl-dispatch [:clj nil]
  [{:keys [port type cljs-env] :as opts}]
  (start-server {:port port :name :replique-tooling-repl
                 :accept 'ewen.replique.server/tooling-repl
                 :server-daemon false})
  (start-server {:port 0 :name :replique-clj-repl
                 :accept 'ewen.replique.server/repl
                 :server-daemon false
                 :args [type nil]})
  (doto (file ".replique-port")
    (spit (str {:tooling-repl (-> @#'clojure.core.server/servers
                                  (get :replique-tooling-repl)
                                  :socket
                                  (.getLocalPort))
                :clj-repl (-> @#'clojure.core.server/servers
                              (get :replique-clj-repl)
                              :socket
                              (.getLocalPort))}))
    (.deleteOnExit))
  (println "REPL started"))

(defmethod server/tooling-msg-handle :repl-infos [msg]
  (server/repl-infos))

(defmethod server/tooling-msg-handle :shutdown [msg]
  (server/shutdown))
