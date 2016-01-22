(ns ewen.replique.server-clj
  (:require [ewen.replique.server :refer [with-tooling-response]
             :as server]
            [clojure.core.server :refer [start-server]]
            [clojure.java.io :as io :refer [file]]))

(defmethod server/repl-dispatch [:clj nil]
  [{:keys [port type cljs-env directory] :as opts}]
  (alter-var-root #'server/directory (constantly directory))
  (start-server {:port port :name :replique
                 :accept 'clojure.core.server/repl
                 :server-daemon false})
  (doto (file ".replique-port")
    (spit (str {:repl (-> @#'clojure.core.server/servers
                          (get :replique) :socket (.getLocalPort))}))
    (.deleteOnExit))
  (println "REPL started"))

(defmethod server/tooling-msg-handle :repl-infos [msg]
  (assoc (server/repl-infos) :repl-type :clj))

(defmethod server/tooling-msg-handle :shutdown [msg]
  (server/shutdown))
