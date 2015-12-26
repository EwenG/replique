(ns ewen.replique.server
  (:require [clojure.main]
            [clojure.core.server :refer [start-server]]))

(def ^:const init-requires
  '[])

(defn repl []
  (clojure.main/repl :init clojure.core.server/repl-init
                     :read clojure.core.server/repl-read))

(defn tooling-repl []
  (let [init-fn (fn [] (in-ns 'ewen.replique.server))]
    (start-server {:port 0 :name :replique-repl
                   :accept 'ewen.replique.server/repl
                   :server-daemon false})
    (clojure.main/repl
     :init init-fn
     :read clojure.core.server/repl-read
     :prompt #())))

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
