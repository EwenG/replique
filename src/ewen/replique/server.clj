(ns ewen.replique.server
  (:require [clojure.main]
            [clojure.java.io :refer [file]]))

(def ^:const init-requires
  '[])

(defmulti repl (fn [type opts] type))

(defn tooling-repl []
  (let [init-fn (fn [] (in-ns 'ewen.replique.server))]
    (clojure.main/repl
     :init init-fn
     :read clojure.core.server/repl-read
     :prompt #())))

(defmulti repl-dispatch (fn [{:keys [type cljs-env]}] [type cljs-env]))

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
