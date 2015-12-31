(ns ewen.replique.server
  (:require [clojure.main]
            [clojure.java.io :refer [file]]))

(def directory nil)
(def ^:const init-requires '[])

(defn normalize-ip-address [address]
  (if (= "0.0.0.0" address) "127.0.0.1" address))

(defn repl-infos []
  (let [{:keys [replique-tooling-repl replique-repl]}
        @#'clojure.core.server/servers]
    {:directory directory
     :replique-tooling-repl
     {:host (-> (:socket replique-tooling-repl)
                (.getInetAddress) (.getHostAddress) normalize-ip-address)
      :port (-> (:socket replique-tooling-repl) (.getLocalPort))}
     :replique-repl
     {:host (-> (:socket replique-tooling-repl)
                (.getInetAddress) (.getHostAddress) normalize-ip-address)
      :port (-> (:socket replique-repl) (.getLocalPort))}}))

(defmulti repl (fn [type opts] type))

(defmethod repl :clj
  [type _]
  (println "Clojure" (clojure-version))
  (clojure.main/repl :init clojure.core.server/repl-init
                     :read clojure.core.server/repl-read))

(defn tooling-repl []
  (let [init-fn (fn [] (in-ns 'ewen.replique.server))]
    (clojure.main/repl
     :init init-fn
     :read clojure.core.server/repl-read
     :prompt #())))

(defmulti repl-dispatch (fn [{:keys [type cljs-env]}]
                          [type cljs-env]))

(defmulti tooling-msg-handle :type)
