(ns ewen.replique.server
  (:require [clojure.main]
            [clojure.core.server :refer [*session*]]
            [clojure.java.io :refer [file]]))

(def directory nil)
(def ^:const init-requires '[])
(defonce ^:dynamic *prompt* nil)
(defonce tooling-out nil)
(defonce tooling-out-lock (Object.))

(defn normalize-ip-address [address]
  (if (= "0.0.0.0" address) "127.0.0.1" address))

(defn repl-infos []
  (let [server-infos (:replique @#'clojure.core.server/servers)]
    {:directory directory
     :replique
     {:host (-> (:socket server-infos)
                (.getInetAddress) (.getHostAddress) normalize-ip-address)
      :port (-> (:socket server-infos) (.getLocalPort))}}))

(defn tooling-repl []
  #_(alter-var-root #'tooling-out (constantly *out*))
  (let [init-fn (fn [] (in-ns 'ewen.replique.server))]
    (clojure.main/repl
     :init init-fn
     :prompt #()
     :print (fn [result]
              (prn result)
              #_(locking tooling-out-lock
                  (prn result))))))

(defn repl-clj []
  (println "Clojure" (clojure-version))
  (clojure.main/repl
   :init clojure.core.server/repl-init))

(defn shutdown []
  (clojure.core.server/stop-servers))

(defmulti repl (fn [type opts] type))

(defmethod repl :clj
  [type _]
  #_(println "Clojure" (clojure-version))
  (binding [*prompt* #()]
    (clojure.main/repl :init clojure.core.server/repl-init
                       :read clojure.core.server/repl-read
                       :prompt (fn [] (*prompt*))
                       :print (fn [result]
                                (binding [*out* tooling-out]
                                  (locking tooling-out-lock
                                    (prn {:type :eval
                                          :repl-type :clj
                                          :session *session*
                                          :result (pr-str result)})))
                                (prn result)))))

(defmulti repl-dispatch (fn [{:keys [type cljs-env]}]
                          [type cljs-env]))

(defmulti tooling-msg-handle :type)
