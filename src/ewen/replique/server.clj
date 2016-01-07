(ns ewen.replique.server
  (:require [clojure.main]
            [clojure.core.server :refer [*session*]]
            [clojure.java.io :refer [file]]))

(def directory nil)
(defonce tooling-out nil)
(defonce tooling-out-lock (Object.))
(defonce tooling-err nil)
(defonce tooling-err-lock (Object.))

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
  (let [init-fn (fn [] (in-ns 'ewen.replique.server))]
    (clojure.main/repl
     :init init-fn
     :prompt #()
     :print (fn [result]
              (prn result)))))

(defn shared-tooling-repl []
  (locking tooling-out-lock
    (alter-var-root #'tooling-out (constantly *out*)))
  (locking tooling-err-lock
    (alter-var-root #'tooling-err (constantly *err*)))
  (let [init-fn (fn [] (in-ns 'ewen.replique.server))]
    (clojure.main/repl
     :init init-fn
     :prompt #()
     :print (fn [result]
              (locking tooling-out-lock
                (prn result))))))

(defn shutdown []
  (clojure.core.server/stop-servers))

(defmulti repl (fn [type] type))

(defmethod repl :clj [type]
  (println "Clojure" (clojure-version))
  (clojure.main/repl
   :init clojure.core.server/repl-init
   :caught (fn [e]
             (binding [*out* tooling-err]
               (locking tooling-err-lock
                 (prn {:type :eval
                       :error true
                       :repl-type :clj
                       :session *session*
                       :ns (str *ns*)
                       :value (.getMessage e)})))
             (clojure.main/repl-caught e))
   :print (fn [result]
            (binding [*out* tooling-out]
              (locking tooling-out-lock
                (prn {:type :eval
                      :repl-type :clj
                      :session *session*
                      :ns (str *ns*)
                      :result (pr-str result)})))
            (prn result))))

(defmulti repl-dispatch (fn [{:keys [type cljs-env]}]
                          [type cljs-env]))

(defmulti tooling-msg-handle :type)
