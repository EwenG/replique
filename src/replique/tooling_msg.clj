(ns replique.tooling-msg
  (:require [replique.utils :as utils]
            [replique.elisp-printer :as elisp]
            [clojure.stacktrace :refer [print-stack-trace]])
  (:import [java.util.concurrent.locks ReentrantLock]))

(defonce process-id nil)
(defonce tooling-out nil)
(defonce tooling-out-lock (ReentrantLock.))
(defonce tooling-err nil)

(defmacro with-tooling-response [msg & resp]
  `(let [type# (:type ~msg)
         process-id# (:process-id ~msg)]
     (try (merge {:type type# :process-id process-id#} (~'do ~@resp))
          (catch Exception t#
            {:process-id process-id#
             :type type#
             :error t#}))))

(defmulti tooling-msg-handle :type)

(defmethod tooling-msg-handle :default
  [{:keys [process-id type] :as msg}]
  {:process-id process-id
   :type type
   :error (format "Invalid tooling message type: %s" type)})

(defn uncaught-exception [thread ex]
  (if (or (nil? tooling-err) (nil? tooling-out-lock))
    (throw ex)
    (binding [*out* tooling-err]
      (utils/with-lock tooling-out-lock
        (elisp/prn {:type :eval
                    :process-id process-id
                    :error true
                    :repl-type :clj
                    :thread (.getName ^Thread thread)
                    :ns (ns-name *ns*)
                    :value (with-out-str (print-stack-trace ex))})))))
