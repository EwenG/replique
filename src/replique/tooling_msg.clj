(ns replique.tooling-msg
  (:require [replique.utils :as utils]
            [replique.elisp-printer :as elisp]
            [clojure.stacktrace :refer [print-stack-trace]])
  (:import [java.util.concurrent.locks ReentrantLock]))

(defonce directory nil)
(defonce tooling-out nil)
(defonce tooling-out-lock (ReentrantLock.))
(defonce tooling-err nil)

(defmacro with-tooling-response [msg & resp]
  `(let [type# (:type ~msg)
         directory# (:directory ~msg)]
     (try (merge {:type type# :directory directory#} (~'do ~@resp))
          (catch Exception t#
            {:directory directory#
             :type type#
             :error t#}))))

(defmulti tooling-msg-handle :type)

(defmethod tooling-msg-handle :default
  [{:keys [directory type] :as msg}]
  {:directory directory
   :type type
   :error (format "Invalid tooling message type: %s" type)})

(defn uncaught-exception [thread ex]
  (if (or (nil? tooling-err) (nil? tooling-out-lock))
    (throw ex)
    (binding [*out* tooling-err]
      (utils/with-lock tooling-out-lock
        (elisp/prn {:type :eval
                    :directory directory
                    :error true
                    :repl-type :clj
                    :thread (.getName ^Thread thread)
                    :ns (ns-name *ns*)
                    :value (with-out-str (print-stack-trace ex))})))))
