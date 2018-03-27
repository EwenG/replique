(ns replique.tooling-msg
  (:refer-clojure :exclude [in-ns])
  (:require [replique.utils :as utils]
            [clojure.stacktrace :refer [print-stack-trace]])
  (:import [java.util.concurrent.locks ReentrantLock]))

(defonce process-id nil)
(defonce tooling-out nil)
(defonce tooling-out-lock (ReentrantLock.))
(defonce tooling-err nil)
(defonce tooling-prn prn)

(defmacro with-tooling-response [msg & resp]
  `(try (dissoc (merge (~'do ~@resp) ~msg) :context)
        (catch Exception t# (assoc ~msg :error t#))))

(defn tooling-msg-handle-dispatch-fn [{:keys [repl-env type]}] [repl-env type])
(defmulti tooling-msg-handle tooling-msg-handle-dispatch-fn)

(defmethod tooling-msg-handle :default
  [{:keys [process-id repl-env type] :as msg}]
  (assoc msg :error (format "Invalid tooling message type: %s for repl-env: %s" type repl-env)))

(defn uncaught-exception [thread ex]
  (if (or (nil? tooling-err) (nil? tooling-out-lock))
    (throw ex)
    (binding [*out* tooling-err
              *print-length* nil
              *print-level* nil
              *print-meta* nil]
      (utils/with-lock tooling-out-lock
        (tooling-prn {:type :error
                      :process-id process-id
                      :repl-type (utils/repl-type utils/*repl-env*)
                      :repl-env utils/*repl-env*
                      :thread (.getName ^Thread thread)
                      :ns (ns-name *ns*)
                      :value ex})))))

(defn tooling-available? []
  (boolean (and tooling-out tooling-err tooling-out-lock)))

(defmulti set-print-format identity)

(defmethod set-print-format :default [print-format]
  ;; Do nothing
  )

(defmethod set-print-format :elisp [print-format]
  (require '[replique.elisp-printer])
  (alter-var-root #'tooling-prn (constantly @(resolve 'replique.elisp-printer/prn))))

