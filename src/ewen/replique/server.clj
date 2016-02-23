(ns ewen.replique.server
  (:require [clojure.main]
            [clojure.core.server :refer [*session*]]
            [clojure.java.io :refer [file]]
            [ewen.replique.compliment.context :as context]
            [ewen.replique.compliment.sources.local-bindings
             :refer [bindings-from-context]]
            [ewen.replique.compliment.core :as compliment]
            [ewen.replique.compliment.sources :as compliment-sources])
  (:import [java.util.concurrent.locks ReentrantLock]
           [java.io File]))

(defonce directory nil)
(defonce sass-bin nil)
(defonce tooling-out nil)
(defonce tooling-out-lock (ReentrantLock.))
(defonce tooling-err nil)
(defonce tooling-err-lock (ReentrantLock.))

(defmacro ^:private with-lock
  [lock-expr & body]
  `(let [lockee# ~(with-meta lock-expr
                    {:tag 'java.util.concurrent.locks.ReentrantLock})]
     (.lock lockee#)
     (try
       ~@body
       (finally
         (.unlock lockee#)))))

(defmulti repl-dispatch (fn [{:keys [type cljs-env]}]
                          [type cljs-env]))

(defmacro with-tooling-response [msg resp]
  `(let [type# (:type ~msg)]
     (try (merge {:type type#} ~resp)
          (catch Throwable t#
            {:type type#
             :error t#}))))

(defmulti tooling-msg-handle :type)

(defn normalize-ip-address [address]
  (cond (= "0.0.0.0" address) "127.0.0.1"
        (= "0:0:0:0:0:0:0:1" address) "127.0.0.1"
        :else address))

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

(comment
  (clojure.main/repl :prompt #())
  )

(defn shared-tooling-repl []
  (with-lock tooling-out-lock
    (alter-var-root #'tooling-out (constantly *out*)))
  (with-lock tooling-err-lock
    (alter-var-root #'tooling-err (constantly *err*)))
  (let [init-fn (fn [] (in-ns 'ewen.replique.server))]
    (clojure.main/repl
     :init init-fn
     :prompt #()
     :print (fn [result]
              (with-lock tooling-out-lock
                (prn result))))))

(defn shutdown []
  (clojure.core.server/stop-servers))

(defmulti repl (fn [type] type))

(defmethod repl :clj [type]
  ;; Make all threads print in this repl by default
  (alter-var-root #'*out* (constantly *out*))
  (alter-var-root #'*err* (constantly *err*))
  (println "Clojure" (clojure-version))
  (clojure.main/repl
   :init clojure.core.server/repl-init
   :caught (fn [e]
             (binding [*out* tooling-err]
               (with-lock tooling-err-lock
                 (prn {:type :eval
                       :error true
                       :repl-type :clj
                       :session *session*
                       :ns (ns-name *ns*)
                       :value (.getMessage e)})))
             (clojure.main/repl-caught e))
   :print (fn [result]
            (binding [*out* tooling-out]
              (with-lock tooling-out-lock
                (prn {:type :eval
                      :repl-type :clj
                      :session *session*
                      :ns (ns-name *ns*)
                      :result (pr-str result)})))
            (prn result))))

(defn format-meta [{:keys [file] :as meta} keys]
  (let [f (and file (File. file))]
    (if (and f (.exists f))
      (select-keys (assoc
                    meta :file
                    (.getAbsolutePath f))
                   keys)
      (select-keys meta (disj keys :file :line :column)))))

(defmethod tooling-msg-handle :clj-var-meta
  [{:keys [context ns symbol keys] :as msg}]
  (with-tooling-response msg
    (let [ctx (when context (read-string context))
          ctx (context/parse-context ctx)
          bindings (bindings-from-context ctx)
          keys (into #{} keys)]
      (cond
        (or (nil? ns) (nil? symbol) (nil? (find-ns ns)))
        {:meta nil}
        (and ctx (contains? (into #{} bindings) (name symbol)))
        {:not-found :local-binding}
        :else
        (let [v (when (symbol? symbol)
                  (try (ns-resolve ns symbol)
                       (catch ClassNotFoundException e
                         nil)))
              meta (when (and v (meta v))
                     (format-meta (meta v) keys))]
          (if (empty? meta)
            {:meta nil}
            {:meta meta}))))))

(comment
  (let [tooling-msg-handle "e"]
    tooling-msg-handle))

(comment
  (tooling-msg-handle {:type :clj-var-meta
                       :context nil
                       :ns 'ewen.replique.server
                       :symbol 'tooling-msg-handle
                       :keys '(:column :line :file)})

  (tooling-msg-handle {:type :clj-var-meta
                       :context nil
                       :ns 'ewen.replique.compliment.core
                       :symbol 'all-sources
                       :keys '(:column :line :file)})

  (tooling-msg-handle {:type :clj-var-meta
                       :context nil
                       :ns 'ewen.foo
                       :symbol 'foo-bar
                       :keys '(:column :line :file)})

  )

(defmethod tooling-msg-handle :clj-completion
  [{:keys [context ns prefix] :as msg}]
  (with-tooling-response msg
    (let [ctx (when context (read-string context))]
      {:candidates (compliment/completions prefix {:ns ns :context ctx})})))


(comment
  (tooling-msg-handle {:type :clj-completion
                       :context nil
                       :ns 'ewen.replique.server
                       :prefix "tooli"})

  (tooling-msg-handle {:type :clj-completion
                       :context nil
                       :ns 'ewen.replique.compliment.sources
                       :prefix "all-s"})

  (tooling-msg-handle {:type :clj-completion
                       :context nil
                       :ns 'ewen.foo
                       :prefix "foo"})

  (let [eeeeee "e"]
    eee)
  )
