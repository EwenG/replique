(ns replique.repl-cljs-common
  (:require [cljs.analyzer :as ana]
            [clojure.java.io :as io]
            [clojure.tools.reader :as reader]
            [cljs.tagged-literals :as tags]
            [clojure.string :as string]
            [replique.http :as http]
            [replique.utils :as utils]
            [replique.repl-common :as repl-common]
            [replique.tooling-msg :as tooling-msg])
  (:import [java.util.concurrent SynchronousQueue RejectedExecutionException ExecutorService]
           [java.util.regex Pattern]))

(defonce cljs-server (atom {:state :stopped}))
(defonce cljs-outs (atom #{}))

(def env {:context :expr :locals {}})

(def ^:dynamic *stopped-eval-executor?* false)

;; Used to send runtime REPL params to the Replique client (*print-length*, *print-level* ...)
;; Also used to reset set! these values when the js runtime reconnects to the REPL
;; Unlike with the Clojure REPL these values are thus global to all cljs REPLs !
;; repl-params are initialized to the same value than the ones of the Clojure process
(defonce repl-params (atom {"cljs.core/*assert*" *assert*
                            "cljs.core/*print-length*" *print-length*
                            "cljs.core/*print-meta*" *print-meta*
                            "cljs.core/*print-level*" *print-level*
                            "cljs.core/*flush-on-newline*" *flush-on-newline*
                            "cljs.core/*print-readably*" *print-readably*
                            "cljs.core/*print-dup*" *print-dup*}))

(defn repl-read [request-exit]
  (binding [*ns* (create-ns ana/*cljs-ns*)
            reader/resolve-symbol ana/resolve-symbol
            reader/*data-readers* tags/*cljs-data-readers*
            reader/*alias-map*
            (apply merge
                   ((juxt :requires :require-macros)
                    (ana/get-namespace ana/*cljs-ns*)))]
    (try
      (repl-common/repl-read request-exit #{:cljs})
      (catch Throwable e
        (throw (ex-info nil {:clojure.error/phase :read-source} e))))))

(defn init-core-bindings []
  `(do
     ~@(for [[k v] @repl-params]
         `(~'set! ~(symbol k) ~v))))

(defn shutdown-eval-executor [executor]
  (let [pendingTasks (.shutdownNow ^ExecutorService executor)]
    (binding [*stopped-eval-executor?* true]
      (doseq [task pendingTasks]
        (.run ^Runnable task)))))

(defn stop-http-server []
  (let [{:keys [eval-executor result-executor]} @cljs-server]
    (swap! cljs-server assoc :state :stopped)
    (when eval-executor (shutdown-eval-executor eval-executor))
    (when result-executor (.shutdownNow ^ExecutorService result-executor))))

;; ============================================================================
;; Eval Task (sends JS to browser and waits for result)
;; ============================================================================

(defprotocol ISubmitted
  (is-submitted? [this]))

(deftype EvalTask [js ^:unsynchronized-mutable submitted?]
  Callable
  (call [this]
    (if *stopped-eval-executor?*
      {:status :error :value "Connection broken"}
      (let [{:keys [js-queue result-queue]} @cljs-server]
        (try
          (.put ^SynchronousQueue js-queue js)
          (set! submitted? true)
          (.take ^SynchronousQueue result-queue)
          ;; If the repl-env is shutdown
          (catch InterruptedException e
            {:status :error :value "Connection broken"})))))
  ISubmitted
  (is-submitted? [this] submitted?))

(defn updated-ns? [prev-comp-env comp-env ns-sym]
  (not (identical? (-> prev-comp-env :cljs.analyzer/namespaces (get ns-sym) :defs)
                   (-> comp-env :cljs.analyzer/namespaces (get ns-sym) :defs))))

(defn updated-var? [prev-comp-env comp-env var-sym]
  (let [ns-sym (symbol (namespace var-sym))
        var-sym (symbol (name var-sym))]
    (not (identical? (-> prev-comp-env :cljs.analyzer/namespaces (get ns-sym) :defs (get var-sym))
                     (-> comp-env :cljs.analyzer/namespaces (get ns-sym) :defs (get var-sym))))))

(defn dispatch-request-session-expired [request callback]
  {:status 500 :body "Session expired" :content-type "text/plain"})

(defn dispatch-request-result [{:keys [content] :as request} callback]
  (let [{:keys [result-queue js-queue result-executor]} @cljs-server
        result-task (reify Callable
                      (call [this]
                        (try
                          (.put ^SynchronousQueue result-queue (read-string (:content content)))
                          (try
                            (callback {:status 200
                                       :content-type "text/javascript"
                                       :body (.take ^SynchronousQueue js-queue)})
                            (catch InterruptedException e (throw e))
                            ;; Socket closed ...
                            (catch Exception e
                              (.put ^SynchronousQueue result-queue
                                    {:status :error :value "Connection broken"})))
                          (catch InterruptedException e
                            (try (callback
                                  {:status 409 :body "Connection closed"
                                   :content-type "text/plain"})
                                 (catch Exception e nil))))))]
    (try (.submit ^ExecutorService result-executor result-task)
         (catch RejectedExecutionException e
           {:status 409 :body "Connection closed" :content-type "text/plain"}))))

(defn dispatch-request-print [{:keys [content]} callback]
  ;; Maybe we should print only in the currently active REPL instead of all REPLs
  (doseq [out @cljs-outs]
    (binding [*out* out]
      (print (:content content))
      (.flush *out*)))
  {:status 200 :body "ignore__" :content-type "text/plain"})

(defn dispatch-request-print-tooling [{:keys [content]} callback]
  (when tooling-msg/tooling-out
    (binding [*out* tooling-msg/tooling-out]
      (utils/with-lock tooling-msg/tooling-out-lock
        (.append *out* ^String (:content content))
        (flush))))
  {:status 200 :body "ignore__" :content-type "text/plain"})

(defn dispatch-request-assets [{path :path :as request} callback]
  (if (not= "/favicon.ico" path)
    (let [path (if (= "/" path) "/index.html" path)
          local-path (cond->
                         (seq (for [x [utils/cljs-compile-path]
                                    :when (.exists (io/file (str x path)))]
                                (str x path)))
                       (complement nil?)
                       first)
          local-path (if (nil? local-path)
                       (cond
                         (re-find #".jar" path)
                         (io/resource (second (string/split path #".jar!/")))
                         (re-find (Pattern/compile (System/getProperty "user.dir")) path)
                         (-> (string/replace path (str (System/getProperty "user.dir") "/") "")
                             io/file)
                         :else nil)
                       local-path)]
      (if local-path
        (if-let [ext (some #(when (.endsWith ^String path %) %) (keys http/ext->mime-type))]
          (let [mime-type (http/ext->mime-type ext)
                encoding (if (contains? http/text-encoding mime-type) "UTF-8" "ISO-8859-1")
                body (slurp local-path :encoding encoding)]
            (future (try (callback {:status 200
                                    :body body
                                    :content-type mime-type
                                    :encoding encoding})
                         ;; Socket closed
                         (catch Exception e
                           (tooling-msg/uncaught-exception (Thread/currentThread) e)))))
          (let [body (slurp local-path)]
            (future
              (try (callback {:status 200 :body body})
                   ;; Socket closed
                   (catch Exception e
                     (tooling-msg/uncaught-exception (Thread/currentThread) e))))))
        (http/make-404 path)))
    (http/make-404 path)))

(defn dispatch-request-default [request callback]
  {:status 500 :body (format "Cannot handle request %s" (str request))
   :content-type "text/plain"})

