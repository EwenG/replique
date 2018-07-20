(ns replique.tooling
  (:require [clojure.set]
            [clojure.java.io :as io]
            [replique.utils :as utils]
            [replique.server :as server]
            [replique.tooling-msg :as tooling-msg]
            [replique.repliquedoc :as repliquedoc]
            [replique.omniscient :as omniscient]
            [replique.classpath :as classpath]
            [replique.meta :as r-meta]
            [replique.environment :as env :refer [->CljsCompilerEnv]]
            [replique.context :as context]
            [replique.completion :as completion]
            [replique.source-meta]
            [replique.watch]
            [replique.find-usage :as find-usage]))

(def ^:private cljs-compiler-env
  (utils/dynaload 'replique.repl-cljs/compiler-env))
(def ^:private cljs-repl-env
  (utils/dynaload 'replique.repl-cljs/repl-env))
(def ^:private cljs-repl-env-nashorn
  (utils/dynaload 'replique.nashorn/repl-env))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :completion]
  [{:keys [context ns prefix] :as msg}]
  (tooling-msg/with-tooling-response msg
    (when prefix
      {:candidates (completion/candidates nil nil ns context prefix)})))

(defmethod tooling-msg/tooling-msg-handle [:replique/browser :completion]
  [{:keys [context ns prefix] :as msg}]
  (tooling-msg/with-tooling-response msg
    (when prefix
      {:candidates (completion/candidates (->CljsCompilerEnv @@cljs-compiler-env)
                                          @@cljs-repl-env
                                          ns context prefix)})))

(defmethod tooling-msg/tooling-msg-handle [:replique/nashorn :completion]
  [{:keys [context ns prefix] :as msg}]
  (tooling-msg/with-tooling-response msg
    (when prefix
      {:candidates (completion/candidates (->CljsCompilerEnv @@cljs-compiler-env)
                                          @@cljs-repl-env-nashorn
                                          ns context prefix)})))

(comment
  (tooling-msg/tooling-msg-handle {:type :completion
                                   :repl-env :replique/clj
                                   :context nil
                                   :ns 'replique.repl
                                   :prefix "tooli"})

  (tooling-msg/tooling-msg-handle {:type :completion
                                   :repl-env :replique/clj
                                   :context nil
                                   :ns 'replique.compliment.sources
                                   :prefix "all-s"})

  (tooling-msg/tooling-msg-handle {:type :completion
                                   :repl-env :replique/clj
                                   :context nil
                                   :ns 'replique.foo
                                   :prefix "foo"})

  )
(comment
  (tooling-msg/tooling-msg-handle {:repl-env :replique/cljs :type :completion, :context nil, :ns "replique.compliment.ns-mappings-cljs-test", :prefix "gg", :process-id "/Users/egr/clojure/replique/"})
  )

(comment
  
  (tooling-msg/tooling-msg-handle {:type :completion
                                   :repl-env :replique/clj
                                   :context {:locals {"eeeeee" [2577 2578 nil]} :in-string? true}
                                   :ns "replique.tooling"
                                   :prefix "ee"})
  )

(comment
  (tooling-msg/tooling-msg-handle {:type :completion
                                   :repl-env :replique/cljs
                                   :context nil
                                   :ns "replique.compliment.ns-mappings-cljs-test"
                                   :prefix ":cljs.c"})
  
  (tooling-msg/tooling-msg-handle {:type :completion
                                   :repl-env :replique/cljs
                                   :context nil
                                   :ns "replique.compliment.ns-mappings-cljs-test"
                                   :prefix "::eee"})
  
  )

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :repliquedoc]
  [{:keys [context ns] :as msg}]
  (tooling-msg/with-tooling-response msg
    {:doc (repliquedoc/handle-repliquedoc nil ns context)}))

(defmethod tooling-msg/tooling-msg-handle [:replique/cljs :repliquedoc]
  [{:keys [context ns] :as msg}]
  (tooling-msg/with-tooling-response msg
    {:doc (repliquedoc/handle-repliquedoc
           (->CljsCompilerEnv @@cljs-compiler-env)
           ns context)}))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :meta]
  [{:keys [context ns] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [prefix (:symbol msg)
          m (r-meta/handle-meta nil ns context prefix)]
      {:meta m})))

(defmethod tooling-msg/tooling-msg-handle [:replique/cljs :meta]
  [{:keys [context ns] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [prefix (:symbol msg)
          comp-env (->CljsCompilerEnv @@cljs-compiler-env)
          m (r-meta/handle-meta comp-env ns context prefix)]
      {:meta m})))

(comment
  (tooling-msg/tooling-msg-handle {:type :meta
                                   :repl-env :replique/clj
                                   :ns "replique.repl"
                                   :symbol "clojure.core$str"
                                   :context nil})

  (tooling-msg/tooling-msg-handle {:type :meta
                                   :repl-env :replique/clj
                                   :ns "replique.tooling"
                                   :symbol "output-main-js-file"
                                   :context nil})
  )

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :list-namespaces]
  [msg]
  (tooling-msg/with-tooling-response msg
    {:namespaces (->> (all-ns)
                      (map str)
                      (sort completion/by-length-comparator))}))

(defmethod tooling-msg/tooling-msg-handle [:replique/cljs :list-namespaces]
  [msg]
  (tooling-msg/with-tooling-response msg
    (if-not (try (realized? @cljs-compiler-env) (catch Exception _ false))
      {:namespaces '()}
      {:namespaces (->> (->CljsCompilerEnv @@cljs-compiler-env)
                        replique.environment/all-ns
                        (map str)
                        (sort completion/by-length-comparator))})))

(defn output-main-js-file [output-to main-ns]
  (let [port (server/server-port)]
    (io/make-parents output-to)
    (spit
     output-to
     (format "//main-js-file autogenerated by replique
var CLOSURE_UNCOMPILED_DEFINES = null;
(function() {
  var port = '%s';
  %s
  document.write('<script src=\"http://localhost:' + port + '/goog/base.js\"></script>');
  document.write('<script src=\"http://localhost:' + port + '/cljs_deps.js\"></script>');
  document.write('<script>goog.require(\"replique.cljs_env.repl\");</script>');
  document.write('<script>goog.require(\"replique.cljs_env.browser\");</script>');
  %s
  document.write('<script src=\"http://localhost:' + port + '/replique/cljs_env/bootstrap.js\"></script>');
  document.write('<script>replique.cljs_env.repl.connect(\"http://localhost:' + port + '\");</script>')})();"
             port
             (if main-ns (str "var mainNs = '" (namespace-munge main-ns) "';") "")
             (if main-ns
               "document.write('<script>goog.require(\"' + mainNs + '\");</script>');"
               "")))))

(defmethod tooling-msg/tooling-msg-handle [:replique/cljs :output-main-js-files]
  [{:keys [output-to main-ns] :as msg}]
  (tooling-msg/with-tooling-response msg
    (output-main-js-file output-to main-ns)
    msg))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :eval] [{:keys [form] :as msg}]
  (tooling-msg/with-tooling-response msg
    (binding [*out* utils/process-out
              *err* utils/process-err]
      {:result (pr-str (eval (read-string {:read-cond :allow} form)))})))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :update-classpath]
  [msg]
  (tooling-msg/with-tooling-response msg
    (let [res (classpath/update-classpath (:classpath msg))]
      (reset! completion/classpath-data (completion/compute-classpath-data))
      res)))

(defn ns-files [comp-env the-ns]
  (distinct
   (for [[s v] (env/ns-interns comp-env the-ns)
         :let [f (:file (env/meta comp-env v))]
         ;; exclude vars defined at the repl
         :when (and f (or (.endsWith ^String f (env/file-extension comp-env))
                          (.endsWith ^String f ".cljc")))]
     f)))

(defn var-with-meta [comp-env [v-name v]]
  (let [metas (env/meta comp-env v)
        filtered-metas (select-keys metas [:line :column])
        f (r-meta/resource-str (:file metas))
        f (when (and f (or (.endsWith ^String f (env/file-extension comp-env))
                           (.endsWith ^String f ".cljc"))) f)
        filtered-metas (if f
                         (assoc filtered-metas :file f)
                         filtered-metas)]
    [v-name filtered-metas]))

(defn list-vars-with-meta [comp-env the-ns]
  (let [indexes (range)
        vars-with-meta (map (partial var-with-meta comp-env)
                            (env/ns-interns comp-env the-ns))
        file-sort-fn #(-> % second :file)
        line-sort-fn #(-> % second :line)
        column-sort-fn #(-> % second :column)]
    (sort-by (juxt file-sort-fn line-sort-fn column-sort-fn)
             vars-with-meta)))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :list-vars]
  [{:keys [ns] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [ns (symbol ns)
          the-ns (find-ns ns)]
      (when the-ns
        {:vars (list-vars-with-meta nil the-ns)}))))

(defmethod tooling-msg/tooling-msg-handle [:replique/cljs :list-vars]
  [{:keys [ns] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [comp-env (->CljsCompilerEnv @@cljs-compiler-env)
          ns (symbol ns)
          the-ns (env/find-ns comp-env ns)]
      (when the-ns
        {:vars (list-vars-with-meta comp-env the-ns)}))))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :context]
  [{:keys [ns repl-env] :as msg}]
  (tooling-msg/with-tooling-response msg
    (when ns
      (context/context-forms
       nil repl-env ns
       context/context-forms-clj))))

(defmethod tooling-msg/tooling-msg-handle [:replique/browser :context]
  [{:keys [ns repl-env] :as msg}]
  (tooling-msg/with-tooling-response msg
    (when ns
      (context/context-forms
       (->CljsCompilerEnv @@cljs-compiler-env) repl-env ns
       context/context-forms-cljs))))

(defmethod tooling-msg/tooling-msg-handle [:replique/nashorn :context]
  [{:keys [ns repl-env] :as msg}]
  (tooling-msg/with-tooling-response msg
    (when ns
      (context/context-forms
       (->CljsCompilerEnv @@cljs-compiler-env) repl-env ns
       context/context-forms-cljs))))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :captured-env-locals]
  [{:keys [ns repl-env captured-env] :as msg}]
  (tooling-msg/with-tooling-response msg
    (when ns
      {:captured-env-locals (omniscient/captured-env-locals nil ns captured-env)})))

(defmethod tooling-msg/tooling-msg-handle [:replique/browser :captured-env-locals]
  [{:keys [ns repl-env captured-env] :as msg}]
  (tooling-msg/with-tooling-response msg
    (when ns
      {:captured-env-locals (omniscient/captured-env-locals-cljs
                             (->CljsCompilerEnv @@cljs-compiler-env)
                             @@cljs-repl-env
                             ns captured-env)})))

(comment
  (tooling-msg/tooling-msg-handle {:repl-env :replique/clj
                                   :type :context
                                   :ns 'replique.tooling})

  (tooling-msg/tooling-msg-handle {:repl-env :replique/cljs
                                   :type :context
                                   :ns 'cljs.user})
  )


(defmethod tooling-msg/tooling-msg-handle [:replique/clj :source-meta]
  [msg]
  (tooling-msg/with-tooling-response msg
    (reset! replique.source-meta/source-meta (select-keys msg [:url :line :column]))
    {}))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :symbols-in-namespaces]
  [{:keys [context ns] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [prefix (:symbol msg)]
      (assoc (find-usage/symbols-in-namespaces
              nil ns context prefix)
             :default-ns "user"))))

(defmethod tooling-msg/tooling-msg-handle [:replique/cljs :symbols-in-namespaces]
  [{:keys [context ns is-string?] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [prefix (:symbol msg)
          comp-env (->CljsCompilerEnv @@cljs-compiler-env)]
      (assoc (find-usage/symbols-in-namespaces
              comp-env ns context prefix)
             :default-ns "cljs.user"))))

;; Exclude system modules / boot classpath since we are only looking for clj/sc sources
(defmethod tooling-msg/tooling-msg-handle [:replique/clj :classpath-for-sources]
  [msg]
  (tooling-msg/with-tooling-response msg
    {:paths (->> (classpath/paths false)
                 (into '() (map str))
                 reverse)}))
