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
            [replique.compliment.core :as compliment]
            [replique.compliment.context :as context]
            [replique.compliment.sources.local-bindings
             :refer [bindings-from-context]]
            [replique.environment :refer [->CljsCompilerEnv]]
            [replique.compliment.context :as context]))

(def ^:private cljs-compiler-env
  (utils/dynaload 'replique.repl-cljs/compiler-env))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :completion]
  [{:keys [context ns prefix] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [{:keys [context]} (context/cache-context nil (when ns (symbol ns)) context)
          context (reverse context)]
      {:candidates (compliment/completions
                    prefix
                    {:ns (when ns (symbol ns))
                     :context context})})))

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

(defmethod tooling-msg/tooling-msg-handle [:replique/cljs :completion]
  [{:keys [context ns prefix] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [{:keys [context]} (context/cache-context
                             (->CljsCompilerEnv @@cljs-compiler-env)
                             (when ns (symbol ns)) context)
          context (reverse context)]
      {:candidates (compliment/completions
                    prefix
                    {:ns (when ns (symbol ns)) :context context
                     :comp-env (->CljsCompilerEnv @@cljs-compiler-env)
                     :sources
                     [:replique.compliment.sources.ns-mappings/ns-mappings
                      :replique.compliment.sources.namespaces-and-classes/namespaces-and-classes
                      :replique.compliment.sources.keywords/keywords
                      :replique.compliment.sources.local-bindings/local-bindings
                      :replique.compliment.sources.special-forms/literals
                      :replique.compliment.sources.special-forms/special-forms]})})))

;; not used but could be useful to dispatch on the reader conditional around the cursor
#_(defmethod tooling-msg/tooling-msg-handle :cljc-completion
  [{:keys [context ns prefix] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [{:keys [reader-conditionals context]} (context/cache-context
                                                 (->CljsCompilerEnv @@cljs-compiler-env)
                                                 (when ns (symbol ns)) context)
          context (reverse context)]
      (if (= #{:cljs} reader-conditionals)
        {:candidates (compliment/completions
                      prefix
                      {:ns (when ns (symbol ns)) :context context
                       :comp-env (->CljsCompilerEnv @@cljs-compiler-env)
                       :sources
                       [:replique.compliment.sources.ns-mappings/ns-mappings
                        :replique.compliment.sources.namespaces-and-classes/namespaces-and-classes
                        :replique.compliment.sources.keywords/keywords
                        :replique.compliment.sources.local-bindings/local-bindings
                        :replique.compliment.sources.special-forms/literals
                        :replique.compliment.sources.special-forms/special-forms]})}
        {:candidates (compliment/completions
                      prefix
                      {:ns (when ns (symbol ns))
                       :context context})}))))

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
  [{:keys [context ns symbol] :as msg}]
  (tooling-msg/with-tooling-response msg
    {:doc (repliquedoc/handle-repliquedoc nil ns context symbol)}))

(defmethod tooling-msg/tooling-msg-handle [:replique/cljs :repliquedoc]
  [{:keys [context ns symbol] :as msg}]
  (tooling-msg/with-tooling-response msg
    {:doc (repliquedoc/handle-repliquedoc
           (->CljsCompilerEnv @@cljs-compiler-env)
           ns context symbol)}))

;; not used but could be useful to dispatch on the reader conditional around the cursor
#_(defmethod tooling-msg/tooling-msg-handle :repliquedoc-cljc
  [{:keys [context ns symbol] :as msg}]
  (tooling-msg/with-tooling-response msg
    {:doc (repliquedoc/handle-repliquedoc-cljc
           (->CljsCompilerEnv @@cljs-compiler-env)
           ns context symbol)}))

(comment

  (context/cache-context "[(print __prefix__)]")

  (context/cache-context "(.ff rr __prefix__)")

  (bindings-from-context
   (context/cache-context "(let [e nil]
__prefix__)"))
  
  )

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :meta]
  [{:keys [context ns is-string?] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [sym-at-point (:symbol msg)
          {:keys [context]} (context/cache-context nil (when ns (symbol ns)) context)
          context (reverse context)
          m (if is-string?
              (r-meta/handle-meta-str sym-at-point)
              (r-meta/handle-meta nil ns context sym-at-point))]
      (assoc msg :meta m))))

(defmethod tooling-msg/tooling-msg-handle [:replique/cljs :meta]
  [{:keys [context ns is-string?] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [sym-at-point (:symbol msg)
          comp-env (->CljsCompilerEnv @@cljs-compiler-env)
          {:keys [context]} (context/cache-context comp-env (when ns (symbol ns)) context)
          context (reverse context)
          m (if is-string?
              (r-meta/handle-meta-str sym-at-point)
              (r-meta/handle-meta comp-env ns context sym-at-point))]
      (assoc msg :meta m))))

;; not used but could be useful to dispatch on the reader conditional around the cursor
#_(defmethod tooling-msg/tooling-msg-handle :meta-cljc
  [{:keys [context ns is-string?] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [sym-at-point (:symbol msg)
          comp-env (->CljsCompilerEnv @@cljs-compiler-env)
          {:keys [reader-conditionals context]} (context/cache-context
                                                 nil (when ns (symbol ns)) context)
          context (reverse context)
          comp-env (when (= #{:cljs} reader-conditionals) comp-env)
          m (if is-string?
              (r-meta/handle-meta-str sym-at-point)
              (r-meta/handle-meta comp-env ns context sym-at-point))]
      (assoc msg :meta m))))

(comment
  (tooling-msg/tooling-msg-handle {:type :meta
                                   :repl-env :replique/clj
                                   :ns "replique.repl"
                                   :symbol "clojure.core$str"
                                   :is-string? true
                                   :context nil})
  )

(defmethod tooling-msg/tooling-msg-handle [:replique/cljs :list-namespaces]
  [msg]
  (tooling-msg/with-tooling-response msg
    (if-not (try (realized? @cljs-compiler-env) (catch Exception _ false))
      (assoc msg :namespace '())
      (->> (->CljsCompilerEnv @@cljs-compiler-env)
           replique.environment/all-ns
           (map str)
           (assoc msg :namespaces)))))

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
      (assoc msg :result (pr-str (eval (read-string {:read-cond :allow} form)))))))

(comment
  (defmethod tooling-msg/tooling-msg-handle :spec-completion
    [{:keys [context prefix spec] :as msg}]
    (tooling-msg/with-tooling-response msg
      (let [{:keys [context]} (context/cache-context nil nil context)]
        {:candidates nil})))

  (defmethod tooling-msg/tooling-msg-handle :repliquedoc-spec
    [{:keys [context ns symbol] :as msg}]
    )
  )

