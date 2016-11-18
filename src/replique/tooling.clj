(ns replique.tooling
  (:require [clojure.set]
            [replique.utils :as utils]
            [replique.server :as server]
            [replique.tooling-msg :as tooling-msg]
            [replique.repliquedoc :as repliquedoc]
            [replique.compliment.core :as compliment]
            [replique.compliment.context :as context]
            [replique.compliment.sources.local-bindings
             :refer [bindings-from-context]]
            [replique.environment :refer [->CljsCompilerEnv]]
            [replique.compliment.context :as context]))

(def ^:private cljs-compiler-env
  (utils/dynaload 'replique.repl-cljs/compiler-env))

(defmethod tooling-msg/tooling-msg-handle :clj-completion
  [{:keys [context ns prefix] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [{:keys [context]} (context/cache-context nil (when ns (symbol ns)) context)
          context (reverse context)]
      {:candidates (compliment/completions
                    prefix
                    {:ns (when ns (symbol ns))
                     :context context})})))

(comment
  (tooling-msg/tooling-msg-handle {:type :clj-completion
                              :context nil
                              :ns 'replique.repl
                              :prefix "tooli"})

  (tooling-msg/tooling-msg-handle {:type :clj-completion
                              :context nil
                              :ns 'replique.compliment.sources
                              :prefix "all-s"})

  (tooling-msg/tooling-msg-handle {:type :clj-completion
                                   :context nil
                                   :ns 'replique.foo
                                   :prefix "foo"})

  )

(defmethod tooling-msg/tooling-msg-handle :cljs-completion
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

(defmethod tooling-msg/tooling-msg-handle :cljc-completion
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
  (tooling-msg/tooling-msg-handle {:type :cljs-completion
                              :context nil
                              :ns "replique.compliment.ns-mappings-cljs-test"
                              :prefix ":cljs.c"})
  
  (tooling-msg/tooling-msg-handle {:type :cljs-completion
                              :context nil
                              :ns "replique.compliment.ns-mappings-cljs-test"
                              :prefix "::eee"})
  
  )

(defmethod tooling-msg/tooling-msg-handle :repliquedoc-clj
  [{:keys [context ns symbol] :as msg}]
  (tooling-msg/with-tooling-response msg
    {:doc (repliquedoc/handle-repliquedoc nil ns context symbol)}))

(defmethod tooling-msg/tooling-msg-handle :repliquedoc-cljs
  [{:keys [context ns symbol] :as msg}]
  (tooling-msg/with-tooling-response msg
    {:doc (repliquedoc/handle-repliquedoc
           (->CljsCompilerEnv @@cljs-compiler-env)
           ns context symbol)}))

(defmethod tooling-msg/tooling-msg-handle :repliquedoc-cljc
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

(defn output-main-cljs-file [output-to main-ns]
  (let [port (server/server-port)]
    (spit output-to
          (str "//main-cljs-file autogenerated by replique
var CLOSURE_UNCOMPILED_DEFINES = null;
document.write('<script src=\"http://localhost:" port "/goog/base.js\"></script>');
document.write('<script src=\"http://localhost:" port "/cljs_deps.js\"></script>');
document.write('<script>goog.require(\"replique.cljs_env.repl\");</script>');
" (when main-ns (str "document.write('<script>goog.require(\"" main-ns "\");</script>');
"))    
               "document.write('<script>replique.cljs_env.repl.connect(\"http://localhost:" port "\");</script>');"))))

(defmethod tooling-msg/tooling-msg-handle :output-main-cljs-files
  [{:keys [main-cljs-files] :as msg}]
  (tooling-msg/with-tooling-response msg
    (doseq [[output-to main-ns] main-cljs-files]
      (output-main-cljs-file output-to main-ns))
    (assoc msg :main-cljs-files main-cljs-files)))


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

