(ns ewen.replique.server-cljs
  (:require [ewen.replique.server :as server]
            [clojure.core.server :refer [start-server]]
            [clojure.java.io :as io :refer [file]]
            [cljs.repl.browser]
            [cljs.closure :as closure]
            [cljs.env :as cljs-env]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.util :as util]
            [cljs.repl]
            [clojure.string :as string]
            [cljs.js-deps :as deps]
            [ewen.replique.cljs])
  (:import [java.io File]
           [java.util.concurrent SynchronousQueue]))

(comment
  (let [res1 (future (cljs.repl/-evaluate ewen.replique.server-cljs/repl-env nil nil "3"))
        res2 (future (cljs.repl/-evaluate ewen.replique.server-cljs/repl-env nil nil "4"))]
    [@res1 @res2])
  )

(defonce compiler-env nil)
(defonce repl-env nil)
(defonce env {:context :expr :locals {}})
(defonce eval-queue (SynchronousQueue. true))
(defonce evaled-queue (SynchronousQueue. true))

(defn f->src [f]
  (cond (util/url? f) f
        (.exists (io/file f)) (io/file f)
        :else (io/resource f)))

(defn repl-compile-cljs [repl-env f opts]
  (let [src (f->src f)
        compiled (binding [ana/*reload-macros* true]
                   (closure/compile
                    src
                    (assoc opts
                           :output-file
                           (closure/src-file->target-file src)
                           :force true
                           :mode :interactive)))]
    ;; copy over the original source file if source maps enabled
    (when-let [ns (and (:source-map opts) (first (:provides compiled)))]
      (spit
       (io/file (io/file (util/output-directory opts))
                (util/ns->relpath ns (util/ext (:source-url compiled))))
       (slurp src)))
    compiled))

(defn repl-cljs-on-disk [compiled repl-env opts]
  (let [sources (closure/add-dependencies
                 (merge
                  (#'cljs.repl/env->opts repl-env) opts)
                 compiled)]
    (doseq [source sources]
      (closure/source-on-disk opts source))))

(defn foreign->output-file [foreign opts]
  (let [output-path (closure/rel-output-path
                     (assoc foreign :foreign true)
                     opts)]
    (assoc foreign :file output-path)))

(defn refresh-cljs-deps [opts]
  (let [parse-js-fn (fn [js-file]
                      (-> js-file
                          slurp
                          string/split-lines
                          deps/parse-js-ns
                          (assoc :file js-file)))
        is-goog (fn [js-file]
                  (some #(.startsWith % "goog.")
                        (:provides js-file)))
        ups-foreign-libs (:ups-foreign-libs opts)
        js-files (deps/find-js-fs (:output-dir opts))
        js-files (map parse-js-fn js-files)
        js-files (filter #(and (seq (:provides %))
                               (not (is-goog %)))
                         js-files)
        js-files (map closure/map->javascript-file js-files)
        js-files (->> ups-foreign-libs
                      (map #(foreign->output-file % opts))
                      (into js-files))]
    (deps/dependency-order js-files)))

(defn init-class-loader []
  (let [cl (.getContextClassLoader (Thread/currentThread))]
    (.setContextClassLoader (Thread/currentThread)
                            (clojure.lang.DynamicClassLoader. cl))))

(defmulti init-opts :cljs-env)

(defmethod init-opts :browser [{:keys [browser-env-port
                                       browser-env-out
                                       browser-env-main] :as opts}]
  (let [output-dir (.getParent (file browser-env-out))]
    {:comp-opts (merge {:output-to browser-env-out
                        :output-dir output-dir
                        :optimizations :none
                        :recompile-dependents false}
                       (when browser-env-main
                         {:main browser-env-main}))
     :repl-opts {:analyze-path []
                 :port browser-env-port
                 :server-static true
                 :static-dir ["." output-dir]
                 :src []}}))

(defrecord BrowserEnv [wrapped setup-ret]
  cljs.repl/IJavaScriptEnv
  (-setup [this opts] setup-ret)
  (-evaluate [this _ _ js]
    (.put eval-queue js)
    (.take evaled-queue))
  (-load [this provides url]
    (cljs.repl/-load wrapped provides url))
  (-tear-down [this] nil)
  cljs.repl/IReplEnvOptions
  (-repl-options [this]
    (cljs.repl/-repl-options wrapped))
  cljs.repl/IParseStacktrace
  (-parse-stacktrace [this st err opts]
    (cljs.repl/-parse-stacktrace wrapped st err opts))
  cljs.repl/IGetError
  (-get-error [this e env opts]
    (cljs.repl/-get-error wrapped e env opts)))

(defn custom-benv [benv setup-ret]
  (merge (BrowserEnv. benv setup-ret) benv))

(defn setup-benv [repl-env]
  (let [setup-ret (cljs.repl.browser/setup repl-env nil)]
    (doto (Thread.
           (fn []
             (binding [cljs.repl.browser/browser-state
                       (:browser-state repl-env)
                       cljs.repl.browser/ordering (:ordering repl-env)
                       cljs.repl.browser/es (:es repl-env)
                       cljs.repl.server/state (:server-state repl-env)]
               (loop []
                 (->> (.take eval-queue)
                      cljs.repl.browser/browser-eval
                      (.put evaled-queue))
                 (recur))))
           "repl-env-eval")
      (.setDaemon true)
      (.start))
    setup-ret))

(defn init-browser-env [comp-opts repl-opts]
  (alter-var-root #'compiler-env (-> comp-opts
                                   closure/add-implicit-options
                                   cljs-env/default-compiler-env
                                   constantly))
  (let [repl-env* (apply cljs.repl.browser/repl-env
                         (apply concat repl-opts))]
    (cljs-env/with-compiler-env compiler-env
      (comp/with-core-cljs nil
        (fn []
          (let [setup-ret (setup-benv repl-env*)]
            (alter-var-root
             #'repl-env
             (constantly (custom-benv repl-env* setup-ret))))))
      ;; Compile ewen.replique.cljs-env.browser, clojure.browser.repl,
      ;; output a main file and call clojure.browser.repl.connect.
      (let [port (-> @(:server-state repl-env)
                     :socket
                     (.getLocalPort))
            host (-> @(:server-state repl-env)
                     :socket
                     (.getInetAddress) (.getHostAddress)
                     server/normalize-ip-address)
            url (str "http://" host ":" port "/repl")
            repl-src "clojure/browser/repl.cljs"
            benv-src "ewen/replique/cljs_env/browser.cljs"
            repl-compiled (repl-compile-cljs
                           repl-env repl-src comp-opts)
            benv-compiled (repl-compile-cljs
                           repl-env benv-src comp-opts)]
        (repl-cljs-on-disk repl-compiled repl-env comp-opts)
        (repl-cljs-on-disk benv-compiled repl-env comp-opts)
        (->> (refresh-cljs-deps comp-opts)
             (closure/output-deps-file
              (assoc comp-opts :output-to
                     (str (util/output-directory comp-opts)
                          File/separator "cljs_deps.js"))))
        (doto (io/file (util/output-directory comp-opts) "goog" "deps.js")
          util/mkdirs
          (spit (slurp (io/resource "goog/deps.js"))))
        (closure/output-main-file comp-opts)
        (spit (io/file (:output-to comp-opts))
              (str "document.write('<script>if (typeof goog != \"undefined\") { goog.require(\"clojure.browser.repl\"); } else { console.warn(\"ClojureScript could not load :main, did you forget to specify :asset-path?\"); };</script>');\n"
                   "document.write('<script>if (typeof goog != \"undefined\") { goog.require(\"ewen.replique.cljs_env.browser\"); } else { console.warn(\"ClojureScript could not load :main, did you forget to specify :asset-path?\"); };</script>');\n"
                   "document.write('<script>clojure.browser.repl.connect(\"" url "\");</script>');\n")
              :append true)))))

(defn output-index-html [{:keys [output-dir]}]
  (closure/output-one-file {:output-to (str output-dir "/index.html")}
                           (str "<html>
    <body>
        <script type=\"text/javascript\" src=\"main.js\"></script>
    </body>
</html>")))

(defmethod server/repl :cljs
  [type {:keys [comp-opts repl-env compiler-env]}]
  (let [repl-requires '[[cljs.repl
                         :refer-macros [source doc
                                        find-doc apropos
                                        dir pst]]
                        [cljs.pprint :refer [pprint]
                         :refer-macros [pp]]]
        init-fn (fn []
                  (cljs.repl/evaluate-form
                   repl-env
                   env
                   "<cljs repl>"
                   (with-meta
                     `(~'ns ~'cljs.user
                        (:require ~@repl-requires))
                     {:line 1 :column 1})
                   identity comp-opts))]
    (when-not (:connection @(:server-state repl-env))
      (println "Waiting for browser to connect ..."))
    (apply
     (partial cljs.repl/repl repl-env)
     (->> (merge
           comp-opts
           {:compiler-env compiler-env
            :init init-fn})
          (apply concat)))))

(defmethod server/repl-dispatch [:cljs :browser]
  [{:keys [port type cljs-env] :as opts}]
  (init-class-loader)
  (let [{:keys [comp-opts repl-opts]} (init-opts opts)]
    (init-browser-env comp-opts repl-opts)
    (output-index-html comp-opts)
    (start-server {:port port :name :replique-tooling-repl
                   :accept 'ewen.replique.server/tooling-repl
                   :server-daemon false})
    (start-server {:port 0 :name :replique-clj-repl
                   :accept 'ewen.replique.server/repl
                   :server-daemon false
                   :args [:clj nil]})
    (start-server {:port 0 :name :replique-cljs-repl
                   :accept 'ewen.replique.server/repl
                   :server-daemon false
                   :args [type {:comp-opts comp-opts
                                :repl-env repl-env
                                :compiler-env compiler-env}]})
    (doto (file ".replique-port")
      (spit (str {:tooling-repl (-> @#'clojure.core.server/servers
                                    (get :replique-tooling-repl)
                                    :socket
                                    (.getLocalPort))
                  :clj-repl (-> @#'clojure.core.server/servers
                                (get :replique-clj-repl)
                                :socket
                                (.getLocalPort))
                  :cljs-repl (-> @#'clojure.core.server/servers
                                 (get :replique-cljs-repl)
                                 :socket
                                 (.getLocalPort))
                  :cljs-env (-> @(:server-state repl-env)
                                :socket
                                (.getLocalPort))}))
      (.deleteOnExit))
    (println "REPL started")))


(defmethod server/tooling-msg-handle :repl-infos [msg]
  (let [{:keys [replique-tooling-repl replique-cljs-repl]}
        @#'clojure.core.server/servers]
    (assoc (server/repl-infos)
           :replique-cljs-repl
           {:host (-> (:socket replique-cljs-repl)
                      (.getInetAddress) (.getHostAddress)
                      server/normalize-ip-address)
            :port (-> (:socket replique-cljs-repl) (.getLocalPort))}
           :cljs-env
           {:host (-> @(:server-state repl-env)
                      :socket
                      (.getLocalPort))
            :port (-> @(:server-state repl-env)
                      :socket
                      (.getInetAddress) (.getHostAddress)
                      server/normalize-ip-address)})))

(defmethod server/tooling-msg-handle :shutdown [msg]
  (binding [cljs.repl.server/state (:server-state repl-env)]
    (cljs.repl.server/stop))
  (.shutdown (:es repl-env))
  (server/shutdown))
