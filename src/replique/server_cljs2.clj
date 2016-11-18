(ns ewen.replique.server-cljs
  (:require [ewen.replique.server :refer [with-tooling-response]
             :as server]
            [clojure.core.server :refer [start-server *session*]]
            [clojure.java.io :as io :refer [file]]
            [cljs.repl.browser]
            [cljs.closure :as closure]
            [cljs.env :as cljs-env]
            [cljs.analyzer :as ana]
            [cljs.analyzer.api :as ana-api]
            [cljs.compiler :as comp]
            [cljs.util :as util]
            [cljs.repl]
            [clojure.string :as string]
            [cljs.js-deps :as deps]
            [clojure.tools.reader :as reader]
            [cljs.closure :as cljsc]
            [ewen.replique.cljs]
            [ewen.replique.sourcemap]
            [compliment.context :as context]
            [compliment.sources.local-bindings
             :refer [bindings-from-context]]
            [clojure.data.json :as json]
            [cljs.tagged-literals :as tags]
            [compliment.core :as compliment])
  (:import [java.io File]
           [java.net URL]
           [java.util.concurrent SynchronousQueue]
           [java.net SocketException]
           [clojure.lang IExceptionInfo]
           [java.util.concurrent.locks ReentrantLock]))

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
(defonce cljs-outs (atom #{}))

(defmacro ^:private with-lock
  [lock-expr & body]
  `(let [lockee# ~(with-meta lock-expr
                    {:tag 'java.util.concurrent.locks.ReentrantLock})]
     (.lock lockee#)
     (try
       ~@body
       (finally
         (.unlock lockee#)))))

(defn f->src [f]
  (cond (util/url? f) f
        (.exists (io/file f)) (io/file f)
        :else (io/resource f)))

(defn repl-compile-cljs
  ([f opts]
   (repl-compile-cljs f opts true))
  ([f opts reload-macros]
   (let [src (f->src f)
         compiled (binding [ana/*reload-macros* reload-macros]
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
     compiled)))

(defn repl-cljs-on-disk [compiled repl-opts opts]
  (let [sources (closure/add-dependencies
                 (merge repl-opts opts)
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
                 :serve-static true
                 :static-dir ["." output-dir]
                 :src []}}))

(defmethod init-opts :webapp [{:keys [webapp-env-port
                                      webapp-env-out
                                      webapp-env-main] :as opts}]
  (let [output-dir (.getParent (file webapp-env-out))]
    {:comp-opts (merge {:output-to webapp-env-out
                        :output-dir output-dir
                        :optimizations :none
                        :recompile-dependents false}
                       (when webapp-env-main
                         {:main webapp-env-main}))
     :repl-opts {:analyze-path []
                 :port webapp-env-port
                 :serve-static false
                 :src []}}))

(defmethod init-opts :replique [{:keys [directory] :as opts}]
  (let [output-dir (file directory "out")]
    {:comp-opts (merge {:output-to (.getAbsolutePath
                                    (file output-dir "main.js"))
                        :output-dir (.getAbsolutePath output-dir)
                        :optimizations :none
                        :recompile-dependents false})
     :repl-opts {:analyze-path []
                 :port 0
                 :serve-static true
                 :static-dir [output-dir]
                 :src []}}))

(defmethod cljs.repl.browser/handle-post :print
  [{:keys [content order]} conn _ ]
  (cljs.repl.browser/constrain-order
   order
   (fn []
     ;; Maybe we should print only in the currently active REPL instead of all REPLs
     (doseq [[out out-lock] @cljs-outs]
       (binding [*out* out]
         (with-lock out-lock
           (print (read-string content))
           (.flush *out*))))))
  (cljs.repl.server/send-and-close conn 200 "ignore__"))

;; Bypass the cljs load-javascript function in order to use the
;; race condition free "browser-eval" function
(defn load-javascript
  "Accepts a REPL environment, a list of namespaces, and a URL for a
  JavaScript file which contains the implementation for the list of
  namespaces. Will load the JavaScript file into the REPL environment
  if any of the namespaces have not already been loaded from the
  ClojureScript REPL."
  [repl-env provides url]
  (cljs.repl/-evaluate repl-env nil nil (slurp url)))

(defrecord BrowserEnv [wrapped setup-ret]
  cljs.repl/IJavaScriptEnv
  (-setup [this opts] setup-ret)
  (-evaluate [this _ _ js]
    (.put eval-queue js)
    (.take evaled-queue))
  (-load [this provides url]
    (load-javascript this provides url))
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

;; Used instead of cljs.repl.browser to avoid compiling client.js.
;; client.js is not needed because the browser repl uses xhr with cors
;; instead of crosspagechannel
(defn setup [repl-env opts]
  (binding [cljs.repl.browser/browser-state (:browser-state repl-env)
            cljs.repl.browser/ordering (:ordering repl-env)
            cljs.repl.browser/es (:es repl-env)
            cljs.repl.server/state (:server-state repl-env)]
    (println "Waiting for browser to connect ...")
    (cljs.repl.server/start repl-env)))

(defn custom-benv [benv setup-ret]
  (merge (BrowserEnv. benv setup-ret) benv))

(defn setup-benv [repl-env]
  (let [setup-ret (setup repl-env nil)]
    (doto (Thread.
           (fn []
             (binding [cljs.repl.browser/browser-state
                       (:browser-state repl-env)
                       cljs.repl.browser/ordering (:ordering repl-env)
                       cljs.repl.browser/es (:es repl-env)
                       cljs.repl.server/state (:server-state repl-env)]
               (loop []
                 (let [js (.take eval-queue)
                       evaled (try
                                (cljs.repl.browser/browser-eval js)
                                (catch SocketException e
                                  {:status :error
                                   :value "Connection broken"}))]
                   (.put evaled-queue evaled))
                 (recur))))
           "repl-env-eval")
      (.setDaemon true)
      (.start))
    setup-ret))

(defn compute-asset-path [asset-path output-dir rel-path]
  (let [asset-path (if asset-path (str "\"" asset-path "\"") "null")
        output-dir (if output-dir (str "\"" output-dir "\"") "null")
        rel-path (if rel-path (str "\"" rel-path "\"") "null")]
    (str "(function(assetPath, outputDir, relPath) {
          if(assetPath) {
            return assetPath;
          }
          var computedAssetPath = assetPath? assetPath : outputDir;
          if(!outputDir ||  !relPath) {
            return computedAssetpath;
          }
          var endsWith = function(str, suffix) {
            return str.indexOf(suffix, str.length - suffix.length) !== -1;
          }
          var origin = window.location.protocol + \"//\" + window.location.hostname + (window.location.port ? ':' + window.location.port: '');
          var scripts = document.getElementsByTagName(\"script\");
          for(var i = 0; i < scripts.length; ++i) {
            var src = scripts[i].src;
            if(src && endsWith(src, relPath)) {
              var relPathIndex = src.indexOf(relPath);
              var originIndex = src.indexOf(origin);
              if(originIndex === 0) {
                return src.substring(origin.length+1, relPathIndex);
              }
            }
          }
          return computedAssetPath;
        })(" asset-path ", " output-dir ", " rel-path ");\n")))

(defn output-main-file [{:keys [closure-defines output-dir output-to]
                         :as opts} port]
  (let [closure-defines (json/write-str closure-defines)
        output-dir-uri (-> output-dir (File.) (.toURI))
        output-to-uri (-> output-to (File.) (.toURI))
        output-dir-path (-> (.normalize output-dir-uri)
                            (.toString))
        output-to-path (-> (.normalize output-to-uri)
                           (.toString))
        ;; If output-dir is not a parent dir of output-to, then
        ;; we don't try to infer the asset path because it may not
        ;; be possible.
        rel-path (if (and (.startsWith output-to-path
                                       output-dir-path)
                          (not= output-dir-path output-to-path))
                   (-> (.relativize output-dir-uri output-to-uri)
                       (.toString))
                   nil)]
    (cljsc/output-one-file
     opts
     (str "(function() {\n"
          "var assetPath = " (compute-asset-path (:asset-path opts) (util/output-directory opts) rel-path)
          "var CLOSURE_UNCOMPILED_DEFINES = " closure-defines ";\n"
          "if(typeof goog == \"undefined\") document.write('<script src=\"'+ assetPath +'/goog/base.js\"></script>');\n"
          "document.write('<script src=\"'+ assetPath +'/cljs_deps.js\"></script>');\n"
          "document.write('<script>if (typeof goog != \"undefined\") { goog.require(\"ewen.replique.cljs_env.repl\"); } else { console.warn(\"ClojureScript could not load :main, did you forget to specify :asset-path?\"); };</script>');\n"
          "document.write('<script>if (typeof goog != \"undefined\") { goog.require(\"ewen.replique.cljs_env.browser\"); } else { console.warn(\"ClojureScript could not load :main, did you forget to specify :asset-path?\"); };</script>');\n"
          (when (:main opts)
            (when-let [main (try (-> (:main opts)
                                     ana-api/parse-ns :ns)
                                 (catch Exception e nil))]
              (str "document.write('<script>if (typeof goog != \"undefined\") { goog.require(\"" (comp/munge main) "\"); } else { console.warn(\"ClojureScript could not load :main, did you forget to specify :asset-path?\"); };</script>');\n"
                   "document.write('<script>if (typeof goog != \"undefined\") {ewen.replique.cljs_env.repl.connect(\"http://localhost:" port "\");} else { console.warn(\"ClojureScript could not load :main, did you forget to specify :asset-path?\"); };</script>');")))
          "})();\n"))))

(defn init-browser-env
  ([comp-opts repl-opts]
   (init-browser-env comp-opts repl-opts true))
  ([comp-opts repl-opts output-main-file?]
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
       (let [port (-> @(:server-state repl-env)
                      :socket
                      (.getLocalPort))
             repl-src "ewen/replique/cljs_env/repl.cljs"
             benv-src "ewen/replique/cljs_env/browser.cljs"
             repl-compiled (repl-compile-cljs repl-src comp-opts false)
             benv-compiled (repl-compile-cljs benv-src comp-opts false)]
         (repl-cljs-on-disk
          repl-compiled (#'cljs.repl/env->opts repl-env) comp-opts)
         (repl-cljs-on-disk
          benv-compiled (#'cljs.repl/env->opts repl-env)comp-opts)
         (->> (refresh-cljs-deps comp-opts)
              (closure/output-deps-file
               (assoc comp-opts :output-to
                      (str (util/output-directory comp-opts)
                           File/separator "cljs_deps.js"))))
         (doto (io/file (util/output-directory comp-opts) "goog" "deps.js")
           util/mkdirs
           (spit (slurp (io/resource "goog/deps.js"))))
         (when output-main-file?
           (output-main-file comp-opts port)))))))

(defn compile-ui []
  (let [{:keys [comp-opts repl-opts]} (init-opts {:cljs-env :replique})]
    (cljs-env/with-compiler-env (cljs-env/default-compiler-env)
      (let [main-src "ewen/replique/ui/main.cljs"
            main-compiled (repl-compile-cljs main-src comp-opts false)]
        (repl-cljs-on-disk main-compiled {} comp-opts)
        (->> (refresh-cljs-deps comp-opts)
             (closure/output-deps-file
              (assoc comp-opts :output-to
                     (str (util/output-directory comp-opts)
                          File/separator "cljs_deps.js"))))
        (doto (io/file (util/output-directory comp-opts) "goog" "deps.js")
          util/mkdirs
          (spit (slurp (io/resource "goog/deps.js"))))))))

(defn output-index-html [{:keys [output-dir]}]
  (closure/output-one-file {:output-to (str output-dir "/index.html")}
                           (str "<html>
    <body>
        <script type=\"text/javascript\" src=\"main.js\"></script>
    </body>
</html>")))

(def special-fns
  {'ewen.replique.server-cljs/server-connection?
   (fn [repl-env env form opts]
     (prn (if (:connection @(:server-state repl-env))
            true false)))})

;; Customize repl-read to avoid quitting the REPL on :clj/quit
;; Unfortunatly, this is cannot be handled by the cljs :read hook
(defn repl-read
  ([request-prompt request-exit]
   (repl-read request-prompt request-exit cljs.repl/*repl-opts*))
  ([request-prompt request-exit opts]
   (binding [*in* (if (true? (:source-map-inline opts))
                    ((:reader opts))
                    *in*)]
     (or ({:line-start request-prompt :stream-end request-exit}
          (cljs.repl/skip-whitespace *in*))
         (let [input (reader/read
                      {:read-cond :allow :features #{:cljs}} *in*)]
           (cljs.repl/skip-if-eol *in*)
           (if (= :cljs/quit input)
             '(do :cljs/quit)
             input))))))

(defn repl-caught [e repl-env opts]
  (binding [*out* server/tooling-err]
    (with-lock server/tooling-err-lock
      (-> (assoc {:type :eval
                  :error true
                  :repl-type :cljs
                  :session *session*
                  :ns (str ana/*cljs-ns*)}
                 :value (if (and (instance? IExceptionInfo e)
                                 (#{:js-eval-error :js-eval-exception}
                                  (:type (ex-data e))))
                          (:value (:error (ex-data e)))
                          (.getMessage e)))
          prn)))
  (cljs.repl/repl-caught e repl-env opts))

(defmethod server/repl :cljs [type]
  (let [out-lock (ReentrantLock.)]
    (swap! cljs-outs conj [*out* out-lock])
    (when-not (:connection @(:server-state repl-env))
      (println "Waiting for browser to connect ..."))
    (apply
     (partial cljs.repl/repl repl-env)
     (->> (merge
           (:options @compiler-env)
           {:compiler-env compiler-env
            :read repl-read
            :quit-prompt #()
            :caught repl-caught
            :print (fn [result]
                     (binding [*out* server/tooling-out]
                       (with-lock server/tooling-out-lock
                         (prn {:type :eval
                               :repl-type :cljs
                               :session *session*
                               :ns ana/*cljs-ns*
                               :result result})))
                     (with-lock out-lock
                       (println result)))})
          (apply concat)))
    (swap! cljs-outs disj [*out* out-lock])))

(defmethod server/repl-dispatch [:cljs :browser]
  [{:keys [port type cljs-env directory sass-bin] :as opts}]
  (alter-var-root #'server/directory (constantly directory))
  (alter-var-root #'server/sass-bin (constantly sass-bin))
  #_(init-class-loader)
  (let [{:keys [comp-opts repl-opts]} (init-opts opts)]
    (init-browser-env comp-opts repl-opts false)
    (start-server {:port port :name :replique
                   :accept 'clojure.core.server/repl
                   :server-daemon false})
    (doto (file ".replique-port")
      (spit (str {:repl (-> @#'clojure.core.server/servers
                            (get :replique) :socket (.getLocalPort))
                  :cljs-env (-> @(:server-state repl-env)
                                :socket (.getLocalPort))}))
      (.deleteOnExit))
    (println "REPL started")))

(defmethod server/repl-dispatch [:cljs :webapp]
  [{:keys [port type cljs-env directory sass-bin] :as opts}]
  (alter-var-root #'server/directory (constantly directory))
  (alter-var-root #'server/sass-bin (constantly sass-bin))
  #_(init-class-loader)
  (let [{:keys [comp-opts repl-opts]} (init-opts opts)]
    (init-browser-env comp-opts repl-opts)
    (start-server {:port port :name :replique
                   :accept 'clojure.core.server/repl
                   :server-daemon false})
    (doto (file ".replique-port")
      (spit (str {:repl (-> @#'clojure.core.server/servers
                            (get :replique) :socket (.getLocalPort))
                  :cljs-env (-> @(:server-state repl-env)
                                :socket (.getLocalPort))}))
      (.deleteOnExit))
    (println "REPL started")))

(defmethod server/repl-dispatch [:cljs :replique]
  [{:keys [port type cljs-env directory sass-bin] :as opts}]
  (alter-var-root #'server/directory (constantly directory))
  (alter-var-root #'server/sass-bin (constantly sass-bin))
  #_(init-class-loader)
  (let [{:keys [comp-opts repl-opts]} (init-opts opts)]
    (init-browser-env comp-opts repl-opts false)
    (start-server {:port port :name :replique
                   :accept 'clojure.core.server/repl
                   :server-daemon false})
    (doto (file ".replique-port")
      (spit (str {:repl (-> @#'clojure.core.server/servers
                            (get :replique) :socket (.getLocalPort))
                  :cljs-env (-> @(:server-state repl-env)
                                :socket (.getLocalPort))}))
      (.deleteOnExit))
    (println "REPL started")))

(defmethod server/tooling-msg-handle :repl-infos [msg]
  (assoc (server/repl-infos)
         :repl-type :cljs
         :cljs-env
         {:host (-> @(:server-state repl-env)
                    :socket
                    (.getLocalPort))
          :port (-> @(:server-state repl-env)
                    :socket
                    (.getInetAddress) (.getHostAddress)
                    server/normalize-ip-address)}))

(defmethod server/tooling-msg-handle :shutdown [msg]
  (binding [cljs.repl.server/state (:server-state repl-env)]
    (cljs.repl.server/stop))
  (.shutdown (:es repl-env))
  (server/shutdown))

(defmethod server/tooling-msg-handle :list-css [msg]
  (with-tooling-response
    msg
    {:css-infos (-> (cljs.repl/-evaluate
                     repl-env "<cljs repl>" 1
                     "ewen.replique.cljs_env.browser.list_css_infos();")
                    :value
                    read-string)}))

(defn css-infos-process-uri [{:keys [file-path uri scheme]
                              :as css-infos}]
  (if (= "data" scheme)
    (assoc css-infos
           :uri
           (->> (slurp file-path)
                ewen.replique.sourcemap/encode-base-64
                (str "data:text/css;base64,")))
    css-infos))

(defmethod server/tooling-msg-handle :load-css [msg]
  (with-tooling-response
    msg
    {:result (->> (css-infos-process-uri msg)
                  pr-str pr-str
                  (format "ewen.replique.cljs_env.browser.reload_css(%s);")
                  (cljs.repl/-evaluate
                   repl-env "<cljs repl>" 1)
                  :value)}))

(defn assoc-css-file [output-dir {:keys [uri] :as css-infos}]
  (let [url (URL. uri)]
    (if (and (= "file" (.getProtocol url))
             (.exists (File. (.toURI url))))
      (assoc css-infos :css-file
             (->> (.getPath url)
                  (File.)
                  (.getAbsolutePath)))
      (assoc css-infos :css-file
             (->> (.getPath url)
                  (File. output-dir)
                  (.getAbsolutePath))))))

(defn assoc-sourcemap [{:keys [scheme css-file uri] :as css-infos}]
  (cond (= "http" scheme)
        (assoc css-infos :sourcemap
               (ewen.replique.sourcemap/css-file->sourcemap css-file))
        (= "data" scheme)
        (-> (assoc css-infos :sourcemap
                   (ewen.replique.sourcemap/data->sourcemap uri))
            (dissoc :uri))
        :else nil))

(defn assoc-child-source [path {:keys [sourcemap css-file file-path]
                                :as css-infos}]
  (if (nil? sourcemap)
    (assoc css-infos :child-source path)
    (let [css-file (-> (or css-file file-path)
                       ewen.replique.sourcemap/str->path)
          paths (->> (get sourcemap "sources")
                     (map #(ewen.replique.sourcemap/str->path %)))
          path (ewen.replique.sourcemap/str->path path)
          compare-fn #(let [ref (.normalize path)
                            other (.normalize
                                   (.resolveSibling css-file %))]
                        (when (.equals ref other) (str %)))
          child-source (some compare-fn paths)]
      (if child-source
        (assoc css-infos :child-source child-source)
        nil))))

(comment
  (assoc-child-source "/home/egr/replique.el/lein-project/resources/ff.scss"
                      {:scheme "http", :uri "http://localhost:36466/ff.css", :css-file "/home/egr/replique.el/lein-project/out/ff.css", :sourcemap {"version" 3, "file" "ff.css", "sources" ["../resources/ff.scss"], "sourcesContent" ["body {\n\n}\n"], "mappings" "", "names" []}})
  )

(defn assoc-main-source [path {:keys [child-source sourcemap]
                               :as css-infos}]
  (if (nil? sourcemap)
    (assoc css-infos :main-source path)
    (let [path (ewen.replique.sourcemap/str->path path)
          main-path (-> (get sourcemap "sources") first
                        ewen.replique.sourcemap/str->path)
          child-path (ewen.replique.sourcemap/str->path child-source)
          relative-path (.relativize child-path main-path)]
      (->> (.resolve path relative-path)
           (.normalize)
           str
           (assoc css-infos :main-source)))))

(defmethod server/tooling-msg-handle :list-sass
  [{:keys [file-path] :as msg}]
  (with-tooling-response msg
    (let [output-dir (:output-dir (:options @compiler-env))
          css-infos (->
                     (cljs.repl/-evaluate
                      repl-env "<cljs repl>" 1
                      "ewen.replique.cljs_env.browser.list_css_infos();")
                     :value
                     read-string)
          {css-http "http" css-data "data"}
          (group-by :scheme css-infos)
          css-http (doall
                    (map (partial assoc-css-file output-dir) css-http))
          css-http (doall
                    (map assoc-sourcemap css-http))
          css-http (doall
                    (keep #(assoc-child-source file-path %) css-http))
          css-http (doall
                    (map #(assoc-main-source file-path %) css-http))
          css-data (doall
                    (map assoc-sourcemap css-data))
          css-data (doall
                    (keep #(assoc-child-source file-path %) css-data))]
      {:sass-infos (into css-http css-data)})))

(comment
  (server/tooling-msg-handle
   {:type :list-sass
    :file-path "/home/egr/electron/resources/replique/resources/stylesheet/main.scss"})
  )

(defn compile-sass [input-path output-path]
  (let [pb (ProcessBuilder.
            (list server/sass-bin input-path output-path))
        p (.start pb)
        out (.getInputStream p)
        err (.getErrorStream p)]
    (if (= 0 (.waitFor p))
      [true (slurp out)]
      [false (slurp err)])))

(comment
  (compile-sass
   "/home/egr/clojure/wreak-todomvc/test.scss"
   "test.css")
  )

(defn remove-path-extension [path]
  (let [last-separator-index (.lastIndexOf path File/separator)
        file-name (if (= -1 last-separator-index)
                    path
                    (.substring path (+ 1 last-separator-index)))
        extension-index (.lastIndexOf file-name ".")
        extension-index (cond
                          (= -1 extension-index)
                          -1
                          (= -1 last-separator-index)
                          (.lastIndexOf file-name ".")
                          :else (-> (.lastIndexOf file-name ".")
                                    (+ (count path))
                                    (- (count file-name))))]
    (if (= -1 extension-index)
      path
      (.substring path 0 extension-index))))

(defn compile-sass-data
  [repl-env {:keys [scheme file-path main-source sass-path]
             :as msg}]
  (let [[success css-text]
        (compile-sass
         main-source
         (str (remove-path-extension file-path) ".css"))]
    (if success
      (->> (ewen.replique.sourcemap/encode-base-64 css-text)
           (str "data:text/css;base64,")
           (assoc msg :uri))
      (assoc msg :error css-text))))

(defn compile-sass-http
  [repl-env {:keys [scheme file-path main-source sass-path uri]
             :as msg}]
  (let [[success css-text] (compile-sass main-source file-path)]
    (when success
      (doto (io/file file-path)
        util/mkdirs
        (spit css-text)))
    (if success
      msg
      (merge msg {:error css-text}))))

(defmethod server/tooling-msg-handle :load-scss
  [{:keys [scheme] :as msg}]
  (with-tooling-response msg
    (let [msg (if (= scheme "data")
                (compile-sass-data repl-env msg)
                (compile-sass-http repl-env msg))]
      (if (:error msg)
        msg
        (->> msg pr-str pr-str
             (format "ewen.replique.cljs_env.browser.reload_css(%s);")
             (cljs.repl/-evaluate
              repl-env "<cljs repl>" 1)
             :value
             (hash-map :result))))))

(comment
  (server/tooling-msg-handle
   {:type :load-scss
    :scheme "http"
    :uri "http://localhost:36466/ff.css"
    :file-path "/home/egr/replique.el/lein-project/out/ff.css"
    :main-source nil})
  )

(defn resolve-ns-alias [comp-env current-ns alias]
  (get-in @comp-env [::ana/namespaces
                     current-ns :requires alias]))

(comment
  (get (get @compiler-env :js-dependency-index) "goog.string.format")
  )

(defn resolve-var
  [comp-env current-ns sym]
  (if (= "js" (namespace sym))
    {:not-found :js}
    (let [s (str sym)]
      (cond
        (not (nil? (namespace sym)))
        (let [ns (namespace sym)
              ns (if (= "clojure.core" ns) "cljs.core" ns)
              ns (symbol ns)
              full-ns (resolve-ns-alias comp-env current-ns ns)]
          (merge (ana/gets @comp-env ::ana/namespaces full-ns
                           :defs (symbol (name sym)))
                 {:name (symbol (str full-ns) (str (name sym)))
                  :ns full-ns}))

        (not (nil? (ana/gets @comp-env
                             ::ana/namespaces current-ns
                             :uses sym)))
        (let [full-ns (ana/gets @comp-env
                                ::ana/namespaces current-ns
                                :uses sym)]
          (merge
           (ana/gets @comp-env ::ana/namespaces full-ns :defs sym)
           {:name (symbol (str full-ns) (str sym))
            :ns full-ns}))

        (not (nil? (ana/gets @comp-env
                             ::ana/namespaces current-ns
                             :imports sym)))
        (recur comp-env current-ns (ana/gets @comp-env
                                             ::ana/namespaces current-ns
                                             :imports sym))

        (not (nil? (ana/gets @comp-env :js-dependency-index s)))
        (merge (select-keys (ana/gets @comp-env :js-dependency-index s)
                            [:file])
               {:name s})

        (and (.contains s ".") (not (.contains s "..")))
        (let [idx (.indexOf s ".")
              prefix (symbol (subs s 0 idx))
              suffix (subs s (inc idx))]
          (let [info (ana/gets @comp-env
                               ::ana/namespaces current-ns
                               :defs prefix)]
            (if-not (nil? info)
              (merge info
                     {:name (symbol (str current-ns) (str sym))
                      :ns current-ns})
              (merge (ana/gets @comp-env
                               ::ana/namespaces prefix
                               :defs (symbol suffix))
                     {:name (if (= "" prefix)
                              (symbol suffix)
                              (symbol (str prefix) suffix))
                      :ns prefix}))))

        :else
        (let [full-ns (cond
                        (not (nil? (ana/gets @comp-env
                                             ::ana/namespaces current-ns
                                             :defs sym)))
                        current-ns
                        (cljs-env/with-compiler-env comp-env
                          (ana/core-name? env sym))
                        'cljs.core
                        :else current-ns)]
          (merge (ana/gets @comp-env
                           ::ana/namespaces full-ns :defs sym)
                 {:name (symbol (str full-ns) (str sym))
                  :ns full-ns}))))))

(defmethod server/tooling-msg-handle :cljs-var-meta
  [{:keys [context ns symbol keys] :as msg}]
  (with-tooling-response msg
    (let [ctx (when context (binding [reader/*data-readers*
                                      tags/*cljs-data-readers*]
                              (reader/read-string context)))
          ctx (context/parse-context ctx)
          bindings (bindings-from-context ctx)
          keys (into #{} keys)]
      (cond
        (or (nil? ns) (nil? symbol))
        {:meta nil}
        (and ctx (contains? (into #{} bindings) (name symbol)))
        {:not-found :local-binding}
        :else
        (let [v (when (symbol? symbol)
                  (resolve-var compiler-env ns symbol))
              meta (when v
                     (server/format-meta v keys))]
          (if (empty? meta)
            {:meta nil}
            {:meta meta}))))))

(comment
  (server/tooling-msg-handle {:type :cljs-var-meta
                              :context nil
                              :ns 'ewen.replique.ui.dashboard
                              :symbol 'goog.string.format
                              :keys [:column :line :file]})

  (server/tooling-msg-handle {:type :cljs-var-meta
                              :context nil
                              :ns 'ewen.replique.ui.dashboard
                              :symbol 'node/require
                              :keys [:column :line :file]})

  (server/tooling-msg-handle {:type :cljs-var-meta
                              :context nil
                              :ns 'ewen.replique.ui.dashboard
                              :symbol 'add-watch
                              :keys [:column :line :file]})
  )

(defmethod server/tooling-msg-handle :cljs-completion
  [{:keys [context ns prefix] :as msg}]
  (with-tooling-response msg
    (let [ctx (when context (binding [reader/*data-readers*
                                      tags/*cljs-data-readers*]
                              (reader/read-string context)))]
      {:candidates (compliment/completions
                    prefix {:ns ns :context ctx
                            :cljs-comp-env compiler-env
                            :sources [:compliment.sources.ns-mappings/ns-mappings]})})))


(comment
  (server/tooling-msg-handle {:type :cljs-completion
                              :context nil
                              :ns 'ewen.replique.ui.dashboard
                              :prefix "replique-d"})
  )
