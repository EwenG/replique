(ns ewen.replique.server-cljs
  (:require [ewen.replique.server :refer [with-tooling-response]
             :as server]
            [clojure.core.server :refer [start-server *session*]]
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
            [clojure.tools.reader :as reader]
            [ewen.replique.cljs]
            [ewen.replique.sourcemap])
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
  ([repl-env f opts]
   (repl-compile-cljs repl-env f opts true))
  ([repl-env f opts reload-macros]
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

(defmethod cljs.repl.browser/handle-post :print
  [{:keys [content order]} conn _ ]
  (cljs.repl.browser/constrain-order
   order
   (fn []
     ;; When sessions are implemented, we should only print in the
     ;; output stream of the "currently active" cljs REPL
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
                           repl-env repl-src comp-opts false)
            benv-compiled (repl-compile-cljs
                           repl-env benv-src comp-opts false)]
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
                               :ns (str ana/*cljs-ns*)
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
    (init-browser-env comp-opts repl-opts)
    (output-index-html comp-opts)
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
  (assoc css-infos :css-file
         (->> uri (URL.) (.getPath)
              (File. output-dir)
              (.getAbsolutePath))))

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
          paths (->> (:sources sourcemap)
                     (map #(str (:sourceRoot sourcemap) %))
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

(defn assoc-main-source [path {:keys [child-source sourcemap]
                               :as css-infos}]
  (if (nil? sourcemap)
    (assoc css-infos :main-source path)
    (let [path (ewen.replique.sourcemap/str->path path)
          main-path (-> (:sources sourcemap) first
                        ewen.replique.sourcemap/str->path)
          child-path (ewen.replique.sourcemap/str->path child-source)
          relative-path (.relativize child-path main-path)]
      (->> (.resolve path relative-path)
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
    :file-path "/home/egr/replique.el/lein-project/out/ee.scss"})
  )

(defn compile-sass [sass-path input-path output-path]
  (let [pb (ProcessBuilder.
            (list sass-path input-path output-path))
        p (.start pb)
        out (.getInputStream p)]
    (if (= 0 (.waitFor p))
      [true (slurp out)]
      [false (-> (slurp out)
                 ewen.replique.sourcemap/json-read-str
                 pr-str)])))

(comment
  (compile-sass
   server/sass-bin
   "/home/egr/clojure/wreak-todomvc/test.scss"
   "test.css")
  )
