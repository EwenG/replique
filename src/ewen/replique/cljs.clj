(ns ewen.replique.cljs
  (:require [cljs.closure :as cljsc]
            [cljs.compiler :as comp]
            [clojure.java.io :as io]
            [cljs.util :as util]
            [clojure.data.json :as json]
            [cljs.repl.server :as server]
            [cljs.repl.browser :as brepl]
            [cljs.analyzer :as ana])
  (:import [java.io File]))

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


;;Patch cljs.closure/output-main-file in order to:
;; - Avoid the need to provide an :asset-path option. :asset-path is
;; computed from the :main namespaces. When using a node.js env,
;; :output-dir is used instead of :asset-path
;; - Allow multiple :main namespaces. This permits leaving HTML markup
;; identical between dev and production even when multiple namespaces
;; are called at startup
#_(alter-var-root
 #'cljsc/output-main-file
 (constantly
  (fn output-main-file [opts]
    (let [closure-defines (json/write-str (:closure-defines opts))]
      (case (:target opts)
        :nodejs
        (let [asset-path (or (:asset-path opts)
                              (util/output-directory opts))]
          (cljsc/output-one-file
           opts
           (cljsc/add-header
            opts
            (str
             "var path = require(\"path\");\n"
             "try {\n"
             "    require(\"source-map-support\").install();\n"
             "} catch(err) {\n"
             "}\n"
             "require(path.join(path.resolve(\".\"),\"" asset-path "\",\"goog\",\"bootstrap\",\"nodejs.js\"));\n"
             "require(path.join(path.resolve(\".\"),\"" asset-path "\",\"cljs_deps.js\"));\n"
             "goog.global.CLOSURE_UNCOMPILED_DEFINES = " closure-defines ";\n"
             (when (:main opts)
               (str "goog.require(\"" (comp/munge (:main opts)) "\");\n"))
             "goog.require(\"cljs.nodejscli\");\n"))))
        (let [output-dir-uri (-> (:output-dir opts) (File.) (.toURI))
              output-to-uri (-> (:output-to opts) (File.) (.toURI))
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
                (when (:main opts)
                  (str "document.write('<script>if (typeof goog != \"undefined\") { goog.require(\"" (comp/munge (:main opts)) "\"); } else { console.warn(\"ClojureScript could not load :main, did you forget to specify :asset-path?\"); };</script>');"))
                  "})();\n"))))))))

;; Patch output-unoptimzed to always output cljs-deps into cljs_deps.js
;; If output-to is defined and main is not defined, a main file is written
;; which does not goog.require any namespace
#_(alter-var-root
 #'cljsc/output-unoptimized
 (constantly
  (fn output-unoptimized
    [opts & sources]
    (let [disk-sources (remove #(= (:group %) :goog)
                               (map #(cljsc/source-on-disk opts %) sources))
          goog-deps    (io/file (util/output-directory opts)
                                "goog" "deps.js")
          main         (:main opts)]
      (util/mkdirs goog-deps)
      (spit goog-deps (slurp (io/resource "goog/deps.js")))
      (cljsc/output-deps-file
       (assoc opts :output-to
              (str (util/output-directory opts)
                   File/separator "cljs_deps.js"))
       disk-sources)
      (cljsc/output-main-file opts)))))



;; Patch cljs.repl.server and cljs.repl.browser in order to support
;; CORS requests instead of crossPageChannel

(alter-var-root
 #'server/send-and-close
 (constantly
  (fn send-and-close
    ([conn status form]
     (send-and-close conn status form "text/html"))
    ([conn status form content-type]
     (send-and-close conn status form content-type "UTF-8"))
    ([conn status form content-type encoding]
     (let [byte-form (.getBytes form encoding)
           content-length (count byte-form)
           headers (map #(.getBytes (str % "\r\n"))
                        [(#'server/status-line status)
                         "Server: ClojureScript REPL"
                         (str "Content-Type: "
                              content-type
                              "; charset=" encoding)
                         (str "Content-Length: " content-length)
                         (str "Access-Control-Allow-Origin: *")
                         (str "Access-Control-Allow-Methods: GET,POST")
                         ""])]
       (with-open [os (.getOutputStream conn)]
         (doseq [header headers]
           (.write os header 0 (count header)))
         (.write os byte-form 0 content-length)
         (.flush os)
         (.close conn)))))))

(swap! server/handlers assoc :get [])

(server/dispatch-on
 :get
 (fn [{:keys [path]} _ _]
   (some #(.endsWith path %) (keys brepl/ext->mime-type)))
 brepl/send-static)

(defn normalize-ip-address [address]
  (cond (= "0.0.0.0" address) "127.0.0.1"
        (= "0:0:0:0:0:0:0:1" address) "127.0.0.1"
        :else address))

(server/dispatch-on
 :get
 (fn [{:keys [path]} _ _]
   (= path "/"))
 (fn [request conn opts]
   (let [url (format "http://%s" (:host (:headers request)))]
     (server/send-and-close
      conn 200
      (str "<html>
<head></head>
<body>
<script>var CLOSURE_UNCOMPILED_DEFINES = null;</script>
<script src=\"goog/base.js\"></script>
<script src=\"cljs_deps.js\"></script>
<script>
goog.require(\"ewen.replique.cljs_env.repl\");
</script>
<script>
goog.require(\"ewen.replique.cljs_env.browser\");
</script>
<script>
ewen.replique.cljs_env.repl.connect(\"" url "\");
</script>
</body>
</html>")
      "text/html"))))

(defmethod brepl/handle-post :ready [_ conn _]
  (send-via brepl/es brepl/ordering (fn [_] {:expecting nil :fns {}}))
  (brepl/send-for-eval
   conn
   (cljsc/-compile
    '[(set! *print-fn* ewen.replique.cljs-env.repl/repl-print)
      (set! *print-err-fn* ewen.replique.cljs-env.repl/repl-print)
      (set! *print-newline* true)
      (when (pos? (count ewen.replique.cljs-env.repl/print-queue))
        (ewen.replique.cljs-env.repl/flush-print-queue!))]
    {})
   identity))

;; Avoid calling ana/analyze two times when evaluating a form at the repl.
;; This is achieved by removing wrap-js.
;; Otherwise, macros get evaled 2 times

(alter-var-root
 #'cljs.repl/evaluate-form
 (constantly
  (fn evaluate-form
    ([repl-env env filename form]
     (evaluate-form repl-env env filename form identity))
    ([repl-env env filename form wrap]
     (evaluate-form repl-env env filename form wrap cljs.repl/*repl-opts*))
    ([repl-env env filename form wrap opts]
     (binding [ana/*cljs-file* filename]
       (let [def-emits-var (:def-emits-var opts)
             ast (ana/analyze (assoc env :repl-env repl-env
                                     :def-emits-var def-emits-var)
                               (wrap form) nil opts)
             js (comp/emit-str ast)]
         (when (= (:op ast) :ns)
           (#'cljs.repl/load-dependencies
            repl-env
            (into (vals (:requires ast))
                  (distinct (vals (:uses ast))))
            opts))
         (when cljs.repl/*cljs-verbose*
           (cljs.repl/err-out (println js)))
         (let [ret (cljs.repl/-evaluate
                    repl-env filename (:line (meta form)) js)]
           (case (:status ret)
             :error (throw
                     (ex-info (:value ret)
                              {:type :js-eval-error
                               :error ret
                               :repl-env repl-env
                               :form form}))
             :exception (throw
                         (ex-info (:value ret)
                                  {:type :js-eval-exception
                                   :error ret
                                   :repl-env repl-env
                                   :form form
                                   :js js}))
             :success (:value ret)))))))))
