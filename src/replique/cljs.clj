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

