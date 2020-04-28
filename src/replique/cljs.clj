(ns replique.cljs
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [cljs.tagged-literals :as tags]
   [cljs.util :as util]
   [cljs.compiler :as comp]
   [cljs.analyzer :as ana]
   [cljs.env :as env]
   [cljs.closure :as cljsc]

   [cljs.repl :refer [repl-caught repl-quit-prompt repl-read repl-prompt known-repl-opts
                      -repl-options read-source-map *cljs-verbose* *repl-opts*
                      default-special-fns -setup evaluate-form analyze-source
                      -tear-down]])
  (:import [java.io File]))

(def err-out @#'cljs.repl/err-out)

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

(defmacro with-version [[min-major min-minor min-qualifier]
                        [max-major max-minor max-qualifier] & body]
  (let [{cljs-major :major
         cljs-minor :minor
         cljs-qualifier :qualifier} cljs.util/*clojurescript-version*]
    (when
        (and
         (or (> cljs-major min-major) (> cljs-minor min-minor)
             (and (= cljs-major min-major) (= cljs-minor min-minor)
                  (>= cljs-qualifier min-qualifier)))
         (or (nil? max-major) (nil? max-minor) (nil? max-qualifier)
             (< cljs-major max-major) (< cljs-minor max-minor)
             (and (= cljs-major max-major) (= cljs-minor max-minor)
                  (<= cljs-qualifier max-qualifier))))
      `(do ~@body))))

;; Support for :reload-all
(def ^:dynamic *reload-all* false)

(def compilation-error (if-let [compilation-error-var (resolve 'cljs.util/compilation-error)]
                         @compilation-error-var
                         (fn compilation-error [cause]
                           (ex-info nil {:clojure.error/phase :compilation} cause))))

;; Alternative to cljs.closure/src-file->goog-require in order to support goog.require
;; with :reload-all
(defn ^String src-file->goog-require
  ([src] (src-file->goog-require src {:wrap true}))
  ([src {:keys [wrap all-provides macros-ns] :as options}]
    (let [goog-ns
          (case (util/ext src)
            ("cljs" "cljc") (let [ns-str (str (comp/munge (:ns (ana/parse-ns src))))]
                              (cond-> ns-str
                                (and macros-ns (not (.endsWith ns-str "$macros")))
                                (str "$macros")))
            "js" (cond-> (:provides (cljsc/parse-js-ns src))
                   (not all-provides) first)
            (throw
             (compilation-error
              (IllegalArgumentException.
               (str "Can't create goog.require expression for " src)))))]
      (if (and (not all-provides) wrap)
        (cond
          (:reload-all options) (str "goog.require(\"" goog-ns "\", \"reload-all\");")
          (:reload options) (str "goog.require(\"" goog-ns "\", true);")
          :else (str "goog.require(\"" goog-ns "\");"))
        (if (vector? goog-ns)
          goog-ns
          (str goog-ns))))))

;; Patch parse 'def to allow :const redefinition
;; Remove constants from the environment before parsing 'def expressions

(defonce parse-def-o (get (methods cljs.analyzer/parse) 'def))

(defn maybe-dissoc-const [env form]
  (if-let [sym (second form)]
    (let [maybe-const-var (get-in env [:ns :defs sym])]
      (if (:const maybe-const-var)
        (do
          (swap! cljs.env/*compiler* update-in
                 [:cljs.analyzer/namespaces (-> env :ns :name) :defs] dissoc sym)
          (update-in env [:ns :defs] dissoc sym))
        env))
    env))

(defmethod cljs.analyzer/parse 'def
  [op env form _1 _2]
  (parse-def-o op (maybe-dissoc-const env form) form _1 _2))


;; patch parse 'ns to allow :reload-all
;; parse 'ns* is not patched since (require ... :reload-all) doesn't really work

(defonce parse-ns-o (get (methods cljs.analyzer/parse) 'ns))

(defmethod cljs.analyzer/parse 'ns
  [_1 env [_2 name & args :as form] _3 opts]
  (let [ns-infos (parse-ns-o _1 env form _3 opts)]
    (if *reload-all*
      (-> ns-infos
          (assoc-in [:reload :user-macros] :reload)
          (assoc-in [:reload :require-macros] :reload))
      ns-infos)))

;; patch cljs.compiler/requires-compilation? to only check for modified file when using :reload-all
;; or when the namespace of the file is not in the compiler environment yet

(with-version
  [1 10 238]
  [nil nil nil]
  (defn requires-compilation?
    ([src dest]
     (requires-compilation? src dest
                            (when env/*compiler*
                              (:options @env/*compiler*))))
    ([^File src ^File dest opts]
     (let [{:keys [ns requires]} (ana/parse-ns src)]
       (env/ensure
        (or (not (.exists dest))
            (and *reload-all* (util/changed? src dest))
            (and (nil? (get-in @env/*compiler* [::ana/namespaces ns])) (util/changed? src dest))
            (let [version' (util/compiled-by-version dest)
                  version (util/clojurescript-version)]
              (and version (not= version version')))
            (and opts
                 (not (and (io/resource "cljs/core.aot.js") (= 'cljs.core ns)))
                 (not= (ana/build-affecting-options opts)
                       (ana/build-affecting-options (util/build-options dest))))
            (and opts (:source-map opts)
                 (if (= (:optimizations opts) :none)
                   (not (.exists (io/file (str (.getPath dest) ".map"))))
                   (not (get-in @env/*compiler* [::compiled-cljs (.getAbsolutePath dest)]))))
            (when-let [recompiled' (and comp/*recompiled* @cljs.compiler/*recompiled*)]
              (some requires recompiled'))))))))

(with-version
  [0 0 0]
  [1 9 946]
  (defn requires-compilation?
    ([src dest]
     (requires-compilation? src dest
                            (when env/*compiler*
                              (:options @env/*compiler*))))
    ([^File src ^File dest opts]
     (let [{:keys [ns requires]} (ana/parse-ns src)]
       (if (and (= 'cljs.loader ns) (not (contains? opts :cache-key)))
         false
         (env/ensure
          (or (not (.exists dest))
              (and *reload-all* (util/changed? src dest))
              (and (nil? (get-in @env/*compiler* [::ana/namespaces ns])) (util/changed? src dest))
              (let [version' (util/compiled-by-version dest)
                    version (util/clojurescript-version)]
                (and version (not= version version')))
              (and opts
                   (not (and (io/resource "cljs/core.aot.js") (= 'cljs.core ns)))
                   (not= (#'comp/build-affecting-options opts)
                         (#'comp/build-affecting-options (util/build-options dest))))
              (and opts (:source-map opts)
                   (if (= (:optimizations opts) :none)
                     (not (.exists (io/file (str (.getPath dest) ".map"))))
                     (not (get-in @env/*compiler* [::compiled-cljs (.getAbsolutePath dest)]))))
              (when-let [recompiled' (and comp/*recompiled* @cljs.compiler/*recompiled*)]
                (some requires recompiled')))))))))

;; goog.require seems to call goog.loadModule
;; cljs.closure/add-goog-load wraps the module into a goog.loadModule call
;; The double goog.loadModule calls seems to break the loading mechanism
;; patch cljs.closure/transpile to avoid wrapping the module into a goog.loadModule call

(defonce transpile-var (resolve 'cljs.closure/transpile))
(defonce transpile-o @transpile-var)

(defn transpile
  [opts res js]
  (transpile-o opts res (assoc js :module :none)))

(when transpile-o
  (alter-var-root transpile-var (constantly transpile)))
