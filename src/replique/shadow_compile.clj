(ns replique.shadow-compile
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [replique.utils :as utils]
            [shadow.build.api :as bapi]
            [shadow.build.classpath :as cp]
            [shadow.build.npm :as npm]
            [shadow.build.output :as out]
            [shadow.build.data :as data]
            [shadow.build.warnings :as warnings]
            [shadow.cljs.util :as util]
            [shadow.cljs.devtools.config :as shadow-config])
  (:import [java.io File PushbackReader]
           [java.net URL]))

;; ============================================================================
;; Build State Management
;; ============================================================================

(declare shadow-init-compiler-env)

(defonce build-state (utils/delay (shadow-init-compiler-env)))
(defonce classpath-service (atom nil))

(defonce build-id :main)

;; Intented to be set by replique.interactive
;; Merged into the default compiler opts
(defonce custom-compiler-opts (atom nil))

(defn default-compiler-opts []
  (merge {:output-dir (str (File. ^String utils/cljs-compile-path))
          :optimizations :none
          :source-map true
          :cache-analysis false
          :language-in :ecmascript-next
          :language-out :no-transpile
          :closure-defines {'goog.DEBUG true
                            'goog.ENABLE_DEBUG_LOADER false}}
         @custom-compiler-opts))

(defn init-classpath-service!
  "Create and index the classpath service. Must be called once."
  []
  (let [cache-dir (io/file ".shadow-cljs" "cache")
        svc (cp/start cache-dir)]
    (cp/index-classpath svc)
    (reset! classpath-service svc)
    svc))

(defn- init-build-state! [opts]
  (let [cp-svc (or @classpath-service (init-classpath-service!))
        output-dir (io/file (:output-dir opts))
        npm-svc (npm/start {})]
    (-> (bapi/init)
        (bapi/with-classpath cp-svc)
        (bapi/with-npm npm-svc)
        (bapi/with-build-options {:output-dir output-dir
                                  :cache-level :all
                                  :dev-inline-js false
                                  :cljs-runtime-path ""
                                  :target :browser
                                  :build-id build-id})
        (bapi/with-compiler-options (dissoc opts :output-dir))
        ;; CRITICAL: :js-provider :shadow wraps npm modules with
        ;; shadow.js.require() calls that work in the browser.
        ;; The default :require leaves raw require("x") calls
        ;; which only work in Node.js.
        (bapi/with-js-options {:js-provider :shadow
                               :generate-externs true
                               :packages {}
                               :source-map-include-sources-content true
                               :source-map-detail-level :all}))))

;; ============================================================================
;; File Path → Namespace Symbol Resolution
;; ============================================================================

(defn file-path->file
  "Convert a file path string or URL string to a java.io.File.
  Returns nil if conversion fails."
  [file-path]
  (cond
    (instance? File file-path) file-path
    (and (string? file-path) (.startsWith ^String file-path "file:"))
    (try (-> (URL. ^String file-path) (.toURI) (io/file))
         (catch Exception _ nil))
    (string? file-path)
    (try (io/file file-path) (catch Exception _ nil))
    :else nil))

(defn file->ns-sym
  "Look up a File in the classpath service index and return its namespace symbol.
  Returns nil if the file is not found in the index."
  [^File file]
  (when-let [cp-svc @classpath-service]
    (when (and file (.exists file))
      (when-let [rc (cp/find-resource-by-file cp-svc file)]
        (:ns rc)))))

(defn read-ns-from-file
  "Read the first form from a CLJS/CLJC source and extract the namespace symbol.
   Handles file URLs, absolute paths, and classpath-relative paths.
   Returns nil if no ns form is found or the file can't be read."
  [file-path]
  (try
    (let [content (if (and (string? file-path) (.startsWith ^String file-path "file:"))
                    (slurp (URL. ^String file-path))
                    (slurp file-path))]
      (with-open [rdr (PushbackReader. (java.io.StringReader. content))]
        (loop []
          (let [form (try (read rdr) (catch Exception _ nil))]
            (cond
              (nil? form) nil
              (and (list? form) (= 'ns (first form)) (symbol? (second form)))
              (second form)
              :else (recur))))))
    (catch Exception _ nil)))

(defn resolve-ns-from-file
  "Resolve a file path/URL to a namespace symbol, trying three strategies:
  1. Classpath service index (most reliable, uses actual ns form)
  2. Read ns form from file content (for files not yet indexed)
  3. Path-based conversion (last resort, only works for relative paths)"
  [file-path]
  (or (when-let [file (file-path->file file-path)]
        (file->ns-sym file))
      (read-ns-from-file file-path)
      (util/filename->ns (str file-path))))

(defn update-classpath-file! [file-path]
  (when-let [cp-svc @classpath-service]
    (when-let [file (file-path->file file-path)]
      (when (.exists file)
        ;; Quick check: is this file even indexed?
        (when (cp/find-resource-by-file cp-svc file)
          (let [abs-path (.toPath (.getAbsoluteFile file))
                cp-entries (cp/get-classpath-entries cp-svc)
                source-path (some (fn [^File cp-entry]
                                    (let [cp-path (.toPath (.getAbsoluteFile cp-entry))]
                                      (when (.startsWith abs-path cp-path)
                                        cp-entry)))
                                  cp-entries)]
            (when source-path
              (cp/file-update cp-svc source-path file))))))))

;; ============================================================================
;; Core Compilation
;; ============================================================================

(defn print-source-warnings
  "Print warnings for compiled sources with proper source excerpts."
  [state resolved-ids]
  (let [sources (->> resolved-ids
                     (map (fn [src-id]
                            (let [src (data/get-source-by-id state src-id)
                                  output (get-in state [:output src-id])]
                              (assoc src :warnings (:warnings output)))))
                     (remove #(empty? (:warnings %))))]
    (when (seq sources)
      (doseq [{:keys [resource-name from-jar] :as src} sources]
        (doseq [[idx {:keys [line column] :as w}] (map-indexed vector (:warnings src))]
          (let [w (assoc w
                         ::warnings/idx (inc idx)
                         :resource-name resource-name)
                ;; Try to add source excerpt if we have line info
                w (if line
                    (try
                      (let [excerpts (warnings/get-source-excerpts-for-rc
                                      state src [{:line line :column column}])]
                        (if-let [excerpt (first excerpts)]
                          (assoc w :source-excerpt excerpt)
                          w))
                      (catch Exception _ w))
                    w)]
            (if (:source-excerpt w)
              (warnings/print-warning w)
              (warnings/print-short-warning w))))))))

(defn with-repl-module
  "Ensure :build-modules is set up for the REPL.
  Uses the actual modules from shadow-cljs.edn if available,
  falling back to a single :main module.
  This populates :shadow.build/ns->mod so shadow.lazy/loadable works."
  [state]
  (let [raw-modules (when build-id
                      (try
                        (when-let [raw-build (shadow-config/get-build build-id)]
                          (:modules raw-build))
                        (catch Exception _ nil)))

        modules
        (if (seq raw-modules)
          ;; Construct :build-modules from shadow-cljs.edn config
          (let [configured-modules
                (for [[module-id module-config] raw-modules]
                  (let [entries (or (:entries module-config) [])
                        ;; Map entry symbols to source IDs if they are resolved
                        src-ids (keep #(get-in state [:sym->id %]) entries)
                        depends-on (or (:depends-on module-config) #{})]
                    {:module-id module-id
                     :sources src-ids
                     :depends-on depends-on}))

                ;; Find sources that are explicitly assigned to a module
                assigned-ids (into #{} (mapcat :sources) configured-modules)

                ;; All other resolved sources go into a :repl module
                unassigned-sources (->> (keys (:sources state))
                                        (remove assigned-ids))

                ;; The :repl module depends on all configured modules
                repl-module (when (seq unassigned-sources)
                              {:module-id :repl
                               :sources (vec unassigned-sources)
                               :depends-on (into #{} (map :module-id) configured-modules)})]

            (vec (cond-> configured-modules repl-module (conj repl-module))))

          ;; Fallback: no shadow-cljs.edn or no modules configured.
          ;; Put everything in a single :main module.
          [{:module-id :main
            :sources (keys (:sources state))}])]

    (assoc state :build-modules modules)))

(defn sync-resource-to-build-state!
  "Updates the build state's :sources map for the given namespace
   with the latest info from the classpath service.
   This is necessary so bapi/resolve-entries sees updated :deps
   when a file is modified and loaded via load-file."
  [ns-sym]
  (when-let [cp-svc @classpath-service]
    (when-let [rc (cp/find-resource-for-provide cp-svc ns-sym)]
      (swap! @build-state
             #(data/overwrite-source % rc)))))

(defn shadow-compile-cljs
  [input opts]
  (let [ns-sym (cond
                 (symbol? input) input
                 (string? input) (resolve-ns-from-file input)
                 :else (util/filename->ns (str input)))
        state @@build-state]
    (let [[resolved-ids state] (bapi/resolve-entries state [ns-sym])]
      (when (empty? resolved-ids)
        (throw (ex-info (str "Could not resolve namespace: " ns-sym) {:ns ns-sym})))
      (let [state (utils/maybe-locking
                   clojure.lang.RT/REQUIRE_LOCK
                   (-> state
                       (with-repl-module)
                       (bapi/compile-sources resolved-ids)))
            ;; Print warnings with source excerpts
            _ (print-source-warnings state resolved-ids)
            state (out/flush-sources state resolved-ids)]
        (reset! @build-state state)
        (let [rc-id (get-in state [:sym->id ns-sym])
              source-info (get-in state [:sources rc-id])
              output-info (get-in state [:output rc-id])
              output-dir (get-in state [:build-options :output-dir])]
          {:ns ns-sym
           :provides (or (:provides source-info) [ns-sym])
           :requires (:requires source-info)
           :macros-ns (when (:macros-ns source-info) (get source-info :source-ns))
           :source (:resource-name source-info)
           :file (when-let [f (:file source-info)] (.getAbsolutePath ^File f))
           :url (:url source-info)
           :output-file (when-let [on (:output-name source-info)] (io/file output-dir on))
           :js (:js output-info)
           :source-map-compact (:source-map-compact output-info)
           :warnings (:warnings output-info)})))))

(defn init-shadow-loader-js
  "Generates JS to initialize and patch shadow.loader for synchronous REPL usage.
   Should be evaluated AFTER shadow.loader has been loaded."
  [state]
  (when-let [modules (:build-modules state)]
    (let [manifest-json
          (->> modules
               (map (fn [{:keys [module-id sources depends-on]}]
                      (let [output-names (vec (keep #(get-in state [:sources % :output-name]) sources))]
                        [(name module-id)
                         (cond-> {:sources output-names}
                           (seq depends-on)
                           (assoc :dependsOn (mapv name depends-on)))])))
               (into {})
               (json/write-str))]
      (str
       "window.__shadowReplManifest = " manifest-json ";\n"
       
       ;; Track which modules are loaded (avoids depending on shadow.loader internal state)
       "window.__shadowReplLoadedModules = {};\n"
       
       ;; Mark all initial modules as loaded since SHADOW_ENV.load
       ;; already loaded them in shadow_repl_main.js
       "for (var id in window.__shadowReplManifest) {\n"
       "  window.__shadowReplLoadedModules[id] = true;\n"
       "}\n"
       
       ;; Initialize shadow.loader with the manifest
       "shadow.loader.init(window.__shadowReplManifest);\n"
       
       ;; Override load_multiple for synchronous REPL loading
       "shadow.loader.load_multiple = function(ids, opt) {\n"
       "  var result = {};\n"
       "  for (var i = 0; i < ids.length; i++) {\n"
       "    var id = ids[i];\n"
       "    var mod = window.__shadowReplManifest[id];\n"
       "    if (mod && mod.sources && !window.__shadowReplLoadedModules[id]) {\n"
       "      SHADOW_ENV.load({}, mod.sources);\n"
       "    }\n"
       "    window.__shadowReplLoadedModules[id] = true;\n"
       "    result[id] = goog.async.Deferred.succeed();\n"
       "  }\n"
       "  return result;\n"
       "};\n"
       
       ;; Override load to use load_multiple
       "shadow.loader.load = function(id, opt) {\n"
       "  return shadow.loader.load_multiple([id], opt)[id];\n"
       "};\n"
       
       ;; Override loaded? to use our tracking instead of internal module state
       "shadow.loader.loaded_QMARK_ = function(id) {\n"
       "  return !!window.__shadowReplLoadedModules[id];\n"
       "};\n"))))

(defn generate-main-module-js [state]
  (let [goog-base-js (get-in state [:output shadow.build.output/goog-base-id :js])

        goog-source-ids (->> (:build-sources state)
                             (filter #(and (get-in state [:sources % :goog-src])
                                           (not= % shadow.build.output/goog-base-id))))

        goog-inline-js
        (->> goog-source-ids
             (map (fn [src-id]
                    (let [output (get-in state [:output src-id])]
                      (str (:js output) "\n"))))
             (clojure.string/join "\n"))

        inlined-source-ids (cons out/goog-base-id goog-source-ids)

        set-loaded-js
        (->> inlined-source-ids
             (keep (fn [src-id]
                     (when-let [output-name (get-in state [:sources src-id :output-name])]
                       (str "SHADOW_ENV.setLoaded('" output-name "');\n"))))
             (clojure.string/join ""))

        cljs-source-ids (->> (:build-sources state)
                             (remove #(= shadow.build.output/goog-base-id %))
                             (remove #(get-in state [:sources % :goog-src])))
        cljs-output-names (vec (keep #(get-in state [:sources % :output-name]) cljs-source-ids))

        runtime-js (slurp (io/resource "shadow_browser_runtime.js"))
        defines-json (out/closure-defines-json state)

        ;; Initialize and patch shadow.loader
        loader-patch (or (init-shadow-loader-js state) "")]

    (str
     ;; Dynamic base path
     "var SHADOW_REPL_BASE_PATH = (function() {\n"
     "  if (typeof document !== 'undefined' && document.currentScript) {\n"
     "    var src = document.currentScript.src;\n"
     "    var idx = src.lastIndexOf('/');\n"
     "    if (idx > 0) return src.substring(0, idx + 1);\n"
     "  }\n"
     "  return '/';\n"
     "})();\n\n"

     ;; CLOSURE defines
     "var CLOSURE_NO_DEPS = true;\n"
     "var CLOSURE_BASE_PATH = '/';\n"
     "var CLOSURE_DEFINES = " defines-json ";\n\n"

     ;; shadow$provide
     "var shadow$provide = {};\n\n"

     ;; goog/base.js
     goog-base-js "\n\n"

     ;; Inlined goog modules
     goog-inline-js "\n\n"

     ;; SHADOW_ENV runtime
     runtime-js "\n\n"

     ;; Mark inlined sources as loaded
     set-loaded-js "\n\n"

     ;; Load initial CLJS modules (including shadow.js and shadow.loader)
     "SHADOW_ENV.load({}, [" (clojure.string/join "," (map pr-str cljs-output-names)) "]);\n\n"

     ;; Initialize and patch shadow.loader for synchronous REPL usage
     loader-patch "\n")))


;; ============================================================================
;; Full Init - Equivalent to init-compiler-env
;; ============================================================================

(defn shadow-init-compiler-env
  "Initialize shadow-cljs compiler env and pre-compile core namespaces."
  []
  (let [opts (default-compiler-opts)
        state (init-build-state! opts)

        ;; Register cljs.user as a synthetic source so get-source-by-provide works
        rn (str (util/ns->path 'cljs.user) ".cljs")
        rc {:resource-id   [:shadow.build.classpath/resource rn]
            :resource-name rn
            :output-name   (util/flat-js-name rn)
            :type          :cljs
            :ns            'cljs.user
            :provides      #{'cljs.user}
            :requires      #{'cljs.core}
            :deps          ['cljs.core]
            :cache-key     []
            :last-modified 0}
        cljs-core-id (get-in state [:sym->id 'cljs.core])
        state (-> state
                  (data/add-source rc)
                  (cond-> cljs-core-id
                    (update :immediate-deps assoc (:resource-id rc) #{cljs-core-id})))

        ;; Resolve and compile all initial namespaces together
        ;; bapi/resolve-entries returns them in dependency order
        namespaces '[shadow.js
                     shadow.loader
                     cljs.core
                     replique.cljs-env.shadow-repl
                     replique.cljs-env.browser
                     replique.cljs-env.watch]
        [ids state] (bapi/resolve-entries state namespaces)
        state (with-repl-module state)
        state (bapi/compile-sources state ids)]

    ;; Flush compiled .js files to disk (goog modules + CLJS modules)
    (out/flush-sources state ids)

    (let [main-js (generate-main-module-js state)
          main-file (io/file (:output-dir opts) "shadow_repl_main.js")]
      (io/make-parents main-file)
      (spit main-file main-js))

    (spit (io/file (:output-dir opts) ".repliqueignore") "")
    (atom state)))

;; ============================================================================
;; Get Compiler Env (for use with other cljs.repl functions)
;; ============================================================================

(defonce compiler-env-atom
  (utils/delay
   (atom (:compiler-env @@build-state))))

(defn get-compiler-env
  "Get the compiler-env as an atom (required by cljs.env/*compiler*)."
  []
  @compiler-env-atom)

(defn sync-compiler-env!
  "Sync the compiler-env atom with the build state's compiler-env."
  []
  (when-let [state @@build-state]
    (reset! @compiler-env-atom (:compiler-env state))))




