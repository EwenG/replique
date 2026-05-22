(ns replique.shadow-repl
  "Shadow-cljs integration for Replique's browser REPL"
  (:refer-clojure :exclude [load-file])
  (:require [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.tools.reader :as reader]
            [cljs.env :as env]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.stacktrace :as st]
            [cljs.source-map :as sm]
            [clojure.stacktrace :as stacktrace]
            [cljs.util]
            [replique.environment :as replique-env]
            [replique.context :as replique-context]
            [replique.completion :as completion]
            [replique.find-usage :as find-usage]
            [replique.meta :as replique-meta]
            [replique.repliquedoc :as repliquedoc]
            [replique.tooling :as tooling]
            [replique.tooling-msg :as tooling-msg]
            [replique.utils :as utils]
            [replique.repl-common :as repl-common]
            [replique.cljs]
            [replique.repl-cljs-common :as repl-cljs-common]
            [replique.source-meta]
            [replique.shadow-compile :as shadow-compile]
            [shadow.build.api :as bapi]
            [shadow.build.output :as out]
            [shadow.build.compiler :as shadow-comp]
            [shadow.build.data :as data]
            [shadow.build.cljs-bridge :as cljs-bridge]
            [shadow.build.npm :as npm]
            [shadow.cljs.util :as shadow-util]
            [shadow.build.warnings :as warnings])
  (:import [java.util.concurrent Executors SynchronousQueue TimeUnit
            RejectedExecutionException ExecutorService TimeoutException CancellationException]
           [java.util.concurrent.atomic AtomicLong]))

;; ============================================================================
;; State
;; ============================================================================

;; shadow.build.compiler/analyze binds ana/*cljs-ns* so we update ana/*cljs-ns* by setting
;; *shadow-repl-ns* and setting ana/*cljs-ns* to the value of *shadow-repl-ns* after analysis
(def ^:dynamic *shadow-repl-ns* nil)

(defn evaluate-form
  "Send JS to browser and wait for result."
  [js & {:keys [timeout-before-submitted]}]
  (let [port (utils/server-port utils/http-server)
        {:keys [state eval-executor]} @repl-cljs-common/cljs-server]
    (cond
      (= :stopped state)
      {:status :error
       :value (format "Waiting for browser to connect on port %d ..." port)}
      :else 
      (try 
        (let [eval-task (repl-cljs-common/->EvalTask js false)
              eval-future (.submit ^ExecutorService eval-executor ^Callable eval-task)
              result (if timeout-before-submitted
                       (try
                         (.get eval-future timeout-before-submitted TimeUnit/MILLISECONDS)
                         (catch TimeoutException e
                           (when-not (repl-cljs-common/is-submitted? eval-task)
                             (.cancel eval-future false))
                           (.get eval-future)))
                       (.get eval-future))]
          (when (:params result)
            (reset! repl-cljs-common/repl-params (:params result)))
          result)
        (catch RejectedExecutionException e
          {:status :error :value "Connection broken"})
        (catch CancellationException e
          {:status :error :value "Cancelled"})))))

(defn shadow-compile-form
  "Compile a CLJS form to JS using shadow-cljs's compilation pipeline.
  Returns {:js ... :new-state ... :resource-name ...}"
  [form {:keys [ns resource-name repl-context?]
         :or {ns ana/*cljs-ns*
              repl-context? true}}]
  (let [state @@shadow-compile/build-state
        rc (data/get-source-by-provide state ns)
        resource-name (or resource-name
                          (some-> rc :resource-name)
                          "<eval>")
        js-result (atom nil)
        new-state
        (cljs-bridge/with-compiler-env
         state
         (shadow-comp/with-warnings
          state
          (binding [comp/*source-map-data*
                    (atom {:source-map (sorted-map)
                           :gen-col 0
                           :gen-line 0})

                    comp/*source-map-data-gen-col*
                    (AtomicLong.)

                    ana/*unchecked-if* ana/*unchecked-if*
                    ana/*unchecked-arrays* ana/*unchecked-arrays*
                    ana/*cljs-warnings*
                    (merge ana/*cljs-warnings*
                           (get-in state [:compiler-options :warnings]))
                    *shadow-repl-ns* ns]

            (let [ast (shadow-comp/analyze
                       state
                       {:ns ns
                        :resource-name resource-name
                        :source (str form)
                        :cljc true
                        :reader-features (data/get-reader-features state)
                        :js ""}
                       form
                       repl-context?)
                  js (with-out-str (shadow-comp/shadow-emit state ast))]
              (reset! js-result js)
              (set! ana/*cljs-ns* *shadow-repl-ns*)
              state))))]
    {:js @js-result
     :new-state new-state
     :resource-name resource-name
     :state-before state}))

;; ============================================================================
;; File Loading
;; ============================================================================

(defn make-load-js [state ns-sym]
  (let [rc-id (get-in state [:sym->id ns-sym])
        target-output-name (when rc-id (get-in state [:sources rc-id :output-name]))]
    (when target-output-name
      (let [[all-ids _] (bapi/resolve-entries state [ns-sym])

            dep-output-names
            (vec (keep (fn [src-id]
                         (when (and (not= src-id rc-id)
                                    ;; Don't load goog/base.js again, it's already inlined
                                    ;; in shadow_repl_main.js
                                    (not= src-id shadow.build.output/goog-base-id))
                           (get-in state [:sources src-id :output-name])))
                       all-ids))

            npm-js
            (->> all-ids
                 (filter #(= :shadow-js (get-in state [:sources % :type])))
                 (map #(npm/shadow-js-require (get-in state [:sources %])))
                 (remove nil?)
                 (string/join "\n"))

            reload-preamble
            (str "goog.implicitNamespaces_['" (comp/munge ns-sym) "'] = true;\n")]

        (str
         (when (seq dep-output-names)
           (str "SHADOW_ENV.load({}, ["
                (string/join "," (map pr-str dep-output-names))
                "]);\n"))
         (when (seq npm-js)
           (str npm-js "\n"))
         "try {\n"
         reload-preamble
         "SHADOW_ENV.reload([" (pr-str target-output-name) "]);\n"
         "} catch (e) {\n"
         "  try { cljs.core._STAR_e_STAR_ = e; } catch(_) {}\n"
         ;; Removed: console.error(e) — evaluate-javascript already logs to console
         "  throw e;\n"
         "}\n")))))

(defn- make-session-ready-js [session process-id]
  (let [{:keys [js]} (binding [ana/*cljs-ns* 'cljs.user]
                       (shadow-compile-form
                         `(do
                            (~'ns ~'cljs.user)
                            (swap! replique.cljs-env.repl/connection assoc :session ~session)
                            ~'(set! *print-fn* replique.cljs-env.repl/repl-print)
                            ~'(set! *print-err-fn* replique.cljs-env.repl/repl-print)
                            ~'(set! *print-newline* true)
                            ~'(when (pos? (count replique.cljs-env.repl/print-queue))
                                (replique.cljs-env.repl/flush-print-queue!))
                            (~'set! ~'replique.cljs-env.repl/*process-id* ~process-id)
                            ~(repl-cljs-common/init-core-bindings)
                            (replique.cljs-env.watch/init))
                         {:ns 'cljs.user}))
        preamble "goog.implicitNamespaces_['cljs.user'] = true;\n"]
    (str preamble js)))

;; ============================================================================
;; Form Wrapping (for *1, *2, *3, *e, error handling, results watcher)
;; ============================================================================

(defn- wrap-form
  "Wrap a form for REPL evaluation."
  [form]
  (cond
    (and (seq? form)
         (#{'ns 'require 'require-macros
            'use 'use-macros 'import 'refer-clojure} (first form)))
    form

    ('#{*1 *2 *3 *e} form) `(let [x# ~form]
                              (if (cljs.core/instance? js/Error x#)
                                x#
                                (cljs.core/pr-str x#)))
    :else
    `(try
       (let [ret# ~form]
         (set! *3 *2)
         (set! *2 *1)
         (set! *1 ret#)
         (if (cljs.core/instance? js/Error ret#)
           ret#
           (do
             (reset! replique.cljs-env.watch/results ret#)
             (cljs.core/pr-str ret#))))
       (catch :default e#
         (set! *e e#)
         (throw e#)))))

(defn call-post-eval-hooks [prev-comp-env comp-env]
  (let [cljs-env-hooks @utils/cljs-env-hooks]
    (when (seq cljs-env-hooks)
      (let [updated-ns? (partial repl-cljs-common/updated-ns? prev-comp-env comp-env)]
        (loop [namespaces (:cljs.analyzer/namespaces comp-env)
               hooks-keys (keys cljs-env-hooks)]
          (when-let [[k v] (first namespaces)]
            

            (if-let [match-hook-keys (filter
                                      #(and (.startsWith (name k) (name %)) (updated-ns? k))
                                      hooks-keys)]
              (let [rest-hooks-keys (keys (apply dissoc cljs-env-hooks match-hook-keys))]
                (doseq [hook-key match-hook-keys]
                  ((get cljs-env-hooks hook-key) nil prev-comp-env comp-env))
                (when (seq rest-hooks-keys)
                  (recur (rest namespaces) rest-hooks-keys)))
              (recur (rest namespaces) hooks-keys))))))))

;; ============================================================================
;; Compilation and Evaluation
;; ============================================================================

(defn- extract-meaningful-ns-state [compiler-env]
  "Extracts only the parts of the compiler-env that indicate meaningful
   state changes like new defs, requires, uses, or imports."
  (into {}
    (map (fn [[ns-sym ns-map]]
           [ns-sym (select-keys ns-map [:defs :requires :uses :imports])]))
    (:cljs.analyzer/namespaces compiler-env)))

(defn compile-form-to-js [form]
  (let [current-ns ana/*cljs-ns*
        {:keys [js new-state resource-name state-before]}
        (shadow-compile-form form {:ns current-ns})]

    ;; Print REPL warnings
    (when (seq (:warnings new-state))
      (doseq [[idx w] (map-indexed vector (:warnings new-state))]
        (warnings/print-short-warning
         (-> w
             (assoc ::warnings/idx (inc idx)
                    :resource-name resource-name)))))

    ;; Concurrent state modification check
    (let [latest-state @@shadow-compile/build-state
          state-changed-by-macro? (not (identical? state-before latest-state))]
      (if state-changed-by-macro?
        (let [old-ns-state (extract-meaningful-ns-state (:compiler-env state-before))
              new-ns-state (extract-meaningful-ns-state (:compiler-env new-state))
              form-updated-ns-state? (not= old-ns-state new-ns-state)]
          (if form-updated-ns-state?
            (throw (ex-info
                     "Concurrent state modification during REPL evaluation. "
                     "A macro modified the build state, but the form also introduced changes (e.g., def, require). "
                     "Please evaluate them separately."
                     {:current-ns current-ns}))
            (do (reset! @shadow-compile/build-state latest-state)
                (shadow-compile/sync-compiler-env!))))
        (do (reset! @shadow-compile/build-state (dissoc new-state :warnings))
            (shadow-compile/sync-compiler-env!))))

    js))

(defn process-result
  [result]
  (cond
    (not (map? result))
    result

    (= :exception (:status result))
    (throw (ex-info (or (:value result) "Evaluation error")
                    {:type :browser-exception
                     :stacktrace (:stacktrace result)
                     :ua-product (:ua-product result)}))

    (= :success (:status result))
    (:value result)

    (= :error (:status result))
    (throw (ex-info (or (:value result) "Connection error")
                    {:type :browser-exception
                     :stacktrace (:stacktrace result)
                     :ua-product (:ua-product result)}))

    :else result))

(defn eval-cljs
  "Evaluate a CLJS form in the browser."
  [form]
  (binding [cljs-bridge/*in-compiler-env* false]
    (let [comp-env @(shadow-compile/get-compiler-env)
          result
          (let [wrapped (wrap-form form)
                js (compile-form-to-js wrapped)]
            (evaluate-form js))
          result (process-result result)]
    
      (replique.repl/print-repl-meta)
      (call-post-eval-hooks comp-env @(shadow-compile/get-compiler-env))
      result)))

(defn eval-cljs-form
  "Evaluate a CLJS form with optional ns and warnings bindings."
  ([form] (eval-cljs-form form nil))
  ([form {:keys [ns warnings]
          :or {ns ana/*cljs-ns*
               warnings ana/*cljs-warnings*}}]
   (binding [ana/*cljs-ns* ns
             ana/*cljs-warnings* warnings]
     (eval-cljs form))))

(defn tooling-form->js [ns form]
  (binding [ana/*analyze-deps* false]
    (:js (shadow-compile-form form {:ns ns :resource-name "<tooling>"}))))

;; ============================================================================
;; File Loading
;; ============================================================================

(defn update-loader-manifest-js
  "Updates window.__shadowReplManifest with the latest module info.
   Called after new modules are resolved in the REPL."
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
      (str "window.__shadowReplManifest = " manifest-json ";\n"))))

(defn load-file [file-path]
  (let [opts (shadow-compile/default-compiler-opts)
        ns-sym (or (shadow-compile/resolve-ns-from-file file-path)
                   (shadow-util/filename->ns file-path))
        _ (shadow-compile/update-classpath-file! file-path)
        _ (shadow-compile/sync-resource-to-build-state! ns-sym)
        _ (swap! @shadow-compile/build-state
                 (fn [state]
                   (if-let [rc-id (get-in state [:sym->id ns-sym])]
                     (data/remove-source-by-id state rc-id)
                     state)))
        ns-info (binding [cljs-bridge/*in-compiler-env* false]
                  (shadow-compile/shadow-compile-cljs ns-sym opts))
        
        ;; Sync the compiler-env-atom so tooling sees the update
        _ (shadow-compile/sync-compiler-env!)
        
        state @@shadow-compile/build-state
        load-js (make-load-js state ns-sym)
        manifest-js (or (update-loader-manifest-js state) "")]
    (when load-js
      (let [result (evaluate-form (str load-js manifest-js))]
        (process-result result)))))

(defn ensure-compiled [namespace]
  (when-not (get-in @(shadow-compile/get-compiler-env) [::ana/namespaces namespace])
    (let [state @@shadow-compile/build-state
          [ids resolved-state] (bapi/resolve-entries state [namespace])]
      (when (seq ids)
        (let [compiled (utils/maybe-locking
                        clojure.lang.RT/REQUIRE_LOCK
                        (-> resolved-state
                            (shadow-compile/with-repl-module)
                            (bapi/compile-sources ids)))]
          (reset! @shadow-compile/build-state compiled)
          (shadow-compile/sync-compiler-env!)
          (doseq [src-id ids]
            (when (get-in compiled [:output src-id])
              (out/flush-source compiled src-id))))))))

;; ============================================================================
;; REPL Loop
;; ============================================================================

(defn read-eval-print [request-exit]
  (let [input (repl-cljs-common/repl-read request-exit)]
    (if (or (= request-exit input)
            (= repl-common/request-prompt input))
      input
      (let [value (eval-cljs input)]
        (try
          (println value)
          (catch Throwable e
            (throw (ex-info nil {:clojure.error/phase :print-eval-result} e))))))))

(defn find-browser-exception
  "Walk the exception chain to find a :browser-exception cause."
  [e]
  (loop [ex e]
    (when ex
      (if (= :browser-exception (:type (ex-data ex)))
        ex
        (recur (ex-cause ex))))))

(defn find-output-for-js-url
  "Given a build state and a JS URL/filename, find the matching source."
  [state url]
  (when (string? url)
    ;; Extract filename from URL (handles /path/to/file.js, file.js, etc.)
    (let [filename (-> url (string/split #"\?") first (string/split #"/") last)]
      (when (and filename (not= "" filename) (not= "<anonymous>" filename))
        (some (fn [[src-id src]]
                (when (= filename (:output-name src))
                  [src-id src]))
              (:sources state))))))

(defn decode-source-map
  "Decode compact source map into {gline {gcol [{:line :col :source}]}}
   as returned by sm/decode. All keys are 0-based."
  [source-map-compact resource-name]
  (when source-map-compact
    (try
      (let [mappings (get source-map-compact "mappings")
            names    (get source-map-compact "names")]
        (when mappings
          ;; 2-arity: pass mappings separately, provide keyword-keyed context
          (sm/decode mappings {:sources [resource-name] :names (vec names)})))
      (catch Exception _ nil))))

(defn map-stacktrace-frame
  "Source-map a single JS stacktrace frame to its CLJS location."
  [state frame]
  (let [{:keys [file line column]} frame]
    (if-let [[src-id src] (find-output-for-js-url state (str file))]
      (if-let [decoded (decode-source-map
                        (get-in state [:output src-id :source-map-compact])
                        (:resource-name src))]
        ;; st/parse-stacktrace: 1-based :line, 0-based :column (Chrome)
        ;; sm/decode: 0-based gline/gcol
        (let [gline (dec line)
              gcol  column
              line-map (get decoded gline)]
          (if line-map
            ;; Source maps only have entries at expression boundaries.
            ;; Find entry with largest gcol <= our gcol.
            (let [best-col (->> (sort (keys line-map))
                                (filter #(<= % gcol))
                                last)]
              (if-let [mapping (first (get-in decoded [gline best-col]))]
                (assoc frame
                       :file (:source mapping)
                       :line (inc (:line mapping))
                       :column (inc (:col mapping))
                       :mapped true)
                (assoc frame :cljs-resource (:resource-name src) :mapped false)))
            (assoc frame :cljs-resource (:resource-name src) :mapped false)))
        (assoc frame :cljs-resource (:resource-name src) :mapped false))
      frame)))

(defn clean-function-name [name]
  (when (and name (not= "" name))
    (let [name (if (.startsWith ^String name "Object.") (subs name 7) name)
          name (if-let [m (re-matches #"(.+?)\s+\[as\s+.+\]" name)] (nth m 1) name)]
      (when (not= "" name) name))))

(defn relevant-frame? [{:keys [file function]}]
  (and (some? file)
       (not= "" (str file))
       (not= "<anonymous>" (str file))
       (not (re-find #"shadow_browser_runtime" (str file)))
       (not (re-find #"shadow_repl_main\.js" (str file)))
       (let [fn-str (str function)]
         (not (or (re-find #"evaluate_javascript" fn-str)
                  (re-find #"process_eval" fn-str)
                  (re-find #"loadSync" fn-str)
                  (re-find #"SHADOW_ENV" fn-str))))))

(defn parse-and-map-stacktrace
  [state stacktrace-str ua-product]
  (try
    (let [frames (st/parse-stacktrace
                  nil
                  stacktrace-str
                  {:ua-product (or ua-product :chrome)}
                  {})]
      (when (seq frames)
        (mapv (partial map-stacktrace-frame state) frames)))
    (catch Exception _
      nil)))

(defn format-cljs-stacktrace [frames]
  (->> frames
       (filter relevant-frame?)
       (map (fn [{:keys [function file line column mapped cljs-resource]}]
              (let [display-file (cond mapped file
                                       cljs-resource cljs-resource
                                       :else file)
                    fn-name (or (clean-function-name function) "<anonymous>")]
                (if (and mapped line)
                  (format "  %s (%s:%d:%d)" fn-name display-file line (or column 0))
                  (format "  %s (%s)" fn-name display-file)))))
       (string/join "\n")))

(defn repl-caught [e opts]
  (if-let [browser-ex (find-browser-exception e)]
    (let [data (ex-data browser-ex)
          state @@shadow-compile/build-state
          mapped-frames (parse-and-map-stacktrace
                         state
                         (:stacktrace data)
                         (:ua-product data))
          first-mapped (some #(when (:mapped %) %) mapped-frames)
          formatted (format-cljs-stacktrace (or mapped-frames []))]
      (binding [*out* *err*]
        (cond
          ;; Source-mapped frames available
          first-mapped
          (do (println (format "Error in %s:%d:%d"
                               (:file first-mapped)
                               (:line first-mapped)
                               (or (:column first-mapped) 0)))
              (println (.getMessage browser-ex))
              (println)
              (println formatted))

          ;; No source mapping — print raw stacktrace
          ;; (which already includes the message as its first line)
          (:stacktrace data)
          (println (:stacktrace data))

          ;; No stacktrace at all
          :else
          (println (.getMessage browser-ex)))))

    ;; JVM/compilation error
    (let [data (ex-data e)]
      (binding [*out* *err*]
        (cond
          (:clojure.error/phase data)
          (let [;; CLJS analyzer wraps macro expansion errors with nil message.
                ;; The actual error is in the cause.
                msg (or (.getMessage e)
                        (some-> (ex-cause e) (.getMessage))
                        "Unknown compilation error")]
            (println (str "Compilation error: " msg))
            (when-let [phase (:clojure.error/phase data)]
              (println (str "  Phase: " phase)))
            ;; Print source location if available
            (when-let [source (:clojure.error/source data)]
              (let [loc (str source
                             (when-let [line (:clojure.error/line data)]
                               (str ":" line))
                             (when-let [col (:clojure.error/column data)]
                               (str ":" col)))]
                (println (str "  At: " loc)))))

          data
          (println (str "Error: " (.getMessage e)))

          :else
          (do (println (str "Error: " (.getMessage e)))
              (when (instance? Throwable e)
                (stacktrace/print-cause-trace e)))))))
  (replique.repl/print-repl-meta))

(defn cljs-repl-vars-bindings []
  `{~@(when-let [v (resolve 'ana/*unchecked-arrays*)]
        [v @v]) ~@[]
    ~@(when-let [v (resolve 'ana/*checked-arrays*)]
        [v @v]) ~@[]})

(comment
  (cljs-repl 'replique.cljs-env.tt)
  )

(defn cljs-repl [main-namespace]
  (let [{:keys [state]} @repl-cljs-common/cljs-server
        comp-env (shadow-compile/get-compiler-env)]
    (when main-namespace (ensure-compiled main-namespace))
    (when (not= :started state)
      (println (format "Waiting for browser to connect on port %d ..."
                       (utils/server-port utils/http-server))))
    (swap! repl-cljs-common/cljs-outs conj *out*)
    (try
      (with-bindings
       (cljs-repl-vars-bindings)
       (binding [utils/*repl-env* :replique/shadow-browser
                 utils/*main-ns* main-namespace
                 replique.repl/*repl-context* nil

                 cljs.env/*compiler* comp-env  ;; ATOM, not plain map
                 ana/*unchecked-if* ana/*unchecked-if*
                 ana/*cljs-ns* ana/*cljs-ns*
                 ana/*cljs-warnings* ana/*cljs-warnings*
                 ana/*cljs-static-fns* ana/*cljs-static-fns*
                 ana/*fn-invoke-direct* ana/*fn-invoke-direct*]

         ;; Ensure cljs.core has been analyzed
         (when-not (get-in @comp-env [::ana/namespaces 'cljs.core :defs])
           (let [opts (shadow-compile/default-compiler-opts)]
             (shadow-compile/shadow-compile-cljs "cljs/core.cljs" opts))
           (shadow-compile/sync-compiler-env!))

         (replique.repl/print-repl-meta)
         (repl-common/repl-quit-prompt :cljs/quit)
         (repl-common/repl-prompt ana/*cljs-ns*)
         (flush)

         (let [request-exit :cljs/quit]
           (loop []
             (when-not
              (try
                (identical? (read-eval-print request-exit) request-exit)
                (catch Throwable e
                  (repl-caught e nil)
                  nil))
               (when (repl-common/repl-need-prompt)
                 (repl-common/repl-prompt ana/*cljs-ns*)
                 (flush))
               (recur))))))
      (finally (swap! repl-cljs-common/cljs-outs disj *out*)))))

(defn in-ns* [ns-name]
  (when-not (ana/get-namespace ns-name)
    ;; Register in compiler env (for immediate analyzer use)
    (swap! (shadow-compile/get-compiler-env)
           assoc-in [::ana/namespaces ns-name]
           {:name ns-name})
    ;; Register in build state's compiler-env
    (swap! @shadow-compile/build-state
           update-in [:compiler-env ::ana/namespaces]
           assoc ns-name {:name ns-name})

    ;; Register in :sources and lookup indexes ONLY if not already present
    (let [resource-name (str (shadow-util/ns->path ns-name) ".cljs")
          rc-id [:shadow.build.classpath/resource resource-name]
          rc {:resource-id   rc-id
              :resource-name resource-name
              :output-name   (shadow-util/flat-js-name resource-name)
              :type          :cljs
              :provides      #{ns-name}
              :requires      #{'cljs.core}
              :deps          ['cljs.core]
              :cache-key     []
              :last-modified 0}]
      (swap! @shadow-compile/build-state
             (fn [state]
               (if (get-in state [:sym->id ns-name])
                 state
                 (let [cljs-core-id (get-in state [:sym->id 'cljs.core])]
                   (-> state
                       (data/add-source rc)
                       (cond-> cljs-core-id
                         (update :immediate-deps assoc rc-id #{cljs-core-id}))
                       ;; Register in ns->mod so shadow.lazy/loadable works
                       (assoc-in [:compiler-env :shadow.build/ns->mod ns-name] (name shadow-compile/build-id))
                       (assoc-in [:compiler-env :shadow.lazy/ns->mod ns-name] (name shadow-compile/build-id)))))))))
  (set! *shadow-repl-ns* ns-name))

;; ============================================================================
;; HTTP Dispatch
;; ============================================================================

(defn dispatch-request-init [{{host :host} :headers} callback]
  (let [url (format "http://%s" host)]
    {:status 200
     :content-type "text/html"
     :body (str "<html>
<head></head>
<body>
<script src=\"/shadow_repl_main.js\"></script>
<script>
replique.cljs_env.repl.connect('" url "');
</script>
</body>
</html>")}))

(defn dispatch-request-session-ready [request callback]
  (let [{:keys [session content]} (:content request)
        main-ns (:main-ns content)
        _ (swap! repl-cljs-common/cljs-server assoc :state :stopped)
        {:keys [result-executor eval-executor js-queue result-queue session]
         :or {session 0}} @repl-cljs-common/cljs-server
        new-eval-executor (Executors/newSingleThreadExecutor)
        new-result-executor (Executors/newSingleThreadExecutor)
        new-js-queue (SynchronousQueue.)
        new-result-queue (SynchronousQueue.)]
    (when eval-executor (repl-cljs-common/shutdown-eval-executor eval-executor))
    (when result-executor (.shutdownNow ^ExecutorService result-executor))

    ;; Ensure the main namespace is compiled
    (when main-ns
      (ensure-compiled (symbol main-ns)))
    
    (let [state @@shadow-compile/build-state
          ;; Generate JS to load the main namespace and all its dependencies
          main-ns-load-js
          (when main-ns
            (make-load-js state (symbol main-ns)))
          init-js (make-session-ready-js (inc session) tooling-msg/process-id)
          combined-js (str main-ns-load-js init-js)]
      (.submit ^ExecutorService new-eval-executor
               (reify Callable
                 (call [this]
                   (.take ^SynchronousQueue new-result-queue))))
      (swap! repl-cljs-common/cljs-server assoc
             :eval-executor new-eval-executor
             :result-executor new-result-executor
             :js-queue new-js-queue
             :result-queue new-result-queue
             :session (inc session)
             :state :started)
      {:status 200 :body combined-js :content-type "text/javascript"})))

(defmethod replique.repl/dispatch-request :default
  [{:keys [method path content] :as request} callback]
  (cond (and (= :get method) (= path "/"))
        (dispatch-request-init request callback)
        (and (= :post method) (= :ready (:type content)))
        (dispatch-request-session-ready request callback)
        (and (= :post method)
             (not= :ready (:type content))
             (not= (:session content) (:session @repl-cljs-common/cljs-server)))
        (repl-cljs-common/dispatch-request-session-expired request callback)
        (and (= :post method) (= :result (:type content)))
        (repl-cljs-common/dispatch-request-result request callback)
        (and (= :post method) (= :print (:type content)))
        (repl-cljs-common/dispatch-request-print request callback)
        (and (= :post method) (= :print-tooling (:type content)))
        (repl-cljs-common/dispatch-request-print-tooling request callback)
        (and (= :get method))
        (repl-cljs-common/dispatch-request-assets request callback)
        :else
        (repl-cljs-common/dispatch-request-default request callback)))

;; ============================================================================
;; Utils Integration
;; ============================================================================

(defmethod utils/repl-ns :replique/shadow-browser [_]
  ana/*cljs-ns*)

(defmethod utils/repl-type :replique/shadow-browser [_]
  :cljs)

(defmethod utils/repl-params :replique/shadow-browser [_]
  (select-keys @repl-cljs-common/repl-params ["cljs.core/*print-length*"
                                              "cljs.core/*print-level*"
                                              "cljs.core/*print-meta*"]))

(derive :replique/shadow-browser :replique/cljs)

;; ============================================================================
;; Tooling Message Handlers
;; ============================================================================

(defmethod tooling-msg/tooling-msg-handle [:replique/shadow-browser :eval] [msg]
  (tooling-msg/with-tooling-response msg
    (binding [*out* utils/process-out
              *err* utils/process-err]
      (let [form (reader/read-string {:read-cond :allow :features #{:cljs}} (:form msg))
            result (eval-cljs-form form)]
        (assoc msg :result result)))))

(defmethod tooling-msg/tooling-msg-handle [:replique/shadow-browser :load-js] [msg]
  (tooling-msg/with-tooling-response msg
    (let [{:keys [status value]} (->> (slurp (:file-path msg))
                                      (evaluate-form))]
      (if (not (= :success status))
        (assoc msg :error value)
        (assoc msg :result value)))))

(defmethod tooling-msg/tooling-msg-handle [:replique/shadow-browser :list-css] [msg]
  (tooling-msg/with-tooling-response msg
    (let [{:keys [status value]} (evaluate-form "replique.cljs_env.browser.list_css_urls();")]
      (if (not (= :success status))
        (assoc msg :error value)
        (assoc msg :css-urls (read-string value))))))

(defmethod tooling-msg/tooling-msg-handle [:replique/shadow-browser :load-css] [msg]
  (tooling-msg/with-tooling-response msg
    (let [{:keys [status value]} (->> (pr-str (:url msg))
                                      (format "replique.cljs_env.browser.reload_css(%s);")
                                      (evaluate-form))]
      (if (not (= :success status))
        (assoc msg :error value)
        (assoc msg :result value)))))


(defmethod tooling-msg/tooling-msg-handle [:replique/shadow-browser :context]
  [{:keys [ns repl-env] :as msg}]
  (tooling-msg/with-tooling-response msg
    (when ns
      (replique-context/context-forms
       (replique-env/->CljsCompilerEnv (shadow-compile/get-compiler-env)) repl-env ns
       replique-context/context-forms-cljs))))

(defmethod tooling-msg/tooling-msg-handle [:replique/shadow-browser :completion]
  [{:keys [context ns prefix] :as msg}]
  (tooling-msg/with-tooling-response msg
    (when prefix
      {:candidates (completion/candidates (replique-env/->CljsCompilerEnv (shadow-compile/get-compiler-env))
                                          nil
                                          ns context prefix)})))

(defmethod tooling-msg/tooling-msg-handle [:replique/shadow-browser :repliquedoc]
  [{:keys [context ns] :as msg}]
  (tooling-msg/with-tooling-response msg
    {:doc (repliquedoc/handle-repliquedoc
           (replique-env/->CljsCompilerEnv (shadow-compile/get-compiler-env))
           ns context)}))

(defmethod tooling-msg/tooling-msg-handle [:replique/shadow-browser :list-namespaces]
  [msg]
  (tooling-msg/with-tooling-response
   msg
   {:namespaces (->> (replique-env/->CljsCompilerEnv (shadow-compile/get-compiler-env))
                     replique.environment/all-ns
                     (map str)
                     (sort completion/by-length-comparator))}))

(defmethod tooling-msg/tooling-msg-handle [:replique/shadow-browser :list-vars]
  [{:keys [ns] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [comp-env (replique-env/->CljsCompilerEnv (shadow-compile/get-compiler-env))
          ns (symbol ns)
          the-ns (replique-env/find-ns comp-env ns)]
      (when the-ns
        {:vars (tooling/list-vars-with-meta comp-env the-ns)}))))

(defmethod tooling-msg/tooling-msg-handle [:replique/shadow-browser :symbols-in-namespaces]
  [{:keys [context ns is-string?] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [prefix (:symbol msg)
          comp-env (replique-env/->CljsCompilerEnv (shadow-compile/get-compiler-env))]
      (assoc (find-usage/symbols-in-namespaces
              comp-env ns context prefix)
             :default-ns "cljs.user"))))

(defmethod tooling-msg/tooling-msg-handle [:replique/shadow-browser :meta]
  [{:keys [context ns] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [prefix (:symbol msg)
          comp-env (replique-env/->CljsCompilerEnv (shadow-compile/get-compiler-env))
          m (replique-meta/handle-meta comp-env ns context prefix)]
      {:meta m})))

