(ns replique.cljs-env.repl
  "Unified Xhr-based REPL for both ClojureScript and Shadow-CLJS.
   JavaScript evaluation is asynchronous — dependencies are loaded
   via async <script> injection with a pending-eval / after-load
   hook mechanism to ensure all deps are available before evaluation
   results are sent back to the server.

   For Shadow-CLJS: SHADOW_ENV.load injects <script> tags and
   signals completion via goog.replique_after_load_hook__.
   For ClojureScript: CLOSURE_IMPORT_SCRIPT (set in bootstrap.js)
   loads dependencies synchronously via XHR; the after-load-hook
   fires immediately in that case because no async loading occurs.
   ENABLE_CHROME_APP_SAFE_SCRIPT_LOADING is enabled to prevent
   document.write after page load."
  (:require
   [goog.object :as o]
   [goog.net.EventType])
  (:import [goog.net XhrIo CorsXmlHttpFactory]))

(defonce connection (atom nil))
(defonce print-queue (array))
(defonce flushing-print-queue? (atom false))

;; ============================================================================
;; Messaging
;; ============================================================================

(defn wrap-message
  ([t data]
   (binding [*print-length* nil *print-level* nil]
     (pr-str {:type t :content data})))
  ([t data session]
   (binding [*print-length* nil *print-level* nil]
     (pr-str {:type t :content data :session session}))))

;; ============================================================================
;; XHR helpers
;; ============================================================================

(defn xhr-connection
  "Returns an XhrIo connection with CORS support."
  []
  (XhrIo. (CorsXmlHttpFactory.)))

;; ============================================================================
;; Printing
;; ============================================================================

(defn send-print [url data callback]
  (let [conn (xhr-connection)]
    (.listen conn goog.net.EventType/COMPLETE callback false)
    (.setTimeoutInterval conn 0)
    (.send conn url "POST" data nil)))

(defn flush-print-queue! []
  (if-let [x (.shift print-queue)]
    (send-print (:url @connection) x flush-print-queue!)
    (reset! flushing-print-queue? false)))

(defn repl-print [data]
  (.push print-queue
         (wrap-message :print (if (string? data) data (pr-str data))
                       (:session @connection)))
  (when-not @flushing-print-queue?
    (reset! flushing-print-queue? true)
    (flush-print-queue!)))

(defn send-print-tooling [s]
  (.push print-queue (wrap-message :print-tooling s (:session @connection)))
  (when-not @flushing-print-queue?
    (reset! flushing-print-queue? true)
    (flush-print-queue!)))

;; ============================================================================
;; Browser Detection
;; ============================================================================

(defn get-ua-product []
  (let [ua js/navigator.userAgent]
    (cond
      (re-find #"Chrome" ua) :chrome
      (re-find #"Safari" ua) :safari
      (re-find #"Firefox" ua) :firefox
      (re-find #"MSIE|Trident" ua) :ie
      :else :unknown)))

;; ============================================================================
;; JavaScript Evaluation
;; ============================================================================

(defn evaluate-javascript [block]
  (let [repl-params {"cljs.core/*assert*" *assert*
                     "cljs.core/*print-length*" *print-length*
                     "cljs.core/*print-meta*" *print-meta*
                     "cljs.core/*print-level*" *print-level*
                     "cljs.core/*flush-on-newline*" *flush-on-newline*
                     "cljs.core/*print-readably*" *print-readably*
                     "cljs.core/*print-dup*" *print-dup*}
        result
        (try
          (let [eval-result (js* "eval(~{block})")]
            (if (instance? js/Error eval-result)
              {:status :success
               :ua-product (get-ua-product)
               :value (str eval-result)
               :stacktrace (if (.hasOwnProperty eval-result "stack")
                             (.-stack eval-result)
                             "No stacktrace available.")
               :params repl-params}
              {:status :success
               :value (str eval-result)
               :params repl-params}))
          (catch :default e
            (try (js/console.error e) (catch :default _))
            {:status :exception
             :ua-product (get-ua-product)
             :value (str e)
             :stacktrace (if (.hasOwnProperty e "stack")
                           (.-stack e)
                           "No stacktrace available.")
             :params repl-params}))]
    (binding [*print-level* nil *print-length* nil]
      (pr-str result))))

;; ============================================================================
;; Async Eval Flow
;; ============================================================================

(declare connect)
(declare eval-connection)

(defn send-result [conn url data]
  (.setTimeoutInterval conn 0)
  (.send conn url "POST" data nil))

(defn process-pending-eval []
  (let [{:keys [pending-eval]} @connection]
    (when pending-eval
      (swap! connection dissoc :pending-eval)
      (let [result (evaluate-javascript pending-eval)]
        (if (o/get js/goog "replique_loading__")
          ;; Dependencies are still loading asynchronously — store the
          ;; result and wait for the after-load hook to send it.
          (swap! connection assoc :pending-result result)
          ;; Nothing loading — send result immediately.
          (let [{:keys [url session]} @connection]
            (send-result
             (eval-connection url) url
             (wrap-message :result result session))))))))

(defn after-load-hook []
  (let [{:keys [pending-result]} @connection]
    (if pending-result
      ;; A previous eval produced a result but deps were still loading.
      ;; Now that loading is complete, send the result.
      (do
        (swap! connection dissoc :pending-result)
        (let [{:keys [url session]} @connection]
          (send-result
           (eval-connection url) url
           (wrap-message :result pending-result session))))
      ;; No pending-result, but a pending-eval may have been waiting
      ;; for loading to complete (e.g. initial page-load scripts
      ;; finished after the REPL connected).
      (process-pending-eval))))

;; Register the after-load hook. Called by shadow_browser_runtime.js
;; (or any async loader) when all pending <script> loads complete.
;; For ClojureScript with bootstrap.js, loading is synchronous so
;; goog.replique_loading__ is never true and this hook is not needed
;; — results are sent immediately from process-pending-eval.
(o/set js/goog "replique_after_load_hook__" after-load-hook)

(defn eval-connection [url]
  (let [conn (xhr-connection)]
    (.listen conn goog.net.EventType/SUCCESS
             (fn [e]
               (let [js-text (.getResponseText (.-currentTarget e))]
                 (swap! connection assoc :pending-eval js-text)
                 ;; If dependencies are still loading, wait for the
                 ;; after-load hook. Otherwise process immediately.
                 (when-not (o/get js/goog "replique_loading__")
                   (process-pending-eval))))
             false)
    (.listen conn goog.net.EventType/ERROR
             (fn [e]
               (let [status (.getStatus conn)]
                 (if (= 409 status)
                   (.log js/console
                         (str "Replique connection broken. To reconnect, call:\n"
                              "replique.cljs_env.repl.connect(" (pr-str url) ");"))
                   (js/setTimeout #(connect url) 1000))))
             false)
    conn))

(defn connect
  ([url]
   (connect url nil))
  ([url main-ns]
   ;; Prevent document.write — use <script> tag injection after page load.
   ;; For ClojureScript with bootstrap.js, this flag is moot because
   ;; CLOSURE_IMPORT_SCRIPT overrides the loading path. For Shadow-CLJS,
   ;; it ensures the Closure debug loader (if invoked) doesn't use doc.write.
   (o/set js/goog "ENABLE_CHROME_APP_SAFE_SCRIPT_LOADING" true)
   (reset! connection {:url url})
   (send-result (eval-connection url) url
                (wrap-message :ready {:main-ns main-ns}))
   url))
