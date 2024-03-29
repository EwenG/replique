(ns replique.cljs-env.repl
  "Xhr based repl. Uses CORS to bypass the same origin policy.
  Adapted from https://github.com/clojure/clojurescript/blob/master/src/main/cljs/clojure/browser/repl.cljs.
  Other changes: 
  Replace :order by a :session id in messages sent to the server.
  Remove the use of goog.event because it is not compatible with reloading (reload-all)."
  (:require
   [goog.array :as garray]
   [goog.object :as o]
   [goog.userAgent.product :as product]
   [goog.net.EventType])
  (:import [goog.net XhrIo CorsXmlHttpFactory]))

(defonce connection (atom nil))

(defn xhr-connection
  "Returns an XhrIo connection"
  []
  (XhrIo. (CorsXmlHttpFactory.)))

(defonce print-queue (array))
(defonce flushing-print-queue? (atom false))

(defn send-result [conn url data]
  (.setTimeoutInterval conn 0)
  (.send conn url "POST" data nil))

(defn send-print
  "Send data to be printed in the REPL."
  [url data callback]
  (let [conn (xhr-connection)]
    (.listen conn goog.net.EventType/COMPLETE callback false)
    (.setTimeoutInterval conn 0)
    (.send conn url "POST" data nil)))

(defn wrap-message
  ([t data]
   (binding [*print-length* nil
             *print-level* nil]
     (pr-str {:type t :content data})))
  ([t data session]
   (binding [*print-length* nil
             *print-level* nil]
     (pr-str {:type t :content data :session session}))))

(defn flush-print-queue! []
  (if-let [x (.shift print-queue)]
    (send-print (:url @connection) x flush-print-queue!)
    (reset! flushing-print-queue? false)))

(defn repl-print [data]
  (if (string? data)
    (.push print-queue (wrap-message :print data (:session @connection)))
    (.push print-queue (wrap-message :print (pr-str data) (:session @connection))))
  (when-not @flushing-print-queue?
    (reset! flushing-print-queue? true)
    (flush-print-queue!)))

(defn send-print-tooling [s]
  (.push print-queue (wrap-message :print-tooling s (:session @connection)))
  (when-not @flushing-print-queue?
    (reset! flushing-print-queue? true)
    (flush-print-queue!)))

(defn get-ua-product []
  (cond
    product/SAFARI :safari
    product/CHROME :chrome
    product/FIREFOX :firefox
    product/IE :ie))

(defn evaluate-javascript
  "Process a single block of JavaScript received from the server"
  [block]
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
               :stacktrace
               (if (.hasOwnProperty eval-result "stack")
                 (.-stack eval-result)
                 "No stacktrace available.")
               :params repl-params}
              {:status :success
               :value (str eval-result)
               :params repl-params}))
          (catch :default e
            {:status :exception
             :ua-product (get-ua-product)
             :value (str e)
             :stacktrace
             (if (.hasOwnProperty e "stack")
               (.-stack e)
               "No stacktrace available.")
             :params repl-params}))]
    (binding [*print-level* nil
              *print-length* nil]
      (pr-str result))))

(declare connect)
(declare eval-connection)

(defn process-pending-eval []
  (let [{:keys [pending-eval]} @connection]
    (when pending-eval
      (swap! connection dissoc :pending-eval)
      (let [result (evaluate-javascript pending-eval)
            {:keys [url session]} @connection]
        (send-result
         (eval-connection url) url (wrap-message :result result session))))))

(o/set js/goog "replique_after_load_hook__" process-pending-eval)

(defn eval-connection [url]
  (let [conn (xhr-connection)]
    (.listen conn goog.net.EventType/SUCCESS
             (fn [e]
               (let [js (.getResponseText (.-currentTarget e))]
                 (swap! connection assoc :pending-eval js)
                 ;; Wait for all file to be loaded
                 ;; This seems necessary on some browsers even if the async attribute of <script>
                 ;; has been set to false
                 (when-not (o/get js/goog "replique_loading__")
                   (process-pending-eval))))
              false)
    (.listen conn goog.net.EventType/ERROR
             (fn [e]
               (if (= 409 (.getStatus conn))
                 ;; Connection closed by the server.
                 ;; Print a reconnection message but do not try to reconnect automatically in order
                 ;; to avoid breaking the connection of an eventual other device trying to
                 ;; connect the the cljs REPL
                 (.log js/console (str
                                   "Replique connection broken. To reconnect, call:\nreplique.cljs_env.repl.connect(" (pr-str url) ");"))
                 ;; Reconnection logic. Try to reconnect once per second
                 ;; We don't try to reconnect immediatly because otherwise, when reloading the page,
                 ;; the error listener is triggered, which generates an error on the server side
                 ;; (broken socket)
                 (js/setTimeout #(connect url) 1000)))
             false)
    conn))

(defn connect [url]
  ;; Tell the load function of goog.base to not use doc.write but instead to append <script ...>
  ;; to the header/body of the page. doc.write cannot be used anymore after the page is finished
  ;; loading
  (o/set js/goog "ENABLE_CHROME_APP_SAFE_SCRIPT_LOADING" true)
  
  (reset! connection {:url url})
  (send-result (eval-connection url) url (wrap-message :ready "ready"))
  url)

(defonce ^:dynamic *process-id* nil)
