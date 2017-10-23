(ns replique.cljs-env.repl
  "Xhr based repl. Uses CORS to bypass the same origin policy.
  Adapted from https://github.com/clojure/clojurescript/blob/master/src/main/cljs/clojure/browser/repl.cljs.
  Other changes: 
  Replace :order by a :session id in messages sent to the server.
  Remove the use of goog.event because it is not compatible with reloading (reload-all)."
  (:require
   [goog.array :as garray]
   [goog.userAgent.product :as product]
   [goog.net.EventType])
  (:import [goog.net XhrIo CorsXmlHttpFactory]))

(defonce connection (atom nil))

(defn xhr-connection
  "Returns an XhrIo connection"
  []
  (XhrIo. (CorsXmlHttpFactory.)))

(def *timeout* 10000)

(defonce print-queue (array))

(defn send-result [conn url data]
  (.setTimeoutInterval conn 0)
  (.send conn url "POST" data nil))

(defn send-print
  "Send data to be printed in the REPL. If there is an error, try again
  up to 10 times."
  ([url data]
   (send-print url data 0))
  ([url data n]
   (let [conn (xhr-connection)]
     (.listen conn goog.net.EventType/ERROR
              (fn [_]
                (if (< n 10)
                  (send-print url data (inc n))
                  (.log js/console
                        (str "Could not send " data
                             " after " n " attempts."))))
              false)
     (.setTimeoutInterval conn 0)
     (.send conn url "POST" data nil))))

(defn wrap-message
  ([t data]
   (pr-str {:type t :content data}))
  ([t data session]
   (pr-str {:type t :content data :session session})))

(defn flush-print-queue! []
  (doseq [str print-queue]
    (send-print (:url @connection) (wrap-message :print str (:session @connection))))
  (garray/clear print-queue))

(defn repl-print [data]
  (.push print-queue (pr-str data))
  (flush-print-queue!))

(defn get-ua-product []
  (cond
    product/SAFARI :safari
    product/CHROME :chrome
    product/FIREFOX :firefox
    product/IE :ie))

(defn evaluate-javascript
  "Process a single block of JavaScript received from the server"
  [block]
  (let [result
        (try
          (let [eval-result (js* "eval(~{block})")]
            (if (instance? js/Error eval-result)
              {:status :success
               :ua-product (get-ua-product)
               :value (str eval-result)
               :stacktrace
               (if (.hasOwnProperty eval-result "stack")
                 (.-stack eval-result)
                 "No stacktrace available.")}
              {:status :success
               :value (str eval-result)}))
          (catch :default e
            {:status :exception
             :ua-product (get-ua-product)
             :value (str e)
             :stacktrace
             (if (.hasOwnProperty e "stack")
               (.-stack e)
               "No stacktrace available.")}))]
    (pr-str result)))

(declare connect)

(defn eval-connection [url]
  (let [conn (xhr-connection)]
    (.listen conn goog.net.EventType/SUCCESS
             (fn [e]
               (let [js (.getResponseText (.-currentTarget e))
                     result (evaluate-javascript js)]
                 (send-result
                  (eval-connection url) url (wrap-message :result result (:session @connection)))))
              false)
    ;; Reconnection logic. Try to reconnect once per second
    ;; We don't try to reconnect immediatly because otherwise, when reloading the page,
    ;; the error listener is triggered, which generates an error on the server side
    ;; (broken socket)
    (.listen conn goog.net.EventType/ERROR
             (fn [] (js/setTimeout #(connect url) 1000))
             false)
    conn))

(defn connect [url]
  (reset! connection {:url url})
  (send-result (eval-connection url) url (wrap-message :ready "ready"))
  url)
