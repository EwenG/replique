(ns replique.cljs-env.repl
  "Xhr based repl. Uses CORS to bypass the same origin policy.
  Adapted from https://github.com/clojure/clojurescript/blob/master/src/main/cljs/clojure/browser/repl.cljs.
  Also, replace :order by a :session id in messages sent to the server"
  (:require
   [goog.array :as garray]
   [goog.userAgent.product :as product]
   [clojure.browser.event :as event])
  [:import [goog.net XhrIo CorsXmlHttpFactory]])

(def connection (atom nil))

(defn xhr-connection
  "Returns an XhrIo connection"
  []
  (XhrIo. (CorsXmlHttpFactory.)))

(def *timeout* 10000)

(def print-queue (array))

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
     (event/listen conn :error
                   (fn [_]
                     (if (< n 10)
                       (send-print url data (inc n))
                       (.log js/console
                             (str "Could not send " data
                                  " after " n " attempts.")))))
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
    (event/listen
     conn "success"
     (fn [e]
       (let [js (.getResponseText (.-currentTarget e))
             result (evaluate-javascript js)]
         (send-result
          (eval-connection url) url (wrap-message :result result (:session @connection))))))
    ;; Reconnection logic. Try to reconnect once per second
    (js/setTimeout
     #(event/listen conn "error" (fn [] (connect url)))
     1000)
    conn))

(defn connect [url]
  (reset! connection {:url url})
  (send-result (eval-connection url) url (wrap-message :ready "ready"))
  url)
