(ns replique.cljs-env.repl
  "Xhr based repl. Uses CORS to bypass the same origin policy.
  Adapted from https://github.com/clojure/clojurescript/blob/master/src/main/cljs/clojure/browser/repl.cljs.
  Also, replace :order by a :session id in messages sent to the server"
  (:require
   [goog.dom :as gdom]
   [goog.object :as gobj]
   [goog.array :as garray]
   [goog.userAgent.product :as product]
   [clojure.browser.event :as event]
   ;; repl-connection callback will receive goog.require('cljs.repl')
   ;; and monkey-patched require expects to be able to derive it
   ;; via goog.basePath, so this namespace should be compiled together
   ;; with clojure.browser.repl:
   [cljs.repl])
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
  (let [{:keys [url session]} @connection]
    (doseq [str print-queue]
      (send-print url (wrap-message :print str session))))
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
          {:status :success
           :value (str (js* "eval(~{block})"))}
          (catch :default e
            {:status :exception
             :ua-product (get-ua-product)
             :value (str e)
             :stacktrace
             (if (.hasOwnProperty e "stack")
               (.-stack e)
               "No stacktrace available.")}))]
    (pr-str result)))

(def load-queue nil)
(def bootstrapped (atom false))

(defn bootstrap
  "Reusable browser REPL bootstrapping. Patches the essential functions
  in goog.base to support re-loading of namespaces after page load."
  []
  ;; Monkey-patch goog.provide if running under optimizations :none - David
  (when (and (not js/COMPILED) (not @bootstrapped))
    (reset! bootstrapped true)
    (set! (.-require__ js/goog) js/goog.require)
    ;; suppress useless Google Closure error about duplicate provides
    (set! (.-isProvided_ js/goog) (fn [name] false))
    ;; provide cljs.user
    (goog/constructNamespace_ "cljs.user")
    (set! (.-writeScriptTag__ js/goog)
      (fn [src opt_sourceText]
        ;; the page is already loaded, we can no longer leverage
        ;; document.write instead construct script tag elements and append
        ;; them to the body of the page, to avoid parallel script loading
        ;; enforce sequential load with a simple load queue
        (let [loaded (atom false)
              onload (fn []
                       (when (and load-queue (false? @loaded))
                         (swap! loaded not)
                         (if (zero? (alength load-queue))
                           (set! load-queue nil)
                           (.apply js/goog.writeScriptTag__
                                   nil (.shift load-queue)))))]
          (.appendChild js/document.body
            (as-> (.createElement js/document "script") script
              (doto script
                (gobj/set "type" "text/javascript")
                (gobj/set "onload" onload)
                (gobj/set "onreadystatechange" onload)) ;; IE
              (if (nil? opt_sourceText)
                (doto script (gobj/set "src" src))
                (doto script (gdom/setTextContext opt_sourceText))))))))
    ;; queue or load
    (set! (.-writeScriptTag_ js/goog)
      (fn [src opt_sourceText]
        (if load-queue
          (.push load-queue #js [src opt_sourceText])
          (do
            (set! load-queue #js [])
            (js/goog.writeScriptTag__ src opt_sourceText)))))
    ;; we must reuse Closure library dev time dependency management,
    ;; under namespace reload scenarios we simply delete entries from
    ;; the correct private locations
    (set! (.-require js/goog)
      (fn [src reload]
        (when (= reload "reload-all")
          (set! (.-cljsReloadAll_ js/goog) true))
        (let [reload? (or reload (.-cljsReloadAll__ js/goog))]
          (when reload?
            (let [path (aget js/goog.dependencies_.nameToPath src)]
              (gobj/remove js/goog.dependencies_.visited path)
              (gobj/remove js/goog.dependencies_.written path)
              (gobj/remove js/goog.dependencies_.written
                (str js/goog.basePath path))))
          (let [ret (.require__ js/goog src)]
            (when (= reload "reload-all")
              (set! (.-cljsReloadAll_ js/goog) false))
            ret))))))

(defn eval-connection [url]
  (let [conn (xhr-connection)]
    (event/listen
     conn "success"
     (fn [e]
       (let [js (.getResponseText (.-currentTarget e))
             result (evaluate-javascript js)
             {:keys [session]} @connection]
         (send-result
          (eval-connection url) url (wrap-message :result result session)))))
    conn))

(defn connect [url]
  (bootstrap)
  (reset! connection {:url url})
  (send-result (eval-connection url) url (wrap-message :ready "ready"))
  url)
