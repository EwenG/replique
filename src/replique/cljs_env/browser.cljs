(ns replique.cljs-env.browser
  (:require [cljs.reader :as reader]
            [goog.cssom]
            [goog.date :as date]
            [goog.Uri]
            [replique.omniscient-runtime]
            [goog.object :as o])
  (:require-macros [replique.interactive]))

(defn remove-query-string [uri]
  (-> (goog.Uri.parse uri)
      (.setQuery "")
      str))

;; Also filter stylsheets without href and stylsheets whith a data scheme
(defn css-current-domain? [css]
  (let [current-domain js/window.location.hostname
        current-port (str js/window.location.port)
        href (.-href css)]
    (and href
         (= (.getDomain (goog.Uri/parse href)) current-domain)
         (= (str (.getPort (goog.Uri/parse href))) current-port))))

(defn list-css-urls []
  (let [css-list (->> (goog.cssom.getAllCssStyleSheets)
                      ;; Filter out css files imported with the
                      ;; @import directive
                      (filter #(.-ownerNode %))
                      (filter css-current-domain?)
                      (map #(.-href %))
                      (map remove-query-string)
                      distinct)]
    (pr-str css-list)))

(comment
  (require '[goog.dom])
  (->> (js-obj "rel" "stylesheet" "type" "text/css" "href" "mystyle.css")
       (goog.dom/createDom "link")
       (.appendChild (.-head js/document)))

  (->> (js-obj "rel" "stylesheet" "type" "text/css" "href" "ee/test.css")
       (goog.dom/createDom "link")
       (.appendChild (.-head js/document)))

  (->> (js-obj "rel" "stylesheet" "type" "text/css" "href" "mystyle2.css?ff=ee")
       (goog.dom/createDom "link")
       (.appendChild (.-head js/document)))

  (->> (js-obj "rel" "stylesheet" "type" "text/css" "href" "http://localhost:50526/mystyle4.css?ff=ee")
       (goog.dom/createDom "link")
       (.appendChild (.-head js/document)))

  (->> (js-obj "rel" "stylesheet" "type" "text/css" "href" "mystyle5.css?ff=ee")
       (goog.dom/createDom "link")
       (.appendChild (.-head js/document)))
  
  (->> (js-obj "rel" "stylesheet" "type" "text/css" "href" "data:text/css;base64,Ym9keSB7CiAgICBib3JkZXI6IDFweCBzb2xpZCByZWQ7Cn0K")
       (goog.dom/createDom "link")
       (.appendChild (.-head js/document)))
  )

(defn match-urls? [css url]
  (= (remove-query-string (.-href css)) url))

(defn stylesheet-with-url [url]
  (->> (goog.cssom.getAllCssStyleSheets)
       ;; Filter out css files imported with the
       ;; @import directive
       (filter #(.-ownerNode %))
       (filter #(match-urls? % url))
       first))

(defn reload-css-node [css-node]
  (let [href (.-href css-node)]
    (goog.dom.setProperties css-node (js-obj "href" href))))

(defn reload-css [url]
  (let [the-stylesheet (stylesheet-with-url url)]
    (reload-css-node (.-ownerNode the-stylesheet))))

;; other parameters may not be supported by all browsers
(defn global-error-handler [msg url]
  (println msg))

(o/set js/window "onerror" global-error-handler)
