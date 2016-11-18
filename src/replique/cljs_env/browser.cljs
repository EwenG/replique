(ns replique.cljs-env.browser
  (:require [cljs.reader :as reader]
            [goog.cssom]
            [goog.date :as date]
            [goog.Uri])
  (:require-macros [replique.interactive]))

(defn remove-query-string [uri]
  (-> (goog.Uri.parse uri)
      (.setQuery "")
      str))

(defn css-current-domain? [css]
  (let [current-domain js/window.location.hostname
        href (.-href css)]
    (if-not href
      false
      (or (= "data" (.-scheme (.-ownerNode css)))
          (= (.getDomain (goog.Uri/parse href))
             current-domain)))))

(defn css-infos [css]
  (cond (and (.-href css)
             (= "data" (.-scheme (.-ownerNode css))))
        {:scheme "data"
         :uri  (.-href css)
         :file-path (.-filepath (.-ownerNode css))}
        (.-href css)
        {:scheme "http"
         :uri (remove-query-string (.-href css))}
        :else nil))

(defn list-css-infos []
  (let [css-list (->> (goog.cssom.getAllCssStyleSheets)
                      ;; Filter out css files imported with the
                      ;; @import directive
                      (filter #(.-ownerNode %))
                      (filter css-current-domain?)
                      (map css-infos)
                      (remove nil?))]
    css-list))

(defn match-css-infos? [css {:keys [scheme uri file-path]}]
  (cond (= scheme "http")
        (= uri (remove-query-string (.-href css)))
        (= scheme "data")
        (= file-path (.-filepath (.-ownerNode css)))
        :else false))

(defn css-infos->css-stylesheets [css-infos]
  (->> (goog.cssom.getAllCssStyleSheets)
       ;; Filter out css files imported with the
       ;; @import directive
       (filter #(.-ownerNode %))
       (filter #(match-css-infos? % css-infos))
       (map #(.-ownerNode %))))

(defn reload-css-http [css-node]
  (let [href (.-href css-node)]
    (goog.dom.setProperties css-node (js-obj "href" href))))

(defn reload-css-file [css-node data-uri]
  (goog.dom.setProperties css-node (js-obj "href" data-uri)))

(defn reload-css [css-infos]
  (let [{:keys [scheme uri file-path]
         :as css-infos}
        (reader/read-string css-infos)
        css-list (css-infos->css-stylesheets css-infos)]
    (cond (= "http" scheme)
          (doseq [css css-list]
            (reload-css-http css))
          (= "data" scheme)
          (if (= (count css-list) 0)
            (let [css-node (goog.dom.createDom
                            "link"
                            (js-obj "rel" "stylesheet"
                                    "type" "text/css"
                                    "href" uri))]
              (goog.dom.setProperties
               css-node (js-obj "scheme" "data"
                                "filepath" file-path))
              (goog.dom.append
               (.querySelector js/document "head")
               css-node))
            (doseq [css css-list]
              (reload-css-file css uri)
              (goog.dom.setProperties
               css (js-obj "scheme" "data"
                           "filepath" file-path))))
          :else nil)))
