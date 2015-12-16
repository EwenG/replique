(ns ewen.replique.ui.view
  (:require [cljs.nodejs :as node]
            [hiccup.core]
            [goog.dom :as dom]
            [goog.events :as events])
  (:require-macros [hiccup.core :refer [html]]
                   [hiccup.def :refer [defhtml]]))

;; Keyboard shortcuts

(def remote (node/require "remote"))
(def current-window (.getCurrentWindow remote))
(def web-contents (aget current-window "webContents"))

(defn keyboard-shortcuts [e]
  (cond (and (aget e "ctrlKey")
             (aget e "shiftKey")
             (= (aget e "keyCode") 73))
        (.toggleDevTools web-contents)
        :else nil))

(defn init-keyboards []
  (.addEventListener js/document "keyup" #(keyboard-shortcuts %)))

;; View
(def repls
  (atom
   '({:type :cljs :name "Replique"})))

(defhtml new-repl []
  [:a.dashboard-item.new-repl {:href "#"} "New REPL"])

(defhtml repl-overview [{:keys [type name]} index]
  [:div.dashboard-item.repl-overview
   {:href "#"
    :data-index index}
   [:img.delete {:src "resources/images/delete.png"}]
   [:img.edit {:src "resources/images/edit.png"}]
   [:div.repl-type
    [:img {:src "resources/images/clj-logo.gif"}]
    [:img {:src "resources/images/cljs-logo.png"}]]
   [:span.repl-name name]])

(defhtml dashboard [repls]
  (html [:div#dashboard
         (new-repl)
         (for [[repl index] (map vector repls (range (count repls)))]
           (repl-overview repl index))]))

(defn make-node [s]
  (dom/htmlToDocumentFragment s))

(defonce counter (atom 0))

(def repl-node
  (memoize
   (fn []
     (.querySelector js/document "#dashboard .new-repl"))))

(declare refresh-view)

(defn add-new-repl []
  (swap! repls conj {:type :cljs :name "Replique"})
  (refresh-view))

(defn overview-clicked [e]
  (let [class-list (-> (aget e "target")
                       (.-classList))]
    (when (.contains class-list "delete")
      (let [overview (aget e "currentTarget")
            index (js/parseInt (.getAttribute overview "data-index"))]
        (prn index)
        (swap! repls (partial keep-indexed #(if(= index %1) nil %2)))))))

(defn refresh-view []
  (when-let [dashboard-node (.getElementById js/document "dashboard")]
    (dom/removeNode dashboard-node))
  (dom/appendChild
   js/document.body
   (dom/htmlToDocumentFragment (dashboard @repls)))
  (when-let [dashboard (.getElementById js/document "dashboard")]
    (events/listen (.querySelector dashboard ".new-repl")
                   events/EventType.CLICK add-new-repl)
    (doseq [overview (-> (.querySelectorAll dashboard ".repl-overview")
                         array-seq)]
      (events/listen overview events/EventType.CLICK overview-clicked))))


(comment

  (refresh-view)



 )
