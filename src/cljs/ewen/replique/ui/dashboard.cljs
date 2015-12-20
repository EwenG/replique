(ns ewen.replique.ui.dashboard
  (:require [hiccup.core]
            [hiccup.page :refer [include-css]]
            [goog.dom :as dom]
            [goog.events :as events]
            [cljs.reader :as reader]
            [ewen.replique.ui.remote :refer [remote]]
            [ewen.replique.ui.core :as core]
            [ewen.replique.ui.utils :as utils]
            [ewen.replique.ui.edit-repl]
            [ewen.replique.ui.settings]
            [ewen.replique.ui.shortcuts]
            [ewen.replique.ui.notifications])
  (:require-macros [hiccup.core :refer [html]]
                   [hiccup.def :refer [defhtml]]))

(defhtml new-repl []
  [:a.dashboard-item.new-repl {:href "#"} "New REPL"])

(defhtml repl-overview [{:keys [type directory]} index]
  [:div.dashboard-item.repl-overview
   {:href "#"
    :data-index index}
   [:img.delete {:src "resources/images/delete.png"}]
   [:img.edit {:src "resources/images/edit.png"}]
   [:div.repl-type
    (when (contains? type "clj")
      [:img {:src "resources/images/clj-logo.gif"}])
    (when (contains? type "cljs")
      [:img {:src "resources/images/cljs-logo.png"}])]
   [:span.repl-directory directory]])

(defhtml dashboard [{:keys [repls]}]
  (html [:div#dashboard
         [:div.settings-wrapper
          [:img.settings-button {:src "/resources/images/settings.png"}]]
         (new-repl)
         (for [[repl index] (map vector repls (range (count repls)))]
           (repl-overview repl index))]))

(defn add-new-repl []
  (swap! core/state update-in [:repls] conj {:type #{} :directory nil}))

(defn settings-button-clicked []
  (swap! core/state assoc :view :settings))

(defn overview-clicked [overview e]
  (let [class-list (-> (aget e "target")
                       (.-classList))]
    (cond (.contains class-list "delete")
          (let [overview (aget e "currentTarget")
                index (js/parseInt (.getAttribute overview "data-index"))]
            (swap! core/state update-in [:repls]
                   (partial keep-indexed #(if(= index %1) nil %2))))
          (.contains class-list "edit")
          (let [index (-> (.getAttribute overview "data-index")
                          (js/parseInt))]
            (swap! core/state assoc :repl-index index)
            (swap! core/state assoc :view :edit-repl))
          :else nil)))

(swap!
 core/refresh-view-fns assoc :dashboard
 (fn [root {:keys [view] :as state}]
   (if (= :dashboard view)
     (let [node (utils/replace-or-append
                           root "#dashboard"
                           (dom/htmlToDocumentFragment
                            (dashboard state)))]
       (events/listen (.querySelector node ".new-repl")
                      events/EventType.CLICK add-new-repl)
       (events/listen (.querySelector node ".settings-button")
                      events/EventType.CLICK settings-button-clicked)
       (doseq [overview (-> (.querySelectorAll node ".repl-overview")
                            array-seq)]
         (events/listen overview events/EventType.CLICK
                        (partial overview-clicked overview))))
     (when-let [node (.querySelector root "#dashboard")]
       (dom/removeNode node)))))


(add-watch core/state :repls-watcher
           (fn [r k o n]
             (when (not= (:repls o) (:repls n))
               (core/persist-state n)
               (core/refresh-view n))))

(comment

  (dom/appendChild
   js/document.head (utils/make-node (html (include-css "main.css"))))
  (core/refresh-view @core/state)

 )
