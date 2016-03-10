(ns ewen.replique.ui.common
  (:require [hiccup.core :refer-macros [html]]
            [hiccup.def :refer-macros [defhtml]]
            [hiccup.page :refer [include-css]]
            [goog.dom :as dom]
            [goog.events :as events]
            [ewen.replique.ui.utils :as utils]
            [ewen.ddom.core :as ddom :refer-macros [defnx]]
            [ewen.replique.ui.core :as core]))

(defnx back-clicked [e prev-view]
  (swap! core/state
         (fn [state]
           (-> (dissoc state :dirty)
               (assoc :view prev-view)))))

(defhtml back-button [prev-view]
  [:a.back-nav {:href "#" :onclick (ddom/handler back-clicked prev-view)}])

(defn save-set-dirty []
  (when-let [save-node (.querySelector js/document ".save")]
    (-> (.-classList save-node)
        (.remove "disabled"))))

(defnx save-clicked [e save-fn]
  (let [save-node (.-target e)]
    (when (save-fn)
      (-> (.-classList save-node)
          (.add "disabled")))))

(defhtml save-button [save-fn]
  [:a {:href "#" :class "button save disabled"
       :onclick (ddom/handler save-clicked save-fn)}
   "Save"])
