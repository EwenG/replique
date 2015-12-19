(ns ewen.replique.ui.settings
  (:require [hiccup.core]
            [hiccup.page :refer [include-css]]
            [goog.dom :as dom]
            [goog.events :as events]
            [cljs.reader :as reader]
            [ewen.replique.ui.remote :refer [remote]]
            [ewen.replique.ui.core :as core]
            [ewen.replique.ui.utils :as utils])
  (:require-macros [hiccup.core :refer [html]]
                   [hiccup.def :refer [defhtml]]))

(defhtml clj-jar-tmpl [clj-jar-source]
  [:fieldset.clj-jar
   [:legend "Clojure jar"]
   [:label {:for "clj-embedded-source"} "Embedded Clojure jar"]
   [:input.field (merge {:type "radio"
                         :name "clj-source"
                         :id "clj-embedded-source"
                         :value "embedded"}
                        (if (= "embedded" clj-jar-source)
                          {:checked true}
                          nil))]
   [:label {:for "clj-custom-source"} "Custom Clojure jar"]
   [:input.field (merge {:type "radio"
                         :name "clj-source"
                         :id "clj-custom-source"
                         :value "custom"}
                        (when (= "custom" clj-jar-source)
                          {:checked true}))] [:br]
   [:select.field.select-downloaded
    (when (= "custom" clj-jar-source)
      {:disabled true})]
   [:a (merge {:href "#"}
              (if (= "custom" clj-jar-source)
                {:class "disabled button download-clj-jar"}
                {:class "button download-clj-jar"}))
    "Download Clojure jar"] [:br]
   [:input.field.custom-clj-jar
    (merge {:type "text" :readonly true
            :value ""}
           (when (= "embedded" clj-jar-source)
             {:disabled true}))]
   [:a (merge {:href "#"}
              (if (= "embedded" clj-jar-source)
                {:class "disabled button new-clj-jar"}
                {:class "button new-clj-jar"}))
    "Select Clojure jar"]])

(defhtml settings [{dirty :dirty
                    {clj-jar-source :clj-jar-source} :settings}]
  (html [:div#settings
         [:a.back-nav {:href "#"}]
         [:form
          [:a.button.save
           (merge {:href "#"}
                  (if dirty
                    {:class "button save"}
                    {:class "button save disabled"}))
           "Save"]
          (clj-jar-tmpl clj-jar-source)]]))

(defn back-clicked []
  (swap! core/state assoc
         :view :dashboard
         :dirty false))

(defn settings-clicked [settings-node e]
  (let [class-list (-> (aget e "target")
                       (aget "classList"))]
    (cond (.contains class-list "save")
          nil
          :else nil)))

(defn settings-changed [settings-node e]
  (let [target (aget e "target")
        class-list (aget target "classList")]
    (cond (= "clj-source" (.getAttribute target "name"))
          (let [source (.getAttribute target "value")
                clj-jar-node (.querySelector settings-node ".clj-jar")]
            (dom/replaceNode (utils/make-node (clj-jar-tmpl source))
                             clj-jar-node)
            (swap! core/state assoc :dirty true))
          (.contains class-list "field")
          (swap! core/state assoc :dirty true)
          :else nil)))

(defmethod core/refresh-view :settings [state]
  (let [root (dom/createDom "div" #js {:id "root"})
        old-root (.getElementById js/document "root")
        settings-node (dom/htmlToDocumentFragment (settings state))]
    (when old-root (dom/removeNode old-root))
    (dom/appendChild js/document.body root)
    (dom/appendChild root settings-node)
    (events/listen (.querySelector settings-node ".back-nav")
                   events/EventType.CLICK back-clicked)
    #_(events/listen (.querySelector settings-node "#settings form")
                     events/EventType.CLICK (partial settings-clicked
                                                     settings-node))
    (events/listen (.querySelector settings-node "#settings form")
                   events/EventType.CHANGE (partial settings-changed
                                                    settings-node))))

(add-watch core/state :edit-watcher
           (fn [r k o n]
             (cond
               (and (not (:dirty o)) (:dirty n))
               (when-let [save-node (.querySelector js/document ".save")]
                 (-> (aget save-node "classList")
                     (.remove "disabled")))
               (and (:dirty o) (not (:dirty n)))
               (when-let [save-node (.querySelector js/document ".save")]
                 (-> (aget save-node "classList")
                     (.add "disabled")))
               :else nil)))
