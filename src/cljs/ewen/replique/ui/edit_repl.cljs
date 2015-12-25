(ns ewen.replique.ui.edit-repl
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

(def dialog (.require remote "dialog"))

(comment
  (.showOpenDialog dialog #js {:properties #js ["openDirectory"]})
  )

(defhtml edit-repl [{:keys [dirty repls repl-index]}]
  (let [{:keys [directory type]} (nth repls repl-index)]
    (html [:div#edit-repl
           [:a.back-nav {:href "#"}]
           [:form
            [:a
             (merge {:href "#"}
                    (if dirty
                      {:class "button save"}
                      {:class "button save disabled"}))
             "Save"]
            [:fieldset.directory
             [:legend "REPL directory"]
             [:input.field {:type "text" :readonly true
                            :value directory}]
             [:a.button.new-directory {:href "#"} "Choose directory"]]
            [:fieldset.type
             [:legend "REPL type"]
             [:label {:for "type-clj"} "Clojure"]
             [:input.field
              {:type "radio"
               :id "type-clj"
               :name "type"
               :value "clj"
               :checked (= type #{"clj"})}]
             [:label {:for "type-cljs"} "Clojure/Clojurescript"]
             [:input.field
              {:type "radio"
               :id "type-cljs"
               :name "type"
               :value "cljs"
               :checked (contains? type "cljs")}]]]])))

(defn back-clicked []
  (swap! core/state assoc :view :dashboard)
  (swap! core/state dissoc :dirty))

(def repl-field-readers (atom #{}))

(swap! repl-field-readers conj
       (fn []
         (let [field (.querySelector
                      js/document ".directory input[type=\"text\"]")]
           [:directory (aget field "value")])))

(swap! repl-field-readers conj
       (fn []
         (let [type (-> (.querySelector
                         js/document "input[name=\"type\"]:checked")
                        (aget "value"))
               types (if (= "cljs" type) #{"clj" "cljs"} #{"clj"})]
           [:type types])))

(defn save-repl []
  (let [{:keys [repl-index repls]} @core/state
        fields (->> (for [field-reader @repl-field-readers]
                      (field-reader))
                    (into {}))]
    (swap! core/state core/update-repls repl-index merge fields)))

(defn edit-repl-clicked [edit-repl e]
  (let [class-list (-> (aget e "target")
                       (aget "classList"))]
    (cond (.contains class-list "save")
          (do
            (save-repl)
            (swap! core/state dissoc :dirty))
          (.contains class-list "new-directory")
          (let [input (.querySelector
                       edit-repl
                       ".directory input[type=\"text\"]")]
            (->> (.showOpenDialog
                  dialog #js {:properties #js ["openDirectory"]})
                 first
                 (aset input "value"))
            (.dispatchEvent input (js/Event. "change" #js {:bubbles true})))
          :else nil)))

(defn edit-repl-changed [edit-repl e]
  (let [target (aget e "target")
        class-list (aget target "classList")]
    (cond (.contains class-list "field")
          (swap! core/state assoc :dirty true)
          :else nil)))

(swap!
 core/refresh-view-fns assoc :edit-repl
 (fn [root {:keys [view] :as state}]
   (if (= :edit-repl view)
     (let [node (utils/replace-or-append
                 root "#edit-repl"
                 (dom/htmlToDocumentFragment
                  (edit-repl state)))]
       (events/listen (.querySelector node ".back-nav")
                      events/EventType.CLICK back-clicked)
       (events/listen (.querySelector node "#edit-repl form")
                      events/EventType.CLICK (partial
                                              edit-repl-clicked node))
       (events/listen (.querySelector node "#edit-repl form")
                      events/EventType.CHANGE (partial
                                               edit-repl-changed node)))
     (when-let [node (.querySelector root "#edit-repl")]
       (dom/removeNode node)))))

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
