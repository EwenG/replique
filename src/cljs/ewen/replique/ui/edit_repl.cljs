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
             [:input.field.type-checkbox
              {:type "checkbox"
               :value "clj"
               :checked (contains? type "clj")}
              "Clojure"]
             [:input.field.type-checkbox
              {:type "checkbox"
               :value "cljs"
               :checked (contains? type "cljs")}
              "Clourescript"]]]])))

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
         (let [fields (-> (.querySelectorAll
                           js/document ".type-checkbox")
                          array-seq)
               checked-values (for [field fields]
                                (if (aget field "checked")
                                  (aget field "value")
                                  nil))]
           [:type (->> (remove nil? checked-values)
                       (into #{}))])))

(defn save-repl []
  (let [{:keys [repl-index repls]} @core/state
        old (nth repls repl-index)
        fields (->> (for [field-reader @repl-field-readers]
                      (field-reader))
                    (into {}))
        new (merge old fields)]
    (swap! core/state update-in [:repls]
           #(map-indexed (fn [i repl] (if (= i repl-index) new repl)) %))))

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

(defmethod core/refresh-view :edit-repl [state]
  (let [root (dom/createDom "div" #js {:id "root"})
        old-root (.getElementById js/document "root")
        edit-repl-node (dom/htmlToDocumentFragment (edit-repl state))]
    (when old-root (dom/removeNode old-root))
    (dom/appendChild js/document.body root)
    (dom/appendChild root edit-repl-node)
    (events/listen (.querySelector edit-repl-node ".back-nav")
                   events/EventType.CLICK back-clicked)
    (events/listen (.querySelector edit-repl-node "#edit-repl form")
                   events/EventType.CLICK (partial edit-repl-clicked
                                                   edit-repl-node))
    (events/listen (.querySelector edit-repl-node "#edit-repl form")
                   events/EventType.CHANGE (partial edit-repl-changed
                                                    edit-repl-node))))

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
