(ns ewen.replique.ui.view
  (:require [cljs.nodejs :as node]
            [hiccup.core]
            [hiccup.page :refer [include-css]]
            [goog.dom :as dom]
            [goog.events :as events]
            [cljs.reader :as reader])
  (:require-macros [hiccup.core :refer [html]]
                   [hiccup.def :refer [defhtml]]))

;; Keyboard shortcuts

(def remote (node/require "remote"))
(def current-window (.getCurrentWindow remote))
(def web-contents (aget current-window "webContents"))
(def dialog (.require remote "dialog"))

(comment
  (.showOpenDialog dialog #js {:properties #js ["openDirectory"]})
  )

(defn keyboard-shortcuts [e]
  (cond (and (aget e "ctrlKey")
             (aget e "shiftKey")
             (= (aget e "keyCode") 73))
        (.toggleDevTools web-contents)
        :else nil))

(defn init-keyboards []
  (.addEventListener js/document "keyup" #(keyboard-shortcuts %)))

;; View

(def state
  (atom
   {:repls '()
    :view :dashboard}))

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
         (new-repl)
         (for [[repl index] (map vector repls (range (count repls)))]
           (repl-overview repl index))]))

(defhtml edit-form [{:keys [repls repl-index]}]
  (let [{:keys [directory type]} (nth repls repl-index)]
    (html [:div#edit-form
           [:a.back-nav {:href "#"}]
           [:form
            [:a.button.save.disabled {:href "#"} "Save"]
            [:fieldset.directory
             [:legend "REPL directory"]
             [:input.field {:type "text" :readonly true
                            :data-field-name "directory"
                            :value directory}]
             [:a.button.new-directory {:href "#"} "Choose directory"]]
            [:fieldset.type
             [:legend "REPL type"]
             [:input.field {:type "hidden"
                            :data-field-name "type"
                            :value (pr-str type)}]
             [:input.type-checkbox
              {:type "checkbox"
               :value "clj"
               :checked (contains? type "clj")}
              "Clojure"]
             [:input.type-checkbox
              {:type "checkbox"
               :value "cljs"
               :checked (contains? type "cljs")}
              "Clourescript"]]]])))

(defn make-node [s]
  (dom/htmlToDocumentFragment s))

(defonce counter (atom 0))

(def repl-node
  (memoize
   (fn []
     (.querySelector js/document "#dashboard .new-repl"))))

(defn add-new-repl []
  (swap! state update-in [:repls] conj {:type #{} :directory nil}))

(defn overview-clicked [overview e]
  (let [class-list (-> (aget e "target")
                       (.-classList))]
    (cond (.contains class-list "delete")
          (let [overview (aget e "currentTarget")
                index (js/parseInt (.getAttribute overview "data-index"))]
            (swap! state update-in [:repls]
                   (partial keep-indexed #(if(= index %1) nil %2))))
          (.contains class-list "edit")
          (let [index (-> (.getAttribute overview "data-index")
                          (js/parseInt))]
            (swap! state assoc :repl-index index)
            (swap! state assoc :view :edit-form))
          :else nil)))

(defn back-clicked []
  (swap! state assoc :view :dashboard))

(defn save-repl []
  (let [{:keys [repl-index repls]} @state
        old (nth repls repl-index)
        fields (for [field (-> (.querySelectorAll js/document ".field")
                               array-seq)]
                 (let [name (.getAttribute field "data-field-name")
                       value (aget field "value")]
                   [(keyword name) (reader/read-string value)]))
        fields (into {} fields)
        new (merge old fields)]
    (swap! state update-in [:repls]
           #(map-indexed (fn [i repl] (if (= i repl-index) new repl)) %))))

(defn edit-form-clicked [edit-form e]
  (let [class-list (-> (aget e "target")
                       (aget "classList"))]
    (cond (.contains class-list "save")
          (do
            (save-repl)
            (swap! state dissoc :dirty))
          (.contains class-list "new-directory")
          (let [input (.querySelector
                       edit-form
                       ".directory input[type=\"text\"]")]
            (->> (.showOpenDialog
                  dialog #js {:properties #js ["openDirectory"]})
                 first pr-str
                 (aset input "value"))
            (.dispatchEvent input (js/Event. "change" #js {:bubbles true})))
          :else nil)))

(defn edit-form-changed [edit-form e]
  (let [target (aget e "target")
        class-list (aget target "classList")
        type-checkboxes (->> (.querySelectorAll edit-form ".type-checkbox")
                            array-seq
                            (into #{}))]
    (cond (.contains class-list "field")
          (swap! state assoc :dirty true)
          (contains? type-checkboxes target)
          (let [type-field (.querySelector edit-form ".type .field")]
            (->> (map #(when (aget % "checked") (aget % "value"))
                      type-checkboxes)
                 (remove nil?)
                 (into #{}) str
                 (aset type-field "value"))
            (.dispatchEvent type-field
                            (js/Event. "change" #js {:bubbles true})))
          :else nil)))

(defmulti refresh-view :view)

(defmethod refresh-view :dashboard [state]
  (let [root (dom/createDom "div" #js {:id "root"})
        old-root (.getElementById js/document "root")
        dashboard-node (dom/htmlToDocumentFragment (dashboard state))]
    (when old-root (dom/removeNode old-root))
    (dom/appendChild js/document.body root)
    (dom/appendChild root dashboard-node)
    (events/listen (.querySelector dashboard-node ".new-repl")
                   events/EventType.CLICK add-new-repl)
    (doseq [overview (-> (.querySelectorAll dashboard-node ".repl-overview")
                         array-seq)]
      (events/listen overview events/EventType.CLICK
                     (partial overview-clicked overview)))))

(defmethod refresh-view :edit-form [state]
  (let [root (dom/createDom "div" #js {:id "root"})
        old-root (.getElementById js/document "root")
        edit-form-node (dom/htmlToDocumentFragment (edit-form state))]
    (when old-root (dom/removeNode old-root))
    (dom/appendChild js/document.body root)
    (dom/appendChild root edit-form-node)
    (events/listen (.querySelector edit-form-node ".back-nav")
                   events/EventType.CLICK back-clicked)
    (events/listen (.querySelector edit-form-node "#edit-form form")
                   events/EventType.CLICK (partial edit-form-clicked
                                                   edit-form-node))
    (events/listen (.querySelector edit-form-node "#edit-form form")
                   events/EventType.CHANGE (partial edit-form-changed
                                                    edit-form-node))))

(defn persist-state [{:keys [repls]}]
  (loop [index 0
         repls repls]
    (cond
      (first repls)
      (do (.setItem js/localStorage (str "repl" index)
                    (pr-str (first repls)))
          (recur (inc index) (rest repls)))
      (.getItem js/localStorage (str "repl" index))
      (do (.removeItem js/localStorage (str "repl" index))
          (recur (inc index) nil))
      :else true)))

(defn load-state []
  (loop [i 0
         repls '()]
    (if-let [repl-str (.getItem js/localStorage
                                (str "repl" i))]
      (let [repl (reader/read-string repl-str)]
        (recur (inc i) (conj repls repl)))
      (swap! state assoc :repls (reverse repls)))))

(add-watch state :view-watcher
           (fn [r k o n]
             (when (not= (:view o) (:view n))
               (refresh-view n))))

(add-watch state :repls-watcher
           (fn [r k o n]
             (when (not= (:repls o) (:repls n))
               (persist-state n)
               (refresh-view n))))

(add-watch state :edit-watcher
           (fn [r k o n]
             (cond
               (and (not (:dirty o)) (:dirty n))
               (-> (.querySelector js/document ".save")
                   (aget "classList")
                   (.remove "disabled"))
               (and (:dirty o) (not (:dirty n)))
               (-> (.querySelector js/document ".save")
                   (aget "classList")
                   (.add "disabled"))
               :else nil)))

(comment

  (dom/appendChild
   js/document.head (make-node (html (include-css "main.css"))))
  (refresh-view @state)




 )
