(ns ewen.replique.ui.edit-repl
  (:require [hiccup.core]
            [hiccup.page :refer [include-css]]
            [goog.dom :as dom]
            [goog.events :as events]
            [cljs.reader :as reader]
            [ewen.replique.ui.remote :refer [remote]]
            [ewen.replique.ui.core :as core]
            [ewen.replique.ui.utils :as utils]
            [ewen.replique.ui.notifications :as notif])
  (:require-macros [hiccup.core :refer [html]]
                   [hiccup.def :refer [defhtml]]))

(def dialog (.require remote "dialog"))

(defn port-tmpl [{:keys [repls repl-index]}]
  (let [{:keys [port random-port]} (nth repls repl-index)]
    (html [:fieldset.port
           [:legend "REPL port"]
           [:input (merge
                    {:type "text" :maxlength "5"
                     :class "custom-port field" :value port}
                    (when random-port
                      {:readonly true
                       :disabled true}))]
           [:label {:for "random-port"} "Random port"]
           [:input (merge
                    {:type "checkbox"
                     :class "field random-port"
                     :id "random-port"}
                    (when random-port
                      {:checked true}))]])))

(defn edit-repl [{:keys [dirty repls repl-index] :as state}]
  (let [{:keys [directory type port random-port]} (nth repls repl-index)]
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
               :checked (contains? type "cljs")}]]
            (port-tmpl state)]])))

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

(swap! repl-field-readers conj
       (fn []
         (let [random-port (-> (.querySelector
                                js/document ".field.random-port")
                               (aget "checked"))]
           [:random-port random-port])))

(swap! repl-field-readers conj
       (fn []
         (let [port (-> (.querySelector js/document ".field.custom-port")
                        (aget "value"))]
           [:port (if (= "" port) nil (js/parseInt port))])))

(defn save-repl []
  (let [{:keys [repl-index repls]} @core/state
        fields (->> (for [field-reader @repl-field-readers]
                      (field-reader))
                    (into {}))]
    (swap! core/state core/update-repls repl-index merge fields)))

(defn valid-port? [port]
  (try (let [port-nb (js/parseInt port)]
         (< -1 port-nb 65535))
       (catch js/Error e false)))

(defn maybe-save-repl-error [edit-repl]
  (let [port (aget (.querySelector edit-repl ".port .field") "value")
        random-port (aget (.querySelector edit-repl ".field.random-port")
                          "checked")]
    (if (and (not random-port) (valid-port? port))
      {:type :err
       :msg "Invalid port number"}
      nil)))

(defn edit-repl-clicked [edit-repl e]
  (let [class-list (-> (aget e "target")
                       (aget "classList"))]
    (cond (.contains class-list "save")
          (if-let [err (maybe-save-repl-error edit-repl)]
            (notif/single-notif err)
            (do (save-repl)
                (swap! core/state dissoc :dirty)))
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
        class-list (aget target "classList")
        index (:repl-index @core/state)]
    (cond (.contains class-list "random-port")
          (let [port-node (.querySelector edit-repl ".port")
                random-port (aget target "checked")]
            (dom/replaceNode (->> (core/update-repls
                                   @core/state index assoc
                                   :random-port random-port)
                                  port-tmpl
                                  utils/make-node)
                             port-node)
            (swap! core/state assoc :dirty true))
          (.contains class-list "field")
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
       (events/listen (.querySelector node "form")
                      events/EventType.CLICK
                      (partial edit-repl-clicked node))
       (events/listen (.querySelector node "form")
                      events/EventType.CHANGE
                      (partial edit-repl-changed node))
       (events/listen (.querySelector
                       node ".port input.field[type=\"text\"]")
                      events/EventType.INPUT
                      (partial edit-repl-changed node)))
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
