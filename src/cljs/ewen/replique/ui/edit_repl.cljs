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

(defn current-repl [{:keys [repls repl-index]}]
  (nth repls repl-index))

(defn cljs-env-tmpl [{:keys [repls repl-index]}]
  (let [{:keys [type cljs-env browser-env-main
                webapp-env-main webapp-env-assets]}
        (nth repls repl-index)]
    (html [:fieldset.cljs-env
           [:legend "Clojurescript environment"]
           [:label {:for "browser-env"
                    :class (if (not= :cljs type) "disabled" "")}
            "Browser"]
           [:input.field
            {:type "radio"
             :id "browser-env"
             :name "cljs-env"
             :value "browser"
             :checked (= cljs-env :browser)
             :disabled (not= :cljs type)}]
           [:input.field.browser-env-main
            {:type "text"
             :readonly true
             :value browser-env-main
             :disabled (or (not= :cljs type)
                           (not= :browser cljs-env))}]
           [:a {:href "#"
                :class (if (or (not= :cljs type) (not= :browser cljs-env))
                         "button new-browser-env-main disabled"
                         "button new-browser-env-main")}
            "Choose main namespace"]
           [:br]
           [:label {:for "webapp-env"
                    :class (if (not= :cljs type) "disabled" "")}
            "Web application"]
           [:input.field
            {:type "radio"
             :id "webapp-env"
             :name "cljs-env"
             :value "webapp"
             :checked (= cljs-env :webapp)
             :disabled (not= :cljs type)}]
           [:input.field.webapp-env-main
            {:type "text"
             :readonly true
             :value webapp-env-main
             :disabled (or (not= :cljs type) (not= :webapp cljs-env))}]
           [:a {:href "#"
                :class (if (or (not= :cljs type) (not= :webapp cljs-env))
                         "button new-webapp-env-main disabled"
                         "button new-webapp-env-main")}
            "Choose main namespace"]
           [:input.field.webapp-env-assets
            {:type "text"
             :readonly true
             :value webapp-env-assets
             :disabled (or (not= :cljs type) (not= :webapp cljs-env))}]
           [:a {:href "#"
                :class (if (or (not= :cljs type) (not= :webapp cljs-env))
                         "button new-webapp-env-assets disabled"
                         "button new-webapp-env-assets")}
            "Choose assets folder"]
           [:br]
           [:label {:for "replique-env"
                    :class (if (not= :cljs type) "disabled" "")}
            "Replique"]
           [:input.field
            {:type "radio"
             :id "replique-env"
             :name "cljs-env"
             :value "replique"
             :checked (= cljs-env :replique)
             :disabled (not= :cljs type)}]])))

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
  (let [{:keys [directory type cljs-env port random-port]}
        (nth repls repl-index)]
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
               :checked (= type :clj)}]
             [:label {:for "type-cljs"} "Clojure/Clojurescript"]
             [:input.field
              {:type "radio"
               :id "type-cljs"
               :name "type"
               :value "cljs"
               :checked (= type :cljs)}]]
            (cljs-env-tmpl state)
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
                        (aget "value")
                        keyword)]
           [:type type])))

(swap! repl-field-readers conj
       (fn []
         (let [random-port (-> (.querySelector
                                js/document ".field.random-port")
                               (aget "checked"))]
           [:random-port random-port])))

(swap! repl-field-readers conj
       (fn []
         (let [cljs-env (-> (.querySelector
                             js/document "input[name=\"cljs-env\"]:checked")
                            (aget "value")
                            keyword)]
           [:cljs-env cljs-env])))

(swap! repl-field-readers conj
       (fn []
         (let [val (-> (.querySelector
                        js/document ".field.browser-env-main")
                       (aget "value"))]
           [:browser-env-main (if (= "" val) nil val)])))

(swap! repl-field-readers conj
       (fn []
         (let [val (-> (.querySelector
                        js/document ".field.webapp-env-main")
                       (aget "value"))]
           [:webapp-env-main (if (= "" val) nil val)])))

(swap! repl-field-readers conj
       (fn []
         (let [val (-> (.querySelector
                        js/document ".field.webapp-env-assets")
                       (aget "value"))]
           [:webapp-env-assets (if (= "" val) nil val)])))

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
    (if (and (not random-port) (not (valid-port? port)))
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
          (and (.contains class-list "new-browser-env-main")
               (not (.contains class-list "disabled")))
          (let [input (.querySelector edit-repl ".browser-env-main")
                directory (-> (current-repl @core/state) :directory str)]
            (->> (.showOpenDialog
                  dialog #js {
                              :filters #js [#js {:name "clojurescript file"
                                                 :extensions
                                                 #js ["cljs" "cljc"]}]
                              :defaultPath directory})
                 first
                 (aset input "value"))
            (.dispatchEvent input (js/Event. "change" #js {:bubbles true})))
          (and (.contains class-list "new-webapp-env-main")
               (not (.contains class-list "disabled")))
          (let [input (.querySelector edit-repl ".webapp-env-main")
                directory (-> (current-repl @core/state) :directory str)]
            (->> (.showOpenDialog
                  dialog #js {:filters #js [#js {:name "clojurescript file"
                                                 :extensions
                                                 #js ["cljs" "cljc"]}]
                              :defaultPath directory})
                 first
                 (aset input "value"))
            (.dispatchEvent input (js/Event. "change" #js {:bubbles true})))
          (and (.contains class-list "new-webapp-env-assets")
               (not (.contains class-list "disabled")))
          (let [input (.querySelector edit-repl ".webapp-env-assets")
                directory (-> (current-repl @core/state) :directory str)]
            (->> (.showOpenDialog
                  dialog #js {:properties #js ["openDirectory"]
                              :defaultPath directory})
                 first
                 (aset input "value"))
            (.dispatchEvent input (js/Event. "change" #js {:bubbles true})))
          :else nil)))

(defn edit-repl-changed [edit-repl e]
  (let [target (aget e "target")
        class-list (aget target "classList")
        index (:repl-index @core/state)]
    (cond (= (.getAttribute target "name") "type")
          (let [cljs-env-node (.querySelector edit-repl ".cljs-env")
                type (keyword (aget target "value"))]
            (dom/replaceNode (->> (core/update-repls
                                   @core/state index assoc
                                   :type type)
                                  cljs-env-tmpl
                                  utils/make-node)
                             cljs-env-node)
            (swap! core/state assoc :dirty true))
          (= (.getAttribute target "name") "cljs-env")
          (let [cljs-env-node (.querySelector edit-repl ".cljs-env")
                type-node (.querySelector
                           edit-repl "[name=\"type\"]:checked")
                type (keyword (aget type-node "value"))
                cljs-env (keyword (aget target "value"))]
            (dom/replaceNode (->> (core/update-repls
                                   @core/state index assoc
                                   :type type
                                   :cljs-env cljs-env)
                                  cljs-env-tmpl
                                  utils/make-node)
                             cljs-env-node)
            (swap! core/state assoc :dirty true))
          (.contains class-list "random-port")
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
