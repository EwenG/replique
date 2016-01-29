(ns ewen.replique.ui.edit-repl
  (:require [hiccup.core :refer-macros [html]]
            [hiccup.page :refer [include-css]]
            [goog.dom :as dom]
            [goog.events :as events]
            [cljs.reader :as reader]
            [ewen.replique.ui.common :as common]
            [ewen.replique.ui.remote :refer [remote]]
            [ewen.replique.ui.core :as core]
            [ewen.replique.ui.utils :as utils]
            [ewen.replique.ui.notifications :as notif]))

(def replique-dir (.getGlobal remote "repliqueRootDir"))
(def dialog (.require remote "dialog"))

(defn cljs-env-tmpl [{:keys [repls repl-id] :as state}]
  (let [{:keys [directory type cljs-env
                browser-env-port browser-env-random-port
                webapp-env-port webapp-env-random-port
                browser-env-main webapp-env-main
                webapp-env-out browser-env-out]}
        (get repls repl-id)]
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
           [:input
            {:type "text" :maxlength "5"
             :class "browser-env-port field" :value browser-env-port
             :placeholder "Port"
             :disabled (or browser-env-random-port
                           (not= type :cljs)
                           (not= cljs-env :browser))}]
           [:label {:for "browser-env-random-port"
                    :class (if (or (not= :cljs type)
                                   (not= :browser cljs-env))
                             "disabled" "")}
            "Random port"]
           [:input
            {:type "checkbox"
             :class "field browser-env-random-port"
             :id "browser-env-random-port"
             :checked browser-env-random-port
             :disabled (or (not= cljs-env :browser) (not= type :cljs))}]
           [:input.field.browser-env-out
            {:type "text"
             :readonly true
             :value browser-env-out
             :disabled (or (not= :cljs type)
                           (not= :browser cljs-env))}]
           [:a {:href "#"
                :class (if (or (not= :cljs type) (not= :browser cljs-env))
                         "button new-browser-env-out disabled"
                         "button new-browser-env-out")}
            "Select output file"]
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
           [:input
            {:type "text" :maxlength "5"
             :class "webapp-env-port field" :value webapp-env-port
             :placeholder "Port"
             :disabled (or browser-env-random-port
                           (not= type :cljs)
                           (not= cljs-env :webapp))}]
           [:label {:for "webapp-env-random-port"
                    :class (if (or (not= :cljs type) (not= :webapp cljs-env))
                             "disabled" "")}
            "Random port"]
           [:input
            {:type "checkbox"
             :class "field webapp-env-random-port"
             :id "webapp-env-random-port"
             :checked webapp-env-random-port
             :disabled (or (not= type :cljs)
                           (not= cljs-env :webapp))}]
           [:input.field.webapp-env-out
            {:type "text"
             :readonly true
             :value webapp-env-out
             :disabled (or (not= :cljs type) (not= :webapp cljs-env))}]
           [:a {:href "#"
                :class (if (or (not= :cljs type) (not= :webapp cljs-env))
                         "button new-webapp-env-out disabled"
                         "button new-webapp-env-out")}
            "Select output file"]
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

(defn port-tmpl [{:keys [repls repl-id] :as state}]
  (let [{:keys [port random-port]} (get repls repl-id)]
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

(defn edit-repl [{:keys [dirty repls repl-id] :as state}]
  (let [{:keys [directory type cljs-env port random-port]}
        (get repls repl-id)]
    (html [:div#edit-repl
           (common/back-button)
           [:form
            (common/save-button dirty)
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
               :name "repl-type"
               :value "clj"
               :checked (= type :clj)}]
             [:label {:for "type-cljs"} "Clojure/Clojurescript"]
             [:input.field
              {:type "radio"
               :id "type-cljs"
               :name "repl-type"
               :value "cljs"
               :checked (= type :cljs)}]]
            (cljs-env-tmpl state)
            (port-tmpl state)]])))

(defn back-clicked []
  (swap! core/state
         (fn [state]
           (-> (dissoc state :dirty)
               (assoc :view :dashboard)))))

(def fields (atom #{}))

(def directory-field
  (reify IDeref
    (-deref [_]
      (specify! (.querySelector js/document ".directory .field")
        common/IField
        (field-val [this] (aget this "value"))
        (field-path [this] [:directory])))))

(def repl-type-field
  (reify IDeref
    (-deref [_]
      (specify! (.querySelector
                  js/document ".type input[name=\"repl-type\"]:checked")
        common/IField
        (field-val [this] (keyword (aget this "value")))
        (field-path [this] [:type])))))

(def cljs-env-field
  (reify IDeref
    (-deref [_]
      (specify! (.querySelector
                  js/document ".cljs-env input[name=\"cljs-env\"]:checked")
        common/IField
        (field-val [this] (keyword (aget this "value")))
        (field-path [this] [:cljs-env])))))

(def browser-env-port-field
  (reify IDeref
    (-deref [_]
      (specify! (.querySelector
                  js/document ".cljs-env .browser-env-port")
        common/IField
        (field-val [this] (let [val (aget this "value")]
                            (if (= "" val) nil (js/parseInt val))))
        (field-path [this] [:browser-env-port])))))

(def browser-env-random-port-field
  (reify IDeref
    (-deref [_]
      (specify! (.querySelector
                  js/document ".cljs-env .browser-env-random-port")
        common/IField
        (field-val [this] (aget this "checked"))
        (field-path [this] [:browser-env-random-port])))))

(def webapp-env-port-field
  (reify IDeref
    (-deref [_]
      (specify! (.querySelector
                  js/document ".cljs-env .webapp-env-port")
        common/IField
        (field-val [this] (let [val (aget this "value")]
                            (if (= "" val) nil (js/parseInt val))))
        (field-path [this] [:webapp-env-port])))))

(def webapp-env-random-port-field
  (reify IDeref
    (-deref [_]
      (specify! (.querySelector
                  js/document ".cljs-env .webapp-env-random-port")
        common/IField
        (field-val [this] (aget this "checked"))
        (field-path [this] [:webapp-env-random-port])))))

(def browser-env-out-field
  (reify IDeref
    (-deref [_]
      (specify! (.querySelector
                  js/document ".cljs-env .browser-env-out")
        common/IField
        (field-val [this] (let [val (aget this "value")]
                            (if (= "" val) nil val)))
        (field-path [this] [:browser-env-out])))))

(def webapp-env-out-field
  (reify IDeref
    (-deref [_]
      (specify! (.querySelector
                  js/document ".cljs-env .webapp-env-out")
        common/IField
        (field-val [this] (let [val (aget this "value")]
                            (if (= "" val) nil val)))
        (field-path [this] [:webapp-env-out])))))

(def browser-env-main-field
  (reify IDeref
    (-deref [_]
      (specify! (.querySelector
                  js/document ".cljs-env .browser-env-main")
        common/IField
        (field-val [this] (let [val (aget this "value")]
                            (if (= "" val) nil val)))
        (field-path [this] [:browser-env-main])))))

(def webapp-env-main-field
  (reify IDeref
    (-deref [_]
      (specify! (.querySelector
                  js/document ".cljs-env .webapp-env-main")
        common/IField
        (field-val [this] (let [val (aget this "value")]
                            (if (= "" val) nil val)))
        (field-path [this] [:webapp-env-main])))))

(def custom-port-field
  (reify IDeref
    (-deref [_]
      (specify! (.querySelector
                  js/document ".port .custom-port")
        common/IField
        (field-val [this] (let [val (aget this "value")]
                            (if (= "" val) nil (js/parseInt val))))
        (field-path [this] [:port])))))

(def random-port-field
  (reify IDeref
    (-deref [_]
      (specify! (.querySelector
                  js/document ".port .random-port")
        common/IField
        (field-val [this] (aget this "checked"))
        (field-path [this] [:random-port])))))

(swap! fields conj
       directory-field repl-type-field cljs-env-field
       browser-env-port-field browser-env-port-field
       browser-env-random-port-field webapp-env-port-field
       browser-env-out-field webapp-env-out-field
       browser-env-main-field webapp-env-main-field
       custom-port-field random-port-field)

(defn save-repl []
  (let [state @core/state
        {:keys [repl-id repls]} state
        fields (loop [new-fields {}
                      field-getters @fields]
                 (if-let [field (first field-getters)]
                   (let [field @field]
                     (recur (assoc-in new-fields
                                      (common/field-path field)
                                      (common/field-val field))
                            (rest field-getters)))
                   new-fields))
        state (swap! core/state update-in [:repls repl-id] merge fields)]
    (try
      (core/persist-state state)
      (catch js/Error e
        (.log js/console (str "Error while saving settings: " e))
        (notif/single-notif
         {:type :err
          :msg (str "Error while saving settings")})))))

(defn valid-port? [port]
  (try (let [port-nb (js/parseInt port)]
         (< -1 port-nb 65535))
       (catch js/Error e false)))

(defn maybe-save-repl-error [edit-repl]
  (let [port (aget (.querySelector edit-repl ".port .field") "value")
        random-port (common/field-val @random-port-field)
        browser-env-port (common/field-val @browser-env-port-field)
        browser-env-random-port (common/field-val
                                 @browser-env-random-port-field)
        webapp-env-port (common/field-val @webapp-env-port-field)
        webapp-env-random-port (common/field-val
                                @webapp-env-random-port-field)]
    (if (or (and (not random-port) (not (valid-port? port)))
            (and (not browser-env-random-port)
                 (not (valid-port? browser-env-port)))
            (and (not webapp-env-random-port)
                 (not (valid-port? webapp-env-port))))
      {:type :err
       :msg "Invalid port number"}
      nil)))

(defn refresh-cljs-env
  ([state edit-repl]
   (refresh-cljs-env state edit-repl false))
  ([{:keys [repls repl-id] :as state} edit-repl keep-cljs-env?]
   (let [cljs-env-node (.querySelector edit-repl ".cljs-env")
         directory (common/field-val @directory-field)
         type (common/field-val @repl-type-field)
         cljs-env (if keep-cljs-env?
                    (common/field-val @cljs-env-field)
                    (:cljs-env (get repls repl-id)))
         id (:repl-id state)]
     (dom/replaceNode (->> (update-in state [:repls id]
                                      assoc
                                      :directory directory
                                      :type type
                                      :cljs-env cljs-env)
                           cljs-env-tmpl
                           utils/make-node)
                      cljs-env-node))))

(defn edit-repl-clicked [edit-repl e]
  (let [class-list (-> (aget e "target") (aget "classList"))]
    (cond (.contains class-list "save")
          (if-let [err (maybe-save-repl-error edit-repl)]
            (notif/single-notif err)
            (do (save-repl)
                (swap! core/state dissoc :dirty)))
          (.contains class-list "new-directory")
          (let [input @directory-field
                directory (first
                           (.showOpenDialog
                            dialog
                            #js {:properties #js ["openDirectory"]}))
                browser-env-out @browser-env-out-field]
            (aset input "value" directory)
            (.dispatchEvent input (js/Event. "change" #js {:bubbles true})))
          (and (.contains class-list "new-browser-env-out")
               (not (.contains class-list "disabled")))
          (let [input @browser-env-out-field
                input-val (common/field-val input)
                directory (common/field-val @directory-field)
                out-file (.showSaveDialog
                          dialog
                          #js {:filters #js [#js {:name "javascript file"
                                                  :extensions #js ["js"]}]
                               :defaultPath (or input-val directory)})]
            (aset input "value" (or out-file nil))
            (.dispatchEvent input (js/Event. "change" #js {:bubbles true})))
          (and (.contains class-list "new-webapp-env-out")
               (not (.contains class-list "disabled")))
          (let [input @webapp-env-out-field
                input-val (common/field-val input)
                directory (common/field-val @directory-field)
                directory (if (= "" directory) nil directory)
                out-file (.showSaveDialog
                          dialog
                          #js {:filters #js [#js {:name "javascript file"
                                                  :extensions #js ["js"]}]
                               :defaultPath (or input-val directory
                                                replique-dir)})]
            (aset input "value" (or out-file nil))
            (.dispatchEvent input (js/Event. "change" #js {:bubbles true})))
          (and (.contains class-list "new-browser-env-main")
               (not (.contains class-list "disabled")))
          (let [input @browser-env-main-field
                input-val (common/field-val @browser-env-main-field)
                directory (common/field-val @directory-field)
                directory (if (= "" directory) nil directory)]
            (->> (.showOpenDialog
                  dialog #js {:filters #js [#js {:name "clojurescript file"
                                                 :extensions
                                                 #js ["cljs" "cljc"]}]
                              :defaultPath (or input-val directory
                                               replique-dir)})
                 first
                 (aset input "value"))
            (.dispatchEvent input (js/Event. "change" #js {:bubbles true})))
          (and (.contains class-list "new-webapp-env-main")
               (not (.contains class-list "disabled")))
          (let [input @webapp-env-main-field
                input-val (common/field-val input)
                directory (common/field-val @directory-field)]
            (->> (.showOpenDialog
                  dialog #js {:filters #js [#js {:name "clojurescript file"
                                                 :extensions
                                                 #js ["cljs" "cljc"]}]
                              :defaultPath (or input-val directory)})
                 first
                 (aset input "value"))
            (.dispatchEvent input (js/Event. "change" #js {:bubbles true})))
          :else nil)))

(defn edit-repl-changed [edit-repl e]
  (let [target (aget e "target")
        class-list (aget target "classList")
        id (:repl-id @core/state)]
    (cond (= (.getAttribute target "name") "repl-type")
          (do (refresh-cljs-env @core/state edit-repl)
              (swap! core/state assoc :dirty true))
          (= (.getAttribute target "name") "cljs-env")
          (do (refresh-cljs-env @core/state edit-repl true)
              (swap! core/state assoc :dirty true))
          (.contains class-list "random-port")
          (let [port-node (.querySelector edit-repl ".port")
                random-port (aget target "checked")]
            (dom/replaceNode (->> (update-in
                                   @core/state [:repls id] assoc
                                   :random-port random-port)
                                  port-tmpl
                                  utils/make-node)
                             port-node)
            (swap! core/state assoc :dirty true))
          (.contains class-list "browser-env-random-port")
          (let [port-node (.querySelector edit-repl ".browser-env-port")
                random-port (aget target "checked")]
            (if random-port
              (.setAttribute port-node "disabled" "disabled")
              (.removeAttribute port-node "disabled"))
            (swap! core/state assoc :dirty true))
          (.contains class-list "webapp-env-random-port")
          (let [port-node @webapp-env-port-field
                random-port (aget target "checked")]
            (if random-port
              (.setAttribute port-node "disabled" "disabled")
              (.removeAttribute port-node "disabled"))
            (swap! core/state assoc :dirty true))
          (.contains class-list "field")
          (swap! core/state assoc :dirty true)
          :else nil)))

(defonce transient-state (atom nil))

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
