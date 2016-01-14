(ns ewen.replique.ui.settings
  (:require [hiccup.core]
            [goog.dom :as dom]
            [goog.events :as events]
            [ewen.replique.ui.remote :refer [remote]]
            [ewen.replique.ui.core :as core]
            [ewen.replique.ui.utils :as utils]
            [ewen.replique.ui.notifications :as notif]
            [cljs.nodejs :as node])
  (:require-macros [hiccup.core :refer [html]]
                   [hiccup.def :refer [defhtml]]))

(def fs (node/require "fs"))
(def https (node/require "https"))
(def dialog (.require remote "dialog"))
(def replique-root-dir (.getGlobal remote "repliqueRootDir"))

(def clj-versions #{"1.8.0-RC5"})
(def clj-file-names {"1.8.0-RC5" "clojure-1.8.0-RC5.jar"})
(def current-clj-v "1.8.0-RC5")
(def clj-file-name (get clj-file-names current-clj-v))
(def clj-urls {"1.8.0-RC5" "https://repo1.maven.org/maven2/org/clojure/clojure/1.8.0-RC5/clojure-1.8.0-RC5.jar"})
(def clj-paths (->> (map (fn [[v f]]
                           [v (str replique-root-dir "/runnables/" f)])
                         clj-file-names)
                    (into {})))

(def cljs-versions #{"1.7.228"})
(def cljs-file-names {"1.7.228" "cljs-1.7.228.jar"})
(def current-cljs-v "1.7.228")
(def cljs-file-name (get cljs-file-names current-cljs-v))
(def cljs-urls {"1.7.228" "https://github.com/clojure/clojurescript/releases/download/r1.7.228/cljs.jar"})
(def cljs-paths (->> (map (fn [[v f]]
                           [v (str replique-root-dir "/runnables/" f)])
                         cljs-file-names)
                    (into {})))

(defn runnable-files []
  (->> (.readdirSync fs (str replique-root-dir "/runnables"))
       (apply vector)))

(defn clj-jar-files []
  (let [clj-file-names (into #{} (vals clj-file-names))]
    (->> (runnable-files)
         (filter #(get clj-file-names %))
         (into #{}))))

(defn cljs-jar-files []
  (let [cljs-file-names (into #{} (vals cljs-file-names))]
    (->> (runnable-files)
         (filter #(get cljs-file-names %))
         (into #{}))))

(defhtml clj-jar-tmpl [{:keys [clj-jar-source downloaded-clj-jar
                               custom-clj-jar]}]
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
      {:disabled true})
    (for [f (conj (clj-jar-files) downloaded-clj-jar nil)]
      [:option (merge {:value f}
                      (when (= f downloaded-clj-jar) {:selected true}))
       f])]
   [:a (merge {:href "#"}
              (if (= "custom" clj-jar-source)
                {:class "disabled button download-clj-jar"}
                {:class "button download-clj-jar"}))
    "Download Clojure jar"] [:br]
   [:input.field.custom-clj-jar
    (merge {:type "text" :readonly true
            :value custom-clj-jar}
           (when (= "embedded" clj-jar-source)
             {:disabled true}))]
   [:a (merge {:href "#"}
              (if (= "embedded" clj-jar-source)
                {:class "disabled button new-clj-jar"}
                {:class "button new-clj-jar"}))
    "Select Clojure jar"]])

(defhtml cljs-jar-tmpl [{:keys [cljs-jar-source downloaded-cljs-jar
                                custom-cljs-jar]}]
  [:fieldset.cljs-jar
   [:legend "Clojurescript jar"]
   [:label {:for "cljs-embedded-source"} "Embedded Clojurescript jar"]
   [:input.field (merge {:type "radio"
                         :name "cljs-source"
                         :id "cljs-embedded-source"
                         :value "embedded"}
                        (if (= "embedded" cljs-jar-source)
                          {:checked true}
                          nil))]
   [:label {:for "cljs-custom-source"} "Custom Clojurescript jar"]
   [:input.field (merge {:type "radio"
                         :name "cljs-source"
                         :id "cljs-custom-source"
                         :value "custom"}
                        (when (= "custom" cljs-jar-source)
                          {:checked true}))] [:br]
   [:select.field.select-downloaded
    (when (= "custom" cljs-jar-source)
      {:disabled true})
    (for [f (conj (cljs-jar-files) downloaded-cljs-jar nil)]
      [:option (merge {:value f}
                      (when (= f downloaded-cljs-jar) {:selected true}))
       f])]
   [:a (merge {:href "#"}
              (if (= "custom" cljs-jar-source)
                {:class "disabled button download-cljs-jar"}
                {:class "button download-cljs-jar"}))
    "Download Clojurescript jar"] [:br]
   [:input.field.custom-cljs-jar
    (merge {:type "text" :readonly true
            :value custom-cljs-jar}
           (when (= "embedded" cljs-jar-source)
             {:disabled true}))]
   [:a (merge {:href "#"}
              (if (= "embedded" cljs-jar-source)
                {:class "disabled button new-cljs-jar"}
                {:class "button new-cljs-jar"}))
    "Select Clojurescript jar"]])

(defhtml lein-tmpl [{:keys [lein-source custom-lein-script]}]
  [:fieldset.lein
   [:legend "Leiningen"]
   [:label {:for "lein-embedded-source"} "Embedded Leiningen script"]
   [:input.field (merge {:type "radio"
                         :name "lein-source"
                         :id "lein-embedded-source"
                         :value "embedded"}
                        (if (= "embedded" lein-source)
                          {:checked true}
                          nil))]
   [:label {:for "lein-custom-source"} "Custom Leiningen script"]
   [:input.field (merge {:type "radio"
                         :name "lein-source"
                         :id "lein-custom-source"
                         :value "custom"}
                        (when (= "custom" lein-source)
                          {:checked true}))] [:br]
   [:input.field.custom-lein-script
    (merge {:type "text" :readonly true
            :value custom-lein-script}
           (when (= "embedded" lein-source)
             {:disabled true}))]
   [:a (merge {:href "#"}
              (if (= "embedded" lein-source)
                {:class "disabled button new-lein-script"}
                {:class "button new-lein-script"}))
    "Select Leiningen script"]])

(defn start-progress [resp id file-name]
  (let [total-length (-> (aget resp "headers")
                         (aget "content-length")
                         js/parseInt)
        length (volatile! 0)
        update-percent (fn [] (/ (* 100 @length) total-length))
        percent (volatile! (update-percent))]
    (.on resp "data"
         (fn [chunk]
           (let [compute-progress
                 (utils/throttle
                  #(let [prev-percent @percent
                         updated-percent (update-percent)]
                     (when (> (- updated-percent prev-percent) 5)
                       (vreset! percent updated-percent)
                       (notif/notif-with-id
                        {:type :download
                         :progress (js/Math.floor updated-percent)
                         :file file-name }
                        id)))
                  1000)]
             (vswap! length + (aget chunk "length"))
             (compute-progress))))))

(defn download-jar [url path file-name]
  (let [file (.createWriteStream fs path #js {:flags "wx"})
        id (.getNextUniqueId utils/next-id)
        req (.get https url
                  (fn [resp]
                    (let [status (aget resp "statusCode")]
                      (cond
                        (= status 302)
                        (let [location (-> (aget resp "headers")
                                           (aget "location"))]
                          (.unlink fs path)
                          (download-jar location path file-name))
                        (not= status 200)
                        (do
                          (.log js/console (str "Error while downloading the file: " file-name ". Recevied HTTP status " status))
                          (notif/single-notif
                           {:type :err
                            :msg (str "Error while downloading the file: " file-name)})
                          (.unlink fs path))
                        :else
                        (do
                          (start-progress resp id file-name)
                          (.pipe resp file))))))]
    (.on req "error"
         (fn [err]
           (.log js/console (str "Error while downloading the file: " file-name ". Recevied error " err))
           (notif/clear-notif id)
           (notif/single-notif
            {:type :err
             :msg (str "Error while downloading the file: " file-name)})
           (.unlink fs path)))
    (.on req "timeout"
         (fn [err]
           (.log js/console (str "Error while downloading the file: " file-name ". Timed out with error " err))
           (notif/clear-notif id)
           (notif/single-notif
            {:type :err
             :msg (str "Error while downloading the file: " file-name)})
           (.abort req)
           (.unlink fs path)))
    (.on file "finish"
         ;; Refresh only the field concerned by the downloaded file
         ;; since the user may have changed other fields meanwhile
         (fn []
           (notif/clear-notif id)
           (notif/single-notif
            {:type :success
             :msg (str file-name " successfully downloaded")})
           (.close file)
           (core/refresh-view @core/state)))
    (.on file "error"
         (fn [err]
           (if (= (aget err "code") "EEXIST")
             (do
               (.abort req)
               (notif/single-notif
                {:type :err
                 :msg "The most recent version has already been downloaded"}))
             (do (.log js/console (str "Error while downloading the file: " file-name ". Recevied error " err))
                 (notif/clear-notif id)
                 (notif/single-notif
                  {:type :err
                   :msg (str "Error while downloading the file: " file-name)})
                 (.unlink fs path)))))))

(defhtml save-tmpl [dirty]
  [:a.button.save
   (merge {:href "#"}
          (if dirty
            {:class "button save"}
            {:class "button save disabled"}))
   "Save"])

(defhtml settings [{settings :settings dirty :dirty}]
  (html [:div#settings
         [:a.back-nav {:href "#"}]
         [:form
          (save-tmpl dirty)
          (clj-jar-tmpl settings)
          (cljs-jar-tmpl settings)
          (lein-tmpl settings)]]))

(def settings-field-readers (atom #{}))

(swap! settings-field-readers conj
       (fn []
         (let [checked-source
               (.querySelector
                js/document
                "#settings input[name=\"clj-source\"]:checked")]
           [:clj-jar-source (.getAttribute checked-source "value")])))

(swap! settings-field-readers conj
       (fn []
         (let [checked-source
               (.querySelector
                js/document
                "#settings input[name=\"cljs-source\"]:checked")]
           [:cljs-jar-source (.getAttribute checked-source "value")])))

(swap! settings-field-readers conj
       (fn []
         (let [checked-source
               (.querySelector
                js/document
                "#settings input[name=\"lein-source\"]:checked")]
           [:lein-source (.getAttribute checked-source "value")])))

(swap! settings-field-readers conj
       (fn []
         (let [downloaded-jar
               (.querySelector
                js/document
                "#settings .clj-jar .select-downloaded option:checked")]
           (if downloaded-jar
             [:downloaded-clj-jar (let [val (.getAttribute
                                             downloaded-jar "value")]
                         (if (= "" val)
                           nil val))]
             [:downloaded-clj-jar nil]))))

(swap! settings-field-readers conj
       (fn []
         (let [custom-jar
               (-> (.querySelector js/document "#settings .custom-clj-jar")
                   (aget "value"))]
           (if (or (nil? custom-jar) (= custom-jar ""))
             [:custom-clj-jar nil]
             [:custom-clj-jar custom-jar]))))

(swap! settings-field-readers conj
       (fn []
         (let [downloaded-jar
               (.querySelector
                js/document
                "#settings .cljs-jar .select-downloaded option:checked")]
           (if downloaded-jar
             [:downloaded-cljs-jar (let [val (.getAttribute
                                              downloaded-jar "value")]
                                    (if (= "" val)
                                      nil val))]
             [:downloaded-cljs-jar nil]))))

(swap! settings-field-readers conj
       (fn []
         (let [custom-jar
               (-> (.querySelector js/document "#settings .custom-cljs-jar")
                   (aget "value"))]
           (if (or (nil? custom-jar) (= custom-jar ""))
             [:custom-cljs-jar nil]
             [:custom-cljs-jar custom-jar]))))

(swap! settings-field-readers conj
       (fn []
         (let [custom-jar
               (-> (.querySelector
                    js/document "#settings .custom-lein-script")
                   (aget "value"))]
           (if (or (nil? custom-jar) (= custom-jar ""))
             [:custom-lein-script nil]
             [:custom-lein-script custom-jar]))))

(defn save-settings []
  (let [{:keys [settings]} @core/state
        fields (->> (for [field-reader @settings-field-readers]
                      (field-reader))
                    (into {}))
        new (merge settings fields)]
    (swap! core/state assoc :settings new)
    (swap! core/state dissoc :dirty)
    (try
      (core/persist-state @core/state)
      (catch js/Error e
        (.log js/console (str "Error while saving settings: " e))
        (notif/single-notif
         {:type :err
          :msg (str "Error while saving settings")})))))

(defn back-clicked []
  (swap! core/state assoc
         :view :dashboard
         :dirty false))

(defn settings-clicked [settings-node e]
  (let [class-list (-> (aget e "target")
                       (aget "classList"))]
    (cond (.contains class-list "save")
          (save-settings)
          (.contains class-list "download-clj-jar")
          (download-jar (get clj-urls current-clj-v)
                        (get clj-paths current-clj-v)
                        clj-file-name)
          (.contains class-list "download-cljs-jar")
          (download-jar (get cljs-urls current-cljs-v)
                        (get cljs-paths current-cljs-v)
                        cljs-file-name)
          (.contains class-list "new-clj-jar")
          (let [input (.querySelector
                       settings-node ".custom-clj-jar")]
            (->> (.showOpenDialog
                  dialog #js {:filters #js [#js {:name "jar files"
                                                 :extensions #js ["jar"]}]
                              :defaultPath replique-root-dir})
                 first
                 (aset input "value"))
            (.dispatchEvent input (js/Event. "change" #js {:bubbles true})))
          (.contains class-list "new-cljs-jar")
          (let [input (.querySelector
                       settings-node ".custom-cljs-jar")]
            (->> (.showOpenDialog
                  dialog #js {:filters #js [#js {:name "jar files"
                                                 :extensions #js ["jar"]}]
                              :defaultPath replique-root-dir})
                 first
                 (aset input "value"))
            (.dispatchEvent input (js/Event. "change" #js {:bubbles true})))
          (.contains class-list "new-lein-script")
          (let [input (.querySelector
                       settings-node ".custom-lein-script")]
            (->> (.showOpenDialog
                  dialog #js {:defaultPath replique-root-dir})
                 first
                 (aset input "value"))
            (.dispatchEvent input (js/Event. "change" #js {:bubbles true})))
          :else nil)))

(defn settings-changed [settings-node e]
  (let [target (aget e "target")
        class-list (aget target "classList")]
    (cond (= "clj-source" (.getAttribute target "name"))
          (let [source (.getAttribute target "value")
                clj-jar-node (.querySelector settings-node ".clj-jar")]
            (dom/replaceNode (utils/make-node
                              (clj-jar-tmpl
                               (->
                                (:settings @core/state)
                                (assoc :clj-jar-source source))))
                             clj-jar-node)
            (swap! core/state assoc :dirty true))
          (= "cljs-source" (.getAttribute target "name"))
          (let [source (.getAttribute target "value")
                cljs-jar-node (.querySelector settings-node ".cljs-jar")]
            (dom/replaceNode (utils/make-node
                              (cljs-jar-tmpl
                               (->
                                (:settings @core/state)
                                (assoc :cljs-jar-source source))))
                             cljs-jar-node)
            (swap! core/state assoc :dirty true))
          (= "lein-source" (.getAttribute target "name"))
          (let [source (.getAttribute target "value")
                lein-node (.querySelector settings-node ".lein")]
            (dom/replaceNode (utils/make-node
                              (lein-tmpl
                               (->
                                (:settings @core/state)
                                (assoc :lein-source source))))
                             lein-node)
            (swap! core/state assoc :dirty true))
          (.contains class-list "field")
          (swap! core/state assoc :dirty true)
          :else nil)))

(defn get-clj-jar [{{:keys [clj-jar-source downloaded-clj-jar
                            custom-clj-jar]} :settings}]
  (case clj-jar-source
    "embedded" (when downloaded-clj-jar
                 (str replique-root-dir "/runnables/" downloaded-clj-jar))
    "custom" (if (= custom-clj-jar "") nil custom-clj-jar)
    :else nil))

(defn get-cljs-jar [{{:keys [cljs-jar-source downloaded-cljs-jar
                             custom-cljs-jar]} :settings}]
  (case cljs-jar-source
    "embedded" (when downloaded-cljs-jar
                 (str replique-root-dir "/runnables/" downloaded-cljs-jar))
    "custom" (if (= custom-cljs-jar "") nil custom-cljs-jar)
    :else nil))

(defn get-lein-script [{{:keys [lein-source custom-lein-script]} :settings}]
  (case lein-source
    "embedded" (str replique-root-dir "/runnables/lein")
    "custom" (if (= custom-lein-script "") nil custom-lein-script)
    :else nil))

(swap!
 core/refresh-view-fns assoc :settings
 (fn [root {:keys [view] :as state}]
   (if (= :settings view)
     (let [node (utils/replace-or-append
                 root "#settings"
                 (dom/htmlToDocumentFragment
                  (settings state)))]
       (events/listen (.querySelector node ".back-nav")
                      events/EventType.CLICK back-clicked)
       (events/listen (.querySelector node "#settings form")
                      events/EventType.CLICK (partial
                                              settings-clicked node))
       (events/listen (.querySelector node "#settings form")
                      events/EventType.CHANGE (partial
                                               settings-changed node)))
     (when-let [node (.querySelector root "#settings")]
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
