(ns ewen.replique.ui.settings
  (:require [hiccup.core :refer-macros [html]]
            [hiccup.def :refer-macros [defhtml]]
            [goog.dom :as dom]
            [goog.events :as events]
            [ewen.replique.ui.remote :refer [remote]]
            [ewen.replique.ui.core :as core]
            [ewen.replique.ui.utils :as utils]
            [ewen.replique.ui.notifications :as notif]
            [ewen.replique.ui.common :as common]
            [cljs.nodejs :as node]))

(def fs (node/require "fs"))
(def https (node/require "https"))
(def dialog (.require remote "dialog"))
(def replique-root-dir (.getGlobal remote "repliqueRootDir"))

(def clj-versions #{"1.8.0"})
(def clj-file-names {"1.8.0" "clojure-1.8.0.jar"})
(def current-clj-v "1.8.0")
(def clj-file-name (get clj-file-names current-clj-v))
(def clj-urls {"1.8.0" "https://repo1.maven.org/maven2/org/clojure/clojure/1.8.0/clojure-1.8.0.jar"})
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

(def handler (utils/handler (namespace ::e)))

(defonce current-settings (atom nil))

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
        id (utils/next-id)
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

(defn ^:export download-clj-jar-clicked [e]
  (download-jar (get clj-urls current-clj-v)
                (get clj-paths current-clj-v)
                clj-file-name))

(defn ^:export download-cljs-jar-clicked [e]
  (download-jar (get cljs-urls current-cljs-v)
                (get cljs-paths current-cljs-v)
                cljs-file-name))

(declare clj-jar-tmpl)
(declare cljs-jar-tmpl)
(declare lein-tmpl)

(defn ^:export new-clj-jar-clicked [e]
  (let [clj-jar-node (.querySelector js/document ".clj-jar")
        clj-jar-file (->> (.showOpenDialog
                           dialog #js {:filters #js [#js {:name "jar files"
                                                          :extensions #js ["jar"]}]
                                       :defaultPath replique-root-dir})
                          first)
        settings (swap! current-settings assoc
                        :custom-clj-jar clj-jar-file)]
    (-> (clj-jar-tmpl settings)
        utils/make-node
        (dom/replaceNode clj-jar-node))
    (common/save-set-dirty)))

(defn ^:export new-cljs-jar-clicked [e]
  (let [cljs-jar-node (.querySelector js/document ".cljs-jar")
        cljs-jar-file (->> (.showOpenDialog
                           dialog #js {:filters
                                       #js [#js {:name "jar files"
                                                 :extensions #js ["jar"]}]
                                       :defaultPath replique-root-dir})
                          first)
        settings (swap! current-settings assoc
                        :custom-cljs-jar cljs-jar-file)]
    (-> (cljs-jar-tmpl settings)
        utils/make-node
        (dom/replaceNode cljs-jar-node))
    (common/save-set-dirty)))

(defn ^:export clj-source-changed [e val]
  (let [clj-jar-node (.querySelector js/document ".clj-jar")
        settings (swap! current-settings assoc :clj-jar-source val)]
    (-> (clj-jar-tmpl settings)
        utils/make-node
        (dom/replaceNode clj-jar-node))
    (common/save-set-dirty)))

(defn ^:export downloaded-changed [e k]
  (let [k (keyword k)
        val (.-value (.-target e))]
    (swap! current-settings assoc k val)
    (common/save-set-dirty)))

(defn ^:export cljs-source-changed [e val]
  (let [cljs-jar-node (.querySelector js/document ".cljs-jar")
        settings (swap! current-settings assoc :cljs-jar-source val)]
    (-> (cljs-jar-tmpl settings)
        utils/make-node
        (dom/replaceNode cljs-jar-node))
    (common/save-set-dirty)))

(defhtml clj-jar-tmpl [{:keys [clj-jar-source downloaded-clj-jar
                               custom-clj-jar]}]
  [:fieldset.clj-jar
   [:legend "Clojure jar"]
   [:label {:for "clj-embedded-source"} "Embedded Clojure jar"]
   [:input.field (merge {:type "radio"
                         :name "clj-source"
                         :id "clj-embedded-source"
                         :value "embedded"
                         :onchange (handler
                                    'clj-source-changed "embedded")}
                        (if (= "embedded" clj-jar-source)
                          {:checked true}
                          nil))]
   [:label {:for "clj-custom-source"} "Custom Clojure jar"]
   [:input.field (merge {:type "radio"
                         :name "clj-source"
                         :id "clj-custom-source"
                         :value "custom"
                         :onchange (handler
                                    'clj-source-changed "custom")}
                        (when (= "custom" clj-jar-source)
                          {:checked true}))] [:br]
   [:select.field.select-downloaded
    (if (= "custom" clj-jar-source)
      {:disabled true}
      {:onchange (handler 'downloaded-changed "downloaded-clj-jar")})
    (for [f (conj (clj-jar-files) downloaded-clj-jar nil)]
      [:option (merge {:value f}
                      (when (= f downloaded-clj-jar) {:selected true}))
       f])]
   [:a (merge {:href "#"}
              (if (= "custom" clj-jar-source)
                {:class "disabled button download-clj-jar"}
                {:class "button download-clj-jar"
                 :onclick (handler 'download-clj-jar-clicked)}))
    "Download Clojure jar"] [:br]
   [:input.field.custom-clj-jar
    (merge {:type "text" :readonly true
            :value custom-clj-jar}
           (when (= "embedded" clj-jar-source)
             {:disabled true}))]
   [:a (merge {:href "#"}
              (if (= "embedded" clj-jar-source)
                {:class "disabled button new-clj-jar"}
                {:class "button new-clj-jar"
                 :onclick (handler 'new-clj-jar-clicked)}))
    "Select Clojure jar"]])

(defhtml cljs-jar-tmpl [{:keys [cljs-jar-source downloaded-cljs-jar
                                custom-cljs-jar]}]
  [:fieldset.cljs-jar
   [:legend "Clojurescript jar"]
   [:label {:for "cljs-embedded-source"} "Embedded Clojurescript jar"]
   [:input.field (merge {:type "radio"
                         :name "cljs-source"
                         :id "cljs-embedded-source"
                         :value "embedded"
                         :onchange (handler
                                    'cljs-source-changed "embedded")}
                        (if (= "embedded" cljs-jar-source)
                          {:checked true}
                          nil))]
   [:label {:for "cljs-custom-source"} "Custom Clojurescript jar"]
   [:input.field (merge {:type "radio"
                         :name "cljs-source"
                         :id "cljs-custom-source"
                         :value "custom"
                         :onchange (handler
                                    'cljs-source-changed "custom")}
                        (when (= "custom" cljs-jar-source)
                          {:checked true}))] [:br]
   [:select.field.select-downloaded
    (if (= "custom" cljs-jar-source)
      {:disabled true}
      {:onchange (handler 'downloaded-changed "downloaded-cljs-jar")})
    (for [f (conj (cljs-jar-files) downloaded-cljs-jar nil)]
      [:option (merge {:value f}
                      (when (= f downloaded-cljs-jar) {:selected true}))
       f])]
   [:a (merge {:href "#"}
              (if (= "custom" cljs-jar-source)
                {:class "disabled button download-cljs-jar"}
                {:class "button download-cljs-jar"
                 :onclick (handler 'download-cljs-jar-clicked)}))
    "Download Clojurescript jar"] [:br]
   [:input.field.custom-cljs-jar
    (merge {:type "text" :readonly true
            :value custom-cljs-jar}
           (when (= "embedded" cljs-jar-source)
             {:disabled true}))]
   [:a (merge {:href "#"}
              (if (= "embedded" cljs-jar-source)
                {:class "disabled button new-cljs-jar"}
                {:class "button new-cljs-jar"
                 :onclick (handler 'new-cljs-jar-clicked)}))
    "Select Clojurescript jar"]])

(defn ^:export new-lein-script-clicked [e]
  (let [lein-node (.querySelector js/document ".lein")
        lein-script (->> (.showOpenDialog
                          dialog #js {:defaultPath replique-root-dir})
                         first)
        settings (swap! current-settings assoc
                        :custom-lein-script lein-script)]
    (-> (lein-tmpl settings)
        utils/make-node
        (dom/replaceNode lein-node))
    (common/save-set-dirty)))

(defn ^:export lein-source-changed [e val]
  (let [lein-node (.querySelector js/document ".lein")
        settings (swap! current-settings assoc :lein-source val)]
    (-> (lein-tmpl settings)
        utils/make-node
        (dom/replaceNode lein-node))
    (common/save-set-dirty)))

(defhtml lein-tmpl [{:keys [lein-source custom-lein-script]}]
  [:fieldset.lein
   [:legend "Leiningen"]
   [:label {:for "lein-embedded-source"} "Embedded Leiningen script"]
   [:input.field (merge {:type "radio"
                         :name "lein-source"
                         :id "lein-embedded-source"
                         :value "embedded"
                         :onchange (handler
                                    'lein-source-changed "embedded")}
                        (if (= "embedded" lein-source)
                          {:checked true}
                          nil))]
   [:label {:for "lein-custom-source"} "Custom Leiningen script"]
   [:input.field (merge {:type "radio"
                         :name "lein-source"
                         :id "lein-custom-source"
                         :value "custom"
                         :onchange (handler
                                    'lein-source-changed "custom")}
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
                {:class "button new-lein-script"
                 :onclick (handler 'new-lein-script-clicked)}))
    "Select Leiningen script"]])

(defhtml settings-tmpl [settings]
  (html [:div#settings
         (common/back-button "dashboard")
         [:form
          (common/save-button 'ewen.replique.ui.settings.save_settings)
          (clj-jar-tmpl settings)
          (cljs-jar-tmpl settings)
          (lein-tmpl settings)]]))

(defn ^:export save-settings [e]
  (swap! core/state assoc :settings @current-settings)
  (try
    (do (core/persist-state @core/state)
        true)
    (catch js/Error e
      (.log js/console (str "Error while saving settings: " e))
      (notif/single-notif
       {:type :err
        :msg (str "Error while saving settings")})
      false)))

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
 (fn [root {:keys [view settings]}]
   (if (= :settings view)
     (let [current-settings (reset! current-settings settings)
           node (utils/replace-or-append
                 root "#settings"
                 (dom/htmlToDocumentFragment
                  (settings-tmpl current-settings)))])
     (when-let [node (.querySelector root "#settings")]
       (dom/removeNode node)))))
