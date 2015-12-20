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
(def replique-root-dir (.getGlobal remote "repliqueRootDir"))

(def clj-versions #{"1.7"})
(def clj-file "clojure-1.7.0.jar")
(def clj-urls {"1.7" "https://repo1.maven.org/maven2/org/clojure/clojure/1.7.0/clojure-1.7.0.jar"})
(def clj-paths
  {"1.7" (str replique-root-dir "/runnables/clojure-1.7.0.jar")})
(def current-clj-v "1.7")

#_(defn downloaded-clj-jars []
  (->> (.readdirSync fs (str replique-root-dir "/runnables"))
       (apply vec)
       (filter )))

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

(defn start-progress [resp id]
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
                         :file clj-file }
                        id)))
                  1000)]
             (vswap! length + (aget chunk "length"))
             (compute-progress))))))

(defn download-clj-jar []
  (let [url (get clj-urls current-clj-v)
        path (get clj-paths current-clj-v)
        file (.createWriteStream fs path #js {:flags "wx"})
        id (.getNextUniqueId utils/id-gen)
        req (.get https url
                  (fn [resp]
                    (let [status (aget resp "statusCode")]
                      (if (not= status 200)
                        (do
                          (.log js/console "Error while downloading the Clojure jar. Recevied HTTP status " status)
                          (notif/single-notif
                           {:type :err-msg
                            :msg "Error while downloading the Clojure jar"})
                          (.unlink fs path))
                        (do
                          (start-progress resp id)
                          (.pipe resp file))))))]
    (.on req "error"
         (fn [err]
           (.log js/console "Error while downloading the Clojure jar. Recevied error " err)
           (notif/single-notif
            {:type :err-msg
             :msg "Error while downloading the Clojure jar"})
           (.unlink fs path)))
    (.on req "timeout"
         (fn [err]
           (prn (str "Req timed out" err))
           (.abort req)
           (.unlink fs path)))
    (.on file "finish"
         (fn []
           (notif/clear-notif id)
           (notif/single-notif
            {:type :success
             :msg (str clj-file " successfully downloaded")})
           (.close file)))
    (.on file "error"
         (fn [err]
           (if (= (aget err "code") "EEXIST")
             (notif/single-notif
              {:type :err
               :msg "The most recent Clojure version has already been downloaded"})
             (do (prn "File error " err)
                 (.unlink fs path)))))))

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
    (cond (.contains class-list "download-clj-jar")
          (download-clj-jar)
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
