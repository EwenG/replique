(ns ewen.replique.ui.dashboard
  (:require [hiccup.core :refer-macros [html]]
            [hiccup.def :refer-macros [defhtml]]
            [hiccup.page :refer [include-css]]
            [goog.dom :as dom]
            [goog.events :as events]
            [cljs.reader :as reader]
            [ewen.replique.ui.remote :refer [remote]]
            [cljs.nodejs :as node]
            [ewen.replique.ui.core :as core]
            [ewen.replique.ui.utils :as utils]
            [ewen.replique.ui.edit-repl]
            [ewen.replique.ui.settings :as settings]
            [ewen.replique.ui.shortcuts]
            [ewen.replique.ui.notifications :as notif]
            [ewen.ddom.core :as ddom]
            [cljs-uuid-utils.core :as uuid])
  (:import [goog.string format]))

(def replique-dir (.getGlobal remote "repliqueRootDir"))

(def spawn (aget (node/require "child_process") "spawn"))
(def tree-kill (.require remote "tree-kill"))
(def fs (node/require "fs"))
(def net (node/require "net"))

(def handler (ddom/handler (namespace ::e)))

(defn ^:export add-new-repl []
  (let [id (uuid/uuid-string (uuid/make-random-uuid))]
    (swap! core/state assoc-in [:repls id]
           {:directory nil
            :type :clj :cljs-env :browser
            :browser-env-random-port true
            :webapp-env-random-port true
            :random-port true})
    (try
      (core/persist-state @core/state)
      (catch js/Error e
        (.log js/console (str "Error while saving settings: " e))
        (notif/single-notif
         {:type :err
          :msg (str "Error while saving settings")})))))

(defhtml new-repl []
  [:a.dashboard-item.new-repl
   {:href "#"
    :onclick (handler 'add-new-repl)}
   "New REPL"])

(defn ^:export delete-clicked [e id]
  (let [overview (.querySelector
                  js/document (format "[data-repl-id=\"%s\"]" id))
        {:keys [proc]} (get (:repls @core/state) id)]
    (when proc (tree-kill (aget proc "pid")))
    (swap! core/state update-in [:repls] dissoc id)
    (try
      (core/persist-state @core/state)
      (catch js/Error e
        (.log js/console (str "Error while saving settings: " e))
        (notif/single-notif
         {:type :err
          :msg (str "Error while saving settings")})))))

(defn ^:export edit-clicked [e id]
  (swap! core/state assoc :repl-id id :view :edit-repl))

(defn is-lein-project [{:keys [repls] :as state} id]
  (let [{:keys [directory]} (get repls id)]
    (and directory (utils/file-exists (str directory "/project.clj")))))

(defn maybe-start-repl-error [{:keys [repls] :as state} id]
  (let [{:keys [directory type cljs-env browser-env-out webapp-env-out]}
        (get repls id)
        clj-jar (settings/get-clj-jar state)
        cljs-jar (settings/get-cljs-jar state)]
    (cond
      (or (= "" directory) (nil? directory))
      {:type :err
       :msg "The REPL directory has not been configured"}
      (and (is-lein-project state id)
           (not (settings/get-lein-script state)))
      {:type :err
       :msg "Leiningen script has not been configured"}
      (and (is-lein-project state id)
           (not (utils/file-exists (settings/get-lein-script state))))
      {:type :err
       :msg "Invalid Leiningen script"}
      (nil? clj-jar)
      {:type :err
       :msg "Clojure jar has not been configured"}
      (not (utils/file-exists clj-jar))
      {:type :err
       :msg "Invalid Clojure jar"}
      (and (= :cljs type) (nil? cljs-jar))
      {:type :err
       :msg "Clojurescript jar has not been configured"}
      (and (= :cljs type) (not (utils/file-exists cljs-jar)))
      {:type :err
       :msg "Invalid Clojurescript jar"}
      (and (= :cljs type) (= :browser cljs-env) (= nil browser-env-out))
      {:type :err
       :msg "Output file for Clojurescript browser environment has not been configured"}
      (and (= :cljs type) (= :webapp cljs-env) (= nil webapp-env-out))
      {:type :err
       :msg "Output file for Clojurescript web application environment has not been configured"}
      :else nil)))

(defn repl-cmd-raw [{:keys [repls settings] :as state} id]
  (let [{:keys [directory type cljs-env port random-port
                browser-env-port browser-env-random-port
                webapp-env-port webapp-env-random-port]
         :as repl}
        (get repls id)
        cp (if (= :cljs type)
              (str (settings/get-clj-jar state) ":"
                   (settings/get-cljs-jar state) ":"
                   (format "%s/src" replique-dir))
              (str (settings/get-clj-jar state) ":"
                   (format "%s/src" replique-dir)))
        port (if random-port 0 port)
        browser-env-port (if browser-env-random-port 0 browser-env-port)
        webapp-env-port (if webapp-env-random-port 0 webapp-env-port)
        cljs-env (if (= :clj type) nil cljs-env)
        cmd-args #js ["-cp" cp "clojure.main"
                      "-m" "ewen.replique.main"
                      (-> (merge repl {:port port :cljs-env cljs-env
                                       :browser-env-port browser-env-port
                                       :webapp-env-port webapp-env-port
                                       :sass-bin (:sass-bin settings)})
                          str)]]
    ["java" cmd-args #js {:cwd directory}]))

(defn repl-cmd-lein [{:keys [repls settings] :as state} id]
  (let [{:keys [directory type cljs-env port random-port
                browser-env-port browser-env-random-port
                webapp-env-port webapp-env-random-port]
         :as repl}
        (get repls id)
        port (if random-port 0 port)
        browser-env-port (if browser-env-random-port 0 browser-env-port)
        webapp-env-port (if webapp-env-random-port 0 webapp-env-port)
        cljs-env (if (= :clj type) nil cljs-env)
        cmd-args #js ["update-in" ":source-paths" "conj"
                      (pr-str (format "%s/src" replique-dir))
                      "--" "run" "-m" "ewen.replique.main/-main"
                      (-> (merge repl {:port port :cljs-env cljs-env
                                       :browser-env-port browser-env-port
                                       :webapp-env-port webapp-env-port
                                       :sass-bin (:sass-bin settings)})
                          str)]]
    [(settings/get-lein-script state)
     cmd-args #js {:cwd directory}]))

;; lein update-in :source-paths conj "\"/home/egr/electron/resources/replique/src\"" -- run -m ewen.replique.main/-main "{:type :clj :port 9001}"

(defn read-port-desc [directory]
  (try
    (->>
     (str directory "/.replique-port")
     (#(.readFileSync fs % "utf-8"))
     (reader/read-string))
    (catch js/Error e
      (.log js/console e)
      nil)))

(defn stop-repl [overview {:keys [repls] :as state} id]
  (let [{:keys [proc repl-port directory]} (get repls id)]
    (if repl-port
      (let [client (.connect net #js {:port repl-port})]
        (.on client "connect"
             (fn []
               (.write client (format "(ewen.replique.server/tooling-msg-handle %s)\n" (str {:type :shutdown})))
               (.end client)))
        (.on client "error"
             (fn [err]
               (tree-kill (aget proc "pid")))))
      (tree-kill (aget proc "pid")))
    (try
      (.unlink fs (str directory "/.replique-port"))
      (catch js/Error e nil))))

(defn start-repl [overview {:keys [repls] :as state} id]
  (let [{:keys [directory port cljs-env] :as repl}
        (get repls id)
        repl-cmd (if (is-lein-project state id)
                   (repl-cmd-lein state id)
                   (repl-cmd-raw state id))
        proc (apply spawn repl-cmd)
        status (.querySelector overview ".repl-status")]
    (.on (aget proc "stdout") "data"
         (fn [data]
           (let [msg (str data)]
             (.log js/console msg)
             (cond (= "REPL started\n" msg)
                   (do
                     (swap! core/state update-in [:repls id] assoc
                            :status msg)
                     (js/setTimeout
                      #(swap! core/state update-in [:repls id]
                              (fn [repl]
                                (if (identical? proc (:proc repl))
                                  (dissoc repl :status)
                                  repl)))
                      2000)
                     (if-let [repl-desc (read-port-desc directory)]
                       (let [cljs-env-port (:cljs-env repl-desc)]
                         (swap! core/state update-in [:repls id] assoc
                                  :cljs-env-port cljs-env-port
                                  :repl-port (:repl repl-desc))
                         (.log js/console (format "ewen.replique.cljs_env.repl.connect(\"http://localhost:%s\")" cljs-env-port)))
                       (notif/single-notif
                        {:type :err
                         :msg "REPL error"})))
                   :else (swap! core/state update-in [:repls id] assoc
                                :proc proc :status msg)))))
    (.on (aget proc "stderr") "data"
         (fn [err]
           (.log js/console (str err))))
    (.on proc "close"
         (fn [code signal]
           (when (or (not= 0 code) signal)
             (.log js/console
                   "REPL error. Code " code ". Signal " signal)
             (notif/single-notif
              {:type :err
               :msg "Error while starting the REPL"}))
           (swap! core/state update-in [:repls id] dissoc
                  :proc :cljs-env-port :repl-port :status)))
    (swap! core/state update-in [:repls id] assoc
           :proc proc :status "REPL starting ...")))

(defn ^:export start-clicked [e id]
  (let [state @core/state
        overview (.querySelector
                  js/document (format "[data-repl-id=\"%s\"]" id))]
    (if-let [err (maybe-start-repl-error state id)]
      (notif/single-notif err)
      (start-repl overview state id))))

(defn ^:export stop-clicked [e id]
  (let [state @core/state
        overview (.querySelector
                  js/document (format "[data-repl-id=\"%s\"]" id))]
    (stop-repl overview state id)))

(defhtml repl-overview [{:keys [type directory proc cljs-env-port status]}
                        id]
  [:div.dashboard-item.repl-overview
   {:href "#"
    :data-repl-id id}
   [:div (if proc
           {:class "start disabled"}
           {:class "start"
            :onclick (handler 'start-clicked id)})]
   [:div (if proc
           {:class "stop"
            :onclick (handler 'stop-clicked id)}
           {:class "stop disabled"})]
   [:img.delete {:src "resources/images/delete.png"
                 :onclick (handler 'delete-clicked id)}]
   [:img.edit {:src "resources/images/edit.png"
               :onclick (handler 'edit-clicked id)}]
   [:span.repl-status status]
   [:div.repl-type
    [:img {:src "resources/images/clj-logo.gif"}]
    (when (= type :cljs)
      [:img {:src "resources/images/cljs-logo.png"}])]
   [:span.repl-directory directory]
   (when cljs-env-port
     [:span.cljs-env-port (str "Cljs environment port: " cljs-env-port)])])

(defn ^:export settings-button-clicked []
  (swap! core/state assoc :view :settings))

(defhtml dashboard [{:keys [repls]}]
  [:div#dashboard
   [:div.settings-wrapper
    [:img.settings-button
     {:src "resources/images/settings.png"
      :onclick (handler 'settings-button-clicked)}]]
   (new-repl)
   (for [[id repl] repls]
     (repl-overview repl id))])

(swap!
 core/refresh-view-fns assoc :dashboard
 (fn [root {:keys [view] :as state}]
   (if (= :dashboard view)
     (let [node (utils/replace-or-append
                 root "#dashboard"
                 (dom/htmlToDocumentFragment
                  (dashboard state)))])
     (when-let [node (.querySelector root "#dashboard")]
       (dom/removeNode node)))))

(add-watch core/state :repls-watcher
           (fn [r k o n]
             (when (not= (:repls o) (:repls n))
               (core/refresh-view n))))
