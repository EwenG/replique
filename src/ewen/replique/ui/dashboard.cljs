(ns ewen.replique.ui.dashboard
  (:require [hiccup.core]
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
            [ewen.replique.ui.notifications :as notif])
  (:import [goog.string format])
  (:require-macros [hiccup.core :refer [html]]
                   [hiccup.def :refer [defhtml]]))

(def replique-dir (.getGlobal remote "repliqueRootDir"))

(def spawn (aget (node/require "child_process") "spawn"))
(def tree-kill (.require remote "tree-kill"))
(def fs (node/require "fs"))

(defhtml new-repl []
  [:a.dashboard-item.new-repl {:href "#"} "New REPL"])

(defhtml repl-overview [{:keys [type directory proc proc-port status]}
                        index]
  [:div.dashboard-item.repl-overview
   {:href "#"
    :data-index index}
   [:div {:class (if proc "start disabled" "start")}]
   [:div {:class (if proc "stop" "stop disabled")}]
   [:img.delete {:src "resources/images/delete.png"}]
   [:img.edit {:src "resources/images/edit.png"}]
   [:span.repl-status status]
   [:div.repl-type
    [:img {:src "resources/images/clj-logo.gif"}]
    (when (= type :cljs)
      [:img {:src "resources/images/cljs-logo.png"}])]
   [:span.repl-directory directory]])

(defhtml dashboard [{:keys [repls]}]
  (html [:div#dashboard
         [:div.settings-wrapper
          [:img.settings-button {:src "/resources/images/settings.png"}]]
         (new-repl)
         (for [[repl index] (map vector repls (range (count repls)))]
           (repl-overview repl index))]))

(defn add-new-repl []
  (swap! core/state update-in [:repls] conj
         {:directory nil
          :type :clj :cljs-env :browser
          :browser-env-random-port true
          :webapp-env-random-port true
          :random-port true}))

(defn settings-button-clicked []
  (swap! core/state assoc :view :settings))

(defn is-lein-project [{:keys [repls] :as state} index]
  (let [{:keys [directory]} (nth repls index)]
    (and directory (utils/file-exists (str directory "/project.clj")))))

(defn maybe-start-repl-error [{:keys [repls] :as state} index]
  (let [{:keys [directory type cljs-env browser-env-out webapp-env-out]}
        (nth repls index)
        clj-jar (settings/get-clj-jar state)
        cljs-jar (settings/get-cljs-jar state)]
    (cond
      (or (= "" directory) (nil? directory))
      {:type :err
       :msg "The REPL directory has not been configured"}
      (and (is-lein-project state index)
           (not (settings/get-lein-script state)))
      {:type :err
       :msg "Leiningen script has not been configured"}
      (and (is-lein-project state index)
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

(defn repl-cmd-raw [{:keys [repls] :as state} index]
  (let [{:keys [directory type cljs-env port random-port
                browser-env-port browser-env-random-port
                webapp-env-port webapp-env-random-port]
         :as repl}
        (nth repls index)
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
                      "-m" "ewen.replique.server"
                      (-> (merge repl {:port port :cljs-env cljs-env
                                       :browser-env-port browser-env-port
                                       :webapp-env-port webapp-env-port})
                          str)]]
    ["java" cmd-args #js {:cwd directory}]))

(defn repl-cmd-lein [{:keys [repls] :as state} index]
  (let [{:keys [directory type cljs-env port random-port
                browser-env-port browser-env-random-port
                webapp-env-port webapp-env-random-port]
         :as repl}
        (nth repls index)
        port (if random-port 0 port)
        browser-env-port (if browser-env-random-port 0 browser-env-port)
        webapp-env-port (if webapp-env-random-port 0 webapp-env-port)
        cljs-env (if (= :clj type) nil cljs-env)
        cmd-args #js ["update-in" ":source-paths" "conj"
                      (pr-str (format "%s/src" replique-dir))
                      "--" "run" "-m" "ewen.replique.server/-main"
                      (-> (merge repl {:port port :cljs-env cljs-env
                                       :browser-env-port browser-env-port
                                       :webapp-env-port webapp-env-port})
                          str)]]
    [(settings/get-lein-script state)
     cmd-args #js {:cwd directory}]))

;; lein update-in :source-paths conj "\"/home/egr/electron/resources/replique/src\"" -- run -m ewen.replique.server/-main "{:type :clj :port 9001}"

(defn read-port-desc [directory]
  (try
    (->>
     (str directory "/.replique-port")
     (#(.readFileSync fs % "utf-8"))
     (reader/read-string))
    (catch js/Error e
      (.log js/console e)
      nil)))

(defn start-repl [overview {:keys [repls] :as state} index]
  (let [{:keys [directory port] :as repl}
        (nth repls index)
        repl-cmd (if (is-lein-project state index)
                   (repl-cmd-lein state index)
                   (repl-cmd-raw state index))
        proc (apply spawn repl-cmd)
        status (.querySelector overview ".repl-status")]
    (.on (aget proc "stdout") "data"
         (fn [data]
           (let [msg (str data)]
             (.log js/console msg)
             (cond (= "REPL started\n" msg)
                   (do
                     (swap! core/state core/update-repls index assoc
                            :status msg)
                     (js/setTimeout
                      #(swap! core/state core/update-repls index
                              (fn [repl]
                                (if (identical? proc (:proc repl))
                                  (dissoc repl :status)
                                  repl)))
                      2000)
                     (if-let [repl-desc (read-port-desc directory)]
                       (swap! core/state core/update-repls index assoc
                              :proc-port (:tooling-repl repl-desc))
                       (notif/single-notif
                        {:type :err
                         :msg "Error while starting the REPL"})))
                   :else (swap! core/state core/update-repls index assoc
                                :proc proc :status msg)))))
    (.on (aget proc "stderr") "data"
         (fn [err]
           (.log js/console (str err))))
    (.on proc "close"
         (fn [code signal]
           ;; When killed with the stop button, the process returns
           ;; code 143 or signal SIGTERM
           (when (and (not= 143 code) (not= "SIGTERM" signal))
             (.log js/console "Error while starting the REPL. Code " code)
             (notif/single-notif
              {:type :err
               :msg "Error while starting the REPL"}))
           (swap! core/state core/update-repls index dissoc
                  :proc :proc-port :status)))
    (swap! core/state core/update-repls index assoc
           :proc proc :status "REPL starting")))

(comment
  (re-matches #"^REPL started on port: ([0-9]+)\n$"
              (str "REPL started on port: " "354\n"))
  (re-matches #"^REPL started on port: ([0-9]+)$"
              "REPL started on port: 3")
  )

(defn stop-repl [overview {:keys [repls] :as state} index]
  (let [{:keys [proc directory]} (nth repls index)]
    (tree-kill (aget proc "pid"))
    (try
      (.unlink fs (str directory "/.replique-port"))
      (catch js/Error e nil))))

(defn overview-clicked [overview e]
  (let [class-list (-> (aget e "target")
                       (.-classList))]
    (cond (.contains class-list "delete")
          (let [overview (aget e "currentTarget")
                index (js/parseInt (.getAttribute overview "data-index"))
                {:keys [proc]} (nth (:repls @core/state) index)]
            (when proc (tree-kill (aget proc "pid")))
            (swap! core/state update-in [:repls]
                   (partial keep-indexed #(if (= index %1) nil %2))))
          (.contains class-list "edit")
          (let [index (-> (.getAttribute overview "data-index")
                          (js/parseInt))]
            (swap! core/state assoc :repl-index index)
            (swap! core/state assoc :view :edit-repl))
          (and (.contains class-list "start")
               (not (.contains class-list "disabled")))
          (let [index (-> (.getAttribute overview "data-index")
                          (js/parseInt))
                state @core/state]
            (if-let [err (maybe-start-repl-error state index)]
              (notif/single-notif err)
              (start-repl overview state index)))
          (and (.contains class-list "stop")
               (not (.contains class-list "disabled")))
          (let [index (-> (.getAttribute overview "data-index")
                          (js/parseInt))
                state @core/state]
            (stop-repl overview state index))
          :else nil)))

(swap!
 core/refresh-view-fns assoc :dashboard
 (fn [root {:keys [view] :as state}]
   (if (= :dashboard view)
     (let [node (utils/replace-or-append
                 root "#dashboard"
                 (dom/htmlToDocumentFragment
                  (dashboard state)))]
       (events/listen (.querySelector node ".new-repl")
                      events/EventType.CLICK add-new-repl)
       (events/listen (.querySelector node ".settings-button")
                      events/EventType.CLICK settings-button-clicked)
       (doseq [overview (-> (.querySelectorAll node ".repl-overview")
                            array-seq)]
         (events/listen overview events/EventType.CLICK
                        (partial overview-clicked overview))))
     (when-let [node (.querySelector root "#dashboard")]
       (dom/removeNode node)))))


(add-watch core/state :repls-watcher
           (fn [r k o n]
             (when (not= (:repls o) (:repls n))
               (core/persist-state n)
               (core/refresh-view n))))

(comment

  (dom/appendChild
   js/document.head (utils/make-node (html (include-css "main.css"))))
  (core/refresh-view @core/state)
  (core/load-state)


 )
