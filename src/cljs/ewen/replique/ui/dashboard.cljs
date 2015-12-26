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

(def replique-root-dir (.getGlobal remote "repliqueRootDir"))

(def spawn (aget (node/require "child_process") "spawn"))
(def tree-kill (.require remote "tree-kill"))

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
    (when (contains? type "clj")
      [:img {:src "resources/images/clj-logo.gif"}])
    (when (contains? type "cljs")
      [:img {:src "resources/images/cljs-logo.png"}])]
   [:span.repl-directory directory]
   (when proc-port
     [:span.repl-port (str "REPL port: " proc-port)])])

(defhtml dashboard [{:keys [repls]}]
  (html [:div#dashboard
         [:div.settings-wrapper
          [:img.settings-button {:src "/resources/images/settings.png"}]]
         (new-repl)
         (for [[repl index] (map vector repls (range (count repls)))]
           (repl-overview repl index))]))

(defn add-new-repl []
  (swap! core/state update-in [:repls] conj
         {:type #{"clj"} :directory nil :random-port true}))

(defn settings-button-clicked []
  (swap! core/state assoc :view :settings))

(defn is-lein-project [{:keys [repls] :as state} index]
  (let [{:keys [directory]} (nth repls index)]
    (and directory (utils/file-exists (str directory "/project.clj")))))

(defn maybe-start-repl-error [{:keys [repls] :as state} index]
  (let [{:keys [directory type]} (nth repls index)
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
      (and (= #{"clj" "cljs"} type) (nil? cljs-jar))
      {:type :err
       :msg "Clojurescript jar has not been configured"}
      (and (= #{"clj" "cljs"} type) (not (utils/file-exists cljs-jar)))
      {:type :err
       :msg "Invalid Clojurescript jar"}
      :else nil)))

(defn repl-cmd-raw [{:keys [repls] :as state} index]
  (let [{:keys [directory type port random-port] :as repl}
        (nth repls index)
        cp (if (= #{"clj" "cljs"} type)
              (str (settings/get-clj-jar state) ":"
                   (settings/get-cljs-jar state) ":"
                   (format "\"%s/src/clj\"" replique-root-dir))
              (str (settings/get-clj-jar state) ":"
                   (format "\"%s/src/clj\"" replique-root-dir)))
        port (if random-port 0 port)
        opts {:port port :accept 'ewen.replique.server/tooling-repl
              :server-daemon false :name :replique-tooling-repl}
        cmd-args #js ["-cp" cp
                      (str "-Dclojure.server.repl=" opts)
                      "clojure.main" "-e"
                      "(-> @#'clojure.core.server/servers
(get :replique-tooling-repl)
:socket
(.getLocalPort))"]]
    ["java" cmd-args #js {:cwd directory}]))

(defn repl-cmd-lein [{:keys [repls] :as state} index]
  (let [{:keys [directory type port random-port] :as repl}
        (nth repls index)
        port (if random-port 0 port)
        opts {:port port :accept 'ewen.replique.server/tooling-repl
              :server-daemon false :name :replique-tooling-repl}
        cmd-args #js ["update-in" ":jvm-opts" "conj"
                      (format "\"-Dclojure.server.repl=%s\"" opts) "--"
                      "update-in" ":source-paths" "conj"
                      (format "\"%s/src/clj\"" replique-root-dir) "--"
                      "run" "-m" "clojure.main/main" "-e"
                      "(-> @#'clojure.core.server/servers
(get :replique-tooling-repl)
:socket
(.getLocalPort))"]]
    [(settings/get-lein-script state)
     cmd-args #js {:cwd directory}]))

;; lein update-in :jvm-opts conj '"-Dclojure.server.repl={:port 5555 :accept clojure.core.server/repl :server-daemon false}"' -- run -m clojure.main/main

(defn start-repl [overview {:keys [repls] :as state} index]
  (let [{:keys [directory] :as repl}
        (nth repls index)
        repl-cmd (if (is-lein-project state index)
                   (repl-cmd-lein state index)
                   (repl-cmd-raw state index))
        proc (apply spawn repl-cmd)
        status (.querySelector overview ".repl-status")]
    (.on (aget proc "stdout") "data"
         (fn [data]
           (let [port (reader/read-string (str data))]
             (swap! core/state core/update-repls index assoc
                    :status "REPL started" :proc-port port)
             (js/setTimeout
              #(swap! core/state core/update-repls index
                      (fn [repl]
                        (if (identical? proc (:proc repl))
                          (dissoc repl :status)
                          repl)))
              2000))))
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

(defn stop-repl [overview {:keys [repls] :as state} index]
  (let [{:keys [proc]} (nth repls index)]
    (tree-kill (aget proc "pid"))))

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
