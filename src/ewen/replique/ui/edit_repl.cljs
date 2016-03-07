(ns ewen.replique.ui.edit-repl
  (:require [hiccup.core :refer-macros [html]]
            [hiccup.def :refer-macros [defhtml]]
            [hiccup.page :refer [include-css]]
            [goog.dom :as dom]
            [goog.events :as events]
            [cljs.reader :as reader]
            [ewen.replique.ui.common :as common]
            [ewen.replique.ui.remote :refer [remote]]
            [ewen.replique.ui.core :as core]
            [ewen.replique.ui.utils :as utils]
            [ewen.ddom.core :as ddom]
            [ewen.replique.ui.notifications :as notif]))

(def replique-dir (.getGlobal remote "repliqueRootDir"))
(def dialog (.require remote "dialog"))

(def handler (ddom/handler (namespace ::e)))

(defonce current-repl (atom nil))

(defn valid-port? [port]
  (try (and (not (nil? port)) (< -1 port 65535))
       (catch js/Error e false)))

(defn maybe-save-repl-error [{:keys [repl-port random-port
                                     browser-env-port
                                     browser-env-random-port
                                     webapp-env-port
                                     webapp-env-random-port]
                              :as repl}]
  (if (or (and (not random-port) (not (valid-port? repl-port)))
          (and (not browser-env-random-port)
               (not (valid-port? browser-env-port)))
          (and (not webapp-env-random-port)
               (not (valid-port? webapp-env-port))))
    {:type :err
     :msg "Invalid port number"}
    nil))

(defn update-repl [{:keys [repl-id] :as repl}]
  (let [repl (dissoc repl :repl-id)
        state (swap! core/state assoc-in [:repls repl-id] repl)]
    (try
      (core/persist-state state)
      (catch js/Error e
        (.log js/console (str "Error while saving settings: " e))
        (notif/single-notif
         {:type :err
          :msg (str "Error while saving settings")})))))

(defn ^:export save-repl [e]
  (if-let [err (maybe-save-repl-error @current-repl)]
    (do (notif/single-notif err)
        false)
    (do (update-repl @current-repl)
        true)))

(declare repl-directory-tmpl)
(declare cljs-env-tmpl)
(declare browser-cljs-env-tmpl)
(declare webapp-cljs-env-tmpl)
(declare port-tmpl)

(defn refresh-cljs-env [cljs-env]
  (let [[class tmpl] (case cljs-env
                       :browser [".browser-env" browser-cljs-env-tmpl]
                       :webapp [".webapp-env" webapp-cljs-env-tmpl]
                       (throw (js/Error.
                               (str "Invalid cljs-env: " cljs-env))))
        cljs-env-node (.querySelector js/document class)]
    (-> (tmpl @current-repl)
        utils/make-node
        (dom/replaceNode cljs-env-node))))

(defn ^:export new-directory-clicked [e]
  (let [directory-node (.-currentTarget e)
        directory (first
                   (.showOpenDialog
                    dialog
                    #js {:properties #js ["openDirectory"]}))
        repl (swap! current-repl assoc :directory directory)]
    (-> (repl-directory-tmpl repl)
        utils/make-node
        (dom/replaceNode directory-node))
    (common/save-set-dirty)))

(defhtml repl-directory-tmpl [{:keys [directory]}]
  [:fieldset.directory {:onclick (handler 'new-directory-clicked)}
   [:legend "REPL directory"]
   [:input.field {:type "text" :readonly true
                  :value directory}]
   [:a.button.new-directory
    {:href "#"}
    "Choose directory"]])

(defn ^:export repl-type-changed [e browser-env]
  (let [current-repl (swap! current-repl assoc :type (keyword browser-env))
        cljs-env-node (.querySelector js/document ".cljs-env")]
    (-> (cljs-env-tmpl current-repl)
        utils/make-node
        (dom/replaceNode cljs-env-node))
    (common/save-set-dirty)))

(defhtml repl-type-tmpl [{:keys [type] :as repl}]
  [:fieldset.type
   [:legend "REPL type"]
   [:label {:for "type-clj"} "Clojure"]
   [:input.field
    {:type "radio"
     :id "type-clj"
     :name "repl-type"
     :value "clj"
     :checked (= type :clj)
     :onchange (handler 'repl-type-changed "clj")}]
   [:label {:for "type-cljs"} "Clojure/Clojurescript"]
   [:input.field
    {:type "radio"
     :id "type-cljs"
     :name "repl-type"
     :value "cljs"
     :checked (= type :cljs)
     :onchange (handler 'repl-type-changed "cljs")}]])

(defn ^:export cljs-env-changed [e cljs-env]
  (let [cljs-env (keyword cljs-env)
        cljs-env-node (.querySelector js/document ".cljs-env")
        repl (swap! current-repl assoc :cljs-env cljs-env)]
    (-> (cljs-env-tmpl repl)
        utils/make-node
        (dom/replaceNode cljs-env-node))
    (common/save-set-dirty)))

(defn ^:export random-port-changed
  ([e]
   (let [port-node (.querySelector js/document ".port")
         repl (swap! current-repl assoc :random-port
                     (.-checked (.-target e)))]
     (-> (port-tmpl repl)
         utils/make-node
         (dom/replaceNode port-node))
     (common/save-set-dirty)))
  ([e cljs-env random-port-key]
   (let [cljs-env (keyword cljs-env)
         random-port-key (keyword random-port-key)]
     (swap! current-repl assoc random-port-key (.-checked (.-target e)))
     (refresh-cljs-env cljs-env)
     (common/save-set-dirty))))

(defn ^:export port-changed
  ([e]
   (let [val (.-value (.-target e))
         val (if (= "" val) nil (js/parseInt val))]
     (swap! current-repl assoc :repl-port val)
     (common/save-set-dirty)))
  ([e cljs-env port-key]
   (let [val (.-value (.-target e))
         val (if (= "" val) nil (js/parseInt val))
         cljs-env (keyword cljs-env)
         port-key (keyword port-key)]
     (swap! current-repl assoc port-key val)
     (common/save-set-dirty))))

(defn ^:export new-out-file-clicked [e cljs-env]
  (let [cljs-env (keyword cljs-env)
        [k class tmpl] (case cljs-env
                         :browser [:browser-env-out ".browser-env"
                                   browser-cljs-env-tmpl]
                         :webapp [:webapp-env-out ".webapp-env"
                                  webapp-cljs-env-tmpl]
                         (throw (js/Error.
                                 (str "Invalid cljs-env: " cljs-env))))
        current-out (get @current-repl k)
        directory (:directory @current-repl)
        out-file (.showSaveDialog
                  dialog
                  #js {:filters #js [#js {:name "javascript file"
                                          :extensions #js ["js"]}]
                       :defaultPath (or current-out directory
                                        replique-dir)})
        repl (swap! current-repl assoc k out-file)
        cljs-env-node (.querySelector js/document class)]
    (-> (tmpl repl)
        utils/make-node
        (dom/replaceNode cljs-env-node))
    (common/save-set-dirty)))

(defn ^:export new-main-clicked [e cljs-env]
  (let [cljs-env (keyword cljs-env)
        [k class tmpl] (case cljs-env
                         :browser [:browser-env-main ".browser-env"
                                   browser-cljs-env-tmpl]
                         :webapp [:webapp-env-main ".webapp-env"
                                  webapp-cljs-env-tmpl]
                         (throw (js/Error.
                                 (str "Invalid cljs-env: " cljs-env))))
        current-main (get @current-repl k)
        directory (:directory @current-repl)
        out-file (-> (.showOpenDialog
                      dialog #js {:filters #js [#js
                                                {:name "clojurescript file"
                                                 :extensions
                                                 #js ["cljs" "cljc"]}]
                                  :defaultPath (or current-main directory
                                                   replique-dir)})
                     first)
        repl (swap! current-repl assoc k out-file)
        cljs-env-node (.querySelector js/document class)]
    (-> (tmpl repl)
        utils/make-node
        (dom/replaceNode cljs-env-node))
    (common/save-set-dirty)))

(defhtml browser-cljs-env-tmpl [{:keys [type cljs-env browser-env-port
                                        browser-env-out
                                        browser-env-random-port
                                        browser-env-main]}]
  [:div.browser-env
   [:label {:for "browser-env"
            :class (if (not= :cljs type) "disabled" "")}
    "Browser"]
   [:input.field
    {:type "radio"
     :id "browser-env"
     :name "cljs-env"
     :value "browser"
     :checked (= cljs-env :browser)
     :disabled (not= :cljs type)
     :onchange (handler 'cljs-env-changed "browser")}]
   [:input
    (merge
     {:type "text" :maxlength "5"
      :class "browser-env-port field" :value browser-env-port
      :placeholder "Port"}
     (if (or browser-env-random-port
             (not= type :cljs)
             (not= cljs-env :browser))
       {:disabled true}
       {:oninput (handler 'port-changed
                          "browser" "browser-env-port")}))]
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
     :disabled (or (not= cljs-env :browser) (not= type :cljs))
     :onchange (handler 'random-port-changed
                        "browser" "browser-env-random-port")}]
   [:input.field.browser-env-out
    {:type "text"
     :readonly true
     :value browser-env-out
     :disabled (or (not= :cljs type)
                   (not= :browser cljs-env))}]
   [:a (merge {:href "#"}
              (if (or (not= :cljs type) (not= :browser cljs-env))
                {:class "button new-browser-env-out disabled"}
                {:class "button new-browser-env-out"
                 :onclick (handler 'new-out-file-clicked "browser")}))
    "Select output file"]
   [:input.field.browser-env-main
    {:type "text"
     :readonly true
     :value browser-env-main
     :disabled (or (not= :cljs type)
                   (not= :browser cljs-env))}]
   [:a (merge {:href "#"}
              (if (or (not= :cljs type) (not= :browser cljs-env))
                {:class
                 "button new-browser-env-main disabled"}
                {:class "button new-browser-env-main"
                 :onclick (handler 'new-main-clicked "browser")}))
    "Choose main namespace"]])

(defhtml webapp-cljs-env-tmpl [{:keys [type cljs-env webapp-env-port
                                       webapp-env-out
                                       webapp-env-random-port
                                       webapp-env-main]}]
  [:div.webapp-env
   [:label {:for "webapp-env"
            :class (if (not= :cljs type) "disabled" "")}
    "Web application"]
   [:input.field
    {:type "radio"
     :id "webapp-env"
     :name "cljs-env"
     :value "webapp"
     :checked (= cljs-env :webapp)
     :disabled (not= :cljs type)
     :onchange (handler 'cljs-env-changed "webapp")}]
   [:input
    (merge {:type "text" :maxlength "5"
            :class "webapp-env-port field" :value webapp-env-port
            :placeholder "Port"}
           (if (or webapp-env-random-port
                   (not= type :cljs)
                   (not= cljs-env :webapp))
             {:disabled true}
             {:oninput (handler 'port-changed
                                "webapp" "webapp-env-port")}))]
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
                   (not= cljs-env :webapp))
     :onchange (handler 'random-port-changed
                        "webapp" "webapp-env-random-port")}]
   [:input.field.webapp-env-out
    {:type "text"
     :readonly true
     :value webapp-env-out
     :disabled (or (not= :cljs type) (not= :webapp cljs-env))}]
   [:a (merge {:href "#"}
              (if (or (not= :cljs type) (not= :webapp cljs-env))
                {:class "button new-webapp-env-out disabled"}
                {:class "button new-webapp-env-out"
                 :onclick (handler 'new-out-file-clicked "webapp")}))
    "Select output file"]
   [:input.field.webapp-env-main
    {:type "text"
     :readonly true
     :value webapp-env-main
     :disabled (or (not= :cljs type) (not= :webapp cljs-env))}]
   [:a (merge {:href "#"}
              (if (or (not= :cljs type) (not= :webapp cljs-env))
                {:class "button new-webapp-env-main disabled"}
                {:class "button new-webapp-env-main"
                 :onclick (handler 'new-main-clicked "webapp")}))
    "Choose main namespace"]])

(defhtml replique-cljs-env-tmpl [{:keys [type cljs-env]}]
  [:div
   [:label {:for "replique-env"
            :class (if (not= :cljs type) "disabled" "")}
    "Replique"]
   [:input.field
    {:type "radio"
     :id "replique-env"
     :name "cljs-env"
     :value "replique"
     :checked (= cljs-env :replique)
     :disabled (not= :cljs type)
     :onchange (handler 'cljs-env-changed "replique")}]])

(defhtml cljs-env-tmpl [{:keys [directory type cljs-env] :as repl}]
  [:fieldset.cljs-env
   [:legend "Clojurescript environment"]
   (browser-cljs-env-tmpl repl)
   (webapp-cljs-env-tmpl repl)
   (replique-cljs-env-tmpl repl)])

(defhtml port-tmpl [{:keys [repl-port random-port]}]
  [:fieldset.port
   [:legend "REPL port"]
   [:input (merge
            {:type "text" :maxlength "5"
             :class "custom-port field" :value repl-port}
            (if random-port
              {:readonly true
               :disabled true}
              {:oninput (handler 'port-changed)}))]
   [:label {:for "random-port"} "Random port"]
   [:input (merge
            {:type "checkbox"
             :class "field random-port"
             :id "random-port"
             :onchange (handler 'random-port-changed)}
            (when random-port
              {:checked true}))]])

(defhtml edit-repl [{:keys [type directory] :as repl}]
  [:div#edit-repl
   (common/back-button "dashboard")
   [:form
    (common/save-button 'ewen.replique.ui.edit_repl.save_repl)
    (repl-directory-tmpl repl)
    (repl-type-tmpl repl)
    (cljs-env-tmpl repl)
    (port-tmpl repl)]])

(swap!
 core/refresh-view-fns assoc :edit-repl
 (fn [root {:keys [view repls repl-id] :as state}]
   (if (= :edit-repl view)
     (let [repl (-> (get repls repl-id)
                    (assoc :repl-id repl-id))
           current-repl (reset! current-repl repl)
           node (utils/replace-or-append
                 root "#edit-repl"
                 (dom/htmlToDocumentFragment
                  (edit-repl current-repl)))])
     (when-let [node (.querySelector root "#edit-repl")]
       (dom/removeNode node)))))

(comment

  (defhtml tt1 [] [:div])

  (defhtml edit-repl* [{:keys [type directory] :as repl}]
    [:div#edit-repl
     (common/back-button "dashboard")
     [:form
      (common/save-button 'ewen.replique.ui.edit_repl.save_repl)
      (repl-directory-tmpl repl)]])

  )
