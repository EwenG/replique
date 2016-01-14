(ns ewen.replique.ui.core
  (:require [cljs.reader :as reader]
            [ewen.replique.ui.remote :refer [remote]]
            [goog.dom :as dom]
            [cljs.nodejs :as node]
            [cljs.reader :as reader]))

(def version "0.0.1")

(def replique-dir (.getGlobal remote "repliqueRootDir"))
(def fs (node/require "fs"))

(def init-state {:repls {}
                 :view :dashboard
                 :settings {:clj-jar-source "embedded"
                            :cljs-jar-source "embedded"
                            :lein-source "embedded"
                            :sass-bin
                            (str
                             replique-dir
                             "/runnables/replique_sass_3.2.5_0.0.1")}})

(defonce state
  (atom init-state))

(defn reset-state []
  (reset! state init-state))

(defonce refresh-view-fns (atom {}))

(defn refresh-view [state]
  (let [root (or (.getElementById js/document "root")
                 (doto (dom/createDom "div" #js {:id "root"})
                   (#(dom/appendChild js/document.body %))))]
    (doseq [[_ f] @refresh-view-fns]
      (f root state))))

(comment
  (refresh-view @state)
  )

(add-watch state :view-watcher
           (fn [r k o n]
             (when (not= (:view o) (:view n))
               (refresh-view n))))

(defn persist-state [{:keys [settings repls]}]
  (let [cleaned-repls (into
                       {}
                       (for [[id repl] repls]
                         [id (select-keys
                              repl
                              [:directory :type :cljs-env
                               :browser-env-random-port
                               :webapp-env-random-port
                               :browser-env-port :browser-env-main
                               :webapp-env-port :webapp-env-main
                               :browser-env-out
                               :webapp-env-out
                               :random-port])]))
        cleaned-state {:settings (dissoc settings :dirty :saving)
                       :repls cleaned-repls
                       :version version}]
    (.writeFileSync
     fs
     (str replique-dir "/data/" version "/replique-tmp.edn")
     (str cleaned-state)
     #js {:flag "w"})
    (try
      (.unlinkSync fs (str replique-dir "/data/" version "/replique.edn"))
      (catch js/Error e
        (if (= "ENOENT" (aget e "code"))
          ;; The file does not exists, there is no need to remove it
          nil
          (throw e))))
    (.linkSync
     fs
     (str replique-dir "/data/" version "/replique-tmp.edn")
     (str replique-dir "/data/" version "/replique.edn"))
    (try (.unlinkSync fs (str replique-dir "/data/"
                              version "/replique-tmp.edn"))
         (catch js/Error e
           ;; Ignore errors
           nil))))

(defn load-state []
  (let [loaded-state (try (.readFileSync
                           fs (str replique-dir "/data/"
                                   version "/replique.edn"))
                          (catch js/Error e
                            (if (= "ENOENT" (aget e "code"))
                              ;; The file does not exists. Try to load the
                              ;; temp data file, if any.
                              (try (.readFileSync
                                    fs (str replique-dir "/data/"
                                            version "/replique-temp.edn"))
                                   (catch js/Error e
                                     (if (= "ENOENT" (aget e "code"))
                                       ;; The file does not exists.
                                       ;; There is nothing to load
                                       nil
                                       (throw e))))
                              (throw e))))]
    (when loaded-state
      (reset! state (assoc (reader/read-string (str loaded-state))
                           :view :dashboard)))))
