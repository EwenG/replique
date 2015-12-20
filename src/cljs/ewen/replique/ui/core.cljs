(ns ewen.replique.ui.core
  (:require [cljs.reader :as reader]
            [ewen.replique.ui.remote :refer [remote]]
            [goog.dom :as dom]))

(def init-state {:repls '()
                 :view :dashboard
                 :settings {:clj-jar-source "embedded"}})

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

(defn persist-state [{:keys [repls]}]
  (loop [index 0
         repls repls]
    (cond
      (first repls)
      (do (.setItem js/localStorage (str "repl" index)
                    (pr-str (first repls)))
          (recur (inc index) (rest repls)))
      (.getItem js/localStorage (str "repl" index))
      (do (.removeItem js/localStorage (str "repl" index))
          (recur (inc index) nil))
      :else true)))

(defn load-state []
  (loop [i 0
         repls '()]
    (if-let [repl-str (.getItem js/localStorage
                                (str "repl" i))]
      (let [repl (reader/read-string repl-str)]
        (recur (inc i) (conj repls repl)))
      (swap! state assoc :repls (reverse repls)))))
