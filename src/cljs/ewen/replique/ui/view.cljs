(ns ewen.replique.ui.view
  (:require [cljs.nodejs :as node]))

(def remote (node/require "remote"))
(def current-window (.getCurrentWindow remote))
(def web-contents (aget current-window "webContents"))

(defn keyboard-shortcuts [e]
  (cond (and (aget e "ctrlKey")
             (aget e "shiftKey")
             (= (aget e "keyCode") 73))
        (.toggleDevTools web-contents)
        :else nil))

(defn init []
  (.addEventListener js/document "keyup" #(keyboard-shortcuts %)))

(comment
  (init)
  )
