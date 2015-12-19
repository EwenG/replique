(ns ewen.replique.ui.shortcuts
  (:require [cljs.nodejs :as node]
            [ewen.replique.ui.remote :refer [current-window]]))

(def web-contents (aget current-window "webContents"))

(defn keyboard-shortcuts [e]
  (cond (and (aget e "ctrlKey")
             (aget e "shiftKey")
             (= (aget e "keyCode") 73))
        (.toggleDevTools web-contents)
        :else nil))

(.addEventListener js/document "keyup" #(keyboard-shortcuts %))
