(ns ewen.replique.ui.remote
  (:require [cljs.nodejs :as node]))

(def remote (node/require "remote"))
(def current-window (.getCurrentWindow remote))
