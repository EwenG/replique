(ns ewen.replique.ui.main
  (:require [ewen.replique.ui.dashboard]
            [ewen.replique.ui.edit-repl]
            [ewen.replique.ui.settings]
            [ewen.replique.ui.core :as core]
            [ewen.replique.ui.utils :as utils]
            [ewen.replique.cljs-env.browser]
            [goog.dom :as dom]
            [hiccup.page :refer [include-css]])
  (:require-macros [hiccup.core :refer [html]]))

(dom/appendChild
 js/document.head (utils/make-node (html (include-css "./out/main.css"))))
(core/load-state)
