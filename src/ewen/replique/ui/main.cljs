(ns ewen.replique.ui.main
  (:require [ewen.replique.ui.dashboard]
            [ewen.replique.ui.edit-repl]
            [ewen.replique.ui.settings]
            [ewen.replique.ui.core :as core]
            [ewen.replique.cljs-env.browser]
            [goog.dom :as dom]
            [hiccup.page :refer [include-css]]
            [ewen.ddom.core :as ddom]
            [ewen.replique.cljs-env.repl])
  (:require-macros [hiccup.core :refer [html]]))

(dom/appendChild
 js/document.head
 (ddom/string->fragment (html (include-css "./out/main.css"))))
(core/load-state)
