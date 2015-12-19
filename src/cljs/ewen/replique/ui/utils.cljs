(ns ewen.replique.ui.utils
  (:require [goog.dom :as dom]
            [goog.dom.classlist]))

(defn make-node [s]
  (dom/htmlToDocumentFragment s))

(defn add-class [elt class]
  (.add (aget elt "classList") class))

(defn rem-class [elt class]
  (.remove (aget elt "classList") class))
