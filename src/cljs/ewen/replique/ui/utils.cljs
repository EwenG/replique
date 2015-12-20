(ns ewen.replique.ui.utils
  (:require [goog.dom :as dom]
            [goog.dom.classlist]
            [goog.async.Throttle :as Throttle])
  (:import [goog.async Throttle]
           [goog.ui IdGenerator]))

(defn make-node [s]
  (dom/htmlToDocumentFragment s))

(defn add-class [elt class]
  (.add (aget elt "classList") class))

(defn rem-class [elt class]
  (.remove (aget elt "classList") class))

(defn replace-or-append
  ([selector node]
   (replace-or-append js/document selector node))
  ([root selector node]
   (if-let [found (.querySelector root selector)]
     (dom/replaceNode node found)
     (dom/appendChild root node))
   node))

(defn throttle [func wait]
  (let [args (volatile! nil)
        action #(apply func @args)
        throttle (Throttle. action wait)]
    (fn [& new-args]
      (vreset! args new-args)
      (.fire throttle))))

(def id-gen (IdGenerator.))
