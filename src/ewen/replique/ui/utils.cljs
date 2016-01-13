(ns ewen.replique.ui.utils
  (:require [goog.dom :as dom]
            [goog.dom.classlist]
            [goog.async.Throttle :as Throttle]
            [cljs.nodejs :as node])
  (:import [goog.async Throttle]))

(def fs (node/require "fs"))

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

(defn file-exists [path]
  (try
    (do (.lstatSync fs path)
        true)
    (catch js/Error e
      (if (= "ENOENT" (aget e "code"))
        false
        (throw e)))))

(let [counter (atom 0)]
  (defn next-id []
    (swap! counter inc)))
