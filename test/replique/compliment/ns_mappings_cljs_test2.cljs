(ns replique.compliment.ns-mappings-cljs-test2
  (:require [goog.object]))

(def ff2 "ff2")

(goog.object/get #js {:e "e"} "e")

(comment
  (goog.object/get #js {:e "e"} :e)
  )
