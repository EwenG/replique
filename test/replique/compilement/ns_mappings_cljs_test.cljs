(ns replique.compliment.ns-mappings-cljs-test
  (:require [clojure.string :as st :refer [split] :rename {split str-split}]
            [replique.compliment.ns-mappings-cljs-test2 :refer [ff2]])
  (:import [goog.events EventType])
  (:require-macros [replique.compliment.ns-mappings-clj-test
                    :as cljs-ns-m :refer [my-macro] :rename {my-macro my-macro-2}]))

(defn my-fn "rr" [e f & {:keys [e r]}])
(def gg nil)
::eeeeee

:replique.compliment.ns-mappings-clj-test/fffff

::ee
::eee
