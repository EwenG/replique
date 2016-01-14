(ns ewen.replique.main
  (:require [ewen.replique.server :refer [repl-dispatch]]))

(defmulti require-dispatch :type)

(defmethod require-dispatch :clj [{:keys [directory] :as opts}]
  (require 'ewen.replique.server-clj)
  (repl-dispatch opts))

(defmethod require-dispatch :cljs [{:keys [directory] :as opts}]
  (require 'ewen.replique.server-cljs)
  (repl-dispatch opts))

(defn -main [opts]
  (require-dispatch (read-string opts)))
