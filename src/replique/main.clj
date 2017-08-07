(ns replique.main
  (:require [replique.utils :as utils]
            [replique.repl]))

;; dynaload to support clojure version checking
;; eagerly loading replique would prevent it from compiling when using a lower clojure version
(def ^:private start-repl-process
  (utils/dynaload 'replique.repl/start-repl-process))

(defn -main [project-map opts]
  (let [{:keys [major minor incremental qualifier]} *clojure-version*]
    (if (and (<= major 1) (< minor 8))
      (print (format "Replique is compatible with clojure 1.8+, current version is: %s.%s.%s%s" major minor incremental (if qualifier (str "-" qualifier) "")))
      (do (require 'replique.interactive)
          (replique.repl/start-repl-process project-map opts)))))

(comment
  (css
   [.class1 .class2 (class3 class4)
    {:background (:1px solid black)}
    class4 {:height :3px}]
   ['id1 .class4 {:width :1px}])
  
  )
