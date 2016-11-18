(ns replique.main
  (:require [replique.utils :as utils]))

(def ^:private start-repl-process
  (utils/dynaload 'replique.repl/start-repl-process))

(defn -main [opts]
  (let [{:keys [major minor incremental qualifier]} *clojure-version*]
    (if (and (<= major 1) (< minor 8))
      (print (format "Replique is compatible with clojure 1.8+, current version is: %s.%s.%s%s" major minor incremental (if qualifier (str "-" qualifier) "")))
      (@start-repl-process (read-string opts)))))

(comment
  (css
   [.class1 .class2 (class3 class4)
    {:background (:1px solid black)}
    class4 {:height :3px}]
   ['id1 .class4 {:width :1px}])
  
  )
