(ns replique.main
  (:require [replique.utils :as utils])
  (:import [java.util UUID]))

(defn -main
  ([]
   (-main (UUID/randomUUID)))
  ([process-id]
   (let [host (System/getProperty "replique.server.host" utils/host)
         port (System/getProperty "replique.server.port" (str utils/port))
         http-host (System/getProperty "replique.http-server.host" utils/http-host)
         http-port (System/getProperty "replique.http-server.port" (str utils/http-port))
         {:keys [major minor incremental qualifier]} *clojure-version*]
     (if (and (<= major 1) (< minor 8))
       (print (format "Replique is compatible with clojure 1.8+, current version is: %s.%s.%s%s" major minor incremental (if qualifier (str "-" qualifier) "")))
       (do (require 'replique.interactive)
           (require 'replique.source-meta)
           ((resolve 'replique.repl/start-repl-process)
            {:host host :port (when port (read-string port)) :process-id process-id
             :http-host http-host :http-port (when http-port (read-string http-port))}))))))

(comment
  (css
   [.class1 .class2 (class3 class4)
    {:background (:1px solid black)}
    class4 {:height :3px}]
   ['id1 .class4 {:width :1px}])
  
  )
