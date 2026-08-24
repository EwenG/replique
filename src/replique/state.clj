(ns replique.state
  "Process wide state. Kept dependency free so that every namespace can read
  from it without introducing a cycle.")

(defonce version "2.0.0-SNAPSHOT")
(defonce protocol-version 1)

;; nil when the process is not started. Otherwise:
;; {:process-id :host :port :directory :port-file :started-at :server
;;  :shutdown-hook}
(defonce process (atom nil))

(defn started? []
  (some? @process))

(defn info
  "The description of this process, as sent to clients. Also the content of the
  port file."
  []
  (let [{:keys [process-id host port directory started-at]} @process]
    {:protocol-version protocol-version
     :replique-version version
     :process-id process-id
     :host host
     :port port
     :directory directory
     :pid (.pid (java.lang.ProcessHandle/current))
     :clojure-version (clojure-version)
     :java-version (System/getProperty "java.version")
     :started-at started-at}))
