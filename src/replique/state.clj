(ns replique.state
  "Process wide state. Kept dependency free so that every namespace can read
  from it without introducing a cycle.")

;; The loader this process shares, and the parent of the one each connection
;; ends up with. clojure.main/repl gives every repl a DynamicClassLoader of its
;; own, whose parent is whatever its thread was holding - so two repls of a
;; process that did nothing about it hold two loaders that are siblings, and a
;; library added at one is a library the other cannot find. What
;; clojure.repl.deps adds to is the highest DynamicClassLoader above the thread
;; it was called on, so one loader for every connection to inherit is what
;; makes an added library reach the whole process, the thread that reads the
;; classpath included.
;;
;; Its parent is the loader clojure itself came from rather than whatever the
;; thread loading this happens to hold: a namespace is loaded through a loader
;; the compiler makes for that load and throws away afterwards.
(defonce class-loader
  (clojure.lang.DynamicClassLoader. (.getClassLoader clojure.lang.RT)))

(defn adopt-class-loader!
  "Load through the loader this process shares, on this thread."
  []
  (.setContextClassLoader (Thread/currentThread) class-loader))

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

(defn connections
  "The connections the server currently holds, by id. Whoever broadcasts an
  event reads them from here rather than keeping references of its own: a
  connection that is closed is removed, and frames stop being queued for it."
  []
  (if-let [server (:server @process)]
    @(:connections server)
    {}))
