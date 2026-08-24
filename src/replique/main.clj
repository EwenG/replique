(ns replique.main
  "Entry point:

    clojure -M -m replique.main
    clojure -M -m replique.main '{:process-id \"my-project\" :port 47000}'

  Options can also be given as -Dreplique.* system properties, the EDN map
  argument takes precedence. On startup, one JSON line describing the process
  is printed on stdout and the same description is written to the port file."
  (:require [clojure.edn :as edn]
            [clojure.stacktrace]))

(def ^:private min-clojure-version {:major 1 :minor 12})

(defn- clojure-version-ok? []
  (let [{:keys [major minor]} *clojure-version*
        {min-major :major min-minor :minor} min-clojure-version]
    (or (> major min-major) (and (= major min-major) (>= minor min-minor)))))

(defn- system-property-opts []
  (let [prop (fn [k] (System/getProperty (str "replique." (name k))))
        read-prop (fn [k] (when-let [v (prop k)] (edn/read-string v)))]
    (cond-> {}
      (prop :process-id) (assoc :process-id (prop :process-id))
      (prop :host) (assoc :host (prop :host))
      (prop :port) (assoc :port (read-prop :port))
      (prop :directory) (assoc :directory (prop :directory))
      (prop :port-file) (assoc :port-file (prop :port-file)))))

(defn- parse-args [args]
  (case (count args)
    0 {}
    1 (let [opts (edn/read-string (first args))]
        (when-not (map? opts)
          (throw (ex-info (str "The argument of replique.main must be an EDN map, got: "
                               (pr-str (first args)))
                          {:argument (first args)})))
        opts)
    (throw (ex-info (str "replique.main takes at most one argument, got " (count args))
                    {:arguments args}))))

(defn- print-line! [line]
  (.write *out* (str line "\n"))
  (.flush *out*))

(defn -main [& args]
  (if-not (clojure-version-ok?)
    (do (binding [*out* *err*]
          (println (format "Replique requires clojure %s.%s+, this process runs %s"
                           (:major min-clojure-version) (:minor min-clojure-version)
                           (clojure-version))))
        (System/exit 1))
    (let [write-str (do (require 'replique.json)
                        (resolve 'replique.json/write-str))]
      (try
        (let [opts (merge (system-property-opts) (parse-args args))
              _ (require 'replique.core)
              start! (resolve 'replique.core/start!)
              info (start! opts)]
          (print-line! (write-str (assoc info :tag "started"))))
        (catch Throwable t
          (print-line! (write-str {:tag "error"
                                   :error "start-failed"
                                   :message (or (.getMessage t) (str (class t)))}))
          (binding [*out* *err*]
            (println "Replique could not start:")
            (clojure.stacktrace/print-cause-trace t))
          (System/exit 1))))))
