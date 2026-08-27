(ns replique.main
  "Entry point:

    clojure -M -m replique.main
    clojure -M -m replique.main '{:process-id \"my-project\" :port 47000}'

  Options can also be given as -Dreplique.* system properties, the EDN map
  argument takes precedence. On startup, one JSON line describing the process
  is printed on stdout and the same description is written to the port file."
  (:require [clojure.edn :as edn]
            [clojure.stacktrace])
  (:import [java.io PrintStream]
           [java.nio.charset StandardCharsets]))

(def ^:private min-clojure-version {:major 1 :minor 12})

(defn- clojure-version-ok? []
  (let [{:keys [major minor]} *clojure-version*
        {min-major :major min-minor :minor} min-clojure-version]
    (or (> major min-major) (and (= major min-major) (>= minor min-minor)))))

(defn- unsupported-runtime []
  (when-not (clojure-version-ok?)
    (format "Replique requires clojure %s.%s+, this process runs %s"
            (:major min-clojure-version) (:minor min-clojure-version) (clojure-version))))

(defn- system-property-opts []
  (let [prop (fn [k] (System/getProperty (str "replique." (name k))))
        read-prop (fn [k] (when-let [v (prop k)] (edn/read-string v)))]
    (cond-> {}
      (prop :process-id) (assoc :process-id (prop :process-id))
      (prop :host) (assoc :host (prop :host))
      (prop :port) (assoc :port (read-prop :port))
      (prop :directory) (assoc :directory (prop :directory))
      (prop :port-file) (assoc :port-file (prop :port-file))
      (prop :tee-output) (assoc :tee-output (read-prop :tee-output)))))

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

(defn- print-line!
  "The startup line is protocol, not display: a client reads it to find the
  process it just started. Written as utf-8 bytes straight to the stream
  rather than through *out*, whose encoding follows the locale - a process
  started without one has a stdout.encoding of US-ASCII, and would announce a
  directory holding an accent as question marks."
  [^PrintStream stdout line]
  (let [bytes (.getBytes (str line "\n") StandardCharsets/UTF_8)]
    (.write stdout bytes 0 (alength bytes))
    (.flush stdout)))

(defn -main [& args]
  ;; before anything of replique's wraps it
  (let [stdout System/out]
   (if-let [unsupported (unsupported-runtime)]
    (do (binding [*out* *err*] (println unsupported))
        (System/exit 1))
    (let [write-str (do (require 'replique.json)
                        (resolve 'replique.json/write-str))]
      (try
        (let [opts (merge (system-property-opts) (parse-args args))
              _ (require 'replique.core)
              start! (resolve 'replique.core/start!)
              info (start! opts)]
          (print-line! stdout (write-str (assoc info :tag "started"))))
        (catch Throwable t
          (print-line! stdout (write-str {:tag "error"
                                          :error "start-failed"
                                          :message (or (.getMessage t) (str (class t)))}))
          (binding [*out* *err*]
            (println "Replique could not start:")
            (clojure.stacktrace/print-cause-trace t))
          (System/exit 1)))))))
