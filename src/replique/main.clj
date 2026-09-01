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

(def ^:private property-prefix "replique.")

(def ^:private option-keys
  "The options, in the order they read best in a message. Said here rather
  than asked of replique.core, which this namespace is careful not to need
  before it has tried to load it - a classpath that cannot is a start failure
  it has to be able to report."
  [:process-id :host :port :directory :port-file])

(defn- property-name ^String [k] (str property-prefix (name k)))

(defn- validate-properties!
  "Refuse a -Dreplique.<name> that is not an option, rather than leave it
  unread. The rule the edn map is already held to, and it matters more here: a
  misspelt key in a map is at least in the map the client wrote, while a
  property that did nothing looks exactly like one that worked. Either way the
  process starts under a name nobody asked for, and the editor then waits for
  a process that is running and cannot be found.

  Only the names directly under replique. - an option is replique.<option>,
  and an option name is one word or two joined by a hyphen, so a misspelling
  of one cannot hold a dot. A replique.something.else is another namespace
  rather than a typo, replique 1's replique.server.port being the one that
  really turns up in a jvm, and stopping a process over a property that was
  never addressed to it is worse than the typo this is here to catch."
  []
  (let [known (set (map property-name option-keys))
        names (fn [ns] (apply str (interpose ", " (map #(str "-D" %) ns))))
        unknown (->> (.stringPropertyNames (System/getProperties))
                     (filter (fn [^String n]
                               (and (.startsWith n property-prefix)
                                    (neg? (.indexOf n (int \.) (count property-prefix))))))
                     (remove known)
                     sort
                     seq)]
    (when unknown
      (throw (ex-info (str "Unknown system propert" (if (next unknown) "ies" "y") ": "
                           (names unknown) ". The options are: "
                           (names (map property-name option-keys)) ".")
                      {:unknown-properties (vec unknown)})))))

(defn- system-property-opts []
  (validate-properties!)
  (let [prop (fn [k] (System/getProperty (property-name k)))
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

(defn- exception-data
  "The structured exception, when replique got far enough to have the code
  that builds one.

  A start can fail before replique.protocol is loadable at all - a
  classpath without replique on it is the usual way - so this is best
  effort: the trace on stderr is what always says what happened, and this
  is what lets a client show it the way it shows any other exception."
  [t]
  (try
    (require 'replique.protocol)
    ((resolve 'replique.protocol/exception->data) t)
    (catch Throwable _ nil)))

(defn- exit!
  "End the process. A var of its own so that a test can run -main without
  taking the test runner with it."
  [status]
  (System/exit status))

(defn- report-start-failure!
  "Say on stdout that the process did not come up, in the shape every start
  failure has. A client that spawned the process reads stdout and nothing
  else: stderr is where the trace goes, which is for a human.

  Best effort, like exception-data - a start can fail before replique.json is
  loadable at all, and the trace on stderr is what always says what happened."
  [^PrintStream stdout ^String message data]
  (try
    (require 'replique.json)
    (print-line! stdout ((resolve 'replique.json/write-str)
                         (cond-> {:tag "error"
                                  :error "start-failed"
                                  :message message}
                           data (assoc :exception data))))
    (catch Throwable _ nil)))

(defn -main [& args]
  ;; before anything of replique's wraps it
  (let [stdout System/out]
    (if-let [unsupported (unsupported-runtime)]
      (do
        ;; On stdout too, and not on stderr alone. This is the one start
        ;; failure replique knows everything about before it has loaded
        ;; anything, and reporting it only to a human would leave the client
        ;; that spawned the process with an exit code to guess from.
        (report-start-failure! stdout unsupported nil)
        (binding [*out* *err*] (println unsupported))
        (exit! 1))
      (try
        (let [opts (merge (system-property-opts) (parse-args args))
              ;; inside the try: a classpath that cannot load replique is one
              ;; of the ways a start fails, and it is reported like the others
              _ (require 'replique.core 'replique.json)
              info ((resolve 'replique.core/start!) opts)]
          (print-line! stdout ((resolve 'replique.json/write-str)
                               (assoc info :tag "started"))))
        (catch Throwable t
          ;; the same exception object every other exception of the protocol
          ;; carries, so that a client has one way of showing all of them
          (report-start-failure! stdout (or (.getMessage t) (str (class t)))
                                 (exception-data t))
          (binding [*out* *err*]
            (println "Replique could not start:")
            (clojure.stacktrace/print-cause-trace t))
          (exit! 1))))))
