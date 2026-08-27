(ns replique.core
  "Starting and stopping a replique process. Replique owns the process it
  runs in - it is started by replique.main, never hosted by an application -
  so stopping is really only what a test does between two processes."
  (:require [replique.json :as json]
            [replique.output :as output]
            [replique.server :as server]
            [replique.state :as state]
            ;; load the roles: they register themselves
            [replique.control]
            [replique.repl])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file CopyOption Files LinkOption OpenOption Path Paths
            StandardCopyOption]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]
           [java.util UUID]))

(declare stop!)

(def ^:private process-id-pattern #"[A-Za-z0-9][A-Za-z0-9._+-]{0,127}")

(defn- path ^Path [dir & more]
  (Paths/get (str dir) (into-array String (map str more))))

(defn- validate-process-id [process-id]
  (let [s (if (string? process-id) process-id (str process-id))]
    (when-not (re-matches process-id-pattern s)
      (throw (ex-info (str "Invalid :process-id: " (pr-str process-id)
                           ". A process id must match " process-id-pattern
                           " - it is used as a file name.")
                      {:process-id process-id})))
    s))

(defn- validate-port [port]
  (when-not (and (integer? port) (<= 0 port 65535))
    (throw (ex-info (str "Invalid port: " (pr-str port)) {:port port})))
  port)

(defn normalize-opts
  "Validate the options and fill in the defaults.
    :process-id unique id of this process, generated when absent
    :host       host to bind to, defaults to the loopback address
    :port       0 (the default) binds to a free port
    :directory  where the port file is written, defaults to the working dir
    :port-file  overrides the port file location
    :tee-output report what the process prints to the control connections,
                true by default"
  [{:keys [process-id host port directory port-file tee-output]
    :or {tee-output true}}]
  (let [process-id (if (some? process-id)
                     (validate-process-id process-id)
                     (str (UUID/randomUUID)))
        directory (str (or directory (System/getProperty "user.dir")))]
    {:process-id process-id
     :host (or host "127.0.0.1")
     :port (validate-port (or port 0))
     :directory directory
     :tee-output (boolean tee-output)
     ;; absolute: the port file must not move when the working directory of
     ;; the process changes, and it always has a parent directory
     :port-file (.toAbsolutePath
                 (if port-file
                   (path port-file)
                   (path directory ".replique" "processes" (str process-id ".json"))))}))

(defn- set-permissions! [^Path p ^String perms]
  ;; Best effort: fails on filesystems that are not posix
  (try (Files/setPosixFilePermissions p (PosixFilePermissions/fromString perms))
       (catch Exception _)))

(defn- write-port-file! [^Path port-file content]
  (let [dir (.getParent port-file)
        tmp (path (str port-file ".tmp"))
        ^"[Ljava.nio.file.OpenOption;" open-opts (make-array OpenOption 0)]
    ;; Only the directories replique creates are made private - a :port-file
    ;; pointing into an existing directory must not change its permissions
    (when-not (Files/exists dir (make-array LinkOption 0))
      (Files/createDirectories dir (make-array FileAttribute 0))
      (set-permissions! dir "rwx------"))
    (try
      (Files/write tmp (.getBytes (str (json/write-str content) "\n")
                                  StandardCharsets/UTF_8)
                   open-opts)
      (set-permissions! tmp "rw-------")
      (Files/move tmp port-file
                  (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING]))
      port-file
      (catch Throwable t
        (try (Files/deleteIfExists tmp) (catch Exception _))
        (throw t)))))

(defn- delete-port-file! [^Path port-file]
  (try (Files/deleteIfExists port-file) (catch Exception _)))

(defn start!
  "Start the replique process. See normalize-opts for the options. Returns the
  process info - the same map that is written to the port file."
  ([] (start! nil))
  ([opts]
   (when (state/started?)
     (throw (ex-info "This process is already started" {:process-info (state/info)})))
   (let [{:keys [process-id host port directory port-file tee-output]} (normalize-opts opts)
         server (server/start-server {:host host
                                      :port port
                                      :name "replique"
                                      :process-id process-id})
         hook (Thread. (fn [] (delete-port-file! port-file)) "replique-shutdown")]
     (reset! state/process {:process-id process-id
                            ;; the address and port the server is really bound
                            ;; to - :port 0 binds to a free port
                            :host (server/server-host server)
                            :port (server/server-port server)
                            :directory directory
                            :port-file port-file
                            :started-at (System/currentTimeMillis)
                            :server server
                            :shutdown-hook hook})
     (.addShutdownHook (Runtime/getRuntime) hook)
     ;; after the process is registered: broadcasting an event reads the
     ;; connections from there
     (output/install! {:tee-output tee-output})
     (try
       (write-port-file! port-file (state/info))
       (catch Throwable t
         (stop!)
         (throw (ex-info (str "Could not write the port file " port-file) {} t))))
     (state/info))))

(defn stop!
  "Stop the replique process: close the server and its connections, delete the
  port file."
  []
  (when-let [{:keys [server port-file ^Thread shutdown-hook]} @state/process]
    (when shutdown-hook
      (try (.removeShutdownHook (Runtime/getRuntime) shutdown-hook)
           (catch IllegalStateException _)))
    (output/uninstall!)
    (when server (server/stop-server! server))
    (delete-port-file! port-file)
    (reset! state/process nil)
    nil))
