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
           [java.nio.file CopyOption Files LinkOption OpenOption Path Paths]
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
    :port-file  overrides the port file location"
  [{:keys [process-id host port directory port-file]}]
  (let [process-id (if (some? process-id)
                     (validate-process-id process-id)
                     (str (UUID/randomUUID)))
        directory (str (or directory (System/getProperty "user.dir")))]
    {:process-id process-id
     :host (or host "127.0.0.1")
     :port (validate-port (or port 0))
     :directory directory
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

(defn- claim! [^Path tmp ^Path port-file]
  ;; Linked into place rather than moved. A move is a rename, and rename
  ;; replaces what it finds: atomically, but what it would be atomically
  ;; taking is the name of a process that is still running. link fails when
  ;; the name is taken, which is the answer wanted here, and it fails against
  ;; a file that appeared after start! looked - which is what makes the claim
  ;; the claim rather than a second guess. Either way the content is whole
  ;; before the name exists.
  ;;
  ;; Filesystems with no hard links - fat, some network mounts - fall back to
  ;; the move, and there the check start! makes is all there is.
  (try
    (Files/createLink port-file tmp)
    (catch UnsupportedOperationException _
      (Files/move tmp port-file (make-array CopyOption 0)))))

(defn- write-port-file! [^Path port-file content]
  (let [dir (.getParent port-file)
        ;; Named after this process: two jvms writing a port file in one
        ;; directory must not be writing over one another's temporary file -
        ;; what would come of that is a port file holding the host and port
        ;; of the process that lost the race
        tmp (path (str port-file "." (.pid (java.lang.ProcessHandle/current)) ".tmp"))
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
      (claim! tmp port-file)
      port-file
      ;; Always, and not only after a failure: what the link leaves behind is
      ;; a second name for the port file, and a second name is a way to write
      ;; the port file without going through the claim - which the next start
      ;; of this same process would do
      (finally
        (try (Files/deleteIfExists tmp) (catch Exception _))))))

(defn- delete-port-file! [^Path port-file]
  (try (Files/deleteIfExists port-file) (catch Exception _)))

(defn start!
  "Start the replique process. See normalize-opts for the options. Returns the
  process info - the same map that is written to the port file.

  Refuses to start when the port file already exists: it is the name a client
  finds this process under, and taking a name that is taken would leave
  whatever holds it running with nothing able to reach it. The name is the
  :process-id, so this is what stops a second process of the same project -
  a directory can hold as many processes as they have names."
  ([] (start! nil))
  ([opts]
   (when (state/started?)
     (throw (ex-info "This process is already started" {:process-info (state/info)})))
   (let [{:keys [process-id host port directory port-file]} (normalize-opts opts)
         ;; Before the server is bound and before anything is installed, so
         ;; that a refusal costs nothing and unwinds nothing.  The port file
         ;; is how anything finds a process: taking the name of a process
         ;; that is running would leave it running with nothing able to reach
         ;; it.  A file whose process is gone says something wrong - what is
         ;; there is what a client finds out by connecting, and it is deleted
         ;; there.  The claim write-port-file! makes is what a process racing
         ;; this one loses against; this is what says why
         _ (when (Files/exists port-file (make-array LinkOption 0))
             (throw (ex-info (str "The process-id " (pr-str process-id) " is taken in "
                                  directory ": " port-file " exists. Start under another"
                                  " :process-id, or delete that file if its process is gone.")
                             {:process-id process-id :port-file (str port-file)})))
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
     (output/install!)
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
