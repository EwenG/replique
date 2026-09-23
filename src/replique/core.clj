(ns replique.core
  "Starting and stopping a replique process. Replique owns the process it
  runs in - it is started by replique.main, never hosted by an application -
  so stopping is really only what a test does between two processes."
  (:require [replique.cljs :as cljs]
            [replique.json :as json]
            [replique.output :as output]
            [replique.server :as server]
            [replique.state :as state]
            ;; load the roles: they register themselves
            [replique.control]
            [replique.repl])
  (:import [java.io IOException]
           [java.nio.charset StandardCharsets]
           [java.nio.file CopyOption FileAlreadyExistsException Files LinkOption
            OpenOption Path Paths]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]
           [java.util UUID]))

(declare stop!)

(def ^:private process-id-pattern #"[A-Za-z0-9][A-Za-z0-9._+-]{0,127}")

(defn- path ^Path [dir & more]
  (Paths/get (str dir) (into-array String (map str more))))

(defn- absolute
  "Resolved against the working directory, with the . and .. a client wrote
  taken out. Lexical: a symlink is left as the name it was given, which is
  the name the client knows the directory under."
  ^Path [^Path p]
  (.normalize (.toAbsolutePath p)))

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

(defn- validate-host
  "A host is a string, and one that is not is refused rather than made one.
  The other options replique is given are names it writes down - a process
  id, a directory - and stringifying those keeps whatever the client meant.
  A host is resolved, and every number resolves: getByName reads 42 as
  0.0.0.42 and 0 as 0.0.0.0, which is every interface of the machine on a
  process that has no authentication. Refusing says which option is wrong,
  where the cast this replaces named java.lang.Long and nothing else."
  [host]
  (when-not (string? host)
    (throw (ex-info (str "Invalid :host: " (pr-str host) ". A host must be a string.")
                    {:host host})))
  host)

(defn- validate-init
  "Whether the init scripts are read. True or false and nothing else: this is
  the one option that turns something off, and an option that turned it off for
  every value but one would be an option nobody can read twice. See
  `load-init-scripts!'."
  [init]
  (when-not (boolean? init)
    (throw (ex-info (str "Invalid :init: " (pr-str init) ". :init is true or false.")
                    {:init init})))
  init)

(def ^:private option-keys #{:process-id :host :port :directory :port-file :init})

(defn- validate-opts
  "Refuse an option that is not one, rather than drop it. The options are how
  a client says what it will look for the process under, so a :proces-id that
  goes unread does not start the process the client asked for - it starts one
  under a random name, successfully, and the editor then waits for a process
  that is running and cannot be found."
  [opts]
  ;; Said here rather than left to the destructuring, which reads a string or
  ;; a vector as a map of nothing and starts a process on all defaults - and,
  ;; since the unknown option check reads the keys, now fails at whatever the
  ;; value happens to be instead
  (when-not (or (nil? opts) (map? opts))
    (throw (ex-info (str "The options must be a map, got: " (pr-str opts))
                    {:options opts})))
  (when-let [unknown (seq (remove option-keys (keys opts)))]
    (let [names (fn [ks] (apply str (interpose ", " (map str ks))))]
      (throw (ex-info (str "Unknown option" (when (next unknown) "s") ": "
                           (names (sort-by str unknown)) ". The options are: "
                           (names (sort option-keys)) ".")
                      {:unknown-options (vec unknown)}))))
  opts)

(defn normalize-opts
  "Validate the options and fill in the defaults. An option this does not know
  is refused rather than ignored.
    :process-id unique id of this process, generated when absent
    :host       host to bind to, defaults to the loopback address
    :port       0 (the default) binds to a free port
    :directory  where the port file is written, defaults to the working dir
    :port-file  overrides the port file location, relative to :directory
    :init       read the init scripts, default true - see `load-init-scripts!'"
  [opts]
  (let [{:keys [process-id host port directory port-file init]} (validate-opts opts)
        process-id (if (some? process-id)
                     (validate-process-id process-id)
                     (str (UUID/randomUUID)))
        ;; Absolute: the directory travels to the editor, in the port file and
        ;; in the reply to every handshake, and a "." there names the working
        ;; directory of whoever reads it rather than the one this process was
        ;; started in
        directory (str (absolute (path (or directory (System/getProperty "user.dir")))))]
    {:process-id process-id
     :host (if (some? host) (validate-host host) "127.0.0.1")
     :port (validate-port (or port 0))
     :directory directory
     ;; absolute for that reason too, and because the port file must not move
     ;; when the working directory of the process changes. It always has a
     ;; parent directory.
     ;;
     ;; A relative :port-file is relative to :directory, which is where the
     ;; default one goes and the only reading that makes the two options say
     ;; one thing: a client that named a directory and then a name inside it
     ;; would otherwise get its port file in the directory the process happens
     ;; to have been started in. Path/resolve leaves an absolute one alone
     :port-file (absolute
                 (if port-file
                   (.resolve (path directory) (path port-file))
                   (path directory ".replique" "processes" (str process-id ".json"))))
     ;; True where nothing says, because the scripts are what a project has
     ;; already said about itself. False is for a client that is starting this
     ;; process to find out what it does WITHOUT them - a test of replique
     ;; itself, and the answer to an init script that broke a start
     :init (if (some? init) (validate-init init) true)}))

(defn- set-permissions! [^Path p ^String perms]
  ;; Best effort: fails on filesystems that are not posix
  (try (Files/setPosixFilePermissions p (PosixFilePermissions/fromString perms))
       (catch Exception _)))

(defn- claim! [^Path tmp ^Path port-file]
  ;; Linked into place rather than moved. Not because a move would take a name
  ;; that is taken - Files/move is not the bare rename(2) that replaces what
  ;; it finds, it refuses an existing target too - but because of how it
  ;; refuses: rename(2) never answers EEXIST for a regular file, so the
  ;; refusal is the provider looking at the target before it renames, and
  ;; between that look and the rename is a window. That window is the one this
  ;; claim exists to close, start! having already looked once; a second look no
  ;; closer to the write would only make it narrower. createLink refuses in one
  ;; operation - the name either becomes this process's or it does not - so it
  ;; fails against a file that appeared after start! looked, which is what
  ;; makes the claim the claim rather than a second guess. Either way the
  ;; content is whole before the name exists.
  ;;
  ;; Filesystems with no hard links - fat, some network mounts - fall back to
  ;; the move. A name that is taken is still refused there, so that part does
  ;; not depend on the filesystem; what is given up is the atomicity, and the
  ;; window start! already lives with is all that is left of the claim. Which
  ;; exception says so depends on where the refusal comes from: a provider
  ;; that does not implement links at all throws UnsupportedOperationException,
  ;; while the unix provider issues link(2) and turns the EPERM or ENOTSUP a
  ;; real filesystem answers with into a FileSystemException. Catching only
  ;; the first is catching the rarer one, and leaves a process unable to start
  ;; where the move it replaced worked.
  (let [move! (fn [] (Files/move tmp port-file (make-array CopyOption 0)))]
    (try
      (Files/createLink port-file tmp)
      ;; The name is taken, which is the answer this is here to get - never a
      ;; reason to go looking for another way to take it
      (catch FileAlreadyExistsException e (throw e))
      (catch UnsupportedOperationException _ (move!))
      (catch IOException _ (move!)))))

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

(defn- taken-message [process-id ^Path port-file directory]
  (str "The process-id " (pr-str process-id) " is taken in " directory ": "
       port-file " exists. Start under another :process-id, or delete that"
       " file if its process is gone."))

(defn- delete-port-file! [^Path port-file]
  (try (Files/deleteIfExists port-file) (catch Exception _)))

;;; What the project says before anything connects

(defn- init-scripts
  "The init scripts of DIRECTORY, in the order they are read: the user's, then
  the project's.

  BESIDE THE PORT FILE, in the .replique directory a process already writes its
  name into, and under :directory rather than the working directory - a process
  told to run for a project elsewhere reads that project's script and not the
  one where it happens to have been started.

  Two of them because they say different kinds of thing. A user's says how they
  like a repl - what *print-length* is, what their editor needs - and holds for
  every project they open; a project's says what the project is, and has the
  last word because it is the one under version control. One file where both
  names point at the same place: a process started in the home directory reads
  it once, not twice."
  [directory]
  (distinct [(path (System/getProperty "user.home") ".replique" "init.clj")
             (path directory ".replique" "init.clj")]))

(defn- load-init-scripts!
  "Read the init scripts of DIRECTORY. See `init-scripts'.

  CODE RATHER THAN CONFIGURATION, and deliberately: what these files do is
  mostly not settable. They install hooks that are functions, define macros and
  put them where a library will find them, require a namespace early because
  something downstream reads a var at load time, make the directories a build
  writes into. A format holding only data would cover the smallest part of that
  and leave a second place to look for the rest.

  BEFORE THE SERVER IS BOUND, because what they say is what a connection finds.
  A repl that connected while one was still running would be standing in a
  process half configured, and a compile environment made before the compiler
  options were read would hold none of them.

  AFTER THE PORT FILE IS LOOKED FOR, because a script does things - the ones in
  the wild make directories and write marker files - and a start about to be
  refused for a name it cannot have must not do them first.

  THROUGH THE LOADER EVERY CONNECTION SHARES, so that a script adding a library
  adds it where the rest of the process will look. The thread is put back the
  way it was found: it is the thread that started the process, not a connection,
  and it does not keep what it borrowed.

  A script that throws is a start that failed, reported the way every other one
  is - the client that spawned the process gets a start-failed naming the file,
  and the trace goes to stderr. The alternative is a process that comes up
  configured differently from what its own project says, which is a difference
  nobody notices until something behaves oddly hours later."
  [directory]
  (let [thread (Thread/currentThread)
        borrowed (.getContextClassLoader thread)]
    (.setContextClassLoader thread state/class-loader)
    (try
      (doseq [^Path script (init-scripts directory)]
        (when (Files/exists script (make-array LinkOption 0))
          (try
            (load-file (str script))
            (catch Throwable t
              (throw (ex-info (str "Could not read the init script " script)
                              {:init-script (str script)} t))))))
      (finally
        (.setContextClassLoader thread borrowed)))))

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
   (let [{:keys [process-id host port directory port-file init]} (normalize-opts opts)
         ;; Before the server is bound and before anything is installed, so
         ;; that a refusal costs nothing and unwinds nothing.  The port file
         ;; is how anything finds a process: taking the name of a process
         ;; that is running would leave it running with nothing able to reach
         ;; it.  A file whose process is gone says something wrong - what is
         ;; there is what a client finds out by connecting, and it is deleted
         ;; there.  The claim write-port-file! makes is what a process racing
         ;; this one loses against; this is what says why
         _ (when (Files/exists port-file (make-array LinkOption 0))
             (throw (ex-info (taken-message process-id port-file directory)
                             {:process-id process-id :port-file (str port-file)})))
         _ (when init (load-init-scripts! directory))
         server (server/start-server {:host host
                                      :port port
                                      :name "replique"
                                      :process-id process-id})
         ;; Deleting the port file is deleting a name, and this process only
         ;; owns that name once its claim went through: a start that lost the
         ;; race must leave the winner's file where it is, or the refusal that
         ;; sent it here would be worse than the overwriting it replaced
         hook (Thread. (fn [] (when (:port-file-claimed @state/process)
                                (delete-port-file! port-file)))
                       "replique-shutdown")]
     (reset! state/process {:process-id process-id
                            ;; the address and port the server is really bound
                            ;; to - :port 0 binds to a free port
                            :host (server/server-host server)
                            :port (server/server-port server)
                            :directory directory
                            :port-file port-file
                            :port-file-claimed false
                            :started-at (System/currentTimeMillis)
                            :server server
                            :shutdown-hook hook})
     ;; Registered, so everything from here unwinds through stop!. A failure
     ;; that left it registered would leave the server listening under a name
     ;; nothing wrote - nothing could find it, and start! would refuse to try
     ;; again, because as far as it can see a process is started
     (try
       (.addShutdownHook (Runtime/getRuntime) hook)
       ;; after the process is registered: broadcasting an event reads the
       ;; connections from there
       (output/install!)
       (catch Throwable t
         (stop!)
         (throw (ex-info (str "Could not start the process in " directory) {} t))))
     (try
       (write-port-file! port-file (state/info))
       ;; The claim went through, so the name is this process's to delete
       (swap! state/process assoc :port-file-claimed true)
       ;; The name went to another process between the check above and the
       ;; claim, which is the window the claim is there to close. The same
       ;; refusal, because it is the same refusal: what the check says early
       ;; the claim says late, and a client that spawns processes must not
       ;; have to read two messages to learn one thing
       (catch FileAlreadyExistsException t
         (stop!)
         (throw (ex-info (taken-message process-id port-file directory)
                         {:process-id process-id :port-file (str port-file)} t)))
       (catch Throwable t
         (stop!)
         (throw (ex-info (str "Could not write the port file " port-file) {} t))))
     (state/info))))

(defn stop!
  "Stop the replique process: close the server and its connections, delete the
  port file it claimed.

  Only the one it claimed: this is what a start that failed unwinds with, and
  a start fails when another process holds the name - whose file this must
  then not touch."
  []
  (when-let [{:keys [server port-file port-file-claimed ^Thread shutdown-hook]}
             @state/process]
    (when shutdown-hook
      (try (.removeShutdownHook (Runtime/getRuntime) shutdown-hook)
           (catch IllegalStateException _)))
    (output/uninstall!)
    ;; Before the server, because this deletes a directory and the connections
    ;; are what might still be compiling into it
    (cljs/release!)
    (when server (server/stop-server! server))
    (when port-file-claimed (delete-port-file! port-file))
    (reset! state/process nil)
    nil))
