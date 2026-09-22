(ns replique.cljs
  "Whether this process compiles ClojureScript, and the environment it compiles
  it in.

  A ClojureScript question - what does this namespace hold, where was this var
  written, what could be written here - is the same question `replique.symbol'
  and `replique.completion' already answer, asked of another symbol table. The
  table is `clojure.cljs.env's: a compile environment holding two worlds of
  namespaces, one for the ClojureScript side and one for what its macros see of
  the JVM.

  ## What is not here, and why there is so little

  Nothing that translates a namespace. A ClojureScript namespace in this
  compiler is a real `clojure.lang.Namespace' - in a `NamespaceWorld' of its
  own rather than in the one `find-ns' reads - holding real Vars with real
  metadata. So `ns-name', `ns-publics', `ns-interns', `ns-aliases' and `meta'
  are already the functions to read one with, and the ops that read them need
  nothing added.

  Only two things are world-bound and so only two things are here: finding a
  namespace BY NAME, which `find-ns' and `all-ns' would answer out of the wrong
  world, and resolving a symbol, which `ns-resolve' would answer by falling
  back to a class. Both are `clojure.cljs.env's already.

  ## Written twice, like the analysis

  This is `replique.analysis's shape and for the same reason. The compiler is
  not a dependency of replique: it is a separate library, it needs a clojure
  that has `NamespaceWorld' in it, and a process is running with it or without
  it. Everything is looked up rather than required, one answer says whether the
  process can be asked at all - `available?' - and an op that cannot be
  answered says so and says what to start the process on instead."
  (:require [replique.directives :as directives]
            [replique.output :as output]
            [replique.protocol :as protocol])
  (:import [java.io Closeable File Writer]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.concurrent.locks ReentrantLock]))

;;; Whether this process can answer at all

(def ^:private subsystem
  "The ClojureScript compiler of this process, or nil where there is none.

  Resolved once and kept. Unlike the analysis subsystem the answer here is not
  quite a property of the jvm - a classpath can grow under a running process,
  and `:add-libs' is how - but a compiler arriving halfway through a session is
  not a thing to design for, and a delay that re-asked would re-ask on every
  keystroke that completes a name.

  Everything is looked up rather than only what says it is there, so that a
  half of this - an older build, a name that moved - is the same answer as none
  of it. A partial subsystem would be reported as available and fail at the one
  op that needed the missing piece.

  `requiring-resolve' loads the compiler where there is one, which is most of a
  second and is why this is a delay: a process nobody asks a ClojureScript
  question of never pays it."
  (delay
    (try
      (let [named (fn [ns sym]
                    (or (requiring-resolve (symbol (str ns) (str sym)))
                        (throw (ex-info (str "No " ns "/" sym) {}))))
            env   (partial named 'clojure.cljs.env)
            drv   (partial named 'clojure.cljs.driver)
            ana   (partial named 'clojure.cljs.analyzer)
            rpl   (partial named 'clojure.cljs.repl)
            rdr   (partial named 'clojure.cljs.reader)]
        {;; the cursor, as the VAR rather than its value: moving it is a
         ;; thread binding and a binding needs the var
         :current-ns    (env '*current-ns*)
         :compile-env   (env 'compile-env)
         :find-cljs-ns  (env 'find-cljs-ns)
         :all-cljs-ns   (env 'all-cljs-ns)
         :resolve-var   (env 'resolve-var)
         :resolve-ns    (env 'resolve-ns)
         :requires      (env 'requires)
         :imports       (env 'imports)
         :remove-var!   (env 'remove-var!)
         :compile-namespace! (drv 'compile-namespace!)
         :default-source-paths (drv 'default-source-paths)
         :find-source   (drv 'find-source)
         ;; the repl's half: one form evaluated, and the two runtimes it can be
         ;; evaluated in
         :eval-form     (rpl 'eval-form)
         :node-runtime  (rpl 'node-runtime)
         ;; and the reader a form is read with, which is the compiler's and not
         ;; clojure's: it resolves in the ClojureScript world and reads
         ;; #?(:cljs ...) the right way round
         :read-one+text (rdr 'read-one+text)
         :push-back-reader (rdr 'push-back-reader)
         ;; as the VAR, for the reason the cursor is one: the tags a repl adds
         ;; are a thread binding
         :host-data-readers (rdr '*host-data-readers*)
         ;; the file a definition records, which a repl knows only because its
         ;; client said so - see `eval-form'. A var rather than a value, again
         :source-file   (ana '*source-file*)})
      (catch Throwable _ nil))))

(defn available?
  "Whether this process can compile ClojureScript.

  Answered in a `:process-info', for somebody looking at a process and
  wondering why it will not complete a name in a .cljs buffer. Nothing needs it
  to ask: an op that cannot be answered says so, and says what to put on the
  classpath instead."
  []
  (some? @subsystem))

(defn- of
  "The compiler function called NAMED, or nil where there is no compiler."
  [named]
  (get @subsystem named))

(defn refuse-unless-available!
  "Refuse the request where this process has no ClojureScript compiler.

  WHAT is what this process cannot do, written as the sentence it goes in.

  Asked once, at the top of an op, rather than where each name is looked up.
  A process with no compiler has no ClojureScript namespaces at all, so the
  answer must not depend on what was asked about: a namespace that happens not
  to exist would otherwise be answered with an empty list, and read as \"there
  is nothing in it\" rather than as \"this process cannot say\".

  Named as what a client would have to change rather than as a missing var.
  Nothing is wrong with the request, and nothing about this process will make
  it work: it is running without the compiler the request is about."
  [what]
  (when-not (available?)
    (throw (ex-info (str "This process cannot " what
                         ": there is no ClojureScript compiler on its classpath."
                         " Start it with clojure.cljs on the classpath, on a"
                         " clojure whose namespaces can live in a world of their"
                         " own.")
                    {:replique/error :no-cljs}))))

;;; The cursor

(defn with-ns*
  "Call F with the compile cursor standing in NS.

  The cursor is a thread binding and not a field of the environment, which is
  the compiler's decision and the right one here: two things asking about two
  namespaces at once must not move each other. `set-current-ns!' is a `set!',
  so a scope that means to move it has to give itself a binding first, and
  every entry point that compiles anything does - this is ours.

  A tooling request always names the namespace it is asking about, so nothing
  here ever reads a cursor somebody else left behind.

  Refuses where there is no compiler, because the var to bind is the
  compiler's: without it this is a `push-thread-bindings' of nil, which throws
  a NullPointerException about a field instead of a sentence about a
  classpath."
  [ns f]
  (refuse-unless-available! "compile ClojureScript")
  (with-bindings* {(of :current-ns) ns} f))

(defmacro with-ns
  "Body with the compile cursor standing in NS. See `with-ns*'."
  [ns & body]
  `(with-ns* ~ns (fn [] ~@body)))

;;; What it is compiled for

(def targets
  "What a ClojureScript program can be compiled for, and so what there can be
  one compile environment of.

  TWO, and not because two was enough to start with: a browser and node resolve
  npm packages differently, and this compiler pins the difference rather than
  guessing it. `clojure.cljs.build_npm' bundles with esbuild's browser platform,
  and shares that option between the probe that decides which specifiers are
  CommonJS and the build that writes the module - so a node program compiled in
  a browser environment gets the browser condition of every package it requires,
  which for some is a different file and for a few is a stub that throws."
  #{:browser :node})

(def default-target
  "What a question that does not say answers about.

  The browser, because that is what the bundler is pinned to: a tooling client
  asking what a namespace holds, with no repl anywhere and nothing to say which
  runtime it means, is answered about the same resolution a build would do."
  :browser)

(def ^:dynamic *target*
  "The target of this thread's ClojureScript questions.

  DYNAMIC, and not an argument threaded through everything, because it is a
  property of who is asking rather than of what is being asked: a repl
  connection binds it once for its loop and everything under it - the reader,
  the compiler, the runtime - is that target's. A reading op binds nothing and
  is answered about `default-target'."
  default-target)

(defn as-target
  "The target X names, or nil where it names none.

  Whatever the client's edn printer writes - a keyword, a string, a symbol -
  read by the one function that reads all three, so that a target is spelled the
  way an op or a role is."
  [x]
  (let [k (protocol/as-keyword x)]
    (when (contains? targets k) k)))

;;; The environment

(defonce ^{:private true
           :doc
           "The state of each target this process has been asked about.

  ONE ENTRY PER TARGET, and within a target one entry for everybody. A compile
  environment is a symbol table, and two editors looking at one project are
  looking at one program: a var defined through one of them is a var the other
  completes, and both of them talk to the one page you have open. What they
  must not share is where each is standing, and they do not - that is the
  cursor, which belongs to whichever thread is asking.

  Made on the first ask rather than at startup. Making one loads and compiles
  cljs.core's macros, which is seconds, and a process that is never asked a
  ClojureScript question must not pay them - nor a process asked only about the
  browser pay for node."}
  environments
  (atom {}))

(defn- make-output-dir!
  "A directory to compile into, deleted when the process stops.

  Temporary, which is right for what this is and is not the last word: a
  browser fetches modules and source maps from here, and a directory that
  outlived a restart would leave a map already open in devtools naming
  something. One per target, because two targets compile two different programs
  out of the same sources."
  ^File []
  (.toFile (Files/createTempDirectory "replique-cljs" (make-array FileAttribute 0))))

(defn- new-environment!
  "A compile environment for TARGET, with cljs.core already in it."
  [t]
  (with-ns 'cljs.user
    (let [made {:target   t
                :cenv     ((of :compile-env) {:ns 'cljs.user :core-macros 'cljs.core})
                :out-dir  (make-output-dir!)
                ;; One evaluation at a time on this target - see `with-evaluation*'
                :lock     (ReentrantLock. true)
                ;; and who is having it, which is what the runtime's own output
                ;; is routed by
                :evaluating (atom nil)
                :runtime  (atom nil)}]
      ;; CLJS.CORE BEFORE ANYBODY LOOKS. A fresh environment is an EMPTY symbol
      ;; table - the two worlds are made here and nothing has been compiled into
      ;; them - so a completion asked of it would answer that the language has no
      ;; names in it, which is a worse answer than a refusal. Every namespace
      ;; requires cljs.core whether it says so or not, so this is what would have
      ;; been compiled by the first question anyway; the compiler's own repl loop
      ;; does the same thing before its first prompt, and for the same reason.
      ;;
      ;; The driver directly rather than `compile-namespace!' below, which asks
      ;; for the environment this is still making.
      ((of :compile-namespace!) (:cenv made) 'cljs.core {:out-dir (:out-dir made)})
      made)))

(defn environment
  "The ClojureScript compile environment of `*target*', made on the first ask.

    {:target   which runtime this is compiled for
     :cenv     the compile environment - clojure.cljs.env/CompileEnv
     :out-dir  where modules are compiled to}

  plus the state a repl needs, which is documented where it is used.

  Refuses where there is no compiler, rather than answering nil: everything
  that asks for this is about to use it, and a nil here would fail one call
  later with an exception about a record instead of a sentence about a
  classpath."
  []
  (refuse-unless-available! "compile ClojureScript")
  (let [t *target*]
    (or (get @environments t)
        ;; The cursor has to exist before compile-env positions itself: it does
        ;; that with a set!, which needs a binding to move. swap! would be wrong -
        ;; making one twice would compile cljs.core's macros twice - so the lock
        ;; is on this var and the second caller through it finds the first one's.
        (locking environments
          (or (get @environments t)
              (let [made (new-environment! t)]
                (swap! environments assoc t made)
                made))))))

;;; The runtime

(defn- runtime-writer
  "Where a runtime's own output goes: the repl having the evaluation, or every
  control connection when there is none.

  BECAUSE THERE ARE TWO KINDS OF IT AND ONE WIRE. A println inside the form you
  just typed is the answer to what you asked and belongs in your repl, beside
  the value; a setTimeout firing an hour later, or a page you left open
  yesterday logging a failed fetch, is the application talking and belongs to
  no repl - which is exactly what `replique.output' broadcasts for the JVM's own
  stdout.

  The runtimes cannot tell them apart - clojure.cljs.browser takes a print from
  any page at all, with or without a turn open - so replique tells them apart by
  the only thing it knows for certain: one evaluation happens on a target at a
  time (`with-evaluation*'), so output arriving while one is in flight is that
  one's."
  ^Writer [{:keys [evaluating target]}]
  (protocol/buffering-writer
   (fn [s]
     (if-let [conn @evaluating]
       (protocol/write-frame! conn (protocol/frame {:tag "out" :string s}))
       (output/broadcast-event!
        (protocol/event "out" {:string s :dialect "cljs" :target (name target)}))))))

(defn- browser-runtime
  "clojure.cljs.browser/browser-runtime, looked up now rather than with the rest.

  NOT PART OF `subsystem', which is the one exception to its rule that a half of
  the compiler is the same answer as none of it. The browser transport needs a
  websocket, the websocket framing is Java under the compiler's src/java, and the
  only build of it is Maven's - so a checkout that has never been built has the
  whole ClojureScript compiler and not this. Asked for eagerly, that would make
  `available?' false and take the reading ops and the node repl away with it, for
  a reason that is neither about them nor about the classpath the refusal would
  name.

  So it is the browser target that is missing rather than the compiler, and this
  is where it says so."
  []
  (or (requiring-resolve 'clojure.cljs.browser/browser-runtime)
      (throw (ex-info (str "This process cannot run a ClojureScript repl in a"
                           " browser: clojure.cljs.browser will not load. Its"
                           " websocket framing is compiled Java, so a source"
                           " checkout of the compiler needs to have been built"
                           " once - and a repl on node needs none of it.")
                      {:replique/error :no-cljs}))))

(defn runtime!
  "The JavaScript runtime of `*target*', started on the first ask.

  SEPARATE FROM THE ENVIRONMENT, and lazier, because the two are wanted by
  different things: a client asking what a namespace holds wants the symbol
  table and must not start a node process or open a port for it. A repl wants
  both.

  ONE PER TARGET, like the environment and for the same reason turned around:
  the browser runtime is an HTTP server and a websocket over one directory, and
  a second one would be a second URL for the same program - you would have to
  choose which of your repls the page you have open belongs to. Node is one
  process for the same reason a JVM is one process.

  Starting one can fail - node may not be installed, a port may be taken - and
  the failure is left to the caller rather than remembered: it is a thing about
  this machine right now, and the next ask may well succeed."
  []
  (let [{:keys [target ^File out-dir runtime] :as env} (environment)]
    (or @runtime
        (locking runtime
          (or @runtime
              (let [made (case target
                           :node    ((of :node-runtime) {:dir out-dir
                                                         :out (runtime-writer env)})
                           :browser ((browser-runtime) {:dir out-dir
                                                       :out (runtime-writer env)}))]
                (reset! runtime made)
                made))))))

;;; One evaluation at a time, per target

(defn with-evaluation*
  "Call F as the one evaluation happening on `*target*', on behalf of CONN.

  SERIALIZED HERE ALTHOUGH THE RUNTIMES ALREADY SERIALIZE. Both of them do -
  node answers by position and has asked one question, the browser holds a fair
  permit - so this adds no waiting that was not there, and it moves the waiting
  somewhere replique can see: while F runs, this target's output belongs to
  CONN and to nothing else, which is what `runtime-writer' reads.

  FAIR, so two repls on one target keep the order they asked in, and
  interruptible, so a repl waiting for the other one's (js/alert ...) answers
  an :interrupt rather than sitting through it. `locking' would do neither."
  [conn f]
  (let [{:keys [^ReentrantLock lock evaluating]} (environment)]
    (.lockInterruptibly lock)
    (try
      (reset! evaluating conn)
      (f)
      (finally
        (reset! evaluating nil)
        (.unlock lock)))))

(defmacro with-evaluation
  "Body as the one evaluation happening on `*target*'. See `with-evaluation*'."
  [conn & body]
  `(with-evaluation* ~conn (fn [] ~@body)))

;;; Letting go

(defn- delete-tree!
  [^File f]
  (when (.isDirectory f) (run! delete-tree! (.listFiles f)))
  (.delete f))

(defn release!
  "Forget every environment, stop every runtime, and delete what they compiled
  into.

  What stopping the process does. Not what a client asks for: a compile
  environment holds every var of every namespace anybody has loaded, and
  throwing it away is throwing away a session rather than clearing a cache.

  The runtimes first, because closing one unblocks whoever is waiting on it -
  a repl blocked on a page that will never answer - and because the browser
  runtime is serving the directory that is about to go."
  []
  (doseq [[_ {:keys [runtime ^File out-dir]}] (first (reset-vals! environments {}))]
    (when-let [^Closeable rt @runtime]
      (try (.close rt) (catch Throwable _ nil)))
    (when out-dir (delete-tree! out-dir)))
  nil)

;;; The seam: the two things that are world-bound

(defn namespaces
  "Every ClojureScript namespace of this process, as `clojure.lang.Namespace'
  objects.

  `all-ns' would answer out of the world `find-ns' reads, which is the JVM's
  and holds none of these."
  []
  ((of :all-cljs-ns) (:cenv (environment))))

(defn find-namespace
  "The ClojureScript namespace called SYM, or nil.

  `find-ns' without the world being wrong."
  [sym]
  ((of :find-cljs-ns) (:cenv (environment)) (symbol sym)))

(defn resolve-var
  "The ClojureScript Var that SYM names from inside NS, or nil.

  `ns-resolve' would answer this one wrongly rather than not at all: it
  resolves through the namespace's own mappings and aliases, which are right,
  and then falls back to a class name, which for ClojureScript is not a thing
  that exists. The compiler's own rule is the one that has to be used, because
  it is the rule the compiler compiled the file with - cljs.core referred into
  every namespace whether it says so or not, clojure.core rewritten to
  cljs.core, a :refer-clojure :exclude honoured."
  [ns sym]
  ;; the environment first and the cursor second: asking for it is what refuses
  ;; where there is no compiler, and a cursor is a binding of one of the
  ;; compiler's vars
  (let [{:keys [cenv]} (environment)]
    (with-ns (symbol ns) ((of :resolve-var) cenv (symbol sym)))))

(defn compile-namespace!
  "Compile NS and everything it requires into this process's output directory.

  What a REPL's require and a client's load both come down to. Here rather than
  in each of them because the output directory is the environment's, and a
  compilation that wrote somewhere else would be a compilation nothing is
  looking at."
  ([ns] (compile-namespace! ns nil))
  ([ns {:keys [source-paths]}]
   (let [{:keys [cenv ^File out-dir]} (environment)]
     (with-ns (symbol ns)
       ((of :compile-namespace!) cenv (symbol ns)
        (cond-> {:out-dir out-dir}
          source-paths (assoc :source-paths source-paths)))))))

;;; Evaluating

(defn current-ns
  "Where the cursor of this thread is standing, as a symbol."
  []
  (deref (of :current-ns)))

(defn reader
  "SRC - a Reader or a string - as the reader `read-form' reads from.

  The compiler's own, which is line-numbering, so the positions it records are
  real ones and a form typed here gets a source map of its own. Idempotent, so
  a repl holding one across forms hands it back unwrapped."
  [src]
  ((of :push-back-reader) src))

(defn read-form
  "The next form of RDR and the text it was read from: [form text], or
  [EOF nil].

  The COMPILER's reader and not clojure's, which is most of why a .cljs repl is
  not a .clj repl with another eval: it resolves symbols in the ClojureScript
  world, takes the :cljs branch of every reader conditional, and reads
  ClojureScript's own tags. Replique's directives are added on top of those -
  see `replique.directives' - because the compiler binds its table rather than
  adding to it and a repl that could not read #replique/ns could not be driven
  by an editor.

  Only the characters of this form are consumed, which is what lets a form move
  the cursor and have the next one resolve where it moved to."
  [rdr eof]
  (let [{:keys [cenv]} (environment)]
    (with-bindings* {(of :host-data-readers) directives/data-readers}
      #((of :read-one+text) cenv rdr eof))))

(defn eval-form
  "Evaluate FORM in the runtime of `*target*' and answer what came back:

    {:status :success/:error :value \"...\" :stacktrace ... :phase ...}

  :value is what the RUNTIME printed, as a string, because printing happens
  where the value is. A failure on this side of the wire - a form that will not
  compile - comes back in the same shape with a :phase saying which side
  noticed.

  :text is the source of FORM as it was typed, which is what lets the input
  carry a source map of its own: a stack frame in a form typed here then names
  the form and the line within it rather than an offset into the runtime.
  Optional, because a form replique built rather than read has no text.

  :file is where that source came from, and is what a var defined by FORM
  records. A repl reads from a socket and so knows no file at all - which is why
  a var defined at one has none (doc/cljs-compiler.md 5.63) - unless the client
  says, which is what #replique/src is for. Clojure's repl answers the same
  question the same way: what a client sends becomes *file*, and the var
  records it."
  ([form] (eval-form form nil))
  ([form {:keys [text file]}]
   (let [{:keys [cenv ^File out-dir]} (environment)
         run #((of :eval-form) cenv (runtime!) form {:out-dir out-dir} text)]
     (if file
       (with-bindings* {(of :source-file) file} run)
       (run)))))
