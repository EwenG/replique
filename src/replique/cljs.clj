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
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [replique.directives :as directives]
            [replique.json :as json]
            [replique.output :as output]
            [replique.protocol :as protocol]
            [replique.state :as state])
  (:import [java.io Closeable File Writer]
           [java.net URI]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.concurrent.locks ReentrantLock]
           [java.util.regex Matcher]))

;;; Whether this process can answer at all

(def ^:private compiler-resource
  "A file every ClojureScript compiler this process could use answers to.

  WHAT SAYS `ON THE CLASSPATH' WITHOUT LOADING ANYTHING, and the reason there
  has to be such a thing is that loading is expensive in a way that is not only
  time: `requiring-resolve' takes clojure's require lock, so a process busy
  loading anything at all holds the compiler up and a process loading the
  compiler holds everything else up. A handshake that has to refuse a process
  with no ClojureScript on it must not pay that to find out - see
  `replique.cljs-repl/accept!'.

  `clojure.cljs.env' rather than any of the others because it is the one the
  rest of them require: a classpath holding it holds a compiler, whether or not
  that compiler will load."
  "clojure/cljs/env.clj")

(defn on-classpath?
  "Whether a ClojureScript compiler looks like it is on this process's classpath.

  WHICH IS NOT WHETHER IT WORKS - `available?' is that question and costs what
  loading the compiler costs. This one is a lookup in a directory listing, and
  what it is for is the answer that can be given before paying it: a process
  that has no compiler at all can be told so at once, and a process that has
  one is worth waiting for.

  OPTIMISTIC ON PURPOSE, AND NEVER PESSIMISTIC. One file is not the compiler,
  so half of one - an older build, a name that moved - answers true here and
  false to `available?'. That is the right way round for the one thing this
  decides: a handshake refused on it is refused only where there is certainly
  nothing to wait for, and everything else goes on to find out properly and to
  say what it found - see `unavailable-reason'."
  []
  (some? (io/resource compiler-resource)))

(def ^:private subsystem
  "The ClojureScript compiler of this process, or what went wrong loading it.

  A MAP WHERE IT LOADED AND THE THROWABLE WHERE IT DID NOT, rather than nil for
  both, because the two send whoever reads them to two different places - see
  `unavailable-reason'. Nothing else tells them apart: by the time anything
  asks, the loading happened once, long ago, on somebody else's thread.

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
            out   (partial named 'clojure.cljs.output)
            rpl   (partial named 'clojure.cljs.repl)
            rdr   (partial named 'clojure.cljs.reader)
            mx    (partial named 'clojure.cljs.macroexpand)
            nms   (partial named 'clojure.cljs.names)
            goog  (partial named 'clojure.cljs.goog)]
        {;; the cursor, as the VAR rather than its value: moving it is a
         ;; thread binding and a binding needs the var
         :current-ns    (env '*current-ns*)
         :compile-env   (env 'compile-env)
         :find-cljs-ns  (env 'find-cljs-ns)
         :all-cljs-ns   (env 'all-cljs-ns)
         :resolve-var   (env 'resolve-var)
         :resolve-ns    (env 'resolve-ns)
         :excluded?     (env 'excluded?)
         :requires      (env 'requires)
         :imports       (env 'imports)
         :remove-var!   (env 'remove-var!)
         ;; the other world: what a namespace can see of the jvm's macros. The
         ;; compiler's own lookups rather than a second spelling of them, since
         ;; a name eldoc answers as a macro has to be the macro the compiler
         ;; expands - see `macro-var'. `macro-namespace' is private there, and
         ;; is taken all the same: it is the rule a qualified macro is read by,
         ;; alias rewrites and all, and the rule written twice would be the rule
         ;; disagreeing with itself the first time either of them changed
         :macro-view    (env 'macro-view)
         :macro-var     (mx 'macro-var)
         :macro-namespace (mx 'macro-namespace)
         ;; and the host's names: what an alias of a JavaScript module stands
         ;; for, whether a namespace is a Closure provide, and how a name of the
         ;; host's is spelled in JavaScript - see `host-object'
         :js-aliases    (env 'js-aliases)
         :goog-ns?      (goog 'goog-ns?)
         :goog-known?   (goog 'known?)
         :host-name     (nms 'host-name)
         ;; and what a name written in code is, where it is the host's - the
         ;; analyzer's own reading of it, see `host-ref'
         :analyze       (ana 'analyze)
         :analysis-env  (env 'analysis-env)
         :compile-namespace! (drv 'compile-namespace!)
         :default-source-paths (drv 'default-source-paths)
         :find-source   (drv 'find-source)
         ;; where a namespace's module is written, relative to the output root:
         ;; the compiler's own answer rather than a second spelling of the rule,
         ;; because a page fetching that path is fetching a FILE and a rule
         ;; written twice is a rule that will disagree with itself
         :ns->path      (out 'ns->path)
         ;; the repl's half: one form evaluated, and the two runtimes it can be
         ;; evaluated in
         :eval-form     (rpl 'eval-form)
         :node-runtime  (rpl 'node-runtime)
         ;; and the tooling's half: raw JavaScript, in every runtime, within a
         ;; bound - see `eval-js'. ONE KEY AND NOT TWO, although the compiler
         ;; has `evaluate-within' beside it: this map is all or nothing by
         ;; design, so a key nothing calls could do nothing here but make
         ;; `available?' false against a compiler that happens not to have it,
         ;; and take the reading ops and the node repl down with it
         :evaluate-all-within (rpl 'evaluate-all-within)
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
      (catch Throwable t t))))

(defn- compiler
  "The compiler functions, or nil where there are none.

  A map is what loading one produced and a Throwable is what failing to
  produced, so `map?' is the whole of the test - see `subsystem'."
  []
  (let [s @subsystem]
    (when (map? s) s)))

(defn- failure
  "What went wrong loading the compiler, or nil where nothing did.

  A classpath with no compiler on it goes wrong too, and says so as a
  FileNotFoundException - which is how `unavailable-reason' tells the two
  apart, that being the one thing `requiring-resolve' throws for a namespace
  it could not FIND rather than could not load."
  []
  (let [s @subsystem]
    (when (instance? Throwable s) s)))

(defn available?
  "Whether this process can compile ClojureScript.

  Answered in a `:process-info', for somebody looking at a process and
  wondering why it will not complete a name in a .cljs buffer. Nothing needs it
  to ask: an op that cannot be answered says so, and says what to put on the
  classpath instead - which is `unavailable-reason', and is the half of this
  worth reading when the answer is no."
  []
  (some? (compiler)))

(defn- of
  "The compiler function called NAMED, or nil where there is no compiler."
  [named]
  (get (compiler) named))

(defn- failure-text
  "What T says, as the end of a sentence.

  THE ROOT ALONGSIDE THE TOP and not instead of it, because the two carry
  different halves of one fact: clojure wraps what a file failed on in a
  CompilerException that names the file and the line, and the thing that
  actually failed is underneath it. Either alone leaves out where or what.

  Without the full stop some of these end with, since what this is is the
  middle of somebody else's sentence."
  [^Throwable t]
  (let [say  (fn [^Throwable x] (or (.getMessage x) (.getName (class x))))
        root (loop [x t] (if-let [c (.getCause x)] (recur c) x))
        text (if (identical? root t)
               (say t)
               (str (say t) " (" (say root) ")"))]
    (string/replace (string/trimr text) #"\.$" "")))

(defn unavailable-reason
  "Why this process cannot compile ClojureScript, as the end of a sentence
  about it, or nil where it can.

  TWO ANSWERS AND NOT ONE, because they send whoever reads them to two
  different places. A classpath with no compiler on it is the ordinary case,
  and the sentence names what to start the process with. A classpath that HAS
  one and could not load it is the other, and answering that as the first sends
  somebody off to check a classpath that is right: what is wrong is the
  compiler itself, and until it is said nothing anywhere says it - the failure
  happens once, inside a delay, on whichever thread asked first, and is seen by
  nobody.

  TOLD APART BY THE FAILURE AND NOT BY `on-classpath?', which is a fast path
  and is allowed to be optimistic. What is exact is what was thrown: a
  FileNotFoundException is `requiring-resolve' saying it could not FIND a
  namespace, and anything else is a namespace it found and could not load. The
  missing file is named either way, so a compiler whose own dependency is what
  is absent - the one case this can read the wrong way round - still says which
  file to go and look for.

  Realises the compiler, so it costs what `available?' costs. Asked where the
  answer is already known to be no, which is the only place it means anything."
  []
  (when-not (available?)
    (let [t (failure)]
      (if (and t (not (instance? java.io.FileNotFoundException t)))
        (str "clojure.cljs is on its classpath and would not load: "
             (failure-text t)
             ". A compiler that is there and will not load is usually one built"
             " against another clojure than the one this process is running.")
        (str "there is no ClojureScript compiler on its classpath"
             (when t (str ": " (failure-text t)))
             ". Start it with clojure.cljs on the classpath, on a clojure whose"
             " namespaces can live in a world of their own.")))))

(defn cannot
  "The sentence refusing WHAT, for a process that cannot compile ClojureScript.

  WHAT is what the caller cannot do, written as the sentence it goes in. Said
  in one place because it is said in three - the ops, the handshake of a
  ClojureScript repl, and the runtime that handshake goes on to start - and a
  reason written three times is a reason that will disagree with itself."
  [what]
  (str "This process cannot " what ": " (unavailable-reason)))

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
    (throw (ex-info (cannot what) {:replique/error :no-cljs}))))

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

;;; How it is compiled
;;
;; HERE, between the environments and the code that makes one, because that is
;; what the two halves of this need. `set-option!' refuses one change once a
;; compile environment exists, which it has to read above; everything that
;; builds an options map is below, and every one of them reads `options'.

(def option-keys
  "The compiler options a project can set. See `set-option!'.

  THE WHOLE OF THEM, and there are three. This compiler reads six keys from an
  options map, and the other three are not a project's to choose:

    :out-dir       the environment's own - a temporary directory per target,
                   and the one place everything looking at this process is
                   looking
    :source-paths  already right without being said: it defaults to the
                   directory entries of the classpath
                   (`clojure.cljs.driver/default-source-paths'), which is
                   exactly what this process was started with
    :main          a repl's rather than a project's. It arrives in the
                   handshake, because it is what one connection wants to stand
                   in and not something about the program

  What is left:

    :closure-library  compile against the Closure Library on the classpath
                      instead of the subset this compiler carries. What a
                      project whose own code or whose libraries reach past that
                      subset - goog.style, goog.net.XhrIo, goog.functions -
                      turns on. Having the library on the classpath is not
                      enough and is not meant to be: anything depending on
                      ClojureScript brings it, so its presence says nothing
                      about intent. See `clojure.cljs.goog'
    :npm              how npm packages are built, as a map: :build, :acquire,
                      :root, :esbuild, :dev. See `clojure.cljs.npm'
    :static-dispatch  compile a call to a var of known shape as a call to the
                      arity that answers it. Faster, and it does not survive a
                      redefinition that drops that arity"
  #{:closure-library :npm :static-dispatch})

(defonce ^{:doc
           "What this process compiles with, over the compiler's own defaults.

  Empty until something sets one, and what is not in here is not passed on: an
  option absent from a compilation's map is the compiler's default, which is
  the one place those are written down. Copying them here would be writing them
  twice, and the copy is the one that would drift.

  Read at every compilation rather than read once into the environment when it
  is made - see `compiler-opts'."}
  options
  (atom {}))

(defn set-option!
  "Set the compiler option K to V for this process, and answer with all of them.

  WHAT AN INIT SCRIPT CALLS. A compile environment is made on the first
  ClojureScript question asked of this process and is never asked for options
  again, so a project says what it needs in `.replique/init.clj' - which is read
  before the process listens, and so before there is anybody to ask a first
  question.

  Refuses a key that is not an option rather than remembering it. An option
  nobody reads looks exactly like one that worked, and what would have shown
  otherwise is a compilation minutes later, on another thread, with the init
  script long out of sight.

  :closure-library is refused once this process has compiled something. Every
  other option is read afresh at each compilation and can be changed whenever;
  the Closure tree cannot, because the output directory already holds files
  compiled against one of them, and a program holding goog.string from the
  subset and goog.style from the library is one nobody can reason about."
  [k v]
  (when-not (contains? option-keys k)
    (throw (ex-info (str "Not a ClojureScript compiler option: " (pr-str k)
                         ". The options are: "
                         (apply str (interpose ", " (sort (map str option-keys))))
                         ".")
                    {:option k :options (vec (sort option-keys))})))
  ;; Setting it to what it already says is not a change, and refusing it would
  ;; be refusing an init script the right to state what is already true. As
  ;; booleans, because that is what the compiler reads it as
  ;; (`clojure.cljs.goog/library-option'): an option absent and an option set
  ;; to false are the same tree, and telling them apart here would refuse a
  ;; script for writing down the default.
  (when (and (= :closure-library k)
             (not= (boolean v) (boolean (:closure-library @options)))
             (seq @environments))
    (throw (ex-info (str "The Closure Library cannot be chosen once ClojureScript"
                         " has been compiled. This process has compiled for "
                         (apply str (interpose ", " (sort (map name (keys @environments)))))
                         " already, and its output holds files compiled against"
                         " the other tree. It is set in .replique/init.clj, which"
                         " is read before this process listens.")
                    {:option k :targets (vec (sort (keys @environments)))})))
  (swap! options assoc k v))

(defn- compiler-opts
  "The options a compilation into OUT-DIR runs under.

  OUT-DIR LAST, so that it is not among the things an init script can set. It
  is the environment's: the runtime fetches its modules from there and the ops
  read what was written there, so a compilation that went anywhere else would
  be a compilation nothing is looking at.

  :ANALYSIS IS NOT A PROJECT'S EITHER, and it is not optional. It is what the
  compiler's repl reads to decide whether the files a `require' or a
  `load-file' compiles keep the analysis model - and replique wants them kept
  always, for the reason `replique.cljs-analysis/with-analysis*' gives: the
  only moment to record a file is the moment it is compiled. It is written here
  rather than in `option-keys' because an init script turning it off would be
  turning off `#replique/reload' and the ClojureScript half of `:usages', with
  nothing to say that is what it had done."
  [out-dir]
  (assoc @options :out-dir out-dir :analysis true))

(defn- delete-tree!
  [^File f]
  (when (.isDirectory f) (run! delete-tree! (.listFiles f)))
  (.delete f))

(defn- delete-at-exit!
  "Arrange for DIR to go when this jvm does, and answer the hook that will do it.

  WHAT `File.deleteOnExit' CANNOT DO, and it is the idiom this is named after.
  That one deletes a FILE, and a directory only while it is empty - this one is
  about to hold a module and a source map per namespace, which for a real program
  is a thousand files. It also keeps its list forever, so a long-lived process
  that made several would hold every path it ever named. A hook that deletes the
  tree is the same promise kept for a directory.

  BESIDE THE ORDERLY PATH RATHER THAN INSTEAD OF IT. `release!' is what a process
  being STOPPED does, and it does more than this: it closes the runtimes first,
  because one of them is serving the directory that is about to go. This is what
  is left when nobody called stop! - a ^C in the terminal the process was started
  from, a SIGTERM from whatever supervises it, an editor exiting. The two are not
  a fallback pair, they are two different endings, and `release!' takes the hook
  off the list when it gets there first.

  NOT EVERY ENDING IS A SHUTDOWN, and the honest limit is worth writing down: a
  SIGKILL and a hard crash run no hook, here or anywhere, so a directory can
  still be orphaned. What would cover that is a sweep of the siblings at startup,
  which is a different piece of work and not this one.

  A HOOK THAT CANNOT THROW. One that did would print a stack trace out of a
  process that is already leaving, over whatever the user was reading. And a
  registration is refused once shutdown has begun, which is not a failure either:
  it means the jvm is already doing what the hook was for."
  ^Thread [^File dir]
  (let [hook (Thread. ^Runnable (fn [] (try (delete-tree! dir) (catch Throwable _ nil)))
                      "replique-cljs-output")]
    (try (.addShutdownHook (Runtime/getRuntime) hook)
         (catch IllegalStateException _ nil))
    hook))

(defn- forget-at-exit!
  "Take HOOK off the jvm's list, for a directory that has already gone.

  Because a process can be stopped without the jvm ending - `replique.core/stop!'
  is a function, and the tests call it many times in one jvm - and a hook nobody
  removed is a promise about a directory that is not there any more. Removed the
  way `stop!' removes its own, refusal and all: `IllegalStateException' here says
  shutdown has begun, and a hook that is about to run is not one to argue with."
  [^Thread hook]
  (when hook
    (try (.removeShutdownHook (Runtime/getRuntime) hook)
         (catch IllegalStateException _ nil)))
  nil)

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
    (let [out-dir (make-output-dir!)
          made {:target   t
                :cenv     ((of :compile-env) {:ns 'cljs.user :core-macros 'cljs.core})
                :out-dir  out-dir
                ;; WHAT DELETES IT WHEN NOBODY STOPS THIS PROCESS. Kept here
                ;; rather than forgotten, so that `release!' can take it off the
                ;; list again - see `delete-at-exit!'.
                :out-dir-hook (delete-at-exit! out-dir)
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
      ((of :compile-namespace!) (:cenv made) 'cljs.core (compiler-opts (:out-dir made)))
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

;; `runtime!' refreshes the main modules of the project when it starts the
;; browser runtime, and those live in the section below - which in turn needs
;; `runtime!' for the port to refresh them to.
(declare refresh-main-js-on-start!)

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
                ;; THE PORT IS NEW AND THE FILES NAMING THE OLD ONE ARE STILL
                ;; THERE. A main module is written into an application's own
                ;; assets and outlives every process that ever wrote it, so
                ;; this - the moment there is a port at all - is the moment the
                ;; ones under this project stop being stale. See
                ;; `refresh-main-js-on-start!'.
                (when (= :browser target)
                  (refresh-main-js-on-start! (:url made)))
                made))))))

(defn runtime-connected?
  "Whether `*target*' has a runtime with something in it to evaluate, and
  WITHOUT starting one to find out.

  TWO TARGETS AND TWO ANSWERS, because the two runtimes are two different kinds
  of thing. NODE IS A PROCESS REPLIQUE STARTS, and `runtime!' does not return
  until it has dialled back - so a node runtime that exists is a node runtime
  that is there, and there is nothing further to ask. A BROWSER IS A PAGE
  SOMEBODY OPENS, and the two servers are up long before the page is:
  `(:session rt)' is nil until one connects and a different number after every
  refresh, which is the only honest way to ask the question.

  FALSE WHERE NO RUNTIME HAS BEEN STARTED, rather than starting one. That is
  what makes this askable at all: everything that wants to know is deciding
  WHETHER to evaluate, and starting a node process in order to discover that
  there was nothing to evaluate in would be the question answering itself."
  []
  (let [{:keys [target runtime]} (environment)]
    (boolean
     (when-let [rt @runtime]
       (case target
         :node    true
         :browser (some? (:session rt)))))))

(defn browser-runtime-url
  "The URL the browser runtime of this process is serving on, or nil.

  WITHOUT STARTING ONE, which is the whole of why this is not `runtime!' asked
  for its `:url'. Everything that wants this is asking whether there is a port
  to name, and a port brought into being in order to answer that question is a
  page pointed at two servers nobody asked for. A process that has never
  compiled a line of ClojureScript answers nil rather than paying for
  cljs.core's macros in order to say so, which is why the environment is read
  where it lies instead of through `environment'.

  NOT `runtime-connected?', which asks whether a PAGE is there. The two servers
  listen from the moment the runtime is made, and a main module naming that port
  is right whether or not anybody has opened the page yet - being opened is what
  the module is for.

  THE BROWSER AND ONLY THE BROWSER. Node is a process replique starts and talks
  to over its own pipe; there is no page on it and no URL for one to name."
  []
  (some-> (:browser @environments) :runtime deref :url))

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

(defn with-target-lock*
  "Call F holding this target's lock, without claiming its output.

  `with-evaluation*' is the other way to hold it, and what differs is who the
  runtime's output belongs to while it is held. An evaluation owns it, which
  is the whole of what `runtime-writer' reads. A TOOLING OP DOES NOT: nothing
  it does makes a runtime print, and a page that logs a failed fetch while it
  runs is talking to nobody in particular - so claiming the output would send
  that line down a control connection as a repl's `out' frame, which is a
  frame no control client is reading for.

  What it is for is the other half: a request that CHANGES this world must not
  run while something is compiling into it. Which is the reason the Clojure
  side of the same op takes clojure's require lock."
  [f]
  (let [{:keys [^ReentrantLock lock]} (environment)]
    (.lockInterruptibly lock)
    (try (f) (finally (.unlock lock)))))

(defmacro with-evaluation
  "Body as the one evaluation happening on `*target*'. See `with-evaluation*'."
  [conn & body]
  `(with-evaluation* ~conn (fn [] ~@body)))

;;; The module a page loads

(def main-js-marker
  "The first line of every file `main-js' writes, and the whole of how one is
  recognised again.

  A CLIENT HAS TO FIND THESE FILES WITHOUT BEING TOLD WHERE THEY ARE, because
  the one who knows is whoever wrote the application's page, and that was not
  replique. Replique master solved it by walking the project for .js files and
  reading the first line of each, and the line it looks for is

    //main-js-file autogenerated by replique

  which this one deliberately IS NOT A PREFIX OF. Master matches its marker by
  comparing that many characters, so a first line beginning with it would be
  rewritten by master's editor with master's port - two repliques quietly
  fighting over one file, in a project where both are used. This one is its own
  string and neither recognises the other's."
  "//replique-2 main module")

(defn main-js
  "The text of the ES module a page loads to reach this process's ClojureScript.

  WHAT THE APPLICATION'S OWN PAGE INCLUDES. `:main' compiles a program into the
  output directory and stops there, because on the browser the runtime is a page
  somebody opens and the page is what loads the program - this is the file that
  does it. Written into the application's own assets, included from its own
  <script type=\"module\">, and served from its own origin; the modules come from
  THIS process, cross-origin, which works because the asset server sends
  Access-Control-Allow-Origin for exactly this case.

  A MODULE AND NOT A SCRIPT, which is the one thing that is not a transcription
  of what master wrote. Master document.wrote four <script> tags and let goog's
  uncompiled loader pull the namespace graph by name; this compiler emits ES
  modules, so the graph is pulled by one import of the entry module and the
  static imports inside it.

  THE FOUR CONSTANTS ARE THE WHOLE OF WHAT GOES STALE, and each is on a line of
  its own so that a client can rewrite it with a regular expression rather than
  by reading JavaScript. The two servers listen on ephemeral ports - a file
  written by yesterday's process names a port nobody is listening on - and
  `mainNs' is there for the client rather than for the browser: it is how an
  editor that found these files learns what namespaces this project can be
  started on, which is where master's menu of them came from.

  MAIN MAY BE NIL, and then the page connects and loads nothing. That is a repl
  in a page of yours with no program in it, which is a thing to want."
  [url main]
  (let [^URI u (URI. url)
        path (when main (str ((of :ns->path) (symbol main))))]
    (str main-js-marker " - autogenerated, and rewritten every time a repl starts\n"
         "//\n"
         "// Include this from your own page:\n"
         "//   <script type=\"module\" src=\"this file\"></script>\n"
         "//\n"
         "// It connects the page to a replique process and imports the program\n"
         "// that process compiled. A 404 from here means a module nothing\n"
         "// compiled - name it in :main, or require it at the repl.\n"
         "const host = \"" (.getHost u) "\";\n"
         "const port = \"" (.getPort u) "\";\n"
         "const mainNs = " (if main (str "\"" main "\"") "null") ";\n"
         "const mainPath = " (if path (str "\"" path "\"") "null") ";\n"
         "\n"
         "const base = \"http://\" + host + \":\" + port + \"/\";\n"
         "globalThis.__replique = {base, mainNs};\n"
         "const {connect} = await import(base + \"runtime_browser.js\");\n"
         "await connect();\n"
         "if (mainPath) await import(base + mainPath);\n")))

(defn write-main-js!
  "Write `main-js' into FILE, for the browser runtime of this process.

  STARTS THE BROWSER RUNTIME IF IT IS NOT UP, because the port is the whole
  point of the file and there is no port until the two servers are listening.
  Refusing instead would only mean every client called this twice - once to be
  told, once after doing what it was told - for a thing that is idempotent and
  that the next `:main' would have started anyway.

  Answers what it wrote, which is what a client shows somebody."
  [^File file main]
  (binding [*target* :browser]
    (let [url (:url (runtime!))
          ^File file (.getAbsoluteFile file)]
      (io/make-parents file)
      (spit file (main-js url main))
      {:file (str file) :url url :main (when main (str main))})))

;;; Finding them again, and the port that went stale

(defn- begins-with-marker?
  "Whether FILE is one of ours, which its first line is the whole of.

  BOUNDED AT THE LENGTH OF THE MARKER rather than reading a line, because this
  is asked of every .js file under a project and some of them are one minified
  line of several megabytes. A reader hands back what it has rather than what
  was asked of it, so it is read until the buffer is full or there is no more."
  [^File file]
  (let [n   (count main-js-marker)
        buf (char-array n)]
    (try
      (with-open [r (io/reader file)]
        (loop [got 0]
          (if (= got n)
            (= main-js-marker (String. buf))
            (let [read (.read r buf got (- n got))]
              (if (neg? read)
                false
                (recur (+ got read)))))))
      (catch Throwable _ false))))

(defn- searched?
  "Whether the walk for main modules goes into DIR.

  A WALK IS THE PRICE OF NOT BEING TOLD WHERE THE FILES ARE - the one who knows
  is whoever wrote the application's page, and that was not replique - and
  these are what keeps it from also being the price of starting a repl. A dot
  directory is somebody's metadata and .git is enormous. node_modules is tens
  of thousands of .js files and not one of them was written by this.

  A SYMBOLIC LINK TO A DIRECTORY IS FOLLOWED, and this is the one rule that was
  once here and is not. The danger it was guarding against is real - a link is
  how the walk of a project becomes the walk of a disk, or of itself - but
  refusing every link answers it by refusing a whole way of working. A project
  laid out as a tree of links into another checkout, so that one directory can
  be pointed at whichever worktree is wanted, is a project whose assets are all
  behind links: the main module is found in neither the menu of what to start a
  repl on nor the refresh that moves it to this process's port, and the second
  one fails SILENTLY, because it only speaks when something moved.

  So the loop is handled where it belongs, by remembering the real path of every
  directory entered (see `main-js-files'), and the size of the walk is left to
  the rules above and to .repliqueignore, which is what they are for.

  .REPLIQUEIGNORE IS REPLIQUE 1'S FILE, read here for its sake rather than on
  its merits: master walks for the same reason and a project that already has
  them has them for this. Master's own source says the mechanism is the wrong
  one - a build directory gets deleted and recreated without the file in it -
  so this is a compatibility, and a list said in the init script is where it
  should end up. It is also now the answer to a link into somewhere enormous."
  [^File dir]
  (let [name (.getName dir)]
    (and (.canRead dir)
         (not (.startsWith name "."))
         (not= "node_modules" name)
         (not (.exists (File. dir ".repliqueignore"))))))

(defn- real-path
  "The path of DIR with every link on the way to it resolved, or nil.

  What two names for one directory have in common and nothing else does, which
  is what makes it the thing to remember a directory by. Nil where the answer
  cannot be had - a link to nowhere, a directory that went away between being
  listed and being asked about - and a directory with no real path is one the
  walk does not go into, since it could not be told apart from any other."
  [^File dir]
  (try (str (.toRealPath (.toPath dir) (make-array java.nio.file.LinkOption 0)))
       (catch Throwable _ nil)))

(defn main-js-files
  "Every main module under DIR, found the way a client would have to find it.

  BY THE FIRST LINE AND NOT BY A LIST KEPT SOMEWHERE, because the file outlives
  the process that wrote it - that is the whole of what makes it go stale - and
  a process started tomorrow has never heard of it and still has to find it.

  SYMBOLIC LINKS ARE FOLLOWED, to a file and to a directory alike: what somebody
  linked into their project is what they meant to be there, and a project laid
  out as a tree of links into a checkout elsewhere is a project whose assets are
  ALL behind them. See `searched?'.

  ONCE EACH, BY REAL PATH. Following links is how a walk becomes a walk of
  itself - a link to an ancestor is a walk with no end - and how one directory
  gets walked twice under two names. Both are the same fact and have the same
  answer: the real path of every directory entered is remembered, starting with
  DIR itself, and one already entered is not entered again. So a loop stops at
  the moment it closes, and a file reached by two routes is found once."
  [^File dir]
  (let [seen (volatile! #{})
        enter? (fn [^File d]
                 (when-let [path (real-path d)]
                   (when-not (contains? @seen path)
                     (vswap! seen conj path)
                     true)))
        walk (fn walk [^File d]
               (reduce (fn [found ^File f]
                         (cond
                           (.isDirectory f)
                           (if (and (searched? f) (enter? f))
                             (into found (walk f))
                             found)

                           (and (.endsWith (.getName f) ".js") (begins-with-marker? f))
                           (conj found f)

                           :else found))
                       []
                       (or (.listFiles d) [])))]
    (if (enter? dir) (walk dir) [])))

;; The two lines that go stale and the one an editor reads. Each constant is on
;; a line of its own for exactly this - see `main-js'.
(def ^:private host-line #"(?m)^const host = \"[^\"]*\";$")
(def ^:private port-line #"(?m)^const port = \"[^\"]*\";$")
(def ^:private main-ns-line #"(?m)^const mainNs = \"([^\"]*)\";$")

(defn- trouble
  "Say what went wrong with FILE, and answer for it anyway.

  ONE FILE FAILING IS NOT THE WALK FAILING - a main module somebody made read
  only is a thing that happens - so what could not be done is reported and the
  rest are still done."
  [^File file answer ^Throwable t]
  (let [message (or (ex-message t) (.getName (class t)))]
    (binding [*out* *err*]
      (println (str "Could not refresh the main module " file ": " message)))
    (assoc answer :refreshed false :error message)))

(defn- refreshed!
  "Move one main module to HOST and PORT, and say what became of it.

  WRITTEN ONLY WHERE IT CHANGED. A file already naming this port is a file
  whose modification time means something to somebody's build, and a refresh
  that touched all of them every time would be a rebuild nobody asked for.

  A FAILURE STILL ANSWERS `:main' WHERE IT GOT THAT FAR: a module that could
  not be rewritten is still a module naming a namespace this project can be
  started on, and the menu that is for has nothing to do with whether the port
  moved."
  [^File file ^String host ^String port]
  (let [start {:file (str file)}
        ;; Either the text, or the answer `trouble' already made for it
        text  (try (slurp file) (catch Throwable t (trouble file start t)))]
    (if-not (string? text)
      text
      (let [now    (-> text
                       (string/replace host-line (Matcher/quoteReplacement host))
                       (string/replace port-line (Matcher/quoteReplacement port)))
            answer (assoc start :main (second (re-find main-ns-line now)))]
        (if (= text now)
          (assoc answer :refreshed false)
          (try (spit file now)
               (assoc answer :refreshed true)
               (catch Throwable t (trouble file answer t))))))))

(defn refresh-main-js!
  "Point every main module under DIR at URL, and answer what was found there.

  WHAT MAKES `main-js's OWN FIRST LINE TRUE. The two servers listen on
  ephemeral ports, so a file written by yesterday's process names a port nobody
  is listening on, and a page including it reaches nothing at all. Replique 1
  refreshed them from the editor: `replique.el' walked the project directory,
  matched each file by its first line, and rewrote the port and the host with a
  regular expression. This is that, moved into the process - which knows its
  own directory, cannot disagree with itself about the port, and will do it for
  a client that has not been written yet as readily as for one that has.

  THE HOST AND THE PORT AND NOTHING ELSE, which is what master rewrote as well.
  `mainNs' and `mainPath' say what the PAGE loads, and that is the choice of
  whoever wrote the page rather than of whichever repl happens to be running
  now - a `:main' given to this repl is a different question and does not get
  to answer this one.

  One map per module found:

    {:file      where it is
     :main      the namespace its page loads, or nil where it names none
     :refreshed whether this moved it, as against finding it already here
     :error     what went wrong, where something did}

  WHAT WAS FOUND IS ANSWERED RATHER THAN KEPT, `:main' most of all: an editor
  that found these files learns from them which namespaces this project can be
  started on, and that is where master's menu of them came from. `main-modules'
  is that question asked on its own, by a client that wants the menu and not
  the move."
  [dir ^String url]
  (let [^URI u (URI. url)
        host   (str "const host = \"" (.getHost u) "\";")
        port   (str "const port = \"" (.getPort u) "\";")]
    (mapv #(refreshed! % host port) (main-js-files (io/file dir)))))

(defn- program-of
  "The namespace the page of the main module FILE loads, or nil.

  NIL ALSO WHERE THE FILE COULD NOT BE READ, which is what `begins-with-marker?'
  answers for one it cannot open. A module that went away between the walk that
  found it and this is a file the answer is now silent about, and not a reason
  for the menu of all the others to be a refusal."
  [^File file]
  (try (second (re-find main-ns-line (slurp file)))
       (catch Throwable _ nil)))

(defn main-modules
  "Every main module under DIR, and the program each one's page loads.

  THE MENU, WHICH IS THE WHOLE OF WHAT `mainNs' IS FOR. A main module names the
  namespace its page loads so that whoever finds the file learns which programs
  this project can be started on - master's editor harvested exactly this, in
  the pass that refreshed the port, and offered it when a ClojureScript repl was
  asked for. The BROWSER never reads it: the page imports `mainPath', and a name
  is of no use to it.

  READ AND NOT WRITTEN, AND ASKED WITHOUT A PORT. `refresh-main-js!' answers the
  same namespaces and has to be given a url, because moving the files is what it
  is for; this is the question on its own, and a client asking which programs a
  project has must not start a browser runtime by asking it.

  One map per module found:

    {:file where it is
     :main the namespace its page loads, or nil where it names none}

  A MODULE THAT NAMES NONE IS STILL ANSWERED, because what was found is a fact
  about the files rather than only about the names in them: a page with no
  program in it is a thing to want, and a client showing what is here would
  otherwise show nothing where one is."
  [dir]
  (mapv (fn [^File f] {:file (str f) :main (program-of f)})
        (main-js-files (io/file dir))))

(defn- refresh-main-js-on-start!
  "The refresh a browser runtime does when it starts, which is the only moment
  it can be done at.

  NOT WHEN THE PROCESS STARTS, which is when master's editor did it, because
  there is no port until the two servers are listening and they are not started
  until something asks. A refresh any earlier would write a port nobody serves.

  NOTHING HERE FAILS A REPL. This is a convenience about files that are not
  this process's own; a project directory that cannot be walked is a reason to
  say so and carry on, and not a reason for the browser repl somebody has just
  asked for to fail to start.

  SAID ONLY WHERE SOMETHING CHANGED. A project with no main modules in it is
  most projects, and a walk that announced itself every time would be noise on
  every repl that ever starts."
  [url]
  (try
    (when-let [dir (:directory (state/info))]
      (let [done (count (filter :refreshed (refresh-main-js! dir url)))]
        (when (pos? done)
          (println (str done " main module" (when (> done 1) "s")
                        " refreshed to " url)))))
    (catch Throwable t
      (binding [*out* *err*]
        (println (str "Could not look for main modules to refresh: "
                      (or (ex-message t) (.getName (class t)))))))))

;;; Letting go

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
  (doseq [[_ {:keys [runtime ^File out-dir out-dir-hook]}]
          (first (reset-vals! environments {}))]
    (when-let [^Closeable rt @runtime]
      (try (.close rt) (catch Throwable _ nil)))
    (when out-dir (delete-tree! out-dir))
    ;; AFTER the directory and not instead of it: the hook is a promise about a
    ;; directory, and the promise is kept here rather than dropped
    (forget-at-exit! out-dir-hook))
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

(def core
  "The namespace every ClojureScript namespace refers whether it says so or not.

  clojure.core's opposite number, and named here because every rule that
  mentions one mentions the other: what a namespace refers before it refers
  anything, what a `:refer-clojure' refers from, and what a bare name means
  where the namespace itself maps nothing of that name."
  'cljs.core)

(defn resolve-namespace
  "The ClojureScript namespace SCOPE names from inside NS, or nil.

  What stands before the slash of a qualified name, which is not simply a
  namespace of that name: ClojureScript resolves an alias first, then the
  namespace's own name, then the ones it REQUIRED - and a namespace that was
  merely compiled is reachable through none of them. Clojure is looser here,
  where `find-ns' answers for anything loaded, so this is a rule of its own
  rather than the same rule asked of another world.

  Creates nothing. The one branch of the compiler's that would - cljs.core,
  which every namespace requires said or not - names a namespace this
  environment compiled before it answered anything at all."
  [ns scope]
  (let [{:keys [cenv]} (environment)]
    (with-ns (symbol ns) ((of :resolve-ns) cenv (symbol scope)))))

(defn excluded?
  "Whether NS said a bare SYM is not to mean cljs.core's var of that name.

  Which is `:refer-clojure :exclude', and it is the one thing standing between
  what cljs.core holds and what this namespace can write without a slash.
  A ClojureScript namespace does not MAP the core vars - the compiler reaches
  them by a rule rather than by a mapping, so that a namespace of thirty lines
  is not thirty lines plus a thousand entries - and a reader of that table has
  to apply the same rule, exclusions and all."
  [ns sym]
  (let [{:keys [cenv]} (environment)]
    ((of :excluded?) cenv (symbol ns) (symbol sym))))

;;; The macros

;; A ClojureScript namespace can write two kinds of names and only one of them
;; is in it. Its vars are; its macros are Clojure vars of this jvm, reached
;; through a view the compiler keeps beside the namespace - what
;; :require-macros aliased and :refer-macros referred - and, for every name
;; nothing else claims, through cljs.core's macros by the same rule that
;; reaches cljs.core's functions. `defn', `when' and `let' are all three of
;; them macros and nothing else, so a reader of the namespace alone says they
;; mean nothing.

(defn macro-var
  "The macro SYM names from inside NS, or nil where it names none.

  The compiler's rule, which is the one the file was expanded with: a
  :refer-macros first, then cljs.core's macros unless a :refer-clojure
  :exclude said otherwise; and for a qualified name an alias of the view, then
  one of the namespace, then the name in full - clojure.core rewritten to
  cljs.core on the way, which is what makes clojure.core/let the macro a .cljs
  file calls."
  ^clojure.lang.Var [ns sym]
  (let [{:keys [cenv]} (environment)]
    (with-ns (symbol ns) ((of :macro-var) cenv {} (symbol sym)))))

(defn macro-namespace
  "The jvm namespace SCOPE names as a namespace of macros from inside NS, or
  nil where it names none. What stands before the slash of `m/a-macro'."
  ^clojure.lang.Namespace [ns scope]
  (let [{:keys [cenv]} (environment)]
    (with-ns (symbol ns) ((of :macro-namespace) cenv (symbol scope)))))

(defn- view-of
  "The macro view of the namespace NS, or nil where the compiler made none.

  Found rather than made: `clojure.cljs.env/macro-view' makes one it does not
  find, and a question asked about a namespace is not a reason to add
  anything to the compiler's world.

  Reflective, since the class is the forked clojure's: named here, this file
  would not compile on the stock clojure a process without the compiler runs."
  ^clojure.lang.Namespace [ns]
  (let [{:keys [cenv]} (environment)]
    (.find (:macro-world cenv) (symbol ns))))

(defn- macro? [v]
  (and (var? v) (.isMacro ^clojure.lang.Var v)))

(defn macro-refers
  "The macros NS can write as a bare name, by that name.

  What its view maps - a :refer-macros, under the name a :rename-macros gave
  it - and cljs.core's macros where the namespace neither maps the name nor
  excluded it - cljs.core itself included, whose own code writes `when' like
  anybody's. Publics only for the second half, for `replique.names/core-refers's
  reason: a name core did not publish is not one to offer."
  [ns]
  (let [{:keys [cenv]} (environment)
        here (symbol ns)
        mapped (into {} (filter (comp macro? val)) (some-> (view-of here) ns-map))
        macros (some-> (:core-macros cenv) find-ns)]
    (merge (when macros
             (into {}
                   (filter (fn [[sym v]] (and (macro? v) (not (excluded? here sym)))))
                   (ns-publics macros)))
           mapped)))

(defn macro-aliases
  "The aliases the :require-macros of NS made, by alias."
  [ns]
  (some-> (view-of (symbol ns)) ns-aliases))

(defn compile-namespace!
  "Compile NS and everything it requires into this process's output directory.

  What a REPL's require and a client's load both come down to. Here rather than
  in each of them because the output directory is the environment's, and a
  compilation that wrote somewhere else would be a compilation nothing is
  looking at.

  WHAT THIS ENVIRONMENT ALREADY HOLDS IS NOT COMPILED AGAIN, which is the driver's
  doing and not this function's: an environment lives as long as this process, so
  the second ask for a namespace in it is free. `:reload-all' is how a caller says
  it wants the graph read off disk regardless - the only way to pick up a file
  edited outside this process - and it is what a repl starting on a `:main' asks
  for. See clojure.cljs.driver/ensure!."
  ([ns] (compile-namespace! ns nil))
  ([ns {:keys [source-paths reload reload-all]}]
   (let [{:keys [cenv ^File out-dir]} (environment)]
     (with-ns (symbol ns)
       ((of :compile-namespace!) cenv (symbol ns)
        (cond-> (compiler-opts out-dir)
          source-paths (assoc :source-paths source-paths)
          reload       (assoc :reload true)
          reload-all   (assoc :reload-all true)))))))

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
         run #((of :eval-form) cenv (runtime!) form (compiler-opts out-dir) text)]
     (if file
       (with-bindings* {(of :source-file) file} run)
       (run)))))

(defn eval-js
  "Evaluate the JavaScript JS in every runtime of `*target*', giving up after
  MS, and answer what came back:

    {:status :success/:error :value \"...\"}

  `eval-form's shape, out of the same runtimes. Everything around the
  evaluation is what differs.

  RAW JAVASCRIPT, BECAUSE WHAT ASKS IS NOT A REPL. Swapping a stylesheet is
  JavaScript with no ClojureScript anywhere in it, and the only road to it
  through `eval-form' would be compiling a `(js* \"...\")' wrapper around a
  string that is already the answer - an analyzer pass and a source map to
  produce what was in hand.

  EVERY RUNTIME, BECAUSE A PAGE IS NOT THE PAGE. The application is open and so
  is the tab beside it, and a stylesheet that reloaded in one of them is a
  stylesheet that did not reload.

  WITHIN MS, AND THE BOUND IS NOT OPTIONAL. What asks this asks it while
  somebody types, and what it asks may be a tab that has been asleep since
  yesterday. `clojure.cljs.repl/evaluate-all-within' tells the two apart in its
  answer - `busy' where the question never got in front of a page, `timed-out'
  where a page has it and is still thinking - which are different things to
  have to report.

  AND IT DOES NOT TAKE THE TARGET'S LOCK, which is the deliberate one.
  `with-evaluation*' is how a repl takes its turn, and a turn behind a
  `require' of a hundred namespaces is a minute long. A stylesheet reload
  queued there would arrive after exactly the wait it exists to avoid. It does
  not claim the target's output either, so anything this printed while a repl
  was evaluating would come out framed as that repl's - it prints nothing,
  which is what makes that acceptable rather than a defect.

  The runtime is started where there is none, as `eval-form' starts one, and a
  browser with no page connected answers with the URL to open rather than
  waiting for somebody to open it."
  [js ms]
  ;; The runtime in a let, so that what refuses a process without the compiler
  ;; is `environment' in writing rather than by argument evaluation order:
  ;; `of' answers nil where there is none, and nil is not a thing to call
  (let [runtime (runtime!)]
    ((of :evaluate-all-within) runtime js ms)))

;;; The host's names

;; What a JavaScript object has is not written down anywhere the compiler can
;; read: js/console is whatever the runtime's console is, and a Closure provide
;; or an npm module is a file nobody declared the exports of. So completing a
;; name written on one of them asks the runtime - see host_names.js - and what
;; is worked out here is which object to ask about. That half is the compiler's,
;; because what an alias stands for is.

(defn host-object
  "Where the object SCOPE names from inside NS is, as host_names.js takes it,
  or nil where SCOPE names nothing of the host's.

  js, a :require of a JavaScript module under an alias - the $ sugar's path
  included - and a Closure provide, whether it is written in full, under the
  alias of a :require or as the name an :import gave it. A ClojureScript
  namespace is none of these: its vars are known without asking anything,
  and asking the runtime would answer them munged."
  [ns ^String scope]
  (let [{:keys [cenv]} (environment)
        here (symbol ns)
        sym (symbol scope)]
    (if-let [[specifier path] (get ((of :js-aliases) cenv here) sym)]
      {:root "module" :name specifier
       :path (if path (string/split path #"\.") [])}
      (if (= "js" scope)
        {:root "global" :path []}
        (when-let [found (or (some-> (find-namespace here) ns-aliases (get sym))
                             (find-namespace sym))]
          (let [named (ns-name found)]
            (when ((of :goog-ns?) named)
              {:root "ns" :name (str named) :path []})))))))

(defn goog-ns?
  "Whether NAMED is a name in the Closure tree - goog, or under goog."
  [named]
  ((of :goog-ns?) (symbol (str named))))

(defn host-name
  "The JavaScript spelling of a name the host owns: js/foo-bar is foo_bar."
  ^String [^String written]
  ((of :host-name) written))

(def ^:private host-names-resource "replique/host_names.js")

(def ^:private host-names-within
  "How long a runtime is given to say what an object has.

  SHORT, because this is asked on a keystroke. A runtime that is busy is one
  in the middle of something somebody sent it, and no names is the right
  answer then rather than an editor that waits for the something to finish."
  300)

(defn host-names
  "The names the object OBJECT (a `host-object') has in the runtime of
  `*target*', as {:functions [...] :fields [...]} - or nil where there is no
  runtime to ask, or it could not answer in time.

  NEVER STARTS A RUNTIME. A completion is not a reason to open a port or start
  node, and a runtime nobody has started has no page and no module loaded, so
  it has nothing to say about them either - see `runtime-connected?'."
  [object]
  (when (runtime-connected?)
    (let [js (str (slurp (io/resource host-names-resource))
                  "(" (json/write-str object) ")")
          {:keys [status value]} (eval-js js host-names-within)]
      (when (= :success status)
        (let [[functions fields] (try (edn/read-string (edn/read-string value))
                                      (catch Exception _ nil))]
          {:functions (vec functions) :fields (vec fields)})))))

(def ^:private host-ops
  "The analyzer's nodes for a reference to the host, and the kind the model
  files each of them under. See clojure.cljs.analysis's host-ref."
  {:js-var :global
   :goog-var :goog-var
   :goog-ns :goog-ns
   :js-module :js-module
   :js-module-var :js-module})

(defn- written-in-full
  "What SYM names of the Closure tree when it is written out in full, or nil
  where it names nothing there.

  Read off the name rather than analysed, because the analyzer treats writing
  one out as requiring it - see clojure.cljs.analyzer's require-goog! - and a
  question asked about a name must not add a require to the namespace it was
  asked in."
  [sym]
  (let [goog? (of :goog-ns?)
        known? (of :goog-known?)
        provide (fn [named] (when (known? named) {:kind :goog-ns :name named}))
        member (fn [ns-part member]
                 (or (provide (symbol (str ns-part "." member)))
                     {:kind :goog-var :name (symbol (str ns-part) (str member))}))]
    (if-let [ns-part (namespace sym)]
      (when (goog? (symbol ns-part)) (member ns-part (name sym)))
      (when (goog? sym)
        (or (provide sym)
            (let [s (str sym)
                  index (.lastIndexOf s ".")]
              (when (pos? index) (member (subs s 0 index) (subs s (inc index))))))))))

(defn- analysable?
  "Whether SYM can be handed to the analyzer without the analyzer changing
  anything.

  It can change something: a namespace written out in full is its own require
  there - a Closure one always, a ClojureScript one that was compiled but not
  required too (clojure.cljs.analyzer's analyze-qualified-symbol) - and so is
  a dotted name whose head is one. What is left is every way a namespace
  writes a host name for itself: js/, an alias, a :refer, an :import, a bare
  name. Each of those reads what the ns form set up and adds nothing to it."
  [ns sym]
  (let [head (fn [^String s] (first (string/split s #"\.")))
        own? (fn [^String written]
               (let [named (symbol written)]
                 (or (= "js" written)
                     (some? (some-> (find-namespace ns) ns-aliases (get named)))
                     (contains? ((of :js-aliases) (:cenv (environment)) ns) named))))]
    (if-let [ns-part (namespace sym)]
      (own? ns-part)
      (or (not (string/includes? (name sym) "."))
          (own? (head (name sym)))))))

(defn host-ref
  "What the name WRITTEN in NS refers to of the host's, as the model names it,
  or nil where it is not the host's.

    {:kind :global    :name js/console.log}
    {:kind :goog-var  :name goog.string/trim}
    {:kind :goog-ns   :name goog.math.Long}
    {:kind :js-module :specifier \"react\" :export \"useState\"}

  THE ANALYZER'S READING wherever it can be had, and not one written again
  here, because the ways of writing one of these are many and every one of
  them is a rule of the compiler's: gstr/trim through an alias, a bare trim
  through a :refer, Long through an :import, useState through a :refer of a
  module, sub through the $ sugar. What the model filed a use under is what
  the analyzer made of it, so asking the analyzer is asking the model's own
  question. A symbol is analysed and nothing else: it compiles nothing,
  expands no macro and runs nothing - and a name the analyzer would answer by
  adding a require is read off the name instead, see `analysable?'.

  One the analyzer refuses - a var that does not exist, a Closure name the
  vendored subset does not have - is not the host's to answer about, and what
  it printed on the way is not the client's to read."
  [ns ^String written]
  (let [{:keys [cenv]} (environment)
        here (symbol ns)
        sym (try (edn/read-string written) (catch Exception _ nil))]
    (when (symbol? sym)
      (or (written-in-full sym)
          (when (analysable? here sym)
            (when-let [node (try
                              (binding [*err* (java.io.StringWriter.)]
                                (with-ns here
                                  ((of :analyze) cenv ((of :analysis-env) cenv) sym)))
                              (catch Throwable _ nil))]
              (when-let [kind (host-ops (:op node))]
                (cond-> {:kind kind}
                  (:name node) (assoc :name (:name node))
                  (:specifier node) (assoc :specifier (:specifier node))
                  (:export node) (assoc :export (:export node))))))))))
