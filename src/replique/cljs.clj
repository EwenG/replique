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
  (:require [replique.state :as state])
  (:import [java.io File]
           [java.nio.file Files Path]
           [java.nio.file.attribute FileAttribute]))

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
            drv   (partial named 'clojure.cljs.driver)]
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
         :find-source   (drv 'find-source)})
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

;;; The environment

(defonce ^{:private true
           :doc
           "The one compile environment of this process, or nil before anything asked.

  ONE, and not one per connection. A compile environment is a symbol table,
  and two editors looking at one project are looking at one program: a var
  defined through one of them is a var the other completes. What they must not
  share is where each is standing, and they do not - that is the cursor above,
  which belongs to whichever thread is asking.

  Made on the first ask rather than at startup. Making one loads and compiles
  cljs.core's macros, which is seconds, and a process that is never asked a
  ClojureScript question must not pay them."}
  the-environment
  (atom nil))

(defn- make-output-dir!
  "A directory to compile into, deleted when the process stops.

  Temporary, which is right for what Stage A uses it for and is not the last
  word: what serves this directory is a runtime, and a runtime that a browser
  fetches from wants a directory that outlives a restart, so that a source map
  already open in devtools still names something. That choice belongs with the
  runtime rather than here."
  ^File []
  (.toFile (Files/createTempDirectory "replique-cljs" (make-array FileAttribute 0))))

(defn environment
  "The ClojureScript compile environment of this process, made on the first ask.

    {:cenv     the compile environment - clojure.cljs.env/CompileEnv
     :out-dir  where modules are compiled to}

  Refuses where there is no compiler, rather than answering nil: everything
  that asks for this is about to use it, and a nil here would fail one call
  later with an exception about a record instead of a sentence about a
  classpath."
  []
  (refuse-unless-available! "compile ClojureScript")
  (or @the-environment
      ;; The cursor has to exist before compile-env positions itself: it does
      ;; that with a set!, which needs a binding to move. swap! would be wrong -
      ;; making one twice would compile cljs.core's macros twice - so the lock
      ;; is on this var and the second caller through it finds the first one's.
      (locking the-environment
        (or @the-environment
            (reset! the-environment
                    (with-ns 'cljs.user
                      (let [made {:cenv ((of :compile-env) {:ns 'cljs.user
                                                            :core-macros 'cljs.core})
                                  :out-dir (make-output-dir!)}]
                        ;; CLJS.CORE BEFORE ANYBODY LOOKS. A fresh environment
                        ;; is an EMPTY symbol table - the two worlds are made
                        ;; here and nothing has been compiled into them - so a
                        ;; completion asked of it would answer that the language
                        ;; has no names in it, which is a worse answer than a
                        ;; refusal. Every namespace requires cljs.core whether it
                        ;; says so or not, so this is what would have been
                        ;; compiled by the first question anyway; the REPL loop
                        ;; does the same thing before its first prompt, and for
                        ;; the same reason.
                        ;;
                        ;; The driver directly rather than `compile-namespace!'
                        ;; below, which asks for the environment this is still
                        ;; making.
                        ((of :compile-namespace!) (:cenv made) 'cljs.core
                         {:out-dir (:out-dir made)})
                        made)))))))

(defn- delete-tree!
  [^File f]
  (when (.isDirectory f) (run! delete-tree! (.listFiles f)))
  (.delete f))

(defn release!
  "Forget the environment and delete what it compiled into.

  What stopping the process does. Not what a client asks for: a compile
  environment holds every var of every namespace anybody has loaded, and
  throwing it away is throwing away a session rather than clearing a cache."
  []
  (when-let [{:keys [^File out-dir]} @the-environment]
    (reset! the-environment nil)
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
