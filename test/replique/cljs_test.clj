(ns replique.cljs-test
  "Whether this process compiles ClojureScript, and what it reads out of the
  compiler when it does.

  Written for both processes, as `replique.analysis-test' is: the compiler is
  not a dependency of replique, and a process is running with it or without
  it. So every test here asks the process which it is rather than assuming,
  and the ones that need a compiler say what a process without one must
  answer instead.

  With one:

    clojure -M:test:cljs ...

  The first question asked of the compiler compiles cljs.core, which is
  seconds. That is why these tests share one process and one environment
  rather than making one each."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [replique.cljs :as cljs]
            [replique.ops]
            [replique.test-client :as client
             :refer [control-client disconnect request! with-process]]))

(defn- compiling?
  "Whether the process running this test has a ClojureScript compiler."
  []
  (cljs/available?))

(defn- refused
  "What was refused, or nil where it was not."
  [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e
         (when (= :no-cljs (:replique/error (ex-data e))) (.getMessage e)))))

;;; What the process says about itself

(deftest test-a-process-says-whether-it-compiles-clojurescript
  ;; Beside :analysis and for the same reason: it is a fact about what the
  ;; process can be asked, and somebody whose .cljs buffer gets no completions
  ;; needs somewhere to find out why. Nothing has to read it to ask - an op
  ;; that cannot be answered says so.
  (with-process [info nil]
    (let [c (control-client info)]
      (try
        (let [answered (request! c {:op :process-info :id 1})]
          (is (contains? answered :cljs))
          (is (= (compiling?) (:cljs answered))))
        (finally (disconnect c))))))

;;; Without a compiler

(deftest test-a-process-without-the-compiler-says-so-rather-than-nothing
  ;; The distinction this exists to keep: a process with no compiler has no
  ;; ClojureScript namespaces AT ALL, so answering an empty list would read as
  ;; "the namespace is empty" rather than as "this process cannot say". It is
  ;; refused once, at the top, and the refusal names what to change.
  (when-not (compiling?)
    (let [message (refused #(cljs/namespaces))]
      (is (some? message) "a process with no compiler must refuse rather than answer")
      (is (re-find #"ClojureScript compiler" message))
      (is (re-find #"classpath" message))
      (is (some? (refused #(cljs/find-namespace 'cljs.core))))
      (is (some? (refused #(cljs/resolve-var 'cljs.user 'map))))
      ;; including the cursor, which is the one primitive here that would
      ;; otherwise fail on its own terms: the var it binds is the compiler's, so
      ;; without one this is a push-thread-bindings of nil and the caller gets a
      ;; NullPointerException about a field rather than a sentence about a
      ;; classpath
      (is (some? (refused #(cljs/with-ns* 'cljs.user (fn [] :never))))))))

;;; With one

(deftest test-a-namespace-is-read-with-the-functions-a-clojure-one-is
  ;; The whole reason there is so little in replique.cljs. A ClojureScript
  ;; namespace is a clojure.lang.Namespace holding real Vars, so what reads one
  ;; is ns-name, ns-publics and meta - the same functions, not translations of
  ;; them. Only finding it BY NAME is ours, because it lives in a world find-ns
  ;; does not read.
  (when (compiling?)
    (let [found (cljs/find-namespace 'cljs.core)]
      (is (instance? clojure.lang.Namespace found))
      (is (= 'cljs.core (ns-name found)))
      (is (< 500 (count (ns-publics found))) "cljs.core is compiled into a new environment")
      ;; and the JVM's own registry is untouched by any of it
      (is (nil? (find-ns 'cljs.user))))
    (testing "and the list of them is that world's and not this jvm's"
      (let [named (set (map ns-name (cljs/namespaces)))]
        (is (contains? named 'cljs.core))
        ;; the one name that says which world answered: every process running
        ;; this test has clojure.core loaded, and no ClojureScript world has it
        (is (not (contains? named 'clojure.core)))))))

(deftest test-a-var-says-where-it-was-written
  ;; What `:symbol' will answer with, and the reason the compiler side of R3
  ;; was one change: a var carries its file, its line and its arglists, and an
  ;; editor opens a definition out of them.
  (when (compiling?)
    (let [v (cljs/resolve-var 'cljs.user 'map)]
      (is (var? v))
      (is (= 'cljs.core/map (symbol v)))
      (let [{:keys [file line arglists]} (meta v)]
        (is (= "cljs/core.cljs" file) "the path under a source directory, not this machine's")
        (is (pos? line))
        (is (seq arglists))))))

(deftest test-a-name-is-resolved-the-way-the-compiler-resolved-it
  ;; ns-resolve would answer this one WRONGLY rather than not at all, which is
  ;; why it is the second thing here. cljs.core is referred into every
  ;; namespace whether it says so or not, and what `map' names is the
  ;; ClojureScript var and not the one this jvm is running on.
  (when (compiling?)
    (let [v (cljs/resolve-var 'cljs.user 'map)]
      (is (not (identical? v #'clojure.core/map)))
      (is (nil? (cljs/resolve-var 'cljs.user 'no-such-name-anywhere)))
      ;; a name qualified with a namespace the asking one does not require does
      ;; not resolve, which is ClojureScript's rule and not an oversight
      (is (nil? (cljs/resolve-var 'cljs.user 'no.such.ns/thing))))))

(deftest test-the-environment-is-one-and-is-made-once
  ;; One symbol table per process: two editors looking at one project are
  ;; looking at one program, and a var defined through either is a var the
  ;; other completes. What they must not share is where each is standing, and
  ;; that is a thread binding rather than a field of this.
  (when (compiling?)
    (is (identical? (:cenv (cljs/environment)) (:cenv (cljs/environment))))
    (is (.isDirectory (java.io.File. (str (:out-dir (cljs/environment))))))))

(deftest test-two-questions-about-two-namespaces-do-not-move-each-other
  ;; The cursor is per asking and not per process. Resolving from inside
  ;; cljs.core and from inside cljs.user are two different questions, and
  ;; neither leaves the other standing somewhere it did not ask to be.
  (when (compiling?)
    ;; `map' resolves from both, but only cljs.core INTERNS it: resolving from
    ;; cljs.user reaches it through the implicit refer, so the two answers are
    ;; the same var asked two different ways
    (is (= (cljs/resolve-var 'cljs.core 'map) (cljs/resolve-var 'cljs.user 'map)))
    (is (= 'cljs.core (ns-name (:ns (meta (cljs/resolve-var 'cljs.user 'map))))))))


;;; Raw JavaScript, for what is not a repl

(def ^:private js-ms
  "The bound every `eval-js' here is asked within.

  Generous, because none of these is testing the deadline - what is bounded is
  a browser that may be a sleeping tab, and the runtime under these is node,
  which is a process that answers or has died. `clojure.cljs.repl's busy and
  timed-out are tested where they are decided, in the compiler's repl-test."
  10000)

(deftest test-raw-javascript-is-evaluated-without-being-compiled
  ;; The seam M1 exists to be. What asks is tooling - a stylesheet reload -
  ;; holding a string of JavaScript with no ClojureScript anywhere in it, and
  ;; the only road to the runtime before this was `eval-form', which would have
  ;; meant compiling a (js* "...") wrapper around the answer to produce the
  ;; answer.
  (when (compiling?)
    (binding [cljs/*target* :node]
      (is (= {:status :success :value "2"} (cljs/eval-js "1 + 1" js-ms)))
      ;; what came back is what the RUNTIME printed, so a JavaScript string
      ;; arrives with its quotes in it - see `eval-form'
      (is (= {:status :success :value "\"ab\""} (cljs/eval-js "'a' + 'b'" js-ms)))
      (testing "and what it threw comes back as what it threw"
        (let [r (cljs/eval-js "(function () { throw new Error('boom'); })()" js-ms)]
          (is (= :error (:status r)))
          (is (re-find #"boom" (str (:value r))))))
      (testing "and the bound is the caller's"
        ;; 0ms is past before any runtime can answer, which is the one way to
        ;; witness the bound on node without wedging it: node answers by
        ;; position, so a script that never returns would take every evaluation
        ;; after it with it.
        (let [r (cljs/eval-js "1 + 1" 0)]
          (is (= :error (:status r)))
          (is (re-find #"0ms" (str (:value r)))))
        (is (= {:status :success :value "2"} (cljs/eval-js "1 + 1" js-ms))
            "and giving up leaves the runtime there for the next question")))))

(deftest test-it-is-the-broadcasting-one-that-was-wired-up
  ;; Which cannot be witnessed here, and so is asserted as the wiring it is.
  ;; Node is ONE runtime: `evaluate-all-within' and `evaluate-within' answer
  ;; identically on it, so no behaviour in this process can tell them apart.
  ;; The fan-out itself is the transport's and is tested against a real page in
  ;; the compiler's browser-test. What this catches is the wire coming off the
  ;; wrong terminal - and it matters here because a stylesheet that reloaded in
  ;; the tab you happen to be evaluating in, and in none of the others, is a
  ;; reload you would report as working.
  (when (compiling?)
    ;; the var and not its value, which is what `subsystem' holds throughout
    (is (identical? (requiring-resolve 'clojure.cljs.repl/evaluate-all-within)
                    (#'cljs/of :evaluate-all-within)))))

(deftest test-it-does-not-wait-behind-the-repl
  ;; THE POINT OF THE SEAM, and the thing a later edit would quietly undo by
  ;; reaching for `with-evaluation' because everything else that evaluates uses
  ;; it. A repl's turn is as long as what it is evaluating - a `require' of a
  ;; hundred namespaces is a minute - and a stylesheet reload queued behind one
  ;; arrives after exactly the wait it exists to avoid.
  ;;
  ;; Witnessed rather than timed: the lock is held until this says so, so an
  ;; answer that arrives while it is still held is an answer that did not wait
  ;; for it. The deref bound is there so that a change which DOES take the lock
  ;; fails this test instead of hanging the suite on a deadlock.
  (when (compiling?)
    (binding [cljs/*target* :node]
      ;; the runtime before the lock: starting node is the environment's, takes
      ;; seconds, and is not what is being timed
      (cljs/eval-js "1" js-ms)
      (let [held     (promise)
            release  (promise)
            released (atom false)
            holder   (future (cljs/with-target-lock*
                              (fn []
                                (deliver held true)
                                (deref release js-ms nil)
                                (reset! released true))))]
        (try
          (is (deref held js-ms nil) "the lock was taken")
          (let [answer (future (cljs/eval-js "1 + 1" js-ms))
                r      (deref answer js-ms ::waited)]
            (is (= {:status :success :value "2"} r)
                "answered while another thread held this target's lock")
            (is (false? @released)
                "and answered before that thread let it go"))
          (finally
            (deliver release true)
            (deref holder js-ms nil)))))))

(deftest test-a-process-without-the-compiler-refuses-raw-javascript-too
  ;; And refuses rather than failing on a nil: `of' answers nil where there is
  ;; no compiler, and nil is not a thing to call. The refusal comes from
  ;; `environment', which is why the runtime is asked for first.
  (when-not (compiling?)
    (is (some? (refused #(cljs/eval-js "1 + 1" js-ms))))))

;;; What it is compiled with

(defn- with-options*
  "Call F with this process's compiler options put back afterwards.

  They are a process-wide atom rather than a thread binding, because that is
  what they are: an init script sets them once, before anything can ask a
  question. So a test that set one and walked away would be setting it for
  every test after it."
  [f]
  (let [before @cljs/options]
    (try (f) (finally (reset! cljs/options before)))))

(defmacro ^:private with-options [& body]
  `(with-options* (fn [] ~@body)))

(deftest test-an-option-that-is-not-one-is-refused
  ;; Rather than remembered. An option nobody reads looks exactly like one that
  ;; worked, and what would have shown otherwise is a compilation minutes
  ;; later, on another thread, with the init script long out of sight. So the
  ;; refusal happens where the mistake is, and names what the options are.
  ;;
  ;; No compiler needed: this is about the name of the option and not about
  ;; anything the option does. A process with no compiler still reads the init
  ;; script that sets one.
  (with-options
    (let [t (try (cljs/set-option! :optimizations :advanced) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (some? t) "a key that is not an option must be refused")
      (let [message (.getMessage t)]
        (is (re-find #":optimizations" message) "and name the key that was wrong")
        (is (re-find #":closure-library" message) "and say what the options are"))
      (is (not (contains? @cljs/options :optimizations))
          "and leave nothing behind, which is the point of refusing"))
    (testing "and one that is one is kept, and answered with"
      (let [answered (cljs/set-option! :static-dispatch true)]
        (is (= true (:static-dispatch @cljs/options)))
        (is (= @cljs/options answered) "set-option! answers with all of them")))))

(deftest test-an-option-reaches-the-compilation
  ;; The whole point of the atom, and the thing that would break silently: an
  ;; option this process was configured with has to be in the map every
  ;; compilation runs under, or it is a setting that reads back correctly and
  ;; does nothing.
  ;;
  ;; :npm is the one that can be witnessed without another dependency. A string
  ;; require is what makes clojure.cljs.npm run at all, and what it decides is
  ;; reported: with nothing turned off it looks for a node_modules and says it
  ;; found none, and with :build false it does not look, and says that instead.
  ;; Two different answers about one fixture, which no default could produce.
  (when (compiling?)
    (with-options
      (reset! cljs/options {})
      (is (= :no-root (:why (:js-build (cljs/compile-namespace! 'rt.npm-program))))
          "without the option, the bundler is looked for")
      (cljs/set-option! :npm {:build false})
      (is (= :disabled (:why (:js-build (cljs/compile-namespace! 'rt.npm-program))))
          "with it, it is not - which only the option map can have said"))))

(deftest test-the-closure-tree-cannot-change-once-something-is-compiled
  ;; The one option with a lifetime. Every other one is read afresh at each
  ;; compilation and can be changed whenever; this one cannot, because the
  ;; output directory already holds files compiled against one tree, and a
  ;; program holding goog.string from the subset and goog.style from the
  ;; library is one nobody can reason about.
  ;;
  ;; Which is also why it is a choice and not a detection: anything depending
  ;; on ClojureScript brings google-closure-library along, so having it on the
  ;; classpath says nothing about wanting to compile against it.
  (when (compiling?)
    (with-options
      ;; the environment first, so that this does not depend on what any other
      ;; test in this namespace has already asked for
      (cljs/environment)
      (let [t (try (cljs/set-option! :closure-library (not (boolean (:closure-library @cljs/options))))
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (some? t) "changing it after a compilation must be refused")
        (is (re-find #"Closure Library" (.getMessage t)))
        (is (re-find #"init.clj" (.getMessage t)) "and say where it is set instead"))
      (testing "and saying again what it already says is not a change"
        (is (map? (cljs/set-option! :closure-library (boolean (:closure-library @cljs/options))))
            "an init script must be free to write down the default")))))

;;; The directory it compiles into

(defn- a-directory-with-something-in-it
  "A directory holding a file in a subdirectory of its own, which is the shape
  an output directory has and the shape `File.deleteOnExit' will not delete."
  ^java.io.File []
  (let [dir (io/file (System/getProperty "java.io.tmpdir")
                     (str "replique-cljs-hook-test-" (System/nanoTime)))]
    (.mkdirs (io/file dir "ns" "app"))
    (spit (io/file dir "ns" "app" "core.js") "// a module\n")
    dir))

(deftest test-an-output-directory-goes-when-the-process-is-killed-rather-than-stopped
  ;; The ending `release!' does not cover. A process stopped through
  ;; `replique.core/stop!' closes its runtimes and deletes what it compiled; a
  ;; process that is merely SIGTERMed or ^C-ed never runs a line of that, and
  ;; what it leaves behind is a temporary directory of a few hundred megabytes
  ;; that nothing will ever look at again. This is the hook that covers it.
  (let [dir  (a-directory-with-something-in-it)
        hook (#'cljs/delete-at-exit! dir)
        rt   (Runtime/getRuntime)]
    (try
      (is (.isDirectory dir))
      ;; REGISTERED WITH THE JVM, which is the half a hook that is merely built
      ;; would not have: removing it answers true only for one that is on the
      ;; list. Put straight back, so this test leaves the process as it was.
      (is (true? (.removeShutdownHook rt hook)) "the hook is on the jvm's list")
      (.addShutdownHook rt hook)
      ;; and what the jvm will run when it goes
      (.run hook)
      (is (not (.exists dir))
          "the tree goes, which is what File.deleteOnExit could not have done")
      ;; TWICE IS NOT AN ERROR, because `release!' may have got there first and
      ;; a hook that threw would print a stack trace over whatever the user was
      ;; reading on the way out
      (.run hook)
      (finally
        (.removeShutdownHook rt hook)
        (when (.exists dir) (#'cljs/delete-tree! dir))))))

(deftest test-the-environment-keeps-the-hook-that-will-delete-its-directory
  ;; Kept rather than registered and forgotten, and that is what makes the
  ;; orderly ending able to undo the other one: a process stopped inside a jvm
  ;; that goes on running - which is every test in this suite - would otherwise
  ;; leave a hook per environment behind, each one a promise about a directory
  ;; that is already gone.
  (when (compiling?)
    (let [{:keys [out-dir out-dir-hook]} (cljs/environment)
          rt (Runtime/getRuntime)]
      (is (instance? Thread out-dir-hook) "the environment carries its hook")
      (is (.isDirectory ^java.io.File out-dir))
      (is (true? (.removeShutdownHook rt out-dir-hook)) "and it is registered")
      (.addShutdownHook rt out-dir-hook))))

(deftest test-stopping-takes-the-hook-off-with-the-directory
  ;; `release!' empties the environments atom, so this runs in the process that
  ;; has none to empty - the one without a compiler. What it tests is not about
  ;; the compiler anyway: it is what release! does with the two things an
  ;; environment holds about its directory.
  (when-not (compiling?)
    (let [dir  (a-directory-with-something-in-it)
          hook (#'cljs/delete-at-exit! dir)
          rt   (Runtime/getRuntime)]
      (try
        (swap! @#'cljs/environments assoc ::fake
               {:out-dir dir :out-dir-hook hook :runtime (atom nil)})
        (cljs/release!)
        (is (not (.exists dir)) "the directory goes")
        (is (false? (.removeShutdownHook rt hook))
            "and the hook goes with it, rather than outliving what it was about")
        (finally
          (swap! @#'cljs/environments dissoc ::fake)
          (.removeShutdownHook rt hook)
          (when (.exists dir) (#'cljs/delete-tree! dir)))))))

