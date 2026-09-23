(ns replique.init-test
  "What a project and a user say about a process before anything connects to
  it.

  The scripts are code - `replique.core/load-init-scripts' says why - so what
  is tested here is not what they can do but WHEN they are read: before the
  process is findable, in the right order, and with a start that fails when one
  of them does."
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [replique.cljs :as cljs]
            [replique.core :as core]
            [replique.state :as state]
            [replique.test-client :as client])
  (:import [java.io File]))

(def ran
  "What the scripts of a test did, in the order they did it. A script is
  ordinary code loaded into this jvm, so the simplest thing it can leave
  behind is a conj onto a var it names in full."
  (atom []))

(defn- write-script!
  "Write TEXT as the init script of DIR, and answer the file."
  ^File [dir text]
  (let [f (File. (File. (str dir) ".replique") "init.clj")]
    (.mkdirs (.getParentFile f))
    (spit f text)
    f))

(defn- with-home*
  "Call F with DIR as the home directory this process reads its user script
  from.

  A property rather than an argument, because that is what `init-scripts'
  reads and what a real process has. Put back afterwards: the tests run in the
  process they are testing, and the home directory of whoever is running them
  is not something to leave changed."
  [dir f]
  (let [before (System/getProperty "user.home")]
    (System/setProperty "user.home" (str dir))
    (try (f) (finally (System/setProperty "user.home" before)))))

(defmacro ^:private with-home [dir & body]
  `(with-home* ~dir (fn [] ~@body)))

(defn- started!
  "Start a process in DIR reading its init scripts, and stop it."
  [dir f]
  (let [info (core/start! (merge {:directory (str dir) :init true} f))]
    (try info (finally (core/stop!)))))

(defn- thrown
  "The exception F threw, or nil."
  [f]
  (try (f) nil (catch Throwable t t)))

(defn- messages
  "Every message down T's chain of causes, which is where the one naming the
  init script is: `start!' says what it could not read and the script's own
  exception says why."
  [^Throwable t]
  (loop [t t acc []]
    (if t (recur (.getCause t) (conj acc (str (.getMessage t)))) acc)))

;;; When they are read

(deftest test-the-user-script-is-read-and-then-the-projects
  ;; The order is the whole of what having two of them means. A user's script
  ;; says how they like a repl and holds for every project they open; a
  ;; project's says what the project is, and goes last so that it has the last
  ;; word on anything both of them mention.
  (reset! ran [])
  (let [home (client/temp-dir)
        project (client/temp-dir)]
    (try
      (write-script! home "(swap! replique.init-test/ran conj :user)")
      (write-script! project "(swap! replique.init-test/ran conj :project)")
      (with-home home (started! project nil))
      (is (= [:user :project] @ran))
      (finally
        (client/delete-recursively home)
        (client/delete-recursively project)))))

(deftest test-a-script-is-read-before-the-process-can-be-found
  ;; What makes a script worth having at all: everything it sets is set before
  ;; there is anybody to see otherwise. A repl that connected while one was
  ;; still running would be standing in a process half configured, and a
  ;; compile environment made before the compiler options were read would hold
  ;; none of them.
  ;;
  ;; The witness is the process's own registration, which happens after the
  ;; server is bound and after the script: a script that can see a started
  ;; process is a script that ran too late.
  (reset! ran [])
  (let [project (client/temp-dir)
        home (client/temp-dir)]
    (try
      (write-script! project "(swap! replique.init-test/ran conj (replique.state/started?))")
      (with-home home
        (core/start! {:directory (str project) :init true})
        (try
          (is (= [false] @ran))
          (is (state/started?) "and the process really did start afterwards")
          (finally (core/stop!))))
      (finally
        (core/stop!)
        (client/delete-recursively home)
        (client/delete-recursively project)))))

(deftest test-one-file-is-read-once
  ;; A process started in the home directory names the same file twice. Reading
  ;; it twice would run whatever it does twice - the ones in the wild make
  ;; directories and install hooks - and the second time is never what the
  ;; script was written for.
  (reset! ran [])
  (let [dir (client/temp-dir)]
    (try
      (write-script! dir "(swap! replique.init-test/ran conj :once)")
      (with-home dir (started! dir nil))
      (is (= [:once] @ran))
      (finally (client/delete-recursively dir)))))

(deftest test-a-script-that-is-not-there-is-not-a-failure
  ;; The ordinary case, and the one a process must not have an opinion about:
  ;; most projects have no init script and are not asking for one.
  (reset! ran [])
  (let [home (client/temp-dir)
        project (client/temp-dir)]
    (try
      (let [info (with-home home (started! project nil))]
        (is (some? (:process-id info)))
        (is (= [] @ran)))
      (finally
        (client/delete-recursively home)
        (client/delete-recursively project)))))

;;; When one of them fails

(deftest test-a-script-that-throws-is-a-start-that-failed
  ;; Reported rather than survived. A process that came up having skipped the
  ;; script its own project wrote is configured differently from what the
  ;; project says, and nobody notices until something behaves oddly hours
  ;; later - so the start fails, and `replique.main' turns that into the
  ;; start-failed line every other start failure is reported as.
  (let [home (client/temp-dir)
        project (client/temp-dir)]
    (try
      (let [script (write-script! project "(throw (ex-info \"the script said no\" {}))")
            t (thrown #(with-home home (started! project nil)))
            said (messages t)]
        (is (some? t) "a script that throws must not be survived")
        ;; THE OUTERMOST MESSAGE, and not just somewhere down the chain. The
        ;; compiler's own exception names the file too - load-file says
        ;; "compiling at (<file>:<line>)" - so an assertion that read the whole
        ;; chain would pass whatever replique itself said, including nothing
        (is (string/includes? (str (.getMessage ^Throwable t)) (str script))
            (str "the failure must name the file that failed: " (pr-str said)))
        (is (some #(string/includes? % "the script said no") said)
            (str "and carry what the script itself said: " (pr-str said)))
        (testing "and nothing is left started"
          (is (not (state/started?)))
          (is (empty? (.list (File. (File. (str project) ".replique") "processes")))
              "no port file, so no client can find a process that is not there")))
      (finally
        (core/stop!)
        (client/delete-recursively home)
        (client/delete-recursively project)))))

;;; The option that turns them off

(deftest test-a-start-can-be-told-not-to-read-them
  ;; For a client starting this process to find out what it does WITHOUT them:
  ;; a test of replique itself, and the answer to an init script that broke a
  ;; start.
  (reset! ran [])
  (let [home (client/temp-dir)
        project (client/temp-dir)]
    (try
      (write-script! home "(swap! replique.init-test/ran conj :user)")
      (write-script! project "(swap! replique.init-test/ran conj :project)")
      (with-home home
        (let [info (core/start! {:directory (str project) :init false})]
          (try (is (some? (:process-id info)))
               (finally (core/stop!)))))
      (is (= [] @ran) "neither of them, not just the project's")
      (finally
        (client/delete-recursively home)
        (client/delete-recursively project)))))

(deftest test-init-is-true-or-false
  ;; The one option that turns something off, so the one where a value nobody
  ;; can read twice is worth refusing: :init "false" is a string, and a string
  ;; is truthy.
  (let [dir (client/temp-dir)]
    (try
      (let [t (thrown #(core/start! {:directory (str dir) :init "false"}))]
        (is (some? t))
        (is (string/includes? (str (.getMessage ^Throwable t)) ":init")))
      (is (not (state/started?)))
      (finally (client/delete-recursively dir)))))

;;; What they are mostly for

(deftest test-a-script-sets-a-compiler-option
  ;; The case this was built for, end to end: a project that needs the compiler
  ;; configured says so in a file it keeps under version control, and the
  ;; process comes up that way. Read before anything connects, which is before
  ;; a compile environment can have been made - so an option that cannot change
  ;; afterwards is still free to be set here.
  (let [before @cljs/options
        home (client/temp-dir)
        project (client/temp-dir)]
    (try
      (reset! cljs/options {})
      (write-script! project "(replique.cljs/set-option! :npm {:build false})")
      (with-home home (started! project nil))
      (is (= {:build false} (:npm @cljs/options)))
      (finally
        (reset! cljs/options before)
        (client/delete-recursively home)
        (client/delete-recursively project)))))
