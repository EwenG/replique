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
  (:require [clojure.test :refer [deftest is testing]]
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
