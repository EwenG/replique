(ns replique.cljs-repl-test
  "The :repl role, in ClojureScript.

  Written for both processes, as `replique.cljs-test' is: the compiler is not a
  dependency of replique, and a process is running with it or without it. With
  one:

    clojure -M:test:cljs ...

  ONE PROCESS FOR THE WHOLE FILE, which is not how the other repl tests are
  written and has to be here: the first ClojureScript question a process is
  asked compiles cljs.core, and the first repl connection starts a runtime on
  top of that - seconds each, and both of them once per process rather than
  once per test. What the tests give up for it is isolation between
  namespaces, which they buy back by each using a namespace of its own.

  The target is node throughout. The browser one cannot be driven without a
  page, and what a page would exercise is the transport rather than this role -
  the compiler's own `browser-test' is where that is tested, against a node
  process running the browser client."
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [replique.cljs :as cljs]
            [replique.core :as core]
            [replique.test-client :as client
             :refer [disconnect eval! frame-tagged frames-tagged printed recv
                     repl-client request! send!]]))

(defn- compiling?
  "Whether the process running this test has a ClojureScript compiler."
  []
  (cljs/available?))

;;; One process, and the handshake that is slow

(def ^:private the-process (atom nil))

(def ^:private slow
  "How long a client waits for a handshake here.

  A ClojureScript handshake compiles cljs.core and starts node before it
  answers - see this file's docstring - and the default ten seconds is a
  timeout written for a process that only has to reply."
  180000)

(defn- with-one-process [f]
  (let [dir (client/temp-dir)
        out *out*
        err *err*]
    (reset! the-process (core/start! {:directory dir}))
    (try
      ;; the streams the process replaced, for the reason `with-process' gives:
      ;; what clojure.test prints would otherwise be broadcast as output events
      (binding [*out* out *err* err] (f))
      (finally (core/stop!) (reset! the-process nil)
               (client/delete-recursively dir)))))

(use-fixtures :once with-one-process)

(defn- cljs-repl!
  "A ClojureScript repl connection on node."
  ([] (cljs-repl! {:dialect :cljs :target :node}))
  ([hello] (repl-client @the-process hello slow)))

(defmacro ^:private with-repl [[sym hello] & body]
  `(let [~sym (cljs-repl! ~@(when hello [hello]))]
     (try ~@body (finally (disconnect ~sym)))))

(defn- node-env
  "What the repls in this file are asking of, from inside the process."
  []
  (binding [cljs/*target* :node] (cljs/environment)))

;;; What a handshake says

(deftest test-a-process-without-the-compiler-refuses-a-clojurescript-repl
  (when-not (compiling?)
    ;; Refused at the handshake and not at the first form: nothing about this
    ;; process will make the next form work, and a client told once can say so
    ;; rather than show a repl that answers every input with the same sentence.
    (let [r (cljs-repl!)]
      (try
        (is (= "error" (:tag (:hello r))))
        (is (= "no-cljs" (:error (:hello r))))
        (is (string/includes? (:message (:hello r)) "ClojureScript compiler"))
        (is (string/includes? (:message (:hello r)) "classpath"))
        (testing "and the connection is closed, as after any failed handshake"
          (is (= :eof (recv r))))
        (finally (disconnect r))))))

(deftest test-a-repl-says-which-dialect-and-target-it-is
  (when (compiling?)
    (with-repl [r]
      (testing "the handshake is any other handshake, plus what it is a repl of"
        (is (= "reply" (:tag (:hello r))))
        (is (= "repl" (:role (:hello r))))
        (is (= "cljs" (:dialect (:hello r))))
        (is (= "node" (:target (:hello r))))
        (is (string? (:connection (:hello r)))))
      (testing "and node has no url to open, unlike the browser"
        (is (nil? (:url (:hello r)))))
      (testing "the first prompt stands in cljs.user and says so the same way"
        (is (= "prompt" (:tag (:prompt r))))
        (is (= "cljs.user" (:ns (:prompt r))))
        (is (= "cljs" (:dialect (:prompt r))))
        (is (= "node" (:target (:prompt r))))
        (is (= (:connection (:hello r)) (:connection (:prompt r)))))
      (testing "and no params, which a Clojure prompt has and this cannot"
        ;; the *print-* the last value went through, which for ClojureScript
        ;; happened in another process
        (is (nil? (:params (:prompt r))))))))

(deftest test-a-dialect-this-process-does-not-speak-is-refused-by-name
  (let [r (repl-client @the-process {:dialect :fortran} slow)]
    (try
      (is (= "error" (:tag (:hello r))))
      (is (= "invalid-dialect" (:error (:hello r))))
      (is (= ["clj" "cljs"] (:dialects (:hello r))))
      (finally (disconnect r)))))

(deftest test-a-target-there-is-no-such-thing-as-is-refused-by-name
  (when (compiling?)
    (let [r (repl-client @the-process {:dialect :cljs :target :toaster} slow)]
      (try
        (is (= "error" (:tag (:hello r))))
        (is (= "invalid-target" (:error (:hello r))))
        (is (= ["browser" "node"] (:targets (:hello r))))
        (finally (disconnect r))))))

(deftest test-a-handshake-with-no-dialect-is-still-a-clojure-repl
  ;; Absent means Clojure, which is how every message of this protocol says
  ;; which dialect it is about - and is what keeps a client that predates
  ;; ClojureScript working unchanged.
  (let [r (repl-client @the-process)]
    (try
      (is (= "reply" (:tag (:hello r))))
      (is (nil? (:dialect (:hello r))))
      (is (= "user" (:ns (:prompt r))))
      (is (= ["ret" "prompt"] (mapv :tag (eval! r "(+ 1 2)"))))
      (finally (disconnect r)))))

;;; Evaluating

(deftest test-a-form-is-evaluated-in-the-runtime-and-its-value-comes-back
  (when (compiling?)
    (with-repl [r]
      (let [frames (eval! r "(+ 1 2)")]
        (is (= ["ret" "prompt"] (mapv :tag frames)))
        (is (= "3" (:value (frame-tagged frames "ret"))))
        (is (= "cljs.user" (:ns (frame-tagged frames "ret")))))
      (testing "printed where the value is, by the ClojureScript printer"
        ;; not transcoded here: a keyword is a keyword and a set is a set, and
        ;; neither of them is what the JVM would have printed for the same text
        (is (= "{:a [1 2], :b :c}"
               (:value (frame-tagged (eval! r "{:a [1 2] :b :c}") "ret"))))))))

(deftest test-what-a-form-prints-arrives-before-its-value
  (when (compiling?)
    (with-repl [r]
      (testing "one frame per line printed"
        (let [frames (eval! r "(dotimes [i 3] (println i))")]
          (is (= ["out" "out" "out" "ret" "prompt"] (mapv :tag frames)))
          (is (= "0\n1\n2\n" (printed frames "out")))))
      (testing "and the value of the form that printed it comes after"
        ;; The thing two channels could not promise: the print travels on the
        ;; socket the result travels on, so their order is the order they
        ;; happened in rather than the order two threads were scheduled in.
        (let [frames (eval! r "(do (println \"before\") 42)")]
          (is (= ["out" "ret" "prompt"] (mapv :tag frames)))
          (is (= "before\n" (printed frames "out")))
          (is (= "42" (:value (frame-tagged frames "ret")))))))))

(deftest test-a-definition-is-there-for-the-next-form
  (when (compiling?)
    (with-repl [r]
      (eval! r "#replique/ns rt.one\n(def f (fn [x] (* x 2)))")
      (is (= "42" (:value (frame-tagged (eval! r "(f 21)") "ret"))))
      (testing "and redefining it is seen by a caller compiled before it"
        (eval! r "(def g (fn [] (f 10)))")
        (is (= "20" (:value (frame-tagged (eval! r "(g)") "ret"))))
        (eval! r "(def f (fn [x] (* x 100)))")
        (is (= "1000" (:value (frame-tagged (eval! r "(g)") "ret"))))))))

(deftest test-a-form-that-throws-comes-back-as-an-exception
  (when (compiling?)
    (with-repl [r]
      (let [frames (eval! r "(throw (js/Error. \"boom\"))")
            f (frame-tagged frames "exception")]
        (is (= ["exception" "prompt"] (mapv :tag frames)))
        (is (string/includes? (:message f) "boom"))
        (testing "with a stack read back as ClojureScript"
          (is (string/includes? (:stacktrace f) ".cljs")))
        (testing "and the JavaScript one beside it, for when the mapping is what you doubt"
          (is (string/includes? (:js-stacktrace f) ".js")))))))

(deftest test-a-form-that-will-not-compile-says-which-side-noticed
  (when (compiling?)
    (with-repl [r]
      (let [f (frame-tagged (eval! r "(this-name-is-not-defined-anywhere)") "exception")]
        (is (some? f))
        ;; :phase is what tells the code that could not be turned into
        ;; JavaScript from the code that ran and threw
        (is (= "compile" (:phase f)))))))

(deftest test-a-form-that-cannot-be-read-is-a-read-failure-and-the-repl-goes-on
  (when (compiling?)
    (with-repl [r]
      ;; The rest of the line goes with it, which is the policy the compiler's
      ;; reader declines to choose and the one clojure.main takes: a repl user
      ;; types one form per line and expects the bad one to be gone. What is
      ;; written after the failure on the SAME line is part of the thing that
      ;; failed - evaluating it would answer a question nobody asked, and the
      ;; answer would arrive as the result of whatever they typed next.
      (let [frames (eval! r "(+ 1 ] (+ 100 100)\n")
            f (frame-tagged frames "exception")]
        (is (= ["exception" "prompt"] (mapv :tag frames)))
        (is (= "read" (:phase f))))
      (testing "so the next form is the next form the client sent"
        (is (= "7" (:value (frame-tagged (eval! r "(+ 3 4)") "ret"))))))))

;;; The directives

(deftest test-a-bare-namespace-directive-moves-the-repl-and-is-answered-with-a-prompt
  (when (compiling?)
    (with-repl [r]
      ;; nothing else would answer it: the prompt of a form is what says where
      ;; the repl is, and there is no form
      (let [frames (eval! r "#replique/ns rt.moved\n\n")]
        (is (= ["prompt"] (mapv :tag frames)))
        (is (= "rt.moved" (:ns (first frames)))))
      (testing "and the repl stays there"
        (is (= "rt.moved" (:ns (frame-tagged (eval! r "1") "ret"))))))))

(deftest test-a-form-under-a-namespace-directive-gets-exactly-one-prompt
  (when (compiling?)
    (with-repl [r]
      ;; the form's own prompt is the one that follows, and two prompts for one
      ;; evaluation is what a client cannot read
      (let [frames (eval! r "#replique/ns rt.two\n(def a 1)")]
        (is (= ["ret" "prompt"] (mapv :tag frames)))
        (is (= "rt.two" (:ns (frame-tagged frames "ret"))))))))

(deftest test-where-a-definition-came-from-is-what-the-client-said
  (when (compiling?)
    (with-repl [r]
      ;; A repl reads from a socket and so knows no file and no line at all.
      ;; #replique/src is the client saying, and both halves of it are honoured:
      ;; the line on the reader before the form is read, the file around the
      ;; evaluation, because that is where a def reads it.
      (eval! r "#replique/ns rt.placed\n")
      (eval! r "#replique/src {:file \"rt/placed.cljs\" :line 42}\n(def q 1)")
      (let [v (binding [cljs/*target* :node] (cljs/resolve-var 'rt.placed 'q))]
        (is (some? v))
        (is (= "rt/placed.cljs" (:file (meta v))))
        (is (= 42 (:line (meta v)))))
      (testing "and it applies to the next form only"
        (eval! r "(def unplaced 1)")
        (let [v (binding [cljs/*target* :node] (cljs/resolve-var 'rt.placed 'unplaced))]
          (is (nil? (:file (meta v)))))))))

(deftest test-a-reload-directive-is-refused-and-says-what-to-do-instead
  (when (compiling?)
    (with-repl [r]
      (let [f (frame-tagged (eval! r "#replique/reload {}\n") "exception")]
        (is (some? f))
        (is (string/includes? (:message f) "reload"))
        ;; named as what to do rather than as what is missing
        (is (string/includes? (:message f) ":reload-all"))))))

(deftest test-a-jar-entry-cannot-be-loaded-and-says-why
  (when (compiling?)
    (with-repl [r]
      (let [f (frame-tagged
               (eval! r "#replique/load {:file \"/tmp/some.jar\" :entry \"a/b.cljs\"}\n")
               "exception")]
        (is (some? f))
        (is (string/includes? (:message f) "source path"))))))

(deftest test-a-file-is-loaded-as-one-unit
  (when (compiling?)
    (let [dir (client/temp-dir)
          f (java.io.File. (str dir) "rt/loaded.cljs")]
      (try
        (.mkdirs (.getParentFile f))
        (spit f "(ns rt.loaded)\n(def from-a-file :yes)\n")
        (with-repl [r]
          (let [frames (eval! r (str "#replique/load {:file \"" (.getPath f) "\"}\n"))]
            (is (= ["ret" "prompt"] (mapv :tag frames))))
          (testing "and what it defined is there"
            (is (= ":yes" (:value (frame-tagged
                                   (eval! r "#replique/ns rt.loaded\nfrom-a-file")
                                   "ret"))))))
        (finally (client/delete-recursively dir))))))

;;; One program, several repls

(deftest test-two-repls-on-one-target-are-two-views-of-one-program
  (when (compiling?)
    (with-repl [a]
      (with-repl [b]
        ;; A compile environment is a symbol table, and two editors looking at
        ;; one project are looking at one program
        (eval! a "#replique/ns rt.shared\n(def shared-by-both 7)")
        (is (= "7" (:value (frame-tagged
                            (eval! b "#replique/ns rt.shared\nshared-by-both")
                            "ret"))))
        (testing "and each keeps its own idea of where it is standing"
          (eval! a "#replique/ns rt.a\n\n")
          (is (= "rt.shared" (:ns (frame-tagged (eval! b "1") "ret"))))
          (is (= "rt.a" (:ns (frame-tagged (eval! a "1") "ret")))))
        (testing "and they share one runtime, which is one program running"
          (is (= 1 (count (frames-tagged (eval! a "1") "ret"))))
          (is (some? (:runtime (node-env)))))))))

;;; The browser, and what makes it a second program

(defn- browser-env []
  (binding [cljs/*target* :browser] (cljs/environment)))

(defn- start-page!
  "A node process running the browser client against the repl's url.

  What the compiler's own browser-test uses, and for the reason it gives:
  runtime_browser.js needs fetch, eval and a websocket and nothing else a
  browser has that node lacks, so the wire under test is the real one. It is
  written into the output directory rather than run from a string so that it
  imports the client by the same relative specifier a served page uses."
  [url]
  (let [^java.io.File dir (:out-dir (browser-env))]
    (spit (java.io.File. dir "page.js")
          "import { connect } from \"./runtime_browser.js\";\nconnect(process.argv[2]);\n")
    (-> (ProcessBuilder. ["node" "page.js" url])
        (.directory dir)
        (.redirectErrorStream true)
        (.start))))

(deftest test-a-browser-repl-says-what-to-open-and-evaluates-in-the-page
  (when (compiling?)
    (with-repl [r {:dialect :cljs :target :browser}]
      (let [url (:url (:hello r))]
        (testing "the url is the whole of what a client has to do something with"
          (is (string/starts-with? (str url) "http://127.0.0.1:")))
        (testing "and evaluating before a page connects says so rather than waiting"
          ;; which is what lets you start the repl first and open the page when
          ;; you get to it
          (let [f (frame-tagged (eval! r "(+ 1 2)") "exception")]
            (is (some? f))
            (is (string/includes? (:message f) url))))
        (let [page (start-page! url)]
          (try
            ;; the page dials out, which takes as long as node takes to start
            (loop [waited 0]
              (when (and (< waited 20000)
                         (not= "3" (:value (frame-tagged (eval! r "(+ 1 2)") "ret"))))
                (Thread/sleep 200)
                (recur (+ waited 200))))
            (is (= "3" (:value (frame-tagged (eval! r "(+ 1 2)") "ret"))))
            (testing "and what a form prints comes back before its value here too"
              (let [frames (eval! r "(do (println \"from the page\") 9)")]
                (is (= ["out" "ret" "prompt"] (mapv :tag frames)))
                (is (= "from the page\n" (printed frames "out")))))
            (testing "and the page can be named, which node has no version of"
              (is (string/includes?
                   (:value (frame-tagged (eval! r "(pages)") "ret")) "*")))
            (finally (.destroy page))))))))

(deftest test-two-targets-are-two-programs
  (when (compiling?)
    ;; Q2, and the reason for it: a browser and node resolve npm packages
    ;; differently - the bundler is pinned to the browser's conditions - so one
    ;; symbol table for both would be one table describing two programs.
    (with-repl [n]
      (with-repl [b {:dialect :cljs :target :browser}]
        (eval! n "#replique/ns rt.split\n(def only-in-node 1)")
        (is (some? (binding [cljs/*target* :node] (cljs/find-namespace 'rt.split))))
        (is (nil? (binding [cljs/*target* :browser] (cljs/find-namespace 'rt.split))))
        (testing "two symbol tables and two directories, and neither is the other's"
          (is (not (identical? (:cenv (node-env)) (:cenv (browser-env)))))
          (is (not= (:out-dir (node-env)) (:out-dir (browser-env)))))))))

(deftest test-a-question-that-does-not-say-which-target-is-about-the-browser
  ;; Q2's default, and the reason for it: the bundler is pinned to the browser's
  ;; conditions - clojure.cljs.build_npm shares one esbuild option map between the
  ;; probe that decides which specifiers are CommonJS and the build that writes
  ;; the module - so a reading op with no repl anywhere is answered about the same
  ;; resolution a build would do.
  (is (= :browser cljs/default-target))
  (is (= :browser cljs/*target*)))

(deftest test-repl-quit-ends-the-connection
  (when (compiling?)
    (let [r (cljs-repl!)]
      (try
        (send! r ":repl/quit")
        (is (= :eof (recv r)))
        (finally (disconnect r))))))

;;; Starting on a namespace

(deftest test-a-repl-can-be-started-on-a-namespace
  (when (compiling?)
    ;; What master's `(cljs-repl 'my.app)' was for, and the half of it that
    ;; matters is not the compile: a repl started on a program is one whose
    ;; program is IN THE RUNTIME, so the first thing you ask about it answers.
    (with-repl [r {:dialect :cljs :target :node :main "rt.main-program"}]
      (testing "nothing is framed for it - there was no form, so there is no ret"
        (is (= "prompt" (:tag (:prompt r)))))
      (testing "and it does not move the repl, which :ns would be for"
        (is (= "cljs.user" (:ns (:prompt r)))))
      (testing "but the program is loaded, with no require typed"
        (is (= "4" (:value (frame-tagged (eval! r "(rt.main-program/twice 2)")
                                         "ret"))))
        (is (= ":yes" (:value (frame-tagged
                               (eval! r "rt.main-program/program-was-loaded")
                               "ret"))))))))

(deftest test-a-main-that-cannot-be-loaded-is-said-before-the-first-prompt
  (when (compiling?)
    ;; The failure has somewhere to go and has to go there: a repl whose :main
    ;; silently did nothing is a repl standing in a program that is not loaded.
    (let [r (cljs-repl! {:dialect :cljs :target :node :main "rt.no-such-program"})]
      (try
        ;; `repl-client' reads the one frame after the reply and calls it the
        ;; prompt. Here that frame is the exception and the prompt is behind it,
        ;; which is the ordering under test.
        (is (= "exception" (:tag (:prompt r))))
        ;; named as the file it looked for, which is what the compile knows
        (is (string/includes? (:message (:prompt r)) "rt/no_such_program.cljs")
            (:message (:prompt r)))
        (is (= "prompt" (:tag (recv r))))
        (testing "and the repl is a repl anyway"
          (is (= "3" (:value (frame-tagged (eval! r "(+ 1 2)") "ret")))))
        (finally (disconnect r))))))

(deftest test-a-main-that-is-not-a-name-is-refused-by-the-handshake
  (when (compiling?)
    ;; Answered where every other malformed field is answered, rather than as a
    ;; failure to load something that was never a namespace.
    (let [r (repl-client @the-process
                         {:dialect :cljs :target :node :main 42} slow)]
      (try
        (is (= "error" (:tag (:hello r))))
        (is (= "invalid-main" (:error (:hello r))))
        (is (string/includes? (:message (:hello r)) "42"))
        (finally (disconnect r))))))
