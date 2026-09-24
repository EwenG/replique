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

  The target is node almost throughout. The browser one cannot be driven without
  a page, and what a page would exercise is the transport rather than this role
  - the compiler's own `browser-test' is where that is tested, against a node
  process running the browser client. THE EXCEPTION IS `:main', whose whole
  point is that it needs no page: the two tests of it on the browser are the
  ones at the bottom of this file, and they start the two servers and never open
  anything."
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [replique.cljs :as cljs]
            [replique.core :as core]
            [replique.protocol :as protocol]
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
    (reset! the-process (core/start! {:directory dir :init false}))
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

(defn- browser-env
  "The same, for the one target the `:main' tests below use.

  A SECOND ENVIRONMENT, not a second view of the first: two targets compile two
  different programs out of the same sources, so this one has its own symbol
  table and its own output directory and knows nothing of what the node repls
  in this file have compiled."
  []
  (binding [cljs/*target* :browser] (cljs/environment)))

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
    ;; What master's `(cljs-repl 'my.app)' was for. ON NODE THE LOAD HAPPENS
    ;; TOO, which is the half this test is about: there is no page to do it and
    ;; the process dialled back before the handshake replied, so a repl started
    ;; on a program is one whose program is IN THE RUNTIME and the first thing
    ;; you ask about it answers. The browser is the other way round and is
    ;; `test-a-main-on-the-browser-is-compiled-with-no-page' below.
    (with-repl [r {:dialect :cljs :target :node :main "rt.main-program"}]
      (testing "the reply says which program, for a client that did not ask"
        ;; A second editor attaching to a repl it did not start has the reply
        ;; and nothing else. Master said the same thing in every repl-meta.
        (is (= "rt.main-program" (:main (:hello r))) (pr-str (:hello r))))
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

(deftest test-a-main-on-node-is-in-the-runtime-before-the-first-form
  (when (compiling?)
    ;; THE HALF THE TEST ABOVE CANNOT SEE. Referring to a namespace is enough to
    ;; make the runtime fetch it - `(rt.main-program/twice 2)' answers whether
    ;; or not anything required it first - so every assertion that asks the
    ;; program about itself passes with :main compiling and not loading. What
    ;; distinguishes them is a side effect at LOAD time, read by a form that
    ;; mentions no ClojureScript and so cannot be what caused it.
    ;;
    ;; And it is worth distinguishing because it is the whole of what :main buys
    ;; on node: the first form you send is not the one that pays for your
    ;; dependency graph, and a program whose top level starts something has
    ;; started it by the first prompt.
    (with-repl [r {:dialect :cljs :target :node :main "rt.loaded-program"}]
      (is (= "true" (:value (frame-tagged
                             (eval! r "(.-__rt_loaded_program js/globalThis)")
                             "ret")))))))

(deftest test-a-repl-started-on-nothing-says-nothing-about-a-main
  (when (compiling?)
    ;; Absent rather than null, which is this protocol's rule everywhere: a
    ;; client reads the key being there, and emacs's json parser maps null and
    ;; false to sentinel objects a caller then has to know about.
    (with-repl [r nil]
      (is (= "reply" (:tag (:hello r))))
      (is (not (contains? (:hello r) :main)) (pr-str (:hello r))))))

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

(deftest test-a-main-that-failed-inside-a-macro-carries-the-trace
  (when (compiling?)
    ;; WHAT THE MESSAGE DOES NOT SAY. A compile that dies inside somebody's
    ;; macro dies with the message that macro's bug produced, and a message can
    ;; name nothing at all - rt.bad-macro is one of those kept as a fixture,
    ;; because it is the shape of the one a project using sci met: a var
    ;; holding nil, called two expansions and one library away from anything
    ;; the person reading it wrote.
    ;;
    ;; SO THE FRAMES ARE THE REPORT. They are the only thing in this frame that
    ;; says whose code the compiler was inside, and a repl that sent the
    ;; sentence without them sent something nobody can act on. `:main' is where
    ;; this matters most: there is no form to look at, because the failure
    ;; happened before the first prompt and on a namespace nobody typed.
    (let [r (cljs-repl! {:dialect :cljs :target :node :main "rt.uses-bad-macro"})]
      (try
        (let [f (:prompt r)]
          (is (= "exception" (:tag f)) (pr-str f))
          (is (= "compile" (:phase f))
              "the compiler in this process is what noticed")
          (testing "the message names nobody"
            (is (string/includes? (:message f) "getRawRoot") (:message f))
            (is (not (string/includes? (:message f) "bad-macro")))
            (is (not (string/includes? (:message f) "bad_macro"))))
          (testing "and the trace names the macro, its file and its line"
            (is (string/includes? (:stacktrace f) "rt.bad_macro") (:stacktrace f))
            (is (string/includes? (:stacktrace f) "bad_macro.clj:")))
          (testing "under the compiler that was expanding it, which is what
          says this was a macro rather than the program"
            (is (string/includes? (:stacktrace f) "clojure.cljs.macroexpand")
                (:stacktrace f))))
        (is (= "prompt" (:tag (recv r))))
        (testing "and the repl is a repl anyway"
          (is (= "3" (:value (frame-tagged (eval! r "(+ 1 2)") "ret")))))
        (finally (disconnect r))))))

(deftest test-what-a-trace-in-a-frame-leaves-out-is-said
  ;; The text half of `replique.repl-test/what-an-exception-frame-leaves-out-is-
  ;; said', and bounded by the same two numbers: a ClojureScript frame carries a
  ;; stack as TEXT, so what a client is handed here is not a vector it can
  ;; count. It has to be told in the text itself, or sixty four frames of a
  ;; three hundred frame trace read as the whole of one.
  ;;
  ;; No process and no compiler: this is the rendering, which is the same
  ;; rendering whether or not this process can compile anything.
  (let [deep (try ((fn f [n] (if (zero? n)
                               (throw (Exception. "the bottom"))
                               (inc (f (dec n)))))
                   200)
                  (catch Throwable t t))
        text (protocol/exception->text deep)]
    (testing "a trace longer than a frame carries"
      (is (string/starts-with? text "java.lang.Exception: the bottom\n"))
      (is (= 64 (count (re-seq #"(?m)^\tat " text))))
      (is (re-find #"(?m)^\t\.\.\. \d+ more frames were left out$" text)
          "how many frames were left out, so whoever reads it knows"))
    (testing "a trace that fits says nothing"
      (let [text (protocol/exception->text (doto (Exception. "shallow")
                                             (.setStackTrace
                                              (make-array StackTraceElement 0))))]
        (is (= "java.lang.Exception: shallow\n" text)))))
  (testing "a chain deeper than a frame carries"
    (let [chain (reduce (fn [c i] (ex-info (str "level " i) {} c)) nil (range 20))
          text (protocol/exception->text chain)]
      (is (= 8 (count (re-seq #"(?m)^Caused by: clojure.lang.ExceptionInfo" text)))
          "eight causes under the one reported, as the data shape carries")
      (is (string/includes? text "the rest of the chain was left out"))))
  (testing "a chain that fits says so by not saying anything"
    (let [text (protocol/exception->text (ex-info "one" {} (Exception. "two")))]
      (is (string/includes? text "Caused by: java.lang.Exception: two"))
      (is (not (string/includes? text "left out"))))))

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

;;; Starting on a namespace, where the runtime is a page nobody has opened

(deftest test-a-main-on-the-browser-is-compiled-with-no-page
  (when (compiling?)
    ;; WHAT :main PROMISES IS THE OUTPUT DIRECTORY. The browser's runtime is a
    ;; page a human opens when they get to it, and the page is what loads the
    ;; program - it asks this process's server for the modules its namespaces
    ;; were compiled into. So the compile is replique's half and the load is
    ;; not, and this is the test that the half that is replique's happens with
    ;; nothing connected and nothing waiting.
    ;;
    ;; It is also the test that nothing is FRAMED for it. Requiring into a
    ;; browser with no page answers "No browser is connected. Open ..." - a
    ;; sentence that is right for a form you typed and wrong for a :main, where
    ;; it would be the whole of what a repl started on a program had to say.
    (with-repl [r {:dialect :cljs :target :browser :main "rt.main-program"}]
      (testing "the frame after the reply is the prompt, not an exception"
        (is (= "prompt" (:tag (:prompt r))) (pr-str (:prompt r))))
      (testing "and it did not move the repl"
        (is (= "cljs.user" (:ns (:prompt r)))))
      (testing "the namespace is in this target's symbol table"
        (is (some? (binding [cljs/*target* :browser]
                     (cljs/find-namespace 'rt.main-program)))))
      (testing "and the module a page would fetch is on disk"
        ;; The path the fork emits, spelled the way it emits it: namespaces
        ;; under ns/, named as they are named rather than munged. A page's
        ;; import of the entry module is a GET of exactly this, which is why
        ;; this file and not the symbol table is the promise.
        (let [^java.io.File out (:out-dir (browser-env))
              module (io/file out "ns" "rt" "main-program.js")]
          (is (.isFile module)
              (str "not in " out ": "
                   (pr-str (mapv str (rest (file-seq out)))))))))))

(deftest test-a-main-on-the-browser-that-cannot-compile-is-still-framed
  (when (compiling?)
    ;; THE HALF A LIVENESS TEST MUST NOT SWALLOW. Not framing "no browser is
    ;; connected" is one thing; not framing a namespace that is not there would
    ;; be a repl standing in a program that was never compiled and saying
    ;; nothing about it - and on the browser there is no later moment when
    ;; anybody finds out, because the page's fetch of it 404s in a console
    ;; replique is not reading.
    (let [r (cljs-repl! {:dialect :cljs :target :browser
                         :main "rt.no-such-program"})]
      (try
        (is (= "exception" (:tag (:prompt r))) (pr-str (:prompt r)))
        (is (string/includes? (:message (:prompt r)) "rt/no_such_program.cljs")
            (:message (:prompt r)))
        (is (= "prompt" (:tag (recv r))))
        (finally (disconnect r))))))

;;; What happens after a load

(defmacro ^:private with-hook
  "Body with a hook registered under PREFIX, and taken off again afterwards."
  [prefix f & body]
  `(do (swap! cljs/env-hooks assoc ~prefix ~f)
       (try ~@body (finally (swap! cljs/env-hooks dissoc ~prefix)))))

(defn- a-file
  "A .cljs file holding NS, written under DIR, as a #replique/load directive."
  [dir ns src]
  (let [f (io/file (str dir) (str (string/replace (str ns) "." "/") ".cljs"))]
    (.mkdirs (.getParentFile f))
    (spit f (str "(ns " ns ")\n" src "\n"))
    (str "#replique/load {:file \"" (.getPath f) "\"}\n")))

(deftest test-a-hook-fires-after-a-load-and-after-nothing-else
  (when (compiling?)
    ;; WHAT MASTER'S env-hooks WERE FOR: a program whose top level built
    ;; something - a React tree - has to be told when its code was replaced
    ;; underneath it, and nothing in this protocol knows that on its behalf.
    ;;
    ;; WHAT FIRES ONE IS A LOAD DIRECTIVE, which is a client SAYING it loaded
    ;; something rather than replique working it out. Master worked it out, and
    ;; could: its namespaces are the JVM's, so a watch on every var's root
    ;; caught a redefinition and ns-map identity caught a var arriving. Neither
    ;; reads a ClojureScript namespace of this compiler - a var of one is
    ;; unbound and its value is in JavaScript - so this is the floor, and the
    ;; ceiling is for the compiler to say what it compiled.
    (let [dir   (client/temp-dir)
          fired (atom [])]
      (try
        (with-hook 'hk.watched (fn [e] (swap! fired conj (:namespace e)))
          (with-repl [r nil]
            (testing "a load of a namespace it covers"
              (eval! r (a-file dir 'hk.watched.one "(def v :one)"))
              (is (= ['hk.watched.one] @fired) (pr-str @fired)))

            (testing "loading it again"
              (eval! r (a-file dir 'hk.watched.one "(def v :two)"))
              (is (= 2 (count @fired))))

            (testing "a require is not a load directive, and does not fire it"
              (eval! r "(require 'hk.watched.one :reload)")
              (is (= 2 (count @fired))))

            (testing "nor does a defn typed at the repl - load the file it is in"
              (eval! r "#replique/ns hk.watched.one\n(defn typed [] :here)")
              (is (= 2 (count @fired))))

            (testing "nor a form that loaded nothing at all"
              (eval! r "(+ 1 2)")
              (is (= 2 (count @fired))))

            (testing "and neither does a load of a namespace no hook covers"
              (eval! r (a-file dir 'hk.elsewhere "(def v :other)"))
              (is (= 2 (count @fired))))))
        (finally (client/delete-recursively dir))))))

(deftest test-a-hook-does-not-fire-for-a-load-that-failed
  (when (compiling?)
    ;; A file that would not compile did not replace the program that is
    ;; running, and saying it did is the one thing a hook must not be told.
    (let [dir   (client/temp-dir)
          fired (atom 0)]
      (try
        (with-hook 'hk.broken (fn [_] (swap! fired inc))
          (with-repl [r nil]
            (let [frames (eval! r (a-file dir 'hk.broken.one "(def v (this-is-not-a-thing))"))]
              (is (= "exception" (:tag (frame-tagged frames "exception")))
                  (pr-str frames)))
            (is (zero? @fired))
            (testing "and the repl is a repl anyway"
              (is (= "3" (:value (frame-tagged (eval! r "(+ 1 2)") "ret")))))))
        (finally (client/delete-recursively dir))))))

(deftest test-a-hook-that-throws-does-not-take-the-repl-with-it
  (when (compiling?)
    ;; What it was called after happened - the file loaded - so turning the
    ;; hook's failure into the form's failure would report the wrong thing
    ;; about the wrong thing. It goes to err, and the form's own result follows.
    (let [dir (client/temp-dir)]
      (try
        (with-hook 'hk.throwing (fn [_] (throw (ex-info "the hook is broken" {})))
          (with-repl [r nil]
            (let [frames (eval! r (a-file dir 'hk.throwing.one "(def v :one)"))]
              (is (= "ret" (:tag (frame-tagged frames "ret"))) (pr-str frames))
              (is (string/includes? (printed frames "err") "the hook is broken")
                  (pr-str frames)))
            (testing "and the repl goes on"
              (is (= "3" (:value (frame-tagged (eval! r "(+ 1 2)") "ret")))))))
        (finally (client/delete-recursively dir))))))
