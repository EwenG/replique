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
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [replique.json :as json]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [replique.cljs :as cljs]
            [replique.core :as core]
            [replique.hooks :as hooks]
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
      (testing "and the reply says nothing about the runtime, which is not started yet"
        ;; The whole of why it is answered at once: the compiler and the runtime
        ;; are seconds and the require lock is shared, so a handshake that waited
        ;; for them is a handshake a client gives up on
        (is (nil? (:url (:hello r)))))
      (testing "where the runtime is arrives after the reply and before the prompt"
        (is (= "event" (:tag (:runtime r))))
        (is (= "runtime" (:event (:runtime r))))
        (is (= "cljs" (:dialect (:runtime r))))
        (is (= "node" (:target (:runtime r))))
        (testing "and node has no url to open, unlike the browser"
          (is (nil? (:url (:runtime r))))))
      (testing "the first prompt stands in cljs.user and says so the same way"
        (is (= "prompt" (:tag (:prompt r))))
        (is (= "cljs.user" (:ns (:prompt r))))
        (is (= "cljs" (:dialect (:prompt r))))
        (is (= "node" (:target (:prompt r))))
        (is (= (:connection (:hello r)) (:connection (:prompt r)))))
      (testing "and params, when there are any, are the runtime's three"
        ;; the *print-* the runtime prints under, as its last result said - so
        ;; absent before anything was evaluated on it, which depends on what ran
        ;; before this - and no reflection, which is the JVM's
        (when-let [params (:params (:prompt r))]
          (is (= #{:print-length :print-level :print-meta} (set (keys params)))))))))

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

(deftest test-a-reload-directive-is-a-form-like-any-other
  ;; The directive is handled beside the loop and answers with a form - the
  ;; compiler's own `stale-reload' special - so what comes back is a value and a
  ;; prompt, framed in that order, the way an evaluated form is. WHAT it reloads
  ;; is `replique.cljs-analysis-test's; this is that it is evaluated at all.
  (when (compiling?)
    (with-repl [r]
      (let [frames (eval! r "#replique/reload {}\n")]
        (is (= ["ret" "prompt"] (mapv :tag frames)))
        ;; the files it recompiled, which under a process nothing has edited
        ;; is a list of none of them - read rather than compared against "[]",
        ;; since what the other tests of this file left on disk is not this
        ;; test's business
        (is (vector? (read-string (:value (frame-tagged frames "ret")))))
        (testing "and it does not move the repl, any more than a load does"
          (is (= "cljs.user" (:ns (frame-tagged frames "prompt")))))))))

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
      (let [url (:url (:runtime r))]
        (testing "the url is the whole of what a client has to do something with"
          (is (string/starts-with? (str url) "http://127.0.0.1:")))
        (testing "and it is an event, the reply having gone out before there was one"
          (is (nil? (:url (:hello r))))
          (is (= "runtime" (:event (:runtime r)))))
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
                ;; TWO out frames for one println, where node makes one, and that
                ;; is the browser's *print-fn* rather than a fault: cljs.core's
                ;; println hands the print fn what it was given and then calls it
                ;; again with the newline, and each call is a message on the
                ;; socket. The console tee that used to carry this made one
                ;; message of them by appending a newline to every console.log -
                ;; which also gave one to (print "x"), which had not asked for one.
                (is (= ["out" "out" "ret" "prompt"] (mapv :tag frames)))
                (is (= "from the page\n" (printed frames "out")))))
            (testing "and what the page logs to its console stays in its console"
              ;; The page's own logging is not something a repl asked for, and on
              ;; an application of any size it is most of what there is. It goes
              ;; where it was going anyway - devtools, which shows it against the
              ;; line that produced it and with the object rather than a printed
              ;; copy of it - and no frame carries it here.
              (let [frames (eval! r "(do (js* \"console.log('in devtools')\") 9)")]
                (is (= ["ret" "prompt"] (mapv :tag frames)))))
            (testing "and the page can be named, which node has no version of"
              (is (string/includes?
                   (:value (frame-tagged (eval! r "(pages)") "ret")) "*")))
            (finally (.destroy page))))))))

;;; The printing a runtime prints under

(def ^:private no-params "(set! *print-length* nil) (set! *print-level* nil) (set! *print-meta* false)")

(deftest test-the-prompt-says-what-the-runtime-prints-under
  (when (compiling?)
    (with-repl [a]
      (with-repl [b]
        (try
          (let [frames (eval! a "(set! *print-length* 2)")]
            (is (= {:print-length 2 :print-level nil :print-meta false}
                   (:params (frame-tagged frames "prompt")))))
          (is (= "(0 1 ...)" (:value (frame-tagged (eval! a "(range 5)") "ret"))))
          (testing "and it is the runtime's, so another repl on it prints the same way"
            (let [frames (eval! b "(range 5)")]
              (is (= "(0 1 ...)" (:value (frame-tagged frames "ret"))))
              (is (= 2 (:print-length (:params (frame-tagged frames "prompt")))))))
          (finally (eval! a no-params)))))))

(deftest test-a-handshake-says-what-the-runtime-is-to-print-under
  (when (compiling?)
    (with-repl [r {:dialect :cljs :target :node :params {:print-level 1}}]
      (try
        (testing "the first prompt says so before anything is evaluated"
          (is (= 1 (:print-level (:params (:prompt r))))))
        (testing "and the first form is printed under it"
          (is (= "[1 #]" (:value (frame-tagged (eval! r "[1 [2]]") "ret")))))
        (finally (eval! r no-params))))))

(deftest test-params-a-clojurescript-repl-cannot-be-started-with-are-refused
  (when (compiling?)
    (doseq [params [{:warn-on-reflection true} {:print-length -1} {:print-meta 1}]]
      (let [r (cljs-repl! {:dialect :cljs :target :node :params params})]
        (try
          (is (= "error" (:tag (:hello r))) (pr-str params))
          (is (= "invalid-params" (:error (:hello r))) (pr-str params))
          (finally (disconnect r)))))))

(deftest test-a-page-that-reloads-prints-the-way-the-last-one-did
  (when (compiling?)
    (with-repl [r {:dialect :cljs :target :browser}]
      (let [url (:url (:runtime r))
            connected! (fn []
                         (loop [waited 0]
                           (when (and (< waited 20000)
                                      (not= "3" (:value (frame-tagged (eval! r "(+ 1 2)") "ret"))))
                             (Thread/sleep 200)
                             (recur (+ waited 200)))))]
        (try
          (let [page (start-page! url)]
            (try
              (connected!)
              (eval! r "(set! *print-length* 2)")
              (is (= "(0 1 ...)" (:value (frame-tagged (eval! r "(range 5)") "ret"))))
              (finally (.destroy page) (.waitFor page))))
          ;; a new page is a new program, with cljs.core's defaults in it - and
          ;; is given the printing the last one had before the first form
          (let [page (start-page! url)]
            (try
              (connected!)
              (let [frames (eval! r "(range 5)")]
                (is (= "(0 1 ...)" (:value (frame-tagged frames "ret"))))
                (is (= 2 (:print-length (:params (frame-tagged frames "prompt"))))))
              (finally (eval! r no-params) (.destroy page) (.waitFor page)))))))))

;;; Inspecting a value of the runtime's

(defn- inspect-events
  "The inspect-changed events C receives in the next MS."
  [c ms]
  (let [^java.net.Socket socket (:socket c)
        deadline (+ (System/currentTimeMillis) ms)]
    (loop [seen []]
      (let [left (- deadline (System/currentTimeMillis))]
        (if (<= left 0)
          seen
          (do (.setSoTimeout socket (int (max 1 left)))
              (let [f (try (recv c) (catch java.net.SocketTimeoutException _ ::none))]
                (.setSoTimeout socket 10000)
                (if (= ::none f)
                  seen
                  (recur (cond-> seen
                           (= "inspect-changed" (:event f)) (conj f)))))))))))

(defn- line-keyed [children k]
  (first (filter #(= k (:key %)) children)))

(deftest test-a-value-of-the-runtime-is-inspected-where-it-is
  (when (compiling?)
    (with-repl [r]
      (let [c (client/control-client @the-process)]
        (try
          (eval! r "#replique/ns insp.node\n(def state (atom {:a 1 :b (vec (range 300)) :o #js {:x 1}}))")
          (let [o (request! c {:op :inspect :dialect :cljs :target :node
                               :source {:var "insp.node/state"} :history 3 :id 1})
                b (line-keyed (:children o) ":b")]
            (is (= "reply" (:tag o)))
            (is (= "map" (:kind (:root o))))
            (is (= 300 (:count b)))
            (is (:expandable b))
            (testing "a page at a time, out of the runtime"
              (let [page (request! c {:op :inspect-children :view (:view o) :node (:node b)
                                      :offset 298 :id 2})]
                (is (= ["298" "299"] (mapv :key (:children page))))
                (is (false? (:more page)))))
            (testing "a JavaScript object is browsed by its properties"
              (is (= "object" (:kind (line-keyed (:children o) ":o")))))
            (testing "and a line reads in the order of the lines it opens to"
              (is (= [":a" ":b" ":o"] (mapv :key (:children o))))
              (is (= ["{" "}"] [(:open o) (:close o)]))
              (is (re-find #"^\{:a 1, :b \[" (:value (:root o)))))
            (testing "and a swap in the runtime is said, by the runtime"
              (eval! r "(swap! state assoc :a 2)")
              (is (= 1 (count (inspect-events c 2000))))
              (testing "once, until the view is asked for again"
                (eval! r "(swap! state assoc :a 2)")
                (is (empty? (inspect-events c 500))))
              (let [again (request! c {:op :inspect-refresh :view (:view o) :id 3})]
                (is (:changed (line-keyed (:children again) ":a")))
                (is (= "2" (:value (line-keyed (:children again) ":a"))))
                (is (= {:count 2} (:history again))))
              (eval! r "(swap! state assoc :a 3)")
              (is (= 1 (count (inspect-events c 2000)))))
            (testing "and what is shown is brought to the repl by the code that
            reaches it, or by the view"
              (let [p (request! c {:op :inspect-path :view (:view o) :node (:node b) :id 4})]
                (is (= "(-> (deref insp.node/state) :b)" (:code p)))
                (is (= "300" (:value (frame-tagged (eval! r (str "(count " (:value p) ")")) "ret")))))))
          (testing "and a var the runtime does not have is said to be one"
            (is (= "unknown-var" (:error (request! c {:op :inspect :dialect :cljs :target :node
                                                      :source {:var "insp.node/nope"} :id 5})))))
          (finally (disconnect c)))))))

(deftest test-what-a-runtime-tapped-and-returned-are-views-too
  (when (compiling?)
    (with-repl [r]
      (let [c (client/control-client @the-process)]
        (try
          ;; before any view: the repl loads what keeps them before its forms
          (eval! r "(tap> {:tapped :early})")
          (Thread/sleep 200)
          (let [o (request! c {:op :inspect :dialect :cljs :target :node
                               :source {:taps true} :id 1})]
            (is (some #{"{:tapped :early}"} (map :value (:children o)))))
          (let [o (request! c {:op :inspect :dialect :cljs :target :node
                               :source {:results true} :id 2})]
            (is (= ["*1" "*2" "*3"] (mapv :key (:children o))))
            (eval! r ":latest")
            (is (seq (inspect-events c 1000)))
            (is (= ":latest" (:value (line-keyed (:children (request! c {:op :inspect-refresh
                                                                         :view (:view o) :id 3}))
                                                 "*1")))))
          (finally (disconnect c)))))))

(deftest test-a-view-of-a-page-that-reloaded-is-gone
  (when (compiling?)
    (with-repl [r {:dialect :cljs :target :browser}]
      (let [url (:url (:runtime r))
            c (client/control-client @the-process)
            connected! (fn []
                         (loop [waited 0]
                           (when (and (< waited 20000)
                                      (not= "3" (:value (frame-tagged (eval! r "(+ 1 2)") "ret"))))
                             (Thread/sleep 200)
                             (recur (+ waited 200)))))]
        (try
          (let [o (let [page (start-page! url)]
                    (try
                      (connected!)
                      (eval! r "#replique/ns insp.page\n(def state (atom 1))")
                      (let [o (request! c {:op :inspect :dialect :cljs :target :browser
                                           :source {:var "insp.page/state"} :id 1})]
                        (is (= "1" (:value (:root o))))
                        (testing "a page says a swap as node does"
                          (eval! r "(reset! state 2)")
                          (is (seq (inspect-events c 2000))))
                        o)
                      (finally (.destroy page) (.waitFor page))))]
            (let [page (start-page! url)]
              (try
                (connected!)
                (testing "a new page is a new program, without the view"
                  (is (= "view-gone" (:error (request! c {:op :inspect-refresh
                                                          :view (:view o) :id 2})))))
                (finally (.destroy page) (.waitFor page)))))
          (finally (disconnect c)))))))

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
        ;; `repl-client' reads up to the prompt and keeps what came before it,
        ;; which here is the runtime event and then the exception - and what is
        ;; under test is that the exception is among them rather than after the
        ;; prompt.
        (let [f (last (:before r))]
          (is (= "exception" (:tag f)) (pr-str (:before r)))
          ;; named as the file it looked for, which is what the compile knows
          (is (string/includes? (:message f) "rt/no_such_program.cljs")
              (:message f)))
        (is (= "prompt" (:tag (:prompt r))))
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
        (let [f (last (:before r))]
          (is (= "exception" (:tag f)) (pr-str (:before r)))
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
        (is (= "prompt" (:tag (:prompt r))))
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
        (let [f (last (:before r))]
          (is (= "exception" (:tag f)) (pr-str (:before r)))
          (is (string/includes? (:message f) "rt/no_such_program.cljs")
              (:message f)))
        (is (= "prompt" (:tag (:prompt r))))
        (finally (disconnect r))))))


;;; What happens after an evaluation replaced some of the program

(defmacro ^:private with-hook
  "Body with a hook registered under PREFIX, and taken off again afterwards."
  [prefix f & body]
  `(do (swap! hooks/cljs-hooks assoc ~prefix ~f)
       (try ~@body (finally (swap! hooks/cljs-hooks dissoc ~prefix)))))

(def ^:private clock
  "Writes are stamped rather than timed: a test writes a file, loads it and writes
  it again inside one millisecond, which on a filesystem whose timestamps are that
  coarse is a file nothing noticed had changed."
  (atom 0))

(defn- a-file
  "A .cljs file holding NS, written under DIR, as a #replique/load directive.

  Stamped with an mtime no earlier write of this run can share, so that a file
  written twice reads as a file that was edited - which is what a reload is
  about and what a filesystem with coarse timestamps would otherwise hide."
  [dir ns src]
  (let [f (io/file (str dir) (str (string/replace (str ns) "." "/") ".cljs"))]
    (.mkdirs (.getParentFile f))
    (spit f (str "(ns " ns ")\n" src "\n"))
    (.setLastModified f (+ (System/currentTimeMillis) (* 10000 (swap! clock inc))))
    (str "#replique/load {:file \"" (.getPath f) "\"}\n")))

(deftest test-a-hook-fires-once-per-evaluation-that-replaced-something
  (when (compiling?)
    ;; WHAT HOOKS ARE FOR: a program whose top level built something - a React
    ;; tree - has to be told when its code was replaced underneath it, and
    ;; nothing in this protocol knows that on its behalf.
    ;;
    ;; AND WHAT FIRES ONE IS THE COMPILER SAYING WHAT IT DEFINED, not a client
    ;; saying what it asked for. The first version of this fired after a load
    ;; directive and after nothing else, so a reload of forty files fired
    ;; nothing and a def typed at a prompt fired nothing; both replace code that
    ;; is running. See `replique.hooks'.
    (let [dir   (client/temp-dir)
          fired (atom [])]
      (try
        (with-hook 'hk.watched (fn [e] (swap! fired conj e))
          (with-repl [r nil]
            (testing "a load of a namespace it covers"
              (eval! r (a-file dir 'hk.watched.one "(def v :one)"))
              (is (= 1 (count @fired)) (pr-str @fired))
              (let [e (first @fired)]
                (is (= :cljs (:dialect e)))
                (is (= ['hk.watched.one] (:namespaces e)) (pr-str e))
                (testing "and the unit is the namespace, because the module was
                          rewritten whole - there is no one var to name"
                  (is (= [] (:vars e))))))

            (testing "a def typed at the prompt, which names no file and is the
                      other unit: one var, in the namespace the cursor is in"
              (reset! fired [])
              (eval! r "#replique/ns hk.watched.one\n(defn typed [] :here)")
              (is (= 1 (count @fired)) (pr-str @fired))
              (let [e (first @fired)]
                (is (= ['hk.watched.one] (:namespaces e)))
                (is (= ['hk.watched.one/typed] (:vars e)) (pr-str e))))

            (testing "a form that defines nothing fires nothing"
              (reset! fired [])
              (eval! r "(+ 1 2)")
              (is (= [] @fired) (pr-str @fired)))

            (testing "and neither does a load of a namespace no hook covers"
              (reset! fired [])
              (eval! r (a-file dir 'hk.elsewhere "(def v :other)"))
              (is (= [] @fired) (pr-str @fired)))))
        (finally (client/delete-recursively dir))))))

(deftest test-a-hook-fires-once-for-a-reload-of-many-namespaces
  (when (compiling?)
    ;; THE CASE THE FIRST VERSION COULD NOT REACH AT ALL. A reload names files
    ;; and works out the rest for itself, so a hook keyed to one namespace fired
    ;; nothing - and a reload after a branch switch is the moment a page most
    ;; needs redrawing. One event for the whole of it, because one redraw is
    ;; what it wants, not one per file.
    (let [dir   (client/temp-dir)
          fired (atom [])]
      (try
        (with-hook 'hk.reloaded (fn [e] (swap! fired conj e))
          (with-repl [r nil]
            (eval! r (a-file dir 'hk.reloaded.a "(def v :one)"))
            (eval! r (a-file dir 'hk.reloaded.b "(def w :one)"))
            (reset! fired [])
            ;; both edited, so both are stale, and one reload answers for both
            (a-file dir 'hk.reloaded.a "(def v :two)")
            (a-file dir 'hk.reloaded.b "(def w :two)")
            (let [frames (eval! r "#replique/reload {}\n")]
              (is (= "ret" (:tag (frame-tagged frames "ret"))) (pr-str frames)))
            (is (= 1 (count @fired)) (pr-str @fired))
            ;; BOTH IN ONE EVENT, which is what is being asserted, rather than
            ;; the two of them being the whole of it: the model is the process's
            ;; and a reload answers for whatever else the tests of this jvm have
            ;; left on it, which is not this test's business and is not stable.
            (let [nss (set (:namespaces (first @fired)))]
              (is (contains? nss 'hk.reloaded.a) (pr-str @fired))
              (is (contains? nss 'hk.reloaded.b) (pr-str @fired)))))
        (finally (client/delete-recursively dir))))))

(deftest test-a-hook-hears-what-a-reload-took-away
  (when (compiling?)
    ;; A def deleted from a file is not deleted by recompiling it; a reload
    ;; prunes it, and that is a definition taken away by something nobody typed.
    (let [dir   (client/temp-dir)
          fired (atom [])]
      (try
        (with-hook 'hk.pruned (fn [e] (swap! fired conj e))
          (with-repl [r nil]
            (eval! r (a-file dir 'hk.pruned.a "(def kept 1)\n(def gone 2)"))
            (reset! fired [])
            (a-file dir 'hk.pruned.a "(def kept 1)")
            (eval! r "#replique/reload {}\n")
            (is (= 1 (count @fired)) (pr-str @fired))
            ;; In it rather than the whole of it, for the reason above.
            (is (contains? (set (:removed (first @fired))) 'hk.pruned.a/gone)
                (pr-str @fired))))
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
    ;; What it was called after happened - the code was replaced - so turning the
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

;;; What code compiled to be recorded keeps, in the runtime

(defn- recorded!
  "Evaluate TEXT in NS on node compiled to be recorded as the unit UNIT, a
  DEFINITION or not, numbering its sites from (inc @SITES) on - into SITES,
  {n site}. What `replique.debug' does for a repl that records, without the
  repl: the runtime's half is what is tested here."
  [ns text unit definition sites]
  (binding [cljs/*target* :node]
    (cljs/with-target-lock* #(cljs/compile-namespace! 'replique.debug))
    (cljs/with-ns* ns
      (fn []
        (let [[form] (cljs/read-form (cljs/reader text) ::eof)
              site (fn [s]
                     (let [n (inc (count @sites))]
                       (swap! sites assoc n (dissoc s :form))
                       n))]
          (with-bindings {(requiring-resolve 'clojure.cljs.emitter/*instrument*)
                          {:site site
                           :hook (str "(await $CLJS.require(\"replique.debug\"),"
                                      " $CLJS.namespaces.get(\"replique.debug\")"
                                      ".unit(" unit ", " definition "))")}}
            (cljs/eval-form form {:text text})))))))

(defn- debug-op
  "Ask the runtime's replique.debug MSG, and answer what it said."
  [msg]
  (binding [cljs/*target* :node]
    (let [{:keys [status value]}
          (cljs/eval-js-here (str "$CLJS.namespaces.get(\"replique.debug\").op("
                                  (json/write-str (json/write-str msg)) ")")
                             5000)]
      (is (= :success status) value)
      (edn/read-string (edn/read-string value)))))

(defn- site-at
  "The number of the site of SITES that starts at LINE and COLUMN."
  [sites line column]
  (some (fn [[n s]] (when (and (= line (:line s)) (= column (:column s))) n)) sites))

(deftest test-what-a-recorded-function-evaluated-to-is-kept-call-by-call
  (when (compiling?)
    (with-repl [r]
      (eval! r "#replique/ns rec.node\n(def x 1)")
      (let [sites (atom {})
            unit 9001]
        (is (= :success (:status (recorded! 'rec.node "(defn twice [x]\n  (let [y (* 2 x)]\n    (inc y)))"
                                            unit true sites))))
        (is (= "5" (:value (frame-tagged (eval! r "(rec.node/twice 2)") "ret"))))
        (is (= "7" (:value (frame-tagged (eval! r "(rec.node/twice 3)") "ret"))))
        (let [{:keys [calls call values]} (debug-op {:op "recorded" :unit unit})
              by-site (into {} (map (juxt :site (juxt :value :times))) values)]
          (is (= 2 (count calls)))
          (is (= call (:call (peek calls))))
          (testing "the last call, a form at a time"
            (is (= ["6" 1] (by-site (site-at @sites 2 11))))
            (is (= ["7" 1] (by-site (site-at @sites 3 5))))
            (is (= ["7" 1] (by-site (site-at @sites 2 3)))))
          (testing "and the one before it, asked for"
            (let [first-call (:call (first calls))
                  {:keys [values]} (debug-op {:op "recorded" :unit unit :call first-call})]
              (is (= #{"4" "5"} (set (map :value (remove :args values)))))
              (testing "what it was called with, the value of its entry"
                (is (= [["2"]] (keep :args values))))))
          (testing "and a call that is not kept is said not to be"
            (is (= "unknown-call" (:kind (debug-op {:op "recorded" :unit unit :call 123456}))))))
        (testing "what a form evaluated to is a view, in the runtime"
          (let [c (client/control-client @the-process)]
            (try
              (let [o (request! c {:op :inspect :dialect :cljs :target :node
                                   :source {:debug {:site (site-at @sites 3 5)}} :id 1})]
                (is (= "reply" (:tag o)) (pr-str o))
                (is (= ["5" "7"] (mapv :value (:children o)))))
              (finally (disconnect c)))))
        (testing "and forgotten, nothing is kept"
          (is (= {:forgotten [unit]} (debug-op {:op "forget" :unit unit})))
          (is (= "unknown-unit" (:kind (debug-op {:op "recorded" :unit unit}))))
          (eval! r "(rec.node/twice 4)")
          (is (= "unknown-unit" (:kind (debug-op {:op "recorded" :unit unit})))))))))

(deftest test-a-fn-made-in-a-call-records-in-that-call
  (when (compiling?)
    (with-repl [r]
      (eval! r "#replique/ns rec.later\n(def x 1)")
      (let [sites (atom {})
            unit 9002]
        (recorded! 'rec.later "(defn later [x]\n  (fn [] (inc x)))" unit true sites)
        (eval! r "(def f (rec.later/later 1))")
        (eval! r "(rec.later/later 10)")
        (testing "called once that call and another are over"
          (is (= "2" (:value (frame-tagged (eval! r "(f)") "ret"))))
          (let [{:keys [calls]} (debug-op {:op "recorded" :unit unit})
                first-call (:call (first calls))]
            (is (= 2 (count calls)))
            (is (= ["2"] (map :value (remove :args (:values (debug-op {:op "recorded" :unit unit
                                                                       :call first-call}))))))
            (is (= [] (remove :args (:values (debug-op {:op "recorded" :unit unit})))))))))))

(deftest test-what-a-recorded-form-evaluated-to-is-printed-without-realizing-it
  (when (compiling?)
    (with-repl [r]
      (eval! r "#replique/ns rec.lazy\n(def x 1)")
      (let [sites (atom {})
            unit 9003
            ;; s is never realized, by the form or by what prints its value
            text "(let [s (map inc (range 50))] (count (vec (range 30))))"]
        (testing "a form that is no definition is recorded in one call, its evaluation"
          (is (= :success (:status (recorded! 'rec.lazy text unit false sites))))
          (let [{:keys [calls values]} (debug-op {:op "recorded" :unit unit})
                by-site (into {} (map (juxt :site :value)) values)
                at #(site-at @sites 1 (inc (string/index-of text %)))]
            (is (= 1 (count calls)))
            (is (= "#<unrealized LazySeq>" (by-site (at "(map inc"))))
            (is (= "(0 1 2 3 4 5 6 7 8 9 ...)" (by-site (at "(range 30)"))))))))))

;;; Recording at a ClojureScript repl

(defn- ask!
  "Send MSG on the control connection C, and answer its reply, past any event."
  [c msg]
  (send! c msg)
  (loop []
    (let [f (recv c)]
      (if (= "event" (:tag f)) (recur) f))))

(defn- await-event
  "Read C until the event NAME, and answer it."
  [c name]
  (loop []
    (let [f (recv c)]
      (cond
        (= :eof f) (throw (ex-info "The connection closed" {}))
        (and (= "event" (:tag f)) (= name (:event f))) f
        :else (recur)))))

(defn- recorded-by-place
  "What the call CALL of the unit UNIT recorded - the last one, CALL nil - with
  :by-place, {[line column] [value times]}."
  [c unit call id]
  (let [said (ask! c (cond-> {:op :debug-recorded :unit unit :id id} call (assoc :call call)))]
    (assoc said :by-place (into {} (map (fn [v] [[(:line v) (:column v)] [(:value v) (:times v)]]))
                                (:values said)))))

(defn- record-src
  "CODE, under the directive that has a repl record it, written at LINE and
  COLUMN."
  [line column code]
  (str "#replique/src {:file \"/tmp/recorded.cljs\" :line " line " :column " column
       " :instrument true :entry false :record true}\n" code))

(deftest test-a-recorded-clojurescript-definition-is-a-unit-like-a-clojure-one
  (when (compiling?)
    (with-repl [r]
      (let [c (client/control-client @the-process)]
        (try
          (eval! r "#replique/ns rec.e2e\n(def x 1)")
          (is (= "ret" (:tag (frame-tagged (eval! r (record-src 1 1 "(defn twice [x]\n  (let [y (* 2 x)]\n    (inc y)))")) "ret"))))
          (let [told (await-event c "debug-instrumented")
                unit (:unit told)]
            (testing "told about as recording, in the runtime it is in"
              (is (true? (:record told)))
              (is (= ["cljs" "node"] [(:dialect told) (:target told)]))
              (is (= [1 1] [(:line told) (:column told)])))
            (is (= "5" (:value (frame-tagged (eval! r "(twice 2)") "ret"))))
            (testing "clients are told it recorded"
              (is (= {:unit unit :calls 1}
                     (select-keys (await-event c "debug-recorded") [:unit :calls]))))
            (testing "and asked, the runtime answers, placed where its forms are"
              (let [{:keys [calls by-place]} (recorded-by-place c unit nil 1)]
                (is (= 1 (count calls)))
                (is (= "node" (:thread (first calls))))
                ;; the fn entered, with what it was called with, first
                (is (= {[1 1] ["[2]" 1] [2 11] ["4" 1] [3 5] ["5" 1] [2 3] ["5" 1]} by-place))))
            (testing "call by call"
              (eval! r "(twice 3)")
              (let [{:keys [calls]} (recorded-by-place c unit nil 2)]
                (is (= 2 (count calls)))
                (is (= ["4" 1] (get-in (recorded-by-place c unit (:call (first calls)) 3)
                                       [:by-place [2 11]])))))
            (testing "defined again the ordinary way, it is let go of"
              (eval! r "(defn twice [x] x)")
              (is (= unit (:unit (await-event c "debug-uninstrumented"))))
              (is (= "unknown-unit" (:error (ask! c {:op :debug-recorded :unit unit :id 4}))))))
          (testing "a form written further along its line is placed there"
            (eval! r (record-src 7 3 "(defn other [] (inc 1))"))
            (let [unit (:unit (await-event c "debug-instrumented"))]
              (eval! r "(other)")
              (is (= {[7 18] ["2" 1]} (:by-place (recorded-by-place c unit nil 5))))
              (testing "and recorded again, the unit before it is let go of"
                (eval! r (record-src 7 3 "(defn other [] (inc 2))"))
                ;; the new one is told first: it is what said the old one is
                ;; replaced
                (let [again (:unit (await-event c "debug-instrumented"))]
                  (is (= unit (:unit (await-event c "debug-uninstrumented"))))
                  (is (not= unit again))
                  (eval! r "(other)")
                  (is (= {[7 18] ["3" 1]} (:by-place (recorded-by-place c again nil 6))))))))
          (testing "stepping through is refused, saying why"
            (let [frames (eval! r (str "#replique/src {:line 1 :instrument true}\n(+ 1 2)"))]
              (is (string/includes? (:message (frame-tagged frames "exception")) "recorded"))))
          (finally (disconnect c)))))))

(deftest test-a-repl-evaluates-in-a-recorded-clojurescript-call
  (when (compiling?)
    (with-repl [r]
      (let [c (client/control-client @the-process)
            ret (fn [repl code] (:value (frame-tagged (eval! repl code) "ret")))]
        (try
          (eval! r "#replique/ns rec.incall\n(def g 1)")
          (eval! r (record-src 1 1 (str "(defn ic [m {:keys [z]} & more]\n"
                                        "  (let [x (inc (:a m))\n"
                                        "        [p q] [(dec x) 7]]\n"
                                        "    (doseq [d more] (inc d))\n"
                                        "    (mapv (fn [y] (* x y)) (range 3))))")))
          (let [unit (:unit (await-event c "debug-instrumented"))
                _ (ret r "(ic {:a 1} {:z 2} 3 4)")
                _ (ret r "(ic {:a 10} {:z 20})")
                ;; told before the test is over, not in the middle of another
                _ (loop [] (when-not (= {:unit unit :calls 2}
                                        (select-keys (await-event c "debug-recorded") [:unit :calls]))
                             (recur)))
                [first-call] (map :call (:calls (recorded-by-place c unit nil 1)))]
            (eval! r "#replique/ns elsewhere\n\n")
            (with-repl [f {:dialect :cljs :target :node
                           :call {:unit unit :call first-call :line 5 :column 21}}]
              (testing "a repl opened on a recorded call says so, in the namespace of its code"
                (is (= {:unit unit :call first-call :line 5 :column 21} (:call (:prompt f))))
                (is (= "rec.incall" (:ns (:prompt f)))))
              (testing "what it reads is evaluated with the locals in scope there, as they were"
                (is (= "[{:a 1} 2 (3 4) 2 1 7 2]" (ret f "[m z more x p q y]"))))
              (testing "recorded, not evaluated again: the function is not called"
                (is (= 2 (count (:calls (recorded-by-place c unit nil 2))))))
              (testing "a local bound by a loop has no value"
                (eval! f (str "#replique/call {:unit " unit " :call " first-call
                              " :line 4 :column 22}\n\n"))
                (is (= ":replique/not-recorded" (ret f "d"))))
              (testing "without a call, the last one"
                (let [frames (eval! f (str "#replique/call {:unit " unit " :line 3 :column 9}\n\n"))]
                  (is (= {:unit unit :line 3 :column 9} (:call (last frames)))))
                (is (= "[{:a 10} 20 nil 11]" (ret f "[m z more x]"))))
              (testing "and at none: the ordinary way again"
                (let [frames (eval! f "#replique/call nil\n\n")]
                  (is (nil? (:call (last frames)))))
                (is (= "1" (ret f "g"))))))
          (finally (disconnect c)))))))

;;; Tracing in the runtime
;;; Tracing in the runtime

(deftest test-what-is-traced-in-the-runtime-is-a-tree-of-calls
  (when (compiling?)
    (with-repl [r]
      (let [c (client/control-client @the-process)
            on {:dialect :cljs :target :node}
            ask (fn [msg] (ask! c (merge msg on)))
            calls (fn ([] (:calls (ask {:op :trace-calls :id 9})))
                    ([parent] (:calls (ask {:op :trace-calls :parent parent :id 9}))))
            said (juxt :var :args #(or (:value %) (:thrown %)))]
        (try
          (eval! r "#replique/ns trc.node\n(def x 1)")
          (eval! r "(defn ta [x] (inc x))")
          (eval! r "(defn tb [m] (+ (ta (:n m)) (ta 10)))")
          (eval! r "(defn tc [] (throw (js/Error. \"none\")))")
          (eval! r "(defn ^:async tw [x] (+ (ta x) (await (js/Promise.resolve 1))))")
          (let [{:keys [asked traced skipped]} (ask {:op :trace :ns "trc.node" :id 1})]
            (is (= ["trc.node/ta" "trc.node/tb" "trc.node/tc" "trc.node/tw"] traced))
            (is (= [["trc.node/x" "not a function"]] (map (juxt :var :why) skipped)))
            (is (= asked (+ (count traced) (count skipped)))))
          (is (= "13" (:value (frame-tagged (eval! r "(tb {:n 1})") "ret"))))
          (let [[tb] (calls)]
            (testing "a call, with what it was called with and returned, where it is written"
              (is (= ["trc.node/tb" ["{:n 1}"] "13"] (said tb)))
              (is (= {:state "returned" :made 2 :kept 2} (select-keys tb [:state :made :kept]))))
            (is (= [["trc.node/ta" ["1"] "2"] ["trc.node/ta" ["10"] "11"]]
                   (map said (calls (:call tb)))))
            (testing "a value to browse, in the runtime"
              (is (= "{:n 1}" (get-in (ask {:op :inspect :id 2
                                            :source {:trace {:call (:call tb) :arg 0}}})
                                      [:root :value]))))
            (testing "a call made again, with what it was called with"
              (is (= "13" (:value (frame-tagged (eval! r (str "(apply trc.node/tb (replique.trace/args-of "
                                                              (:call tb) "))"))
                                                "ret"))))
              (is (= ["trc.node/tb" ["{:n 1}"] "13"] (said (last (calls)))))
              (is (string/includes? (:message (frame-tagged (eval! r "(replique.trace/args-of 100000)")
                                                            "exception"))
                                    "not kept"))))
          (testing "a call that threw"
            (eval! r "(try (tc) (catch :default _ nil))")
            (is (= ["trc.node/tc" [] "Error: none"] (said (last (calls))))))
          (testing "an async call is over once its promise is"
            (eval! r "(tw 1)")
            (let [tw (loop [i 0]
                       (let [tw (last (calls))]
                         (if (or (= "returned" (:state tw)) (> i 50))
                           tw
                           (do (Thread/sleep 100) (recur (inc i))))))]
              (is (= ["trc.node/tw" ["1"] "3"] (said tw)))
              (is (= [["trc.node/ta" ["1"] "2"]] (map said (calls (:call tw)))))))
          (testing "defined again, it is traced still"
            (eval! r "(defn ta [x] (+ x 2))")
            (eval! r "(ta 1)")
            (is (= ["trc.node/ta" ["1"] "3"] (said (last (calls))))))
          (testing "cleared, and let go of"
            (is (pos? (:cleared (ask {:op :trace-clear :id 3}))))
            (is (= 4 (count (:untraced (ask {:op :untrace :id 4})))))
            (is (= "3" (:value (frame-tagged (eval! r "(ta 1)") "ret"))))
            (is (= [] (calls))))
          (finally (disconnect c)))))))

(deftest test-a-recorded-clojurescript-definition-is-let-go-of-when-its-file-loads
  (when (compiling?)
    (let [dir (client/temp-dir)]
      (try
        (with-repl [r]
          (let [c (client/control-client @the-process)]
            (try
              (eval! r (a-file dir 'rec.loaded "(defn f [x] (inc x))"))
              (eval! r "#replique/ns rec.loaded\n(def y 1)")
              (eval! r (record-src 2 1 "(defn f [x] (inc x))"))
              (let [unit (:unit (await-event c "debug-instrumented"))]
                (eval! r (a-file dir 'rec.loaded "(defn f [x] (dec x))"))
                (is (= unit (:unit (await-event c "debug-uninstrumented")))))
              (finally (disconnect c)))))
        (finally (client/delete-recursively dir))))))
