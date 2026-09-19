(ns replique.repl-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [replique.core]
            [replique.output]
            [replique.test-client :as client
             :refer [connect send! recv request! disconnect with-process
                     control-client repl-client eval! recv-until
                     frame-tagged frames-tagged printed]]))

;;; Handshake

(deftest hello
  (with-process [info nil]
    (let [r (repl-client info)]
      (try
        (testing "the handshake is the same as any other connection"
          (is (= "reply" (:tag (:hello r))))
          (is (= "repl" (:role (:hello r))))
          (is (= (:process-id info) (:process-id (:hello r))))
          (is (string? (:connection (:hello r)))))
        (testing "and the repl says it is ready"
          (is (= "prompt" (:tag (:prompt r))))
          (is (= "user" (:ns (:prompt r))))
          (is (= (:connection (:hello r)) (:connection (:prompt r)))))
        (finally (disconnect r))))))

;;; Evaluating

(deftest a-form-produces-a-result
  (with-process [info nil]
    (let [r (repl-client info)]
      (try
        (let [frames (eval! r "(+ 1 2)")]
          (is (= ["ret" "prompt"] (mapv :tag frames)))
          (is (= "3" (:value (frame-tagged frames "ret"))))
          (is (= "user" (:ns (frame-tagged frames "ret")))))
        (testing "values are printed by the clojure printer, not transcoded"
          (let [frames (eval! r "{:a 1/2 :b 'sym}")]
            (is (= "{:a 1/2, :b sym}" (:value (frame-tagged frames "ret"))))))
        (finally (disconnect r))))))

(deftest output-is-framed
  (with-process [info nil]
    (let [r (repl-client info)]
      (try
        (testing "one frame per line printed"
          (let [frames (eval! r "(dotimes [i 3] (println i))")]
            (is (= ["out" "out" "out" "ret" "prompt"] (mapv :tag frames)))
            (is (= "0\n1\n2\n" (printed frames "out")))))
        (testing "stderr is told apart from stdout"
          (let [frames (eval! r "(binding [*out* *err*] (println \"oops\"))")]
            (is (= "oops\n" (printed frames "err")))
            (is (= "" (printed frames "out")))))
        (testing "output that never reached a newline is flushed before the result"
          (let [frames (eval! r "(do (print \"pending\") :done)")]
            (is (= ["out" "ret" "prompt"] (mapv :tag frames)))
            (is (= "pending" (printed frames "out")))
            (is (= ":done" (:value (frame-tagged frames "ret"))))))
        (finally (disconnect r))))))

(deftest exceptions-are-framed
  (with-process [info nil]
    (let [r (repl-client info)]
      (try
        (let [frames (eval! r "(/ 1 0)")
              f (frame-tagged frames "exception")]
          (is (= ["exception" "prompt"] (mapv :tag frames)))
          (testing "the message is the one a terminal repl would print"
            (is (string/starts-with? (:message f) "Execution error (ArithmeticException)"))
            (is (string/includes? (:message f) "Divide by zero")))
          (is (= "execution" (:phase f)))
          (is (= "java.lang.ArithmeticException" (:class (:exception f))))
          (is (= "Divide by zero" (:message (:exception f))))
          (is (seq (:trace (:exception f)))))
        (testing "a cause is carried along"
          (let [f (frame-tagged (eval! r "(throw (ex-info \"outer\" {:a 1} (Exception. \"inner\")))")
                                "exception")]
            (is (= "outer" (:message (:exception f))))
            (is (= "{:a 1}" (:data (:exception f))))
            (is (= "inner" (:message (:cause (:exception f)))))))
        (testing "the repl survives, and *e is bound"
          (let [frames (eval! r "(ex-message *e)")]
            (is (= "\"outer\"" (:value (frame-tagged frames "ret"))))))
        (testing "code that could not even be read is told apart from code
        that ran and threw"
          (let [f (frame-tagged (eval! r "(let [)") "exception")]
            (is (= "read-source" (:phase f)))))
        (finally (disconnect r))))))

(deftest what-an-exception-frame-leaves-out-is-said
  (testing "a frame carries a bounded piece of an exception - the top of the
  trace, the outermost causes. A client that is handed 64 frames of a 300
  frame trace, or a chain whose root cause was cut off, must be able to tell
  that from a whole one: the root cause is the one the reported message
  names."
    (with-process [info nil]
      (let [r (repl-client info)]
        (try
          (testing "a chain deeper than the frame carries"
            (let [f (frame-tagged
                     (eval! r (str "(throw (reduce (fn [c i] (ex-info (str \"level \" i) {} c))"
                                   " nil (range 20)))"))
                     "exception")]
              (is (string/includes? (:message f) "level 0")
                  "the reported message still names the root cause")
              (let [chain (take-while some? (iterate :cause (:exception f)))]
                (is (= 9 (count chain)))
                (is (true? (:cause-dropped (last chain)))
                    "the deepest one carried says the chain goes on")
                (is (every? nil? (map :cause-dropped (butlast chain)))))))
          (testing "a chain that fits says nothing"
            (let [f (frame-tagged (eval! r "(throw (ex-info \"one\" {} (Exception. \"two\")))")
                                  "exception")]
              (is (nil? (:cause-dropped (:exception f))))
              (is (nil? (:cause-dropped (:cause (:exception f)))))))
          (testing "a trace longer than the frame carries"
            (let [e (:exception (frame-tagged (eval! r "((fn f [n] (inc (f (inc n)))) 0)")
                                              "exception"))]
              (is (= 64 (count (:trace e))))
              (is (pos? (:trace-dropped e))
                  "how many frames were left out, so an editor can say so")))
          (testing "a trace that fits says nothing"
            (let [e (:exception (frame-tagged (eval! r "(throw (Exception. \"shallow\"))")
                                              "exception"))]
              (is (< (count (:trace e)) 64))
              (is (nil? (:trace-dropped e)))))
          (finally (disconnect r)))))))

(deftest an-ex-data-that-cannot-be-printed-does-not-replace-the-exception
  (testing "ex-data travels as printed text and printing a value can fail.
  Reporting that failure in its place shows the editor the trouble replique
  had describing the problem rather than the problem"
    (with-process [info nil]
      (let [r (repl-client info)]
        (try
          (eval! r (str "(def bad (reify Object (toString [_] "
                        "(throw (RuntimeException. (str \"cannot print me\"))))))"))
          (let [f (frame-tagged (eval! r "(throw (ex-info (str \"boom\") {:bad bad}))")
                                "exception")]
            (is (= "clojure.lang.ExceptionInfo" (:class (:exception f))))
            (is (= "boom" (:message (:exception f))))
            (is (string/includes? (:data (:exception f)) "Could not be printed")))
          (testing "and *e is what was thrown"
            (is (= "\"boom\"" (:value (frame-tagged (eval! r "(ex-message *e)") "ret")))))
          (finally (disconnect r)))))))

(deftest an-ex-data-that-never-ends-does-not-wedge-the-connection
  (testing "clojure.main prints no ex-data at all when it reports an
  exception, so printing it here must not turn an ordinary
  (ex-info \"...\" {:rows (map parse lines)}) into a repl that never answers"
    (with-process [info nil]
      (let [r (repl-client info)]
        (try
          (let [f (frame-tagged (eval! r "(throw (ex-info (str \"failed\") {:rows (range)}))")
                                "exception")]
            (is (= "failed" (:message (:exception f))))
            (testing "the printer marks what it left out"
              (is (string/includes? (:data (:exception f)) "..."))))
          (finally (disconnect r)))))))

(deftest printing-a-value-may-itself-print
  (testing "a print-method can say something while it prints, and where that
  goes is not the same for the two streams: ret-frame prints through pr-str,
  which binds *out* to a StringWriter, so what a print-method prints there
  ends up inside the value. *err* is left alone and reaches the connection -
  which is why the result frame is built before the output is flushed, so that
  such a warning comes out before the result it is about"
    (with-process [info nil]
      (let [r (repl-client info)]
        (try
          (eval! r "(defrecord Noisy [x])")
          (testing "what it prints on *out* lands inside the value"
            (eval! r (str "(defmethod print-method user.Noisy [v w] "
                          "(println \"printing\") (.write w \"<noisy>\"))"))
            (let [frames (eval! r "(->Noisy 1)")]
              (is (= "printing\n<noisy>" (:value (frame-tagged frames "ret"))))
              (is (empty? (frames-tagged frames "out")))))
          (testing "what it prints on *err* is a frame of its own, before the result"
            (eval! r (str "(defmethod print-method user.Noisy [v w] "
                          "(binding [*out* *err*] (println \"warning\")) "
                          "(.write w \"<noisy>\"))"))
            (let [frames (eval! r "(->Noisy 1)")
                  tags (mapv :tag frames)]
              (is (= "warning\n" (printed frames "err")))
              (is (= "<noisy>" (:value (frame-tagged frames "ret"))))
              (is (< (.indexOf tags "err") (.indexOf tags "ret")))))
          (finally (disconnect r)))))))

(deftest the-prompt-describes-the-repl
  (with-process [info nil]
    (let [r (repl-client info)]
      (try
        (testing "the namespace the next form will be read in"
          (eval! r "(ns replique.a-test-namespace)")
          (let [frames (eval! r "1")]
            (is (= "replique.a-test-namespace" (:ns (frame-tagged frames "prompt"))))
            (is (= "replique.a-test-namespace" (:ns (frame-tagged frames "ret"))))))
        (testing "and the printing the result went through"
          (let [frames (eval! r "(set! *print-length* 3)")]
            (is (= 3 (:print-length (:params (frame-tagged frames "prompt"))))))
          (let [frames (eval! r "(range 100)")]
            (is (= "(0 1 2 ...)" (:value (frame-tagged frames "ret"))))))
        (finally (disconnect r))))))

(deftest stdin-is-a-real-stream
  (testing "the repl reads from the socket itself, which is what makes
  (read-line), nested repls and debuggers work"
    (with-process [info nil]
      (let [r (repl-client info)]
        (try
          (send! r "(read-line)\nsome input")
          (let [frames (recv-until r "prompt")]
            (is (= "\"some input\"" (:value (frame-tagged frames "ret")))))
          (finally (disconnect r)))))))

(deftest several-forms-may-share-a-line
  (with-process [info nil]
    (let [r (repl-client info)]
      (try
        (send! r "(+ 1 1) (+ 2 2)")
        (is (= "2" (:value (frame-tagged (recv-until r "prompt") "ret"))))
        (is (= "4" (:value (frame-tagged (recv-until r "prompt") "ret"))))
        (finally (disconnect r))))))

(deftest a-form-may-span-several-lines
  (testing "unlike a control message: a repl reads code, not framed messages"
    (with-process [info nil]
      (let [r (repl-client info)]
        (try
          (send! r "(+ 1\n   2\n   3)")
          (let [frames (recv-until r "prompt")]
            (is (= "6" (:value (frame-tagged frames "ret")))))
          (finally (disconnect r)))))))

(defn- frames-of
  "The frames one send produces, delimited by a form of its own. The question
  here is how many prompts there are, so reading until the first one would be
  assuming the answer, and reading until the connection goes quiet would make
  the test a race."
  [r code]
  (send! r (str code "\n:replique-test-end"))
  ;; up to and including the prompt that ends the marker, so that nothing of
  ;; this send is left on the connection for the next one to read
  (loop [acc [] ended? false]
    (let [f (recv r)
          acc (conj acc [(:tag f) (:value f)])]
      (cond
        (and ended? (= "prompt" (:tag f))) acc
        (= ":replique-test-end" (:value f)) (recur acc true)
        :else (recur acc ended?)))))

(deftest a-blank-line-is-not-answered-with-a-prompt
  (testing "an editor delimits an evaluation by the prompt that ends it, and a
  blank line between two top level forms is what most files look like. Given a
  prompt of its own - which is what a terminal wants, where return on an empty
  line gives you a fresh one - it would end the first form twice and lose
  where the second began"
    (with-process [info nil]
      (let [r (repl-client info)
            end ["ret" ":replique-test-end"]
            one-each [["ret" "2"] ["prompt" nil] end ["prompt" nil]]]
        (try
          (testing "the shape a client counts on, with no blank line at all"
            (is (= one-each (frames-of r "(+ 1 1)"))))
          (testing "a blank line before the first form"
            (is (= one-each (frames-of r "\n(+ 1 1)"))))
          (testing "a blank line between the two"
            (is (= one-each (frames-of r "(+ 1 1)\n"))))
          (testing "and a line holding nothing but whitespace, which is the
          same thing once an editor has grabbed a region"
            (is (= one-each (frames-of r "(+ 1 1)\n   "))))
          (testing "a blank line on its own is read past rather than answered:
          it is not a form, and the prompt that follows a form has gone out
          already"
            (send! r "")
            (.setSoTimeout ^java.net.Socket (:socket r) 800)
            (is (nil? (try (recv r) (catch java.net.SocketTimeoutException _ nil))))
            (.setSoTimeout ^java.net.Socket (:socket r) 10000)
            (testing "and the repl is where it was, waiting for a form"
              (is (= "3" (:value (frame-tagged (eval! r "(+ 1 2)") "ret"))))))
          (finally (disconnect r)))))))

;;; Lifecycle inside the repl

(deftest output-does-not-pile-up-without-a-newline
  (testing "output is flushed into a frame per line, but a form that prints a
  lot without ever emitting one must not hold it all in memory"
    (with-process [info nil]
      (let [r (repl-client info)]
        (try
          (let [frames (eval! r "(dotimes [_ 5000] (print \"abcd\"))")
                out (frames-tagged frames "out")]
            (is (< 1 (count out)))
            (is (every? #(<= (count (:string %)) 8192) out))
            (is (= 20000 (count (printed frames "out")))))
          (finally (disconnect r)))))))

(deftest the-repl-can-be-quit
  (with-process [info nil]
    (let [r (repl-client info)]
      (try
        (send! r ":repl/quit")
        (is (= :eof (last (recv-until r "nothing-ever-has-this-tag"))))
        (finally (disconnect r))))))

;;; Source metadata

(deftest source-directive-places-the-code
  (with-process [info nil]
    (let [r (repl-client info)]
      (try
        (eval! r "#replique/src {:file \"/home/me/src/foo.clj\" :line 42}\n(defn foo [] 1)")
        (let [frames (eval! r "[(:file (meta #'foo)) (:line (meta #'foo))]")]
          (is (= "[\"/home/me/src/foo.clj\" 42]" (:value (frame-tagged frames "ret")))))
        (testing "the directive applies to the next form only"
          (let [frames (eval! r "*file*")]
            (is (= "\"NO_SOURCE_PATH\"" (:value (frame-tagged frames "ret"))))))
        (testing "a blank line between the directive and the form does not drop
        it - and is not a form either, so what comes back is the one prompt of
        the one form that was sent"
          (send! r "#replique/src {:file \"/home/me/src/bar.clj\" :line 7}\n\n(defn bar [] 1)")
          (recv-until r "prompt")
          (let [frames (eval! r "[(:file (meta #'bar)) (:line (meta #'bar))]")]
            (is (= "[\"/home/me/src/bar.clj\" 7]" (:value (frame-tagged frames "ret"))))))
        (testing "a stack trace points at the file the code came from"
          (eval! r "#replique/src {:file \"/home/me/src/boom.clj\" :line 12}\n(defn boom [] (/ 1 0))")
          (let [f (frame-tagged (eval! r "(boom)") "exception")]
            (is (some #(string/includes? % "boom.clj:12") (:trace (:exception f))))))
        (testing "the directive must be a map"
          (let [f (frame-tagged (eval! r "#replique/src 1") "exception")]
            (is (= "read-source" (:phase f)))
            (is (string/includes? (:message f) "#replique/src takes a map"))))
        (finally (disconnect r))))))

;;; The namespace

(deftest the-ns-directive-moves-the-repl
  (with-process [info nil]
    (let [r (repl-client info)]
      (try
        (send! r "#replique/ns clojure.string\n(ns-name *ns*)")
        (let [frames (recv-until r "prompt")]
          (is (= "clojure.string" (:value (frame-tagged frames "ret"))))
          (testing "and the prompt says where the repl now is, which is how the
          client knows without asking"
            (is (= "clojure.string" (:ns (frame-tagged frames "prompt"))))))
        (testing "it stays there - unlike the source directive it is not about
        one form. Switching to the repl after evaluating something lands at a
        prompt of the namespace that was being worked in"
          (is (= "clojure.string" (:value (frame-tagged (eval! r "(ns-name *ns*)")
                                                        "ret")))))
        (finally (disconnect r))))))

(deftest a-bare-ns-directive-is-answered-with-a-prompt
  (testing "a directive with no form after it is a client asking to be moved
  rather than to have something evaluated, and nothing else would say where
  the repl now is: the prompt of a form is what says that, and there is no
  form. The blank line is what ends a bare move, and this is the answer to it"
    (with-process [info nil]
      (let [r (repl-client info)]
        (try
          ;; Namespaces of this test's own: entering one makes it, and a
          ;; test that made clojure.set would be a test that decides what
          ;; another one finds
          (send! r "#replique/ns replique.test-bare-move\n")
          (let [frames (recv-until r "prompt")]
            (is (= ["prompt"] (mapv :tag frames)))
            (is (= "replique.test-bare-move" (:ns (frame-tagged frames "prompt")))))
          (testing "and one with a form after it is not - the form's own
          prompt says where the repl is, and two prompts for one evaluation
          is what a client cannot read"
            (send! r "#replique/ns replique.test-bare-move-2\n(+ 1 1)")
            (let [frames (recv-until r "prompt")]
              (is (= ["ret" "prompt"] (mapv :tag frames)))
              (is (= "2" (:value (frame-tagged frames "ret"))))
              (is (= "replique.test-bare-move-2"
                     (:ns (frame-tagged frames "prompt"))))))
          (testing "a blank line on its own is still nothing to answer: it is
          what lies between the forms of most files"
            (send! r "\n\n(+ 2 2)")
            (let [frames (recv-until r "prompt")]
              (is (= ["ret" "prompt"] (mapv :tag frames)))
              (is (= "4" (:value (frame-tagged frames "ret"))))))
          (finally (disconnect r)))))))

(deftest the-ns-directive-applies-before-the-form-is-read
  (testing "not before it is evaluated: ::keyword resolves against *ns* while
  reading, so a directive applied any later would read the form in the
  namespace being left, and the aliases of that one"
    (with-process [info nil]
      (let [r (repl-client info)]
        (try
          (eval! r "(ns replique.ns-test-target (:require [clojure.string :as s]))")
          (eval! r "(in-ns 'user)")
          (send! r "#replique/ns replique.ns-test-target\n[::here ::s/there]")
          (let [frames (recv-until r "prompt")]
            (is (= "[:replique.ns-test-target/here :clojure.string/there]"
                   (:value (frame-tagged frames "ret")))))
          (finally (disconnect r)))))))

(deftest the-ns-directive-creates-a-namespace-that-is-usable
  (testing "in-ns alone makes a namespace holding nothing at all, not even
  def. A repl that answered a buffer's namespace with one where defn does not
  resolve would be a repl making that buffer look broken"
    (with-process [info nil]
      (let [r (repl-client info)]
        (try
          (send! r "#replique/ns replique.ns-test-fresh\n(defn fresh [] (inc 1))")
          (recv-until r "prompt")
          (is (= "2" (:value (frame-tagged (eval! r "(fresh)") "ret"))))
          (testing "and a namespace that already exists is left as it is - what
          it refers is its own business, and a file that excluded something
          from clojure.core meant it"
            (eval! r (str "(ns replique.ns-test-excluding "
                          "(:refer-clojure :exclude [inc]))"))
            (eval! r "(in-ns 'user)")
            (send! r "#replique/ns replique.ns-test-excluding\n(resolve 'inc)")
            (is (= "nil" (:value (frame-tagged (recv-until r "prompt") "ret")))))
          (finally (disconnect r)))))))

(deftest a-namespace-that-is-only-a-file-is-made-empty-and-then-loaded
  (testing "the honest limit of creating a namespace that does not exist:
  one that exists only as a file on the classpath is made empty here, and
  what that file defines is not in it until the file has been loaded.
  Requiring it afterwards still loads it - require goes by what has been
  loaded, not by what namespaces exist - so the empty one is a namespace
  that heals rather than one that shadows the file for good"
    (with-process [info nil]
      (let [r (repl-client info)]
        (try
          (is (= "false" (:value (frame-tagged (eval! r "(some? (find-ns 'clojure.set))")
                                               "ret"))))
          (send! r "#replique/ns clojure.set\n(some? (resolve 'union))")
          (is (= "false" (:value (frame-tagged (recv-until r "prompt") "ret"))))
          (is (= "true" (:value (frame-tagged
                                 (eval! r (str "(do (require 'clojure.set) "
                                               "(some? (resolve 'clojure.set/union)))"))
                                 "ret"))))
          (finally (disconnect r)))))))

(deftest the-ns-directive-goes-with-a-source-directive
  (testing "the two are sent together for every form taken from a buffer, and
  neither takes the other back"
    (with-process [info nil]
      (let [r (repl-client info)]
        (try
          (send! r (str "#replique/ns replique.ns-test-both\n"
                        "#replique/src {:file \"/home/me/src/both.clj\" :line 30}\n"
                        "(defn both [] 1)"))
          (recv-until r "prompt")
          (let [frames (eval! r (str "[(ns-name *ns*) (:file (meta #'both)) "
                                     "(:line (meta #'both))]"))]
            (is (= "[replique.ns-test-both \"/home/me/src/both.clj\" 30]"
                   (:value (frame-tagged frames "ret")))))
          (testing "and in the other order, since a client may send either first"
            (send! r (str "#replique/src {:file \"/home/me/src/other.clj\" :line 8}\n"
                          "#replique/ns replique.ns-test-other\n"
                          "(defn other [] 1)"))
            (recv-until r "prompt")
            (let [frames (eval! r (str "[(ns-name *ns*) (:file (meta #'other)) "
                                       "(:line (meta #'other))]"))]
              (is (= "[replique.ns-test-other \"/home/me/src/other.clj\" 8]"
                     (:value (frame-tagged frames "ret"))))))
          (finally (disconnect r)))))))

(deftest an-ns-directive-that-cannot-be-used-says-why
  (testing "a namespace name is a symbol with no namespace of its own.
  Qualified ones are the mistake worth naming: it is what comes out of a
  client that took the symbol at point rather than the namespace around it"
    (with-process [info nil]
      (doseq [code ["#replique/ns foo/bar" "#replique/ns \"foo.bar\"" "#replique/ns 1"]]
        (let [r (repl-client info)]
          (try
            (send! r (str code "\n(ns-name *ns*)"))
            (let [f (frame-tagged (recv-until r "prompt") "exception")]
              (is (= "read-source" (:phase f)))
              (is (string/includes? (:message f)
                                    "#replique/ns takes an unqualified symbol")))
            (testing "and the repl has not moved"
              (is (= "user" (:value (frame-tagged (recv-until r "ret") "ret")))))
            (finally (disconnect r))))))))

;;; Interrupt

(defn- eval-in-background! [r code]
  (send! r code)
  ;; give the process the time to start evaluating
  (Thread/sleep 200))

(deftest interrupt-stops-an-evaluation
  (with-process [info nil]
    (let [ctrl (control-client info)
          r (repl-client info)
          repl-id (:connection (:hello r))]
      (try
        (eval-in-background! r "(do (Thread/sleep 60000) :never)")
        (let [reply (request! ctrl {:op :interrupt :connection repl-id :id 1})]
          (is (= true (:interrupted reply)))
          (is (= repl-id (:connection reply))))
        (let [f (frame-tagged (recv-until r "prompt") "exception")]
          (is (= "java.lang.InterruptedException" (:class (:exception f)))))
        (testing "and the repl is still usable"
          (is (= "2" (:value (frame-tagged (eval! r "(+ 1 1)") "ret")))))
        (finally (disconnect r) (disconnect ctrl))))))

(deftest interrupting-an-idle-repl-does-nothing
  (testing "reading is not interruptible: interrupting a repl that waits for
  the next form would break the connection rather than an evaluation"
    (with-process [info nil]
      (let [ctrl (control-client info)
            r (repl-client info)
            repl-id (:connection (:hello r))]
        (try
          (is (= false (:interrupted (request! ctrl {:op :interrupt :connection repl-id :id 1}))))
          (is (= false (:interrupted (request! ctrl {:op :interrupt :connection repl-id :id 2}))))
          (testing "and no interrupt is left over for the next evaluation"
            (is (= "2" (:value (frame-tagged (eval! r "(+ 1 1)") "ret"))))
            (is (= "false" (:value (frame-tagged (eval! r "(.isInterrupted (Thread/currentThread))")
                                                 "ret")))))
          (finally (disconnect r) (disconnect ctrl)))))))

(deftest repls-are-interrupted-one-at-a-time
  (testing "the bookkeeping :interrupt needs is per connection, so recording
  an evaluation never makes another repl wait, and interrupting one repl
  leaves the others alone"
    (with-process [info nil]
      (let [ctrl (control-client info)
            r1 (repl-client info)
            r2 (repl-client info)]
        (try
          (eval-in-background! r1 "(do (Thread/sleep 60000) :never)")
          (testing "another repl evaluates while the first one is busy"
            (is (= "2" (:value (frame-tagged (eval! r2 "(+ 1 1)") "ret")))))
          (testing "and one that is evaluating is not disturbed by its
          neighbour being interrupted"
            (send! r2 "(do (Thread/sleep 1500) :finished)")
            (Thread/sleep 300)
            (is (= true (:interrupted (request! ctrl {:op :interrupt
                                                      :connection (:connection (:hello r1))
                                                      :id 1}))))
            (let [f (frame-tagged (recv-until r1 "prompt") "exception")]
              (is (= "java.lang.InterruptedException" (:class (:exception f)))))
            (let [frames (recv-until r2 "prompt")]
              (is (= ":finished" (:value (frame-tagged frames "ret"))))
              (is (nil? (frame-tagged frames "exception")))))
          (finally (disconnect r1) (disconnect r2) (disconnect ctrl)))))))

(deftest a-source-directive-that-cannot-be-used-says-why
  (testing "the client is the editor being written against this protocol. A
  directive it got wrong must name what is wrong with it, rather than fail
  later inside replique with a cast error pointing at replique's own code"
    (with-process [info nil]
      (doseq [[code expected]
              [["#replique/src 1" "#replique/src takes a map"]
               ["#replique/src {:file 42}" ":file must be a string"]
               ["#replique/src {:line :nope}" ":line must be an integer"]
               ;; The range, not only the type. A line number ends up in
               ;; LineNumberingPushbackReader.setLineNumber, which takes an
               ;; int, so a client that counted lines into a long used to get
               ;; an integer overflow raised from inside clojure - reported as
               ;; an execution failure, which is not what it is, and naming
               ;; replique.repl rather than the key it got wrong
               ["#replique/src {:line 2147483648}"
                ":line must be an integer between 1 and 2147483647"]
               ["#replique/src {:line 99999999999999999999}"
                ":line must be an integer between 1 and 2147483647"]
               ;; and the same rule at the other end: an editor counts lines
               ;; from 1, and these used to be written into the metadata of
               ;; the var they name as if they were a place in a file
               ["#replique/src {:line 0}"
                ":line must be an integer between 1 and 2147483647"]
               ["#replique/src {:line -5}"
                ":line must be an integer between 1 and 2147483647"]]]
        (let [r (repl-client info)]
          (try
            (send! r (str code "\n(+ 1 1)"))
            (let [f (frame-tagged (recv-until r "prompt") "exception")]
              (is (= "read-source" (:phase f)))
              (is (string/includes? (:message f) expected)))
            (testing "and the form that followed it is still evaluated"
              (is (= "2" (:value (frame-tagged (recv-until r "ret") "ret")))))
            (finally (disconnect r))))))))

(deftest an-evaluation-outlives-the-client-and-stays-interruptible
  (testing "closing a repl buffer must not leave a runaway evaluation that
  nothing can reach. The connection stays registered until its evaluation
  ends, so :interrupt can still name it, and it is cleaned up afterwards"
    (with-process [info nil]
      (let [ctrl (control-client info)
            r (repl-client info)
            id (:connection (:hello r))]
        (try
          (eval-in-background! r "(do (Thread/sleep 30000) :never)")
          (disconnect r)
          (Thread/sleep 300)
          (is (some? (get (replique.state/connections) id)))
          (is (= true (:interrupted (request! ctrl {:op :interrupt :connection id :id 1}))))
          (is (loop [n 0]
                (cond
                  (nil? (get (replique.state/connections) id)) true
                  (< n 100) (do (Thread/sleep 20) (recur (inc n)))
                  :else false)))
          (finally (disconnect ctrl)))))))

(deftest interrupt-needs-a-repl-connection
  (with-process [info nil]
    (let [ctrl (control-client info)]
      (try
        (testing "a connection that does not exist"
          (is (= "unknown-connection" (:error (request! ctrl {:op :interrupt :connection "nope" :id 1})))))
        (testing "a control connection has nothing to interrupt"
          (let [reply (request! ctrl {:op :interrupt :connection (:connection (:hello ctrl)) :id 2})]
            (is (= "not-a-repl" (:error reply)))))
        (testing "no connection at all"
          (is (= "invalid-message" (:error (request! ctrl {:op :interrupt :id 3})))))
        (finally (disconnect ctrl))))))

;;; What the process prints on its own

(deftest process-output-reaches-the-control-connections
  (testing "output produced outside of a repl belongs to no repl - it is
  broadcast to the editor as an event"
    (with-process [info nil]
      (let [ctrl (control-client info)
            r (repl-client info)]
        (try
          (eval! r "(.println System/out \"from java\")")
          (let [event (recv ctrl)]
            (is (= "event" (:tag event)))
            (is (= "out" (:event event)))
            (is (= "from java\n" (:string event))))
          (testing "including what a thread of the application prints - a
          future conveys the bindings of the repl that started it, a plain
          thread prints where the process prints"
            (eval! r "(doto (Thread. (fn [] (println \"from a thread\"))) (.start) (.join))")
            (let [event (recv ctrl)]
              (is (= "out" (:event event)))
              (is (= "from a thread\n" (:string event)))))
          (testing "and stderr"
            (eval! r "(.println System/err \"bad news\")")
            (let [event (recv ctrl)]
              (is (= "err" (:event event)))
              (is (= "bad news\n" (:string event)))))
          (testing "but what the repl printed stays on the repl connection"
            (let [frames (eval! r "(println \"mine\")")]
              (is (= "mine\n" (printed frames "out")))))
          (finally (disconnect r) (disconnect ctrl)))))))

(deftest an-event-never-overtakes-the-handshake-reply
  (testing "a connection only counts as a control connection once its :hello
  reply is out. Recording it earlier means a process that emits events while
  a client connects answers that client's handshake with an event, and every
  client reads the frame after :hello as its reply"
    (with-process [info nil]
      (let [r (repl-client info)]
        (try
          (eval! r (str "(require '[replique.output :as output] "
                        "'[replique.protocol :as protocol])"))
          (eval! r "(def stop (atom false))")
          (eval! r (str "(.start (Thread. (fn [] (while (not @stop) "
                        "(output/broadcast-event! (protocol/event \"noise\" {}))))))"))
          (let [tags (doall (for [_ (range 100)]
                              (let [client (connect info)]
                                (try (:tag (request! client {:op :hello :role :control :id 1}))
                                     (finally (disconnect client))))))]
            (is (= {"reply" 100} (frequencies tags))))
          (finally
            (eval! r "(reset! stop true)")
            (disconnect r)))))))

(deftest a-surrogate-pair-is-never-split-across-frames
  (testing "clojure prints a string one char at a time, so a long string
  holding an emoji reaches the output buffer's size limit between the two
  halves of a surrogate pair. Each half alone is not valid text and would go
  out as U+FFFD at both ends of the split"
    (with-process [info nil]
      (let [r (repl-client info)]
        (try
          (eval! r (str "(def s (str (apply str (repeat 8191 (char 97))) "
                        "(str (char 0xD83D) (char 0xDE00)) (str (char 33))))"))
          (let [frames (eval! r "(do (doseq [ch s] (.write *out* (int ch))) (flush) :done)")
                out (printed frames "out")]
            (is (< 1 (count (frames-tagged frames "out"))))
            (is (= 8194 (count out)))
            (testing "the pair survives the frame boundary"
              (is (= (str (char 0xD83D) (char 0xDE00)) (subs out 8191 8193)))))
          (finally (disconnect r)))))))

(deftest process-output-is-not-limited-by-the-terminal-encoding
  (testing "the tee encodes in UTF-8 whatever stdout.encoding is. An unset
  locale gives US-ASCII, and decoding what the terminal received would report
  every accent and every emoji to the editor as a question mark"
    (with-process [info nil]
      (let [ctrl (control-client info)
            r (repl-client info)]
        (try
          (eval! r (str "(.println System/out (str (char 233) (char 0xD83D) (char 0xDE00)))"))
          (let [event (recv ctrl)]
            (is (= "out" (:event event)))
            (is (= (str (char 233) (char 0xD83D) (char 0xDE00) "\n") (:string event))))
          (finally (disconnect r) (disconnect ctrl)))))))

(defn- out-text
  "The out events, read until they add up to n characters."
  [client n]
  (loop [s ""]
    (if (<= n (count s))
      s
      (let [f (recv client)]
        (if (= :eof f)
          s
          (recur (if (= "out" (:event f)) (str s (:string f)) s)))))))

(deftest a-character-split-across-two-writes-is-not-broken-in-half
  (testing "a write is whatever chunk the caller happened to hold - io/copy
  hands over its buffer - so a character straddles two of them regularly.
  Decoded a write at a time it is two broken halves rather than one
  character, and the terminal shows U+FFFD where the process printed a
  character. The stream replique replaced passes those same two writes
  through unchanged, so the tee must not be where the character is lost"
    (let [text (str "w" (char 0xF6) "rld " (char 0x2713))
          bs (.getBytes text "UTF-8")
          cut 2                         ; w, and the first byte of the o umlaut
          split! (fn [^java.io.OutputStream o]
                   (.write o bs 0 cut)
                   (.write o bs cut (- (alength bs) cut))
                   (.flush o))
          through (fn [write!]
                    (let [sink (java.io.ByteArrayOutputStream.)
                          original (java.io.PrintStream. sink true "UTF-8")]
                      (write! original)
                      (.toString sink "UTF-8")))]
      (testing "the stream the tee replaced shows the character"
        (is (= text (through split!))))
      (testing "and so does the tee"
        (is (= text (through (fn [original]
                               (split! (#'replique.output/tee-stream original "out"))))))))))

(deftest closing-the-tee-reports-the-character-that-was-cut-off
  (testing "holding back the beginning of a character is what makes the one
  that straddles two writes whole, and it is right up to the point where
  nothing more is coming. A flush is not that point - only the bytes that
  complete the character are - so what a flush leaves behind must survive it.
  A close is that point: what is held back then is a broken character rather
  than an unfinished one, and it is reported as broken rather than dropped"
    (let [sink (java.io.ByteArrayOutputStream.)
          original (java.io.PrintStream. sink true "UTF-8")
          tee (#'replique.output/tee-stream original "out")
          ;; the first byte of a two byte character, and nothing after it
          truncated (byte-array [(unchecked-byte 0xC3)])]
      (.write tee truncated 0 1)
      (.flush tee)
      (is (= "" (.toString sink "UTF-8"))
          "a flush does not finish a character, so it does not give up on one")
      (.close tee)
      (is (= (str (char 0xFFFD)) (.toString sink "UTF-8"))))))

(deftest output-written-in-chunks-reaches-the-editor-whole
  (testing "the same thing seen from where it matters: a stream copied to
  stdout arrives at the editor as the text that was printed, and not with a
  U+FFFD wherever a character fell across a buffer boundary"
    (with-process [info nil]
      (let [ctrl (control-client info)
            r (repl-client info)
            text (str "w" (char 0xF6) "rld " (char 0x2713) " na" (char 0xEF)
                      "ve caf" (char 0xE9) "\n")]
        (try
          (eval! r (str "(do (require (quote clojure.java.io))"
                        "    (clojure.java.io/copy"
                        "      (java.io.ByteArrayInputStream. (.getBytes " (pr-str text) " \"UTF-8\"))"
                        "      System/out :buffer-size 8)"
                        "    (.flush System/out) :done)"))
          (is (= text (out-text ctrl (count text))))
          (finally (disconnect r) (disconnect ctrl)))))))

(deftest the-writer-out-is-rebound-to-encodes-utf8-whatever-it-wraps
  (testing "*out* and *err* are rebound to a writer over the tee, and what
  that writer encodes in has to be replique's answer rather than the
  stream's: the tee is UTF-8 by construction, and reading the charset back
  off a PrintStream is a jdk 18 method - a java version replique would then
  require without checking for it or saying so"
    (let [sink (java.io.ByteArrayOutputStream.)
          ;; a stream that says it is US-ASCII, the way an unset locale makes
          ;; the real one say it
          ascii (java.io.PrintStream. sink true java.nio.charset.StandardCharsets/US_ASCII)
          w (#'replique.output/print-writer ascii)
          text (str "caf" (char 233))]
      (.write w text)
      (.flush w)
      (is (= text (String. (.toByteArray sink) "UTF-8"))))))

(deftest uncaught-exceptions-reach-the-control-connections
  (with-process [info nil]
    (let [ctrl (control-client info)
          r (repl-client info)]
      (try
        (eval! r "(.start (Thread. (fn [] (throw (ex-info \"boom\" {}))) \"a-doomed-thread\"))")
        (let [event (first (filter #(= "uncaught-exception" (:event %))
                                   (recv-until ctrl "event")))]
          (is (= "a-doomed-thread" (:thread event)))
          (is (= "boom" (:message event)))
          (is (= "clojure.lang.ExceptionInfo" (:class (:exception event)))))
        (finally (disconnect r) (disconnect ctrl))))))

(deftest stopping-the-process-puts-the-streams-back
  (let [out System/out
        err System/err
        dir (client/temp-dir)]
    (try
      (client/with-process [_info nil]
        (is (not (identical? out System/out)))
        (is (not (identical? err System/err))))
      (is (identical? out System/out))
      (is (identical? err System/err))
      (finally (client/delete-recursively dir)))))

(deftest stopping-the-process-closes-the-tee
  (testing "putting the streams back is a close and not only a flush: what a
  flush leaves behind is the beginning of a character whose remaining bytes
  are never coming, and only the close reports it. The streams are put back
  before the connections are closed, so the last thing the process has to say
  still reaches the editor"
    (let [dir (client/temp-dir)
          out *out*
          err *err*
          info (replique.core/start! {:directory dir})
          ctrl (control-client info)]
      (try
        (binding [*out* out *err* err]
          ;; the first byte of a two byte character, written straight to the
          ;; tee - it is held back, waiting for the byte that completes it
          (.write System/out (byte-array [(unchecked-byte 0xC3)]) 0 1)
          (.flush System/out)
          (replique.core/stop!)
          (let [broken (str (char 0xFFFD))
                event (->> (recv-until ctrl "event")
                           (filter #(= "out" (:event %)))
                           first)]
            (is (= broken (:string event)))))
        (finally (replique.core/stop!)
                 (disconnect ctrl)
                 (client/delete-recursively dir))))))


;;; Loading

(defn- write-file!
  "Write SOURCE into DIR under NAME, and answer the path of it."
  [dir name source]
  (let [f (java.io.File. (str dir) (str name))]
    (spit f source)
    (.getPath f)))

(defn- write-jar!
  "Write SOURCE into a jar in DIR, at ENTRY, and answer the path of the jar."
  [dir name entry source]
  (let [f (java.io.File. (str dir) (str name))]
    (with-open [out (java.util.jar.JarOutputStream. (java.io.FileOutputStream. f))]
      (.putNextEntry out (java.util.jar.JarEntry. (str entry)))
      (.write out (.getBytes (str source) "UTF-8"))
      (.closeEntry out))
    (.getPath f)))

(defn- loaded!
  "Send a load directive for M and read everything it produces."
  [r m]
  (eval! r (str "#replique/load " (pr-str m))))

(deftest the-load-directive-loads-a-file
  (testing "evaluating a buffer form by form is not the same as loading the
  file those forms are in: a file is loaded as one unit, which is how the
  compiler and the application will see it"
    (with-process [info nil]
      (let [dir (client/temp-dir)
            r (repl-client info)]
        (try
          (let [path (write-file! dir "probe_loaded.clj"
                                  (str "(ns probe.loaded)\n"
                                       "(println \"loading\")\n"
                                       "(defn answer [] 42)\n"))
                frames (loaded! r {:file path})]
            (testing "what the file printed is this repl's output, in order with
            the result - which is why loading is asked for here rather than as
            an op, where it would reach every control connection as something
            the application happened to print"
              (is (= "loading\n" (printed frames "out"))))
            (testing "and one prompt follows it, as after any other form"
              (is (= 1 (count (frames-tagged frames "prompt")))))
            (testing "what it defined is in the process"
              (is (= "42" (:value (frame-tagged (eval! r "(probe.loaded/answer)") "ret"))))))
          (finally (disconnect r) (client/delete-recursively dir)))))))

(deftest a-file-is-loaded-under-the-require-lock
  (testing "the lock clojure's own serialized-require takes. Nothing in
  clojure.core takes it for a plain require - a process is assumed to have one
  repl in it - and replique accepting as many repl connections as an editor
  opens is exactly what makes that not enough"
    (with-process [info nil]
      (let [dir (client/temp-dir)
            r (repl-client info)]
        (try
          (let [path (write-file!
                      dir "probe_locked.clj"
                      (str "(ns probe.locked)\n"
                           "(def held (Thread/holdsLock clojure.lang.RT/REQUIRE_LOCK))\n"))]
            (loaded! r {:file path})
            (is (= "true" (:value (frame-tagged (eval! r "probe.locked/held") "ret"))))
            (testing "and it is let go of afterwards: a lock held for as long as
            an arbitrary form takes to run would stop every requiring-resolve in
            the process"
              (is (= "false"
                     (:value (frame-tagged
                              (eval! r "(Thread/holdsLock clojure.lang.RT/REQUIRE_LOCK)")
                              "ret"))))))
          (finally (disconnect r) (client/delete-recursively dir)))))))

(deftest the-load-directive-loads-a-jar-entry
  (testing "a namespace read out of a dependency is an entry inside a jar, and
  jumping into one and loading what is there is most of the point of being able
  to jump into one. The entry travels beside the jar, which is how a file
  inside a jar is written everywhere else in this protocol"
    (with-process [info nil]
      (let [dir (client/temp-dir)
            r (repl-client info)]
        (try
          (let [jar (write-jar! dir "probe.jar" "probe/jarred.clj"
                                (str "(ns probe.jarred)\n"
                                     "(def where *file*)\n"
                                     "(defn answer [] :from-a-jar)\n"))]
            (loaded! r {:file jar :entry "probe/jarred.clj"})
            (is (= ":from-a-jar"
                   (:value (frame-tagged (eval! r "(probe.jarred/answer)") "ret"))))
            (testing "loaded as the entry rather than as the jar: the inner path
            is what a stack trace shows and what the code reads as *file*"
              (is (= "\"probe/jarred.clj\""
                     (:value (frame-tagged (eval! r "probe.jarred/where") "ret")))))
            (testing "and an entry the jar does not hold is said to be missing
            by name, rather than reported as though the jar were what could not
            be found"
              (let [f (frame-tagged (loaded! r {:file jar :entry "probe/nope.clj"})
                                    "exception")]
                (is (string/includes? (:message f) "No entry probe/nope.clj")))))
          (finally (disconnect r) (client/delete-recursively dir)))))))

(deftest a-path-is-a-path-rather-than-a-url
  (testing "a jar under a directory with a space in its name is a %20 in a url
  and is not in a path, and the escaping a url would need is escaping neither
  side has to get right"
    (with-process [info nil]
      (let [dir (str (client/temp-dir) java.io.File/separator "a directory")
            r (repl-client info)]
        (try
          (.mkdirs (java.io.File. dir))
          (let [path (write-file! dir "probe_spaced.clj"
                                  "(ns probe.spaced)\n(defn answer [] :spaced)\n")]
            (is (string/includes? path " "))
            (loaded! r {:file path})
            (is (= ":spaced" (:value (frame-tagged (eval! r "(probe.spaced/answer)") "ret"))))
            (testing "and a jar under it too"
              (let [jar (write-jar! dir "probe spaced.jar" "probe/spaced_jar.clj"
                                    "(ns probe.spaced-jar)\n(defn answer [] :spaced-jar)\n")]
                (loaded! r {:file jar :entry "probe/spaced_jar.clj"})
                (is (= ":spaced-jar"
                       (:value (frame-tagged (eval! r "(probe.spaced-jar/answer)") "ret")))))))
          (finally (disconnect r) (client/delete-recursively dir)))))))

(deftest a-file-that-throws-is-this-repl-s-exception
  (testing "which is the other half of why loading is asked for here: it comes
  back as this repl's exception frame, with the phase clojure.main triages and
  a trace pointing into the file"
    (with-process [info nil]
      (let [dir (client/temp-dir)
            r (repl-client info)]
        (try
          (let [path (write-file! dir "probe_boom.clj"
                                  (str "(ns probe.boom)\n"
                                       "(defn answer [] 1)\n"
                                       "(/ 1 0)\n"))
                frames (loaded! r {:file path})
                f (frame-tagged frames "exception")]
            (is (some? f))
            (is (= "execution" (:phase f)))
            (testing "the message is the one a terminal repl would print, and it
            names the line of the file that threw - which is what an editor
            shows, and what it can put point at"
              (is (string/includes? (:message f) "Divide by zero"))
              (is (string/includes? (:message f) "probe_boom.clj:3")))
            (testing "and the trace points into the file rather than into the
            loading"
              (is (some #(string/includes? % "probe_boom.clj:3")
                        (get-in f [:exception :cause :trace]))))
            (testing "and the repl is still usable"
              (is (= "2" (:value (frame-tagged (eval! r "(+ 1 1)") "ret"))))))
          (finally (disconnect r) (client/delete-recursively dir)))))))

(deftest a-load-is-answered-with-nothing
  (testing "what a load leaves behind is the namespace it defined, and the
  value the reading it went through hands back is an accident of that
  reading - the last form of the file from one, the path it was found under
  from another.  A repl prints that value, so the answer to a load has to be
  the same sentence however the file was read"
    (with-process [info nil]
      (let [dir (client/temp-dir)
            r (repl-client info)]
        (try
          (let [path (write-file! dir "probe_answered.clj"
                                  "(ns probe.answered)\n(def answer 42)\n")]
            (is (= "nil" (:value (frame-tagged (loaded! r {:file path}) "ret"))))
            (testing "and the file was loaded all the same"
              (is (= "42" (:value (frame-tagged (eval! r "probe.answered/answer")
                                                "ret"))))))
          (finally (disconnect r) (client/delete-recursively dir)))))))

(deftest a-load-directive-that-cannot-be-used-says-why
  (with-process [info nil]
    (let [r (repl-client info)
          refused (fn [code] (:message (frame-tagged (eval! r code) "exception")))]
      (try
        (testing "it takes a map - and is refused as the directive it is,
        rather than as the type of a hash map, which is what asking a number
        what it contains would report"
          (let [f (frame-tagged (eval! r "#replique/load 1") "exception")]
            (is (= "read-source" (:phase f)))
            (is (string/includes? (:message f) "#replique/load takes a map"))))
        (testing "naming something to load, spelt as a string"
          (is (string/includes? (refused "#replique/load {}") "the :file to load"))
          (is (string/includes? (refused "#replique/load {:file 1}") "the :file to load")))
        (testing "and an entry, where there is one, spelt as a string too"
          (is (string/includes? (refused "#replique/load {:file \"a.jar\" :entry 1}")
                                ":entry of a #replique/load must be a string"))
          (testing "while none at all is a file, which is what an editor holds
          for code that is not inside a jar"
            (is (string/includes? (refused "#replique/load {:file \"no_such_file.clj\"}")
                                  "no_such_file.clj"))))
        (testing "the repl survives all of it"
          (is (= "2" (:value (frame-tagged (eval! r "(+ 1 1)") "ret")))))
        (finally (disconnect r))))))

(deftest a-reload-directive-that-cannot-be-used-says-why
  (with-process [info nil]
    (let [r (repl-client info)
          refused (fn [code] (:message (frame-tagged (eval! r code) "exception")))]
      (try
        (testing "it takes a map, like the load it is one of"
          (let [f (frame-tagged (eval! r "#replique/reload 1") "exception")]
            (is (= "read-source" (:phase f)))
            (is (string/includes? (:message f) "#replique/reload takes a map"))))
        (testing "and nothing in it yet.  A key is a client asking for
        something this does not do, and quietly doing the other thing is
        worse than saying that it does not"
          (is (string/includes? (refused "#replique/reload {:prune true}")
                                "takes nothing in its map yet")))
        (testing "a source directive above one is dropped, the way it is above
        a load: what this reads is files, each from where it is, so the
        directive was about a form that never came - and left pending it would
        place the next form the client sends in a file it has nothing to do
        with"
          (send! r (str "#replique/src {:file \"/home/me/elsewhere.clj\" :line 99}\n"
                        "#replique/reload {}"))
          (recv-until r "prompt")
          (is (= "\"NO_SOURCE_PATH\"" (:value (frame-tagged (eval! r "*file*") "ret")))))
        (testing "the repl survives all of it"
          (is (= "2" (:value (frame-tagged (eval! r "(+ 1 1)") "ret")))))
        (finally (disconnect r))))))

(deftest a-source-directive-above-a-load-is-dropped
  (testing "where the code comes from is the file's own business - the
  compiler reads it from there - so a #replique/src above a load was about a
  form that never came, and leaving it pending would place the next form the
  client sends at a line of a file it has nothing to do with"
    (with-process [info nil]
      (let [dir (client/temp-dir)
            r (repl-client info)]
        (try
          (let [path (write-file! dir "probe_placed.clj"
                                  "(ns probe.placed)\n(defn answer [] 1)\n")]
            (send! r (str "#replique/src {:file \"/home/me/elsewhere.clj\" :line 99}\n"
                          "#replique/load " (pr-str {:file path})))
            (recv-until r "prompt")
            (testing "the very next form, which is the one a directive left
            pending would have been applied to"
              (is (= "\"NO_SOURCE_PATH\""
                     (:value (frame-tagged (eval! r "*file*") "ret")))))
            (testing "while the file placed its own definitions, from where it is"
              (is (= (str "\"" path "\"")
                     (:value (frame-tagged (eval! r "(:file (meta #'probe.placed/answer))")
                                           "ret"))))))
          (finally (disconnect r) (client/delete-recursively dir)))))))

(deftest a-namespace-directive-above-a-load-is-answered-by-the-load
  (testing "a bare #replique/ns is answered with a prompt of its own, since
  nothing else would say where the repl went. One with something after it is
  not: the form's own prompt says that, and two prompts for one evaluation is
  what a client cannot read"
    (with-process [info nil]
      (let [dir (client/temp-dir)
            r (repl-client info)]
        (try
          (let [path (write-file! dir "probe_after_ns.clj"
                                  "(ns probe.after-ns)\n(defn answer [] 1)\n")]
            ;; With the blank line a file has between two top level forms,
            ;; which is what makes this reachable: a blank line is what a bare
            ;; move is acknowledged by, so one arriving after a load that was
            ;; already answered is what a directive left un-acknowledged turns
            ;; into a second prompt.
            (send! r (str "#replique/ns clojure.string\n"
                          "#replique/load " (pr-str {:file path}) "\n"
                          "\n"
                          "(ns-name *ns*)"))
            (let [frames (recv-until r "prompt")]
              (is (= 1 (count (frames-tagged frames "prompt"))))
              (testing "and the load was answered by its own prompt, not by the
              directive's"
                (is (= 1 (count (frames-tagged frames "ret"))))))
            (testing "so the form after the blank line is the next thing
            answered, and the repl is where the directive put it"
              (let [frames (recv-until r "prompt")]
                (is (= "clojure.string" (:value (frame-tagged frames "ret"))))
                (is (= 1 (count (frames-tagged frames "prompt")))))))
          (finally (disconnect r) (client/delete-recursively dir)))))))

;;; Lifecycle

(deftest the-repl-ends-when-the-client-goes
  (with-process [info nil]
    (let [r (repl-client info)
          id (:connection (:hello r))]
      (eval! r "(+ 1 1)")
      (disconnect r)
      (let [gone? (loop [n 0]
                    (cond
                      (nil? (get (replique.state/connections) id)) true
                      (< n 100) (do (Thread/sleep 20) (recur (inc n)))
                      :else false))]
        (is gone?)))))
