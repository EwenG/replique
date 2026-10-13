(ns replique.debug-test
  "A thread stopped by `replique.debug/break!', and what a client does to it.

  In a process of its own, started for it: stopping needs the JDWP agent, which
  is a flag of the jvm - and the tests run in a jvm that was not started with
  it, which is the other thing tested here."
  (:require [clojure.data.json :as djson]
            [clojure.test :refer [deftest is testing]]
            [replique.debug :as debug]
            [replique.test-client :as client
             :refer [connect delete-recursively disconnect eval! recv recv-until
                     send! temp-dir]])
  (:import [java.io BufferedReader InputStreamReader]
           [java.net SocketTimeoutException]
           [java.nio.charset StandardCharsets]
           [java.util.concurrent TimeUnit]))

;;; A process started with the agent

(defn- start-debuggable!
  "Start a replique process with the JDWP agent, in DIR, on the classpath of
  this one. Returns {:process :info}."
  [dir]
  (let [java (str (System/getProperty "java.home") "/bin/java")
        process (.start (doto (ProcessBuilder.
                               ^"[Ljava.lang.String;"
                               (into-array String
                                           [java
                                            "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=127.0.0.1:0"
                                            "-cp" (System/getProperty "java.class.path")
                                            "clojure.main" "-m" "replique.main"
                                            (pr-str {:directory dir :init false})]))
                          (.redirectError java.lang.ProcessBuilder$Redirect/INHERIT)))
        out (BufferedReader. (InputStreamReader. (.getInputStream process) StandardCharsets/UTF_8))]
    ;; The agent says where it listens before the process says anything
    (loop []
      (let [line (.readLine out)]
        (cond
          (nil? line) (throw (ex-info "The process did not start" {}))
          (.startsWith line "{")
          (let [info (djson/read-str line :key-fn keyword)]
            (when-not (= "started" (:tag info))
              (throw (ex-info "The process did not start" info)))
            ;; whatever else it prints must not fill the pipe
            (future (try (while (.readLine out)) (catch Exception _)))
            {:process process :info info})
          :else (recur))))))

(defmacro ^:private with-debuggable [[info-sym] & body]
  `(let [dir# (temp-dir)
         {process# :process ~info-sym :info} (start-debuggable! dir#)]
     (try ~@body
          (finally
            (.destroy ^Process process#)
            (.waitFor ^Process process# 10 TimeUnit/SECONDS)
            (delete-recursively dir#)))))

;;; Talking to it

(defn- control [info]
  (let [c (connect info 60000)]
    (client/request! c {:op :hello :role :control :id 0})
    c))

(defn- next-frame [c]
  (let [f (recv c)]
    (when (= :eof f) (throw (ex-info "The connection closed" {})))
    f))

(defn- ask!
  "Send MSG and return its answer, past any event."
  [c msg]
  (send! c msg)
  (loop []
    (let [f (next-frame c)]
      (if (= "event" (:tag f)) (recur) f))))

(defn- await-event
  "Read until the event NAME, and return it."
  [c name]
  (loop []
    (let [f (next-frame c)]
      (if (and (= "event" (:tag f)) (= name (:event f))) f (recur)))))

(defn- eval-in!
  "Run CODE in a frame of THREAD, and return the event that says what came of
  it - after the reply that numbered it, which comes first."
  [c thread frame code]
  (let [reply (ask! c {:op :debug-eval :thread thread :frame frame :code code :id 100})
        said (await-event c "debug-evaluated")]
    (is (= "reply" (:tag reply)))
    (is (= (:evaluation reply) (:evaluation said)))
    said))

(defn- ret [r code]
  (:value (client/frame-tagged (eval! r code) "ret")))

(defn- keyed [children k]
  (:value (first (filter #(= k (:key %)) children))))

(def ^:private program
  "(do
     (defn g [x] (* 10 x))
     (defn f [n]
       (let [a (inc n)
             b (g a)]
         (replique.debug/break!)
         (+ a b)))
     (defn h [{:keys [n] :as m}] (when-let [k n] (f k))))")

(deftest a-thread-stops-and-is-worked-on
  (with-debuggable [info]
    (let [r (client/repl-client info nil 60000)
          c (control info)]
      (try
        (testing "locals are cleared until asked to be kept, for the code compiled after"
          (is (= {:clear true} (select-keys (ask! c {:op :locals-clearing :id 20}) [:clear])))
          (is (true? (:debugger (ask! c {:op :process-info :id 21}))))
          (ret r "(defn cleared [x] (replique.debug/break!) x)")
          (is (= {:clear false} (select-keys (ask! c {:op :locals-clearing :clear false :id 22})
                                             [:clear]))))
        (ret r program)
        (send! r "(cleared 1)")
        (testing "and a stop says whether the function that stopped clears them"
          (let [paused (await-event c "debug-paused")]
            (is (true? (:locals-cleared paused)))
            (ask! c {:op :debug-continue :thread (:thread paused) :id 23})
            (recv-until r "prompt")))
        (send! r "(h {:n 1})")
        (let [paused (await-event c "debug-paused")
              thread (:thread paused)]
          (testing "the editor is told where, and which repl is waiting"
            (is (integer? thread))
            (is (nil? (:locals-cleared paused)))
            (is (= "user" (:ns paused)))
            (is (= 6 (:line paused)))
            (is (= (get-in r [:hello :connection]) (:connection paused))))
          (testing "and can ask what is stopped"
            (is (= [thread] (mapv :thread (:paused (ask! c {:op :debug-paused :id 1}))))))
          (testing "the frames, innermost first, from the one that asked"
            (let [frames (:frames (ask! c {:op :debug-frames :thread thread :id 2}))]
              (is (= ["user/f" "user/h"] (mapv :fn (take 2 frames))))
              (is (= 6 (:line (first frames))))))
          (testing "the locals of the frame that stopped, by the names they were written with"
            (let [o (ask! c {:op :inspect :source {:debug {:thread thread :frame 0}} :id 3})]
              (is (= "reply" (:tag o)))
              (is (= "1" (keyed (:children o) "n")))
              (is (= "2" (keyed (:children o) "a")))
              (is (= "20" (keyed (:children o) "b")))))
          (testing "and of a frame further out, read off it by the debugger"
            (let [o (ask! c {:op :inspect :source {:debug {:thread thread :frame 1}} :id 4})]
              (is (= "{:n 1}" (keyed (:children o) "m")))
              (testing "in the order they were bound in, without the ones the compiler made"
                (is (= ["m" "n" "k"] (mapv :key (:children o)))))))
          (testing "code runs in a frame, with its locals"
            (is (= "1022" (:value (eval-in! c thread 0 "(+ a b 1000)"))))
            (is (= "{:n 1}" (:value (eval-in! c thread 1 "m"))))
            (testing "and a frame of Java code with its own"
              (let [frames (:frames (ask! c {:op :debug-frames :thread thread :id 17}))
                    java (:index (first (remove :fn frames)))]
                (is (= "false" (:value (eval-in! c thread java
                                                 "(contains? replique.debug/*bound* 'a)")))))))
          (testing "and on the thread that stopped"
            (is (= (pr-str (:name paused))
                   (:value (eval-in! c thread 0 "(.getName (Thread/currentThread))")))))
          (testing "an exception is an answer"
            (is (some? (:exception (eval-in! c thread 0 "(/ 1 0)")))))
          (testing "and the connection answers while it runs"
            (let [reply (ask! c {:op :debug-eval :thread thread :code "(Thread/sleep 2000)" :id 15})
                  started (System/currentTimeMillis)]
              (is (number? (:evaluation reply)))
              (is (= [thread] (mapv :thread (:paused (ask! c {:op :debug-paused :id 16})))))
              (is (< (- (System/currentTimeMillis) started) 1500))
              (is (= (:evaluation reply) (:evaluation (await-event c "debug-evaluated"))))))
          (testing "a call started over runs the code as it is now"
            (eval-in! c thread 0 "(defn g [x] (* 100 x))")
            (is (true? (:restarted (ask! c {:op :debug-restart :thread thread :frame 0 :id 10}))))
            (let [again (await-event c "debug-paused")
                  o (ask! c {:op :inspect :source {:debug {:thread thread :frame 0}} :id 11})]
              (is (= thread (:thread again)))
              (is (= "200" (keyed (:children o) "b")))))
          (testing "and continuing lets the repl have its value"
            (is (true? (:resumed (ask! c {:op :debug-continue :thread thread :id 12}))))
            (let [frames (recv-until r "prompt")]
              (is (= "202" (:value (client/frame-tagged frames "ret"))))))
          (testing "a view of a frame of a thread that runs says so"
            (let [o (ask! c {:op :inspect :source {:debug {:thread thread :frame 0}} :id 13})]
              (is (= ":running" (:value (:root o))))))
          (testing "aborting throws where it stopped"
            (send! r "(f 1)")
            (let [t (:thread (await-event c "debug-paused"))]
              (ask! c {:op :debug-continue :thread t :abort true :id 14})
              (let [frames (recv-until r "prompt")]
                (is (re-find #"Aborted from the debugger"
                             (pr-str (client/frames-tagged frames "exception"))))))))
        (finally (disconnect r) (disconnect c))))))

(deftest a-thread-is-let-go-of-when-the-last-editor-goes
  (with-debuggable [info]
    (let [r (client/repl-client info nil 60000)
          c (control info)]
      (try
        (ret r program)
        (send! r "(f 1)")
        (await-event c "debug-paused")
        (disconnect c)
        (let [frames (recv-until r "prompt")]
          (is (= "22" (:value (client/frame-tagged frames "ret")))))
        (finally (disconnect r))))))

(deftest a-repl-evaluates-in-a-frame
  (with-debuggable [info]
    (let [r (client/repl-client info nil 60000)
          c (control info)]
      (try
        ;; a frame further out is read off by the debugger, which needs them
        (ask! c {:op :locals-clearing :clear false :id 40})
        (ret r program)
        (send! r "(h {:n 1})")
        (let [paused (await-event c "debug-paused")
              thread (:thread paused)
              f (client/repl-client info {:frame {:thread thread :frame 0}} 60000)
              point! (fn [directive]
                       (send! f (str directive "\n"))
                       (recv-until f "prompt"))]
          (try
            (testing "a repl opened on a frame says so, from its first prompt"
              (is (= {:thread thread :frame 0} (:frame (:prompt f))))
              (is (= "user" (:ns (:prompt f)))))
            (testing "what it reads is evaluated with the locals of the frame"
              (is (= "1022" (ret f "(+ a b 1000)"))))
            (testing "on the thread that stopped"
              (is (= (pr-str (:name paused)) (ret f "(.getName (Thread/currentThread))"))))
            (testing "printing to the repl, which keeps its results"
              (let [frames (eval! f "(do (println :printed) a)")]
                (is (= ":printed\n" (client/printed frames "out")))
                (is (= "2" (:value (client/frame-tagged frames "ret")))))
              (is (= "2" (ret f "*1"))))
            (testing "what it throws is the repl's exception"
              (is (re-find #"Divide by zero"
                           (:message (client/frame-tagged (eval! f "(/ 1 0)") "exception")))))
            (testing "interrupting the repl interrupts the code on the thread"
              (send! f "(Thread/sleep 60000)")
              (Thread/sleep 500)
              (is (true? (:interrupted (ask! c {:op :interrupt :id 30
                                                :connection (get-in f [:hello :connection])}))))
              (is (client/frame-tagged (recv-until f "prompt") "exception"))
              (is (= "2" (ret f "a"))))
            (testing "a directive points it at another frame, and at its namespace"
              (point! "#replique/ns elsewhere")
              (let [prompt (last (point! (str "#replique/frame {:thread " thread " :frame 1}")))]
                (is (= {:thread thread :frame 1} (:frame prompt)))
                (is (= "user" (:ns prompt))))
              (is (= "{:n 1}" (ret f "m"))))
            (testing "and at none: it evaluates on its own thread again"
              (let [prompt (last (point! "#replique/frame nil"))]
                (is (nil? (:frame prompt))))
              (is (= "3" (ret f "(+ 1 2)")))
              (is (client/frame-tagged (eval! f "m") "exception")))
            (testing "a thread that runs is not evaluated in"
              (point! (str "#replique/frame {:thread " thread "}"))
              (ask! c {:op :debug-continue :thread thread :id 31})
              (recv-until r "prompt")
              (is (re-find #"is not stopped"
                           (:message (client/frame-tagged (eval! f "a") "exception")))))
            (finally (disconnect f))))
        (finally (disconnect r) (disconnect c))))))

(deftest a-local-is-kept-as-a-var
  (with-debuggable [info]
    (let [r (client/repl-client info nil 60000)
          c (control info)]
      (try
        ;; a frame further out is read off by the debugger, which needs them
        (ask! c {:op :locals-clearing :clear false :id 40})
        (ret r program)
        (send! r "(h {:n 1})")
        (let [thread (:thread (await-event c "debug-paused"))
              o (ask! c {:op :inspect :source {:debug {:thread thread :frame 1}} :id 1})
              node (:node (first (filter #(= "m" (:key %)) (:children o))))]
          (testing "in the namespace of the frame"
            (is (= {:var "user/kept"}
                   (select-keys (ask! c {:op :inspect-def :view (:view o) :node node
                                         :name "kept" :id 2})
                                [:var]))))
          (testing "or the one named"
            (is (= {:var "clojure.core/kept-too"}
                   (select-keys (ask! c {:op :inspect-def :view (:view o) :node node
                                         :name "kept-too" :ns "clojure.core" :id 3})
                                [:var]))))
          (testing "and refused without a name"
            (is (= "invalid-message" (:error (ask! c {:op :inspect-def :view (:view o)
                                                      :node node :name "a/b" :id 4})))))
          (ask! c {:op :debug-continue :thread thread :id 5})
          (recv-until r "prompt")
          (testing "which stays once the thread went on"
            (is (= "{:n 1}" (ret r "kept")))))
        (finally (disconnect r) (disconnect c))))))

(deftest asking-about-a-thread-that-is-not-stopped
  (with-debuggable [info]
    (let [c (control info)]
      (try
        (let [e (ask! c {:op :debug-frames :thread 1 :id 1})]
          (is (= "not-paused" (:error e))))
        (finally (disconnect c))))))

(deftest without-the-agent-break-does-nothing
  (is (false? (debug/available?)))
  (let [said (with-out-str (binding [*err* *out*]
                             (is (nil? (let [x 1] (debug/break!))))
                             (is (nil? (debug/break!)))))]
    (testing "and says so, once"
      (is (= 1 (count (re-seq #"did not stop" said)))))))

;;; Stepping through code - which needs the compiler of replique-clj

(defn- stepped
  "Two functions from FILE, at line 1 and line 5, evaluated to be stepped
  through: the second calls the first."
  [file]
  [(str "#replique/src {:file " (pr-str file) " :line 1 :instrument true}
(defn sg [y]
  (* y 10))")
   (str "#replique/src {:file " (pr-str file) " :line 5 :instrument true}
(defn sf [m]
  (let [x (inc (:a m))]
    (+ x (sg x))))")])

(defn- step! [c thread mode id]
  (is (true? (:resumed (ask! c {:op :debug-step :thread thread :mode mode :id id}))))
  (await-event c "debug-paused"))

(defn- at
  "Where a stop is, and what was evaluated there."
  [paused]
  (select-keys paused [:kind :line :column :end-line :end-column :value]))

(deftest a-thread-steps-through-code
  (if-not (debug/instrumenting?)
    (println "replique.debug-test: no compiler to step with - run with :analysis")
    (with-debuggable [info]
      (let [r (client/repl-client info nil 60000)
            c (control info)
            file (java.io.File/createTempFile "stepped" ".clj")]
        (try
          (doseq [code (stepped (str file))] (ret r code))
          (send! r "(sf {:a 1})")
          (let [paused (await-event c "debug-paused")
                thread (:thread paused)]
            (testing "it stops on entering the function evaluated to be stepped through"
              (is (= {:kind "enter" :line 5 :column 1} (select-keys paused [:kind :line :column])))
              (is (= (str file) (:file paused))))
            (testing "with its arguments as the locals of the frame"
              (let [o (ask! c {:op :inspect :source {:debug {:thread thread :frame 0}} :id 2})]
                (is (= "{:a 1}" (keyed (:children o) "m")))))
            (testing "and goes form by form, saying what each evaluated to"
              (is (= {:kind "value" :line 6 :column 16 :end-line 6 :end-column 22 :value "1"}
                     (at (step! c thread "next" 3))))
              (testing "which is a value to inspect"
                (is (= "1" (get-in (ask! c {:op :inspect :id 30
                                             :source {:debug {:thread thread :value true}}})
                                   [:root :value]))))
              (is (= {:kind "value" :line 6 :column 11 :end-line 6 :end-column 23 :value "2"}
                     (at (step! c thread "next" 4)))))
            (testing "into a call of code compiled the same way"
              (is (= {:kind "enter" :line 1} (select-keys (step! c thread "in" 5) [:kind :line])))
              (is (= {:kind "value" :line 2 :value "20"}
                     (select-keys (at (step! c thread "next" 6)) [:kind :line :value]))))
            (testing "and out of it, to the form that made the call"
              (is (= {:kind "value" :line 7 :column 10 :value "20"}
                     (select-keys (at (step! c thread "out" 7)) [:kind :line :column :value]))))
            (testing "with the locals bound so far"
              (is (= "2" (:value (eval-in! c thread 0 "x"))))
              (let [o (ask! c {:op :inspect :source {:debug {:thread thread :frame 0}} :id 8})]
                (is (= ["m" "x"] (mapv :key (:children o))))))
            (testing "over what the call it is in calls"
              (is (= {:kind "value" :line 7 :value "22"}
                     (select-keys (at (step! c thread "next" 9)) [:kind :line :value]))))
            (testing "until it is told to continue, and the repl has its value"
              (ask! c {:op :debug-continue :thread thread :id 10})
              (is (= "22" (:value (client/frame-tagged (recv-until r "prompt") "ret"))))))
          (testing "a form that does not start a line is where it was written"
            (ret r (str "#replique/src {:file " (pr-str (str file)) " :line 20 :column 5 :instrument true}\n"
                        "(defn sh [] (inc 1))"))
            (send! r "(sh)")
            (let [paused (await-event c "debug-paused")]
              (is (= {:kind "enter" :line 20 :column 5} (select-keys paused [:kind :line :column])))
              (is (= {:kind "value" :line 20 :column 17 :end-line 20 :end-column 24 :value "2"}
                     (at (step! c (:thread paused) "next" 12))))
              (ask! c {:op :debug-continue :thread (:thread paused) :id 13})
              (recv-until r "prompt")))
          (testing "a step is one of three"
            (is (= "invalid-message"
                   (:error (ask! c {:op :debug-step :thread 1 :mode "sideways" :id 11})))))
          (finally (disconnect r) (disconnect c) (.delete file)))))))

(deftest what-is-stepped-through-is-a-unit
  (if-not (debug/instrumenting?)
    (println "replique.debug-test: no compiler to step with - run with :analysis")
    (with-debuggable [info]
      (let [r (client/repl-client info nil 60000)
            c (control info)
            file (java.io.File/createTempFile "units" ".clj")
            instrument (fn [line code]
                         (ret r (str "#replique/src {:file " (pr-str (str file)) " :line " line
                                     " :instrument true}\n" code)))]
        (try
          (ret r "(def seen (atom []))")
          (instrument 1 "(defn sl []
  (let [xs (map #(do (swap! seen conj %) %) [1 2])
        ys (iterate inc 0)]
    (+ (count xs) (first ys))))")
          (let [told (await-event c "debug-instrumented")
                unit (:unit told)]
            (testing "a definition evaluated to be stepped through is told about, with its span"
              (is (integer? unit))
              (is (= {:file (str file) :line 1 :column 1 :end-line 4 :end-column 32}
                     (select-keys told [:file :line :column :end-line :end-column]))))
            (send! r "(sl)")
            (let [thread (:thread (await-event c "debug-paused"))]
              (testing "a value is printed without realizing what of it is lazy"
                (is (= "#<unrealized LazySeq>" (:value (step! c thread "next" 2))))
                (is (= "(0 #<unrealized>)" (:value (step! c thread "next" 3))))
                (is (= "[]" (:value (eval-in! c thread 0 "@seen")))))
              (ask! c {:op :debug-continue :thread thread :id 4})
              (is (= "2" (:value (client/frame-tagged (recv-until r "prompt") "ret")))))
            (testing "evaluated again, it is another unit"
              (instrument 1 "(defn sl [] (inc 2))")
              (is (= unit (:unit (await-event c "debug-uninstrumented"))))
              (let [again (:unit (await-event c "debug-instrumented"))]
                (testing "and evaluated the ordinary way, none"
                  (ret r "(defn sl [] 3)")
                  (is (= again (:unit (await-event c "debug-uninstrumented"))))
                  (is (= "3" (ret r "(sl)")))))))
          (testing "a unit is let go of on asking"
            (instrument 10 "(defn sm [] (inc 3))")
            (let [unit (:unit (await-event c "debug-instrumented"))]
              ;; the event saying so comes before the reply
              (is (= [unit] (:uninstrumented (ask! c {:op :debug-uninstrument :unit unit :id 5}))))
              (is (= "4" (ret r "(sm)")))
              (is (= [] (:uninstrumented (ask! c {:op :debug-uninstrument :unit unit :id 6}))))))
          (testing "a thread that steps out of the code it was stepping through runs free"
            (instrument 20 "(defn smk [] (fn [] (inc 1)))")
            (send! r "(def made (smk))")
            (let [thread (:thread (await-event c "debug-paused"))]
              (is (true? (:resumed (ask! c {:op :debug-step :thread thread :mode "in" :id 7}))))
              (recv-until r "prompt"))
            ;; the fn smk made is code compiled to be stepped through, called
            ;; from code that is not, once smk returned
            (is (= "2" (ret r "(made)"))))
          (testing "every unit is let go of without one named"
            (instrument 30 "(defn sn [] 1)")
            (await-event c "debug-instrumented")
            (is (seq (:uninstrumented (ask! c {:op :debug-uninstrument :id 8})))))
          (finally (disconnect r) (disconnect c) (.delete file)))))))

(deftest a-thread-stops-at-a-breakpoint
  (if-not (debug/instrumenting?)
    (println "replique.debug-test: no compiler to step with - run with :analysis")
    (with-debuggable [info]
      (let [r (client/repl-client info nil 60000)
            c (control info)
            file (java.io.File/createTempFile "breaks" ".clj")
            break! (fn [unit line column condition id]
                     (ask! c (cond-> {:op :debug-break :unit unit :line line :column column :id id}
                               condition (assoc :condition condition))))
            stopped (fn [code]
                      (send! r code)
                      (await-event c "debug-paused"))
            continue! (fn [paused id]
                        (ask! c {:op :debug-continue :thread (:thread paused) :id id})
                        (:value (client/frame-tagged (recv-until r "prompt") "ret")))]
        (try
          (ret r (str "#replique/src {:file " (pr-str (str file)) " :line 40 :instrument true"
                      " :entry false}\n"
                      "(defn sb [n]
  (let [x (inc n)]
    (* x 2)))"))
          (let [unit (:unit (await-event c "debug-instrumented"))]
            (testing "code evaluated to stop at its breakpoints does not stop on entry"
              (is (= "4" (ret r "(sb 1)"))))
            (testing "a breakpoint is on a form, which stops the thread once it has its value"
              (let [{:keys [site] :as set} (break! unit 41 11 nil 1)]
                (is (= {:kind "value" :line 41 :column 11 :end-line 41 :end-column 18}
                       (select-keys set [:kind :line :column :end-line :end-column])))
                (let [paused (stopped "(sb 1)")]
                  (is (= {:kind "value" :line 41 :column 11 :value "2" :break true}
                         (select-keys paused [:kind :line :column :value :break])))
                  (testing "and from there it is stepped through"
                    (is (= {:kind "value" :line 42 :value "4"}
                           (select-keys (step! c (:thread paused) "next" 2) [:kind :line :value]))))
                  (is (= "4" (continue! paused 3))))
                (testing "until it is taken away"
                  (is (true? (:unbroken (ask! c {:op :debug-unbreak :site site :id 4}))))
                  (is (= "4" (ret r "(sb 1)"))))))
            (testing "a breakpoint with a condition stops where it holds"
              (let [{:keys [site]} (break! unit 42 5 "(> x 5)" 5)]
                (is (= "4" (ret r "(sb 1)")))
                (let [paused (stopped "(sb 9)")]
                  (is (= {:line 42 :value "20"} (select-keys paused [:line :value])))
                  (is (= "20" (continue! paused 6))))
                (ask! c {:op :debug-unbreak :site site :id 7})))
            (testing "and where it cannot be run, saying why"
              (break! unit 42 5 "(throw (ex-info \"boom\" {}))" 8)
              (let [paused (stopped "(sb 1)")]
                (is (= "boom" (:condition-error paused)))
                (is (= "4" (continue! paused 9)))))
            (testing "a form that is not evaluated has none"
              (is (= "no-form" (:error (break! unit 41 9 nil 10)))))
            (testing "nor has a unit that is not stepped through"
              (is (= "unknown-unit" (:error (break! 123456 41 11 nil 11))))))
          (finally (disconnect r) (disconnect c) (.delete file)))))))

;;; Recording - which needs the compiler of replique-clj, and not the debugger

(defn- recorded
  "What the call CALL of the unit UNIT recorded - the last one, CALL nil - by
  the [line column] of each form, as [value times]."
  [c unit call id]
  (let [said (ask! c (cond-> {:op :debug-recorded :unit unit :id id} call (assoc :call call)))]
    (assoc said :by-place (into {} (map (fn [v] [[(:line v) (:column v)] [(:value v) (:times v)]]))
                                (:values said)))))

(deftest what-a-function-recorded-is-kept-call-by-call
  (if-not (debug/instrumenting?)
    (println "replique.debug-test: no compiler to record with - run with :analysis")
    (client/with-process [info nil]
      (let [r (client/repl-client info nil 60000)
            c (control info)
            file (java.io.File/createTempFile "recorded" ".clj")
            record (fn [line code]
                     (ret r (str "#replique/src {:file " (pr-str (str file)) " :line " line
                                 " :instrument true :entry false :record true}\n" code)))]
        (try
          (record 1 "(defn rf [m]
  (let [x (inc (:a m))]
    (mapv (fn [y] (* x y)) (range 3))))")
          (let [told (await-event c "debug-instrumented")
                unit (:unit told)]
            (testing "a definition evaluated to record is told about as recording"
              (is (true? (:record told)))
              (is (true? (:breaks-only told))))
            (testing "and stops nothing, without the debugger"
              (is (false? (debug/available?)))
              (is (= "[0 2 4]" (ret r "(rf {:a 1})"))))
            (testing "clients are told it recorded"
              (is (= {:unit unit :calls 1} (select-keys (await-event c "debug-recorded")
                                                         [:unit :calls]))))
            (let [{:keys [calls call by-place]} (recorded c unit nil 1)]
              (testing "what each form evaluated to in the call, and how many times"
                (is (= 1 (count calls)))
                (is (= call (:call (first calls))))
                (is (string? (:thread (first calls))))
                ;; the fn entered, with what it was called with
                (is (= {[1 1] ["[{:a 1}]" 1]
                        [2 16] ["1" 1]
                        [2 11] ["2" 1]
                        [3 19] ["4" 3]
                        [3 28] ["(0 1 2)" 1]
                        [3 5] ["[0 2 4]" 1]
                        [2 3] ["[0 2 4]" 1]}
                       (dissoc by-place [3 11]))))
              (testing "call by call"
                (ret r "(rf {:a 2})")
                (let [again (recorded c unit nil 2)]
                  (is (= 2 (count (:calls again))))
                  (is (= ["3" 1] (get-in again [:by-place [2 11]])))
                  (is (= ["2" 1] (get-in (recorded c unit call 3) [:by-place [2 11]])))))
              (testing "each value is one to browse"
                (let [site (:site (first (filter #(= [2 11] [(:line %) (:column %)])
                                                 (:values (recorded c unit call 4)))))]
                  (let [o (ask! c {:op :inspect :id 5 :source {:debug {:site site :call call}}})]
                    (is (= "2" (get-in o [:root :value])))
                    (testing "kept in a var of the namespace of its code"
                      (is (= "user/recorded-kept"
                             (:var (ask! c {:op :inspect-def :view (:view o)
                                            :node (get-in o [:root :node])
                                            :name "recorded-kept" :id 50}))))
                      (is (= "2" (ret r "recorded-kept")))))
                  (testing "and so are the values of a form, one per call"
                    (is (= "[2 3]" (get-in (ask! c {:op :inspect :id 6
                                                     :source {:debug {:site site}}})
                                           [:root :value]))))))
              (testing "of the last calls only"
                (dotimes [i 12] (ret r (str "(rf {:a " i "})")))
                (is (= 10 (count (:calls (recorded c unit nil 7)))))
                (is (= "unknown-call" (:error (ask! c {:op :debug-recorded :unit unit :call call
                                                       :id 8})))))))
          (testing "a call is its own, recursive or not"
            (record 10 "(defn rr [n]
  (if (pos? n)
    (+ n (rr (dec n)))
    0))")
            (let [unit (:unit (await-event c "debug-instrumented"))]
              (is (= "3" (ret r "(rr 2)")))
              (let [{:keys [calls]} (recorded c unit nil 9)
                    [outer inner innermost] (map #(:by-place (recorded c unit (:call %) 10)) calls)]
                (is (= 3 (count calls)))
                (is (= {[11 3] ["3" 1] [12 5] ["3" 1] [12 10] ["1" 1]}
                       (select-keys outer [[11 3] [12 5] [12 10]])))
                (is (= {[11 3] ["1" 1] [12 10] ["0" 1]} (select-keys inner [[11 3] [12 10]])))
                (is (= {[10 1] ["[0]" 1] [11 3] ["0" 1] [11 7] ["false" 1]} innermost)))))
          (testing "and one thrown out of is over"
            (record 20 "(defn rt [n]
  (if (pos? n)
    (inc (try (rt (dec n)) (catch Exception _ 1)))
    (throw (ex-info \"none\" {}))))")
            (let [unit (:unit (await-event c "debug-instrumented"))]
              (is (= "2" (ret r "(rt 1)")))
              (let [[outer _] (map #(:by-place (recorded c unit (:call %) 11))
                                   (:calls (recorded c unit nil 12)))]
                (is (= ["2" 1] (get outer [21 3]))))))
          (testing "anything else recorded is recorded as it is evaluated"
            (record 30 "(let [a (inc 1)] (* a 2))")
            (let [unit (:unit (await-event c "debug-instrumented"))]
              ;; the form evaluated is the repl's to show
              (is (= {[30 9] ["2" 1] [30 18] ["4" 1]}
                     (:by-place (recorded c unit nil 13))))))
          (testing "traced, and let go of by tracing, it records still"
            (record 40 "(defn rk [n] (inc n))")
            (let [unit (:unit (await-event c "debug-instrumented"))]
              (is (= ["user/rk"] (:traced (ask! c {:op :trace :vars ["user/rk"] :id 16}))))
              (is (= "2" (ret r "(rk 1)")))
              (ask! c {:op :untrace :id 17})
              (is (= "3" (ret r "(rk 2)")))
              (is (= 2 (count (:calls (recorded c unit nil 18)))))
              (testing "and defined again, it does not"
                (ret r "(defn rk [n] n)")
                (is (= unit (:unit (await-event c "debug-uninstrumented")))))))
          (testing "what is let go of is forgotten"
            (let [{:keys [uninstrumented]} (ask! c {:op :debug-uninstrument :id 14})]
              (is (seq uninstrumented))
              (is (= "unknown-unit" (:error (ask! c {:op :debug-recorded
                                                     :unit (first uninstrumented) :id 15}))))))
          (finally (disconnect r) (disconnect c) (.delete file)))))))
