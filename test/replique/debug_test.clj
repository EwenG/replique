(ns replique.debug-test
  "A thread stopped by `replique.debug/break!', and what a client does to it.

  In a process of its own, started for it: stopping needs the JDWP agent, which
  is a flag of the jvm - and the tests run in a jvm that was not started with
  it, which is the other thing tested here."
  (:require [clojure.data.json :as djson]
            [clojure.test :refer [deftest is testing]]
            [replique.classpath :as classpath]
            [replique.debug :as debug]
            [replique.state :as state]
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

(deftest a-local-is-recorded-where-its-name-is-written
  (if-not (debug/instrumenting?)
    (println "replique.debug-test: no compiler to record with - run with :analysis")
    (client/with-process [info nil]
      (let [r (client/repl-client info nil 60000)
            c (control info)
            file (java.io.File/createTempFile "locals" ".clj")]
        (try
          (ret r (str "#replique/src {:file " (pr-str (str file)) " :line 1"
                      " :instrument true :entry false :record true}\n"
                      "(defn im [a b c]
  (doall (for [x a :let [y (inc x)]]
           (reduce (fn [[s t] u] [(+ s u) t]) [b c] [x y]))))"))
          (let [unit (:unit (await-event c "debug-instrumented"))
                _ (ret r "(im [1 2] 10 20)")
                {:keys [by-place values]} (recorded c unit nil 1)
                x (:site (some #(when (= [2 16] [(:line %) (:column %)]) %) values))]
            (testing "the locals of a for are recorded where the name was written, each time it was bound"
              (is (= ["2" 2] (get by-place [2 16])))
              (is (= "local" (:kind (some #(when (= [2 16] [(:line %) (:column %)]) %) values))))
              (is (= ["3" 2] (get by-place [2 26])))
              (is (= ["12" 4] (get by-place [3 26]))))
            (testing "code is evaluated against them past the fns of macros: a fn a macro made
              is not the fn its code is in"
              (is (= 2 (:index (ask! c {:op :debug-find-iteration :unit unit :site x
                                        :line 3 :column 40 :id 2
                                        :code "(= [a b c x y] [[1 2] 10 20 2 3])"}))))))
          (finally (disconnect r) (disconnect c) (.delete file)))))))

(deftest an-iteration-of-a-loop-is-what-it-evaluated
  (if-not (debug/instrumenting?)
    (println "replique.debug-test: no compiler to record with - run with :analysis")
    (client/with-process [info nil]
      (let [r (client/repl-client info nil 60000)
            c (control info)
            file (java.io.File/createTempFile "iterations" ".clj")
            in (fn [unit site index id]
                 (let [said (ask! c {:op :debug-recorded :unit unit :id id
                                     :iteration {:site site :index index}})]
                   (assoc said :by-place (into {} (map (fn [v] [[(:line v) (:column v)]
                                                                (if (:not-kept v)
                                                                  :not-kept
                                                                  [(:value v) (:times v)])]))
                                               (:values said)))))]
        (try
          (ret r (str "#replique/src {:file " (pr-str (str file)) " :line 1"
                      " :instrument true :entry false :record true}\n"
                      "(defn il [xs]
  (doseq [x xs
          y (range x)]
    (when (odd? x) (* x y)))
  (loop [i 0 acc []]
    (if (< i 25) (recur (inc i) (conj acc (* i i))) (count acc))))"))
          (let [unit (:unit (await-event c "debug-instrumented"))
                _ (ret r "(il [1 2 3])")
                {:keys [loops by-place]} (recorded c unit nil 1)]
            (testing "the loops of a call, by the name they bind each time round"
              (is (= [{:line 2 :column 3 :iterations 3 :kept [[1 3]]}
                      {:line 2 :column 3 :iterations 6 :kept [[1 6]]}
                      {:line 5 :column 3 :iterations 26 :kept [[1 9] [17 26]]}]
                     (mapv #(select-keys % [:line :column :iterations :kept]) loops)))
              (is (= 3 (count (set (map :site loops)))))
              (is (= ["3" 3] (get by-place [2 11]))))
            (let [[x y i] (map :site loops)]
              (testing "what the code a loop goes round evaluated in an iteration of it"
                (let [{:keys [iteration by-place]} (in unit x 2 2)]
                  (is (= {:site x :index 2} iteration))
                  (is (= {[2 11] ["2" 1]
                          [3 11] ["1" 2]
                          [3 13] ["(0 1)" 1]
                          [4 11] ["false" 2]}
                         (select-keys by-place [[2 11] [3 11] [3 13] [4 11] [4 20]])))
                  (testing "and the rest as in the whole call"
                    (is (= ["[[1 2 3]]" 1] (get by-place [1 1])))
                    (is (= ["25" 26] (get by-place [5 10]))))))
              (testing "in a loop in another, within the iteration of that one it is in"
                (let [{:keys [by-place]} (in unit y 5 3)]
                  (is (= {[2 11] ["3" 1]
                          [3 11] ["1" 1]
                          [3 13] ["(0 1 2)" 1]
                          [4 11] ["true" 1]
                          [4 20] ["3" 1]}
                         (select-keys by-place [[2 11] [3 11] [3 13] [4 11] [4 20]])))))
              (testing "the first iterations are kept, and the last ones"
                (is (= {[5 10] ["2" 1] [5 14] ["[0 1]" 1] [6 25] ["3" 1] [6 43] ["4" 1]}
                       (select-keys (:by-place (in unit i 3 4)) [[5 10] [5 14] [6 25] [6 43]])))
                (is (= {[5 10] ["19" 1] [6 43] ["361" 1]}
                       (select-keys (:by-place (in unit i 20 5)) [[5 10] [6 43]])))
                (is (= {[5 10] ["25" 1] [6 9] ["false" 1] [6 53] ["25" 1]}
                       (select-keys (:by-place (in unit i 26 6)) [[5 10] [6 9] [6 53]])))
                (is (= "unknown-iteration" (:error (ask! c {:op :debug-recorded :unit unit :id 7
                                                            :iteration {:site i :index 12}}))))
                (is (= "unknown-iteration" (:error (ask! c {:op :debug-recorded :unit unit :id 8
                                                            :iteration {:site 999999 :index 1}})))))
              (let [site-at (fn [line column]
                              (:site (some #(when (= [line column] [(:line %) (:column %)]) %)
                                           (:values (recorded c unit nil 9)))))
                    square (site-at 6 43)]
                (testing "a value of an iteration is one to browse"
                  (is (= "361" (get-in (ask! c {:op :inspect :id 10
                                                :source {:debug {:site square
                                                                 :iteration {:site i :index 20}}}})
                                       [:root :value])))
                  (is (= ":not-recorded"
                         (get-in (ask! c {:op :inspect :id 11
                                          :source {:debug {:site square
                                                           :iteration {:site i :index 26}}}})
                                 [:root :value]))))
                (testing "and so is what each iteration kept did, by iteration"
                  (is (= "iteration 1 x 1  y 0 · iteration 2 x 2  y 1 · iteration 3 x 3  y 2"
                         (get-in (ask! c {:op :inspect :id 12 :source {:debug {:loop x}}})
                                 [:root :value])))
                  (testing "and of a form in it, what it evaluated to last in each"
                    (is (= [["iteration 1" "x 1  y 0  (* x y) 0"] ["iteration 2" "x 2  y 1"]
                            ["iteration 3" "x 3  y 2  (* x y) 6"]]
                           (mapv (juxt :key :value) (:children (ask! c {:op :inspect :id 12
                                                            :source {:debug {:loop x
                                                                             :site (site-at 4 20)}}})))))
                    (is (= "iteration 1 0  iteration 3 6"
                           (get-in (ask! c {:op :inspect :id 12
                                            :source {:debug {:loop x :site (site-at 4 20)
                                                             :only true}}})
                                   [:root :value])))
                    ;; not of a form outside it
                    (is (some? (site-at 5 3)))
                    (is (= "iteration 1 x 1  y 0 · iteration 2 x 2  y 1 · iteration 3 x 3  y 2"
                           (get-in (ask! c {:op :inspect :id 12
                                            :source {:debug {:loop x :site (site-at 5 3)
                                                             :only true}}})
                                   [:root :value]))))
                  ;; the ones kept
                  (is (= 19 (get-in (ask! c {:op :inspect :id 13 :source {:debug {:loop i}}})
                                    [:root :count])))))
              (testing "the first iteration where something holds"
                (let [find (fn [site code & {:as more}]
                             (ask! c (merge {:op :debug-find-iteration :unit unit :site site
                                             :line 4 :column 20 :code code :id 14}
                                            more)))]
                  (is (= 3 (:index (find y "(= y 1)"))))
                  (testing "with the locals in scope there as they were in it"
                    (is (= 5 (:index (find y "(= [xs x y] [[1 2 3] 3 1])")))))
                  (is (= 5 (:index (find y "(= y 1)" :from 3))))
                  (is (= 3 (:index (find y "(= y 1)" :from 5 :backward true))))
                  (is (nil? (:index (find y "(= y 7)"))))
                  (is (= 22 (:index (find i "(> i 20)" :line 6 :column 43))))
                  (is (nil? (:index (find i "(= i 12)" :line 6 :column 43))))
                  (is (re-find #"boom" (:error (find y "(throw (ex-info \"boom\" {}))"))))))))
          (finally (disconnect r) (disconnect c) (.delete file)))))))

(deftest an-iteration-starts-with-the-first-name-bound
  (if-not (debug/instrumenting?)
    (println "replique.debug-test: no compiler to record with - run with :analysis")
    (client/with-process [info nil]
      (let [r (client/repl-client info nil 60000)
            c (control info)
            file (java.io.File/createTempFile "iterations" ".clj")]
        (try
          (ret r (str "#replique/src {:file " (pr-str (str file)) " :line 1"
                      " :instrument true :entry false :record true}\n"
                      "(defn dl [ms]
  (doall (for [{:keys [a] :as m} ms]
           (inc a))))"))
          (let [unit (:unit (await-event c "debug-instrumented"))
                _ (ret r "(dl [{:a 1} {:a 2} {:a 3}])")
                {:keys [loops values]} (recorded c unit nil 1)
                loop-site (:site (first loops))
                m (:site (some #(when (= [2 31] [(:line %) (:column %)]) %) values))
                m-in (fn [index id]
                       (get-in (ask! c {:op :inspect :id id
                                        :source {:debug {:site m
                                                         :iteration {:site loop-site :index index}}}})
                               [:root :value]))]
            (testing "the :as of a map is bound before its keys, and is of the same iteration"
              (is (= "{:a 1}" (m-in 1 2)))
              (is (= "{:a 3}" (m-in 3 3)))
              (is (= "iteration 1 a 1  m {:a 1} · iteration 2 a 2  m {:a 2} · ..."
                     (get-in (ask! c {:op :inspect :id 4 :source {:debug {:loop loop-site}}})
                             [:root :value])))))
          (finally (disconnect r) (disconnect c) (.delete file)))))))

(deftest a-fn-in-a-definition-is-entered-where-it-is-written
  (if-not (debug/instrumenting?)
    (println "replique.debug-test: no compiler to record with - run with :analysis")
    (client/with-process [info nil]
      (let [r (client/repl-client info nil 60000)
            c (control info)
            file (java.io.File/createTempFile "entries" ".clj")]
        (try
          (ret r (str "#replique/src {:file " (pr-str (str file)) " :line 1"
                      " :instrument true :entry false :record true}\n"
                      "(defn ef [xs ys]
  (let [a (mapv (fn [x] (inc x)) xs)
        b (mapv #(* 2 %) ys)]
    (doall (for [y ys] (+ y (count a) (count b))))))"))
          (let [unit (:unit (await-event c "debug-instrumented"))
                _ (ret r "(ef [1 2] [3 4])")
                place (juxt :line :column :end-line :end-column)
                entries (filter #(= "enter" (:kind %)) (:values (recorded c unit nil 1)))]
            (testing "a fn and #(...) are entered where they are written - not the definition"
              (is (= {["x"] [2 17 2 33] ["xs" "ys"] [1 1 4 53]}
                     (into {} (for [e entries :when (not (re-find #"__" (first (:params e))))]
                                [(:params e) (place e)]))))
              (is (= [3 17 3 25] (place (some #(when (re-find #"^p1__" (first (:params %))) %)
                                              entries)))))
            (testing "one a macro made says nothing of its args, having no params written"
              (is (= 3 (count entries))))
            (testing "a frame of a loop is at the form the cursor is at"
              (let [at (ask! c {:op :debug-cursor :unit unit :id 2
                                :site (:site (some #(when (= [2 25] [(:line %) (:column %)]) %)
                                                   (:values (recorded c unit nil 3))))})
                    [loop call] (:frames at)]
                (is (= ["loop" "fn [x]" [2 25 2 32]] ((juxt :kind :label place) loop)))
                (is (= [2 25 2 32] (place call)))))
            (testing "#(...) is said as it is written"
              (is (= "(mapv #(* 2 %) ys)"
                     (:code (first (:frames (ask! c {:op :debug-cursor :unit unit :id 4
                                                     :site (:site (some #(when (= [3 11] [(:line %) (:column %)]) %)
                                                                        (:values (recorded c unit nil 5))))})))))))
            (testing "the entry of a fn a macro made is gone past"
              (let [start (ask! c {:op :debug-cursor :unit unit :id 6})
                    codes (loop [cur start codes []]
                            (let [next (ask! c (merge {:op :debug-cursor :id 7 :move "next"}
                                                      (select-keys cur [:unit :call :at])))]
                              (if (or (:error next) (> (count codes) 60))
                                codes
                                (recur next (conj codes (:code (first (:frames next))))))))]
                (is (some #{"y"} codes))
                (is (not-any? #(re-find #"__" (str %)) codes))
                (is (some #{"[%]"} codes))
                (is (some #{"(* 2 %)"} codes)))))
          (finally (disconnect r) (disconnect c) (.delete file)))))))

(deftest each-call-of-a-fn-is-a-time-round
  (if-not (debug/instrumenting?)
    (println "replique.debug-test: no compiler to record with - run with :analysis")
    (client/with-process [info nil]
      (let [r (client/repl-client info nil 60000)
            c (control info)
            file (java.io.File/createTempFile "fn-loops" ".clj")
            in (fn [unit call site index id]
                 (let [said (ask! c (cond-> {:op :debug-recorded :unit unit :id id
                                             :iteration {:site site :index index}}
                                      call (assoc :call call)))]
                   ;; a fn entered by its site, the entries of the fns of a
                   ;; definition being where it is
                   (into {} (map (fn [v] [(if (= "enter" (:kind v)) (:site v) [(:line v) (:column v)])
                                          [(:value v) (:times v)]]))
                         (:values said))))]
        (try
          (ret r (str "#replique/src {:file " (pr-str (str file)) " :line 1"
                      " :instrument true :entry false :record true}\n"
                      "(defn fl [xs]
  (let [total (reduce (fn [acc x] (+ acc x)) 0 xs)
        doubled (doall (map (fn [x] (* 2 x)) xs))
        f (fn self [n] (if (pos? n) (self (dec n)) n))]
    [total doubled (f 2) (map (fn [y] (inc y)) xs)]))"))
          (let [unit (:unit (await-event c "debug-instrumented"))
                told (fn [calls]
                       (loop [] (when-not (= {:unit unit :calls calls}
                                             (select-keys (await-event c "debug-recorded")
                                                          [:unit :calls]))
                                  (recur))))
                _ (ret r "(fl [1 2 3])")
                _ (told 1)
                {:keys [loops calls]} (recorded c unit nil 1)
                [add double] (map :site loops)]
            (testing "each fn the code wrote is a loop going round once for each call of it"
              (is (= [{:line 2 :column 23 :iterations 3 :fn true}
                      {:line 3 :column 29 :iterations 3 :fn true}
                      {:line 5 :column 31 :iterations 3 :fn true}]
                     (mapv #(select-keys % [:line :column :iterations :fn]) loops))))
            (testing "a call of it is what it evaluated then"
              (is (= {add ["[1 2]" 1] [2 35] ["3" 1]}
                     (select-keys (in unit nil add 2 2) [add [2 35]])))
              (is (= [["iteration 1" "acc 0  x 1"] ["iteration 2" "acc 1  x 2"]
                      ["iteration 3" "acc 3  x 3"]]
                     (mapv (juxt :key :value) (:children (ask! c {:op :inspect :id 3
                                                      :source {:debug {:loop add}}}))))))
            (testing "what a doall realizes in the call is of the call"
              (ret r "(fl [10 20])")
              (told 2)
              (let [{:keys [loops]} (recorded c unit nil 4)
                    first-call (:call (first calls))]
                (is (= 2 (:iterations (some #(when (= double (:site %)) %) loops))))
                (is (= {double ["[20]" 1] [3 37] ["40" 1]}
                       (select-keys (in unit nil double 2 5) [double [3 37]])))
                (is (= {double ["[3]" 1] [3 37] ["6" 1]}
                       (select-keys (in unit first-call double 3 6) [double [3 37]])))))
            (testing "every call of a form is by the number of the call, the ones it was not
                      evaluated in left out"
              (ret r "(fl [])")
              (told 3)
              (let [sum (:site (some #(when (= [2 35] [(:line %) (:column %)]) %)
                                     (:values (recorded c unit (:call (first calls)) 7))))]
                (is (= "call 1 6  call 2 30" (get-in (ask! c {:op :inspect :id 8 :source {:debug {:site sum}}})
                                             [:root :value])))
                (testing "and so is every call, by call, of its params and the form"
                  (is (= [["call 1" "xs [1 2 3]  (+ acc x) 6"] ["call 2" "xs [10 20]  (+ acc x) 30"]
                          ["call 3" "xs []"]]
                         (mapv (juxt :key :value) (:children (ask! c {:op :inspect :id 9
                                                          :source {:debug {:calls unit :site sum}}})))))
                  (is (= [["call 1" "xs [1 2 3]"] ["call 2" "xs [10 20]"] ["call 3" "xs []"]]
                         (mapv (juxt :key :value) (:children (ask! c {:op :inspect :id 10
                                                          :source {:debug {:calls unit}}})))))))))
          (finally (disconnect r) (disconnect c) (.delete file)))))))

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
                  (testing "and so are the values of a form, by call"
                    (is (= "call 1 2  call 2 3" (get-in (ask! c {:op :inspect :id 6
                                                          :source {:debug {:site site}}})
                                                [:root :value])))
                    (testing "the ones it was evaluated in, numbered still"
                      (let [entry (:site (first (filter #(= ["m"] (:params %))
                                                        (:values (recorded c unit call 60)))))]
                        (is (= "call 1 {:a 1}  call 2 {:a 2}"
                               (get-in (ask! c {:op :inspect :id 61
                                                :source {:debug {:site entry :arg 0}}})
                                       [:root :value]))))))))
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
          (testing "its calls recorded too, and let go of, it records still"
            (record 40 "(defn rk [n] (inc n))")
            (let [unit (:unit (await-event c "debug-instrumented"))
                  _ (send! c {:op :record-calls :vars ["user/rk"] :id 16})
                  wrapper (await-event c "debug-instrumented")]
              (is (:calls-only wrapper))
              ;; its answer, which followed the event
              (is (= ["user/rk"] (:recorded (loop [] (let [f (next-frame c)]
                                                       (if (= "event" (:tag f)) (recur) f))))))
              (is (= "2" (ret r "(rk 1)")))
              (ask! c {:op :debug-uninstrument :unit (:unit wrapper) :id 17})
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

(deftest the-calls-recorded-are-a-tree
  (if-not (debug/instrumenting?)
    (println "replique.debug-test: no compiler to record with - run with :analysis")
    (client/with-process [info nil]
      (let [r (client/repl-client info nil 60000)
            c (control info)
            file (java.io.File/createTempFile "calls" ".clj")
            record (fn [line code]
                     (ret r (str "#replique/src {:file " (pr-str (str file)) " :line " line
                                 " :instrument true :entry false :record true}\n" code))
                     (:unit (await-event c "debug-instrumented")))
            said (juxt :label :args :value :made :index :of)]
        (try
          (let [g (record 1 "(defn tg [x] (* x 10))")
                f (record 2 "(defn tf [xs]\n  (reduce + (map tg xs)))")
                _ (ret r "(tf [1 2 3])")
                _ (ret r "(tg 4)")
                ;; of the other tests too, in this JVM
                calls (filterv #(#{f g} (:unit %)) (:calls (ask! c {:op :debug-calls :id 1})))]
            (testing "the calls made in none, oldest first, with what they were called with
                      and returned"
              (is (= [["tf" ["[1 2 3]"] "60" 3 1 1] ["tg" ["4"] "40" 0 4 4]] (mapv said calls)))
              (is (every? :started calls))
              (is (= [f g] (mapv :unit calls))))
            (testing "and the calls each made"
              (let [made (:calls (ask! c {:op :debug-calls :unit f :call (:call (first calls)) :id 2}))]
                (is (= [["tg" ["1"] "10" 0 1 4] ["tg" ["2"] "20" 0 2 4] ["tg" ["3"] "30" 0 3 4]]
                       (mapv said made)))
                (testing "a call is where a cursor goes"
                  (is (= (:call (second made))
                         (:call (ask! c {:op :debug-cursor :unit g :call (:call (second made)) :id 3})))))))
            (is (= "invalid-message" (:error (ask! c {:op :debug-calls :unit f :id 4})))))
          (finally (disconnect r) (disconnect c) (.delete file)))))))

(deftest a-literal-a-function-returns-is-what-it-returned
  (if-not (debug/instrumenting?)
    (println "replique.debug-test: no compiler to record with - run with :analysis")
    (client/with-process [info nil]
      (let [r (client/repl-client info nil 60000)
            c (control info)
            file (java.io.File/createTempFile "literals" ".clj")
            record (fn [line code]
                     (ret r (str "#replique/src {:file " (pr-str (str file)) " :line " line
                                 " :instrument true :entry false :record true}\n" code))
                     (:unit (await-event c "debug-instrumented")))
            site-at (fn [unit line column]
                      (:site (some #(when (= [line column] [(:line %) (:column %)]) %)
                                   (:values (ask! c {:op :debug-recorded :unit unit :id 1})))))
            code (fn [cur] (:code (first (:frames cur))))]
        (try
          (ret r "(defn lh [x] (dec x))")
          (let [g (record 1 "(defn lg [x] {:a (inc x) :b [x]})")
                f (record 2 "(defn lf [x]\n  (let [m (lg x)]\n    [m (lh x)]))")
                _ (ret r "(lf 1)")
                calls (filterv #(#{f g} (:unit %)) (:calls (ask! c {:op :debug-calls :id 2})))]
            (testing "the value of a literal a fn ends with is what its call returned"
              (is (= [["lf" "[{:a 2, :b [1]} 0]"]] (mapv (juxt :label :value) calls)))
              (is (= [["lg" "{:a 2, :b [1]}"]]
                     (mapv (juxt :label :value)
                           (:calls (ask! c {:op :debug-calls :unit f :call (:call (first calls))
                                            :id 3}))))))
            (testing "which a move into the call goes to"
              (let [in (ask! c (merge {:op :debug-cursor :move "in" :id 5}
                                      (select-keys (ask! c {:op :debug-cursor :unit f :id 6
                                                            :site (site-at f 3 11)})
                                                   [:unit :call :at])))]
                (is (= "[x]" (code in)))
                (is (= "{:a (inc x), :b [x]}"
                       (code (reduce (fn [at _]
                                       (ask! c (merge {:op :debug-cursor :move "next" :id 9}
                                                      (select-keys at [:unit :call :at]))))
                                     in (range 2)))))))
            (testing "a call of a function recorded since is said to be one, where it was not"
              (record 3 "(defn lh [x] (dec x))")
              (let [at (ask! c {:op :debug-cursor :unit f :site (site-at f 4 8) :id 7})
                    in (ask! c (merge {:op :debug-cursor :move "in" :id 8}
                                      (select-keys at [:unit :call :at])))]
                (is (= "(lh x)" (code at)))
                (is (= "no-move" (:error in)))
                (is (re-find #"^lh was called here, but that call is not recorded" (str (:message in)))))))
          (finally (disconnect r) (disconnect c) (.delete file)))))))

(deftest the-calls-made-for-a-value-kept-are-kept
  (if-not (debug/instrumenting?)
    (println "replique.debug-test: no compiler to record with - run with :analysis")
    (client/with-process [info nil]
      (let [r (client/repl-client info nil 60000)
            c (control info)
            file (java.io.File/createTempFile "made" ".clj")
            record (fn [line code]
                     (ret r (str "#replique/src {:file " (pr-str (str file)) " :line " line
                                 " :instrument true :entry false :record true}\n" code))
                     (:unit (await-event c "debug-instrumented")))
            site-at (fn [unit line column]
                      (:site (some #(when (= [line column] [(:line %) (:column %)]) %)
                                   (:values (ask! c {:op :debug-recorded :unit unit :id 1})))))
            made (fn [unit]
                   (let [call (:call (ask! c {:op :debug-cursor :unit unit :id 2}))]
                     (:calls (ask! c {:op :debug-calls :unit unit :call call :id 3}))))
            into (fn [unit site at]
                   (ask! c (merge {:op :debug-cursor :move "in" :id 4}
                                  (select-keys (ask! c {:op :debug-cursor :unit unit :site site
                                                        :at at :id 5})
                                               [:unit :call :at]))))]
        (try
          (let [h (record 1 "(defn mh [f xs] (mapv f xs))")
                o (record 2 (str "(defn mo [xs]\n"
                                 "  (doall (for [x xs]\n"
                                 "          [(mh (fn [y] (inc y)) [x])\n"
                                 "           (mh #(* 2 %) [x x])])))"))
                _ (ret r "(mo (range 30))")]
            (testing "of the calls made in a loop, the ones made for the values kept"
              (is (= (* 2 2 10) (count (made o)))))
            (testing "a move into one of them, a fn of the call that made it called in it"
              (doseq [[line at] [[4 0] [4 Integer/MAX_VALUE] [5 0] [5 Integer/MAX_VALUE]]]
                (let [in (into o (site-at o line 12) at)]
                  (is (= h (:unit in)))
                  (is (= "[f xs]" (:code (first (:frames in)))))))))
          (testing "of the calls made for the same value, the first ones and the last ones"
            (let [_ (record 7 "(defn mh2 [x] (inc x))")
                  g (record 8 "(defn mg [xs] (mapv mh2 xs))")]
              (ret r "(mg (range 30))")
              (is (= (* 2 10) (count (made g))))
              (let [in (into g (site-at g 8 15) 0)]
                (is (= "[x]" (:code (first (:frames in))))))))
          (finally (disconnect r) (disconnect c) (.delete file)))))))

(defn- source-root! []
  (let [dir (java.io.File. (str (client/temp-dir)) "src")]
    (.mkdirs dir)
    (.addURL state/class-loader (.toURL (.toURI dir)))
    (state/adopt-class-loader!)
    (classpath/rescan!)
    (.getPath dir)))

(defn- written-file! [dir name source]
  (let [f (java.io.File. (str dir) (str name))]
    (.mkdirs (.getParentFile f))
    (spit f source)
    (.getPath f)))

(deftest the-calls-only-of-a-function-are-recorded
  (if-not (debug/instrumenting?)
    (println "replique.debug-test: no compiler to record with - run with :analysis")
    (client/with-process [info nil]
      (let [r (client/repl-client info nil 60000)
            c (control info)
            file (java.io.File/createTempFile "only" ".clj")
            record (fn [line code]
                     (ret r (str "#replique/src {:file " (pr-str (str file)) " :line " line
                                 " :instrument true :entry false :record true}\n" code))
                     (loop [] (let [e (await-event c "debug-instrumented")]
                                (if (:calls-only e) (recur) (:unit e)))))
            said (juxt :label :args :value :threw)
            roots (fn [label] (filterv #(= label (:label %)) (:calls (ask! c {:op :debug-calls :id 1}))))
            wrapped? (fn [v] (ret r (str "(boolean (:replique.debug/wraps (meta " v ")))")))]
        (try
          ;; what other tests recorded in this JVM: the calls only of a
          ;; function are let go of with the last code recorded
          (ask! c {:op :debug-uninstrument :id 0})
          (ret r "(ns lc)")
          (ret r "(defn la [x] (inc x))")
          (ret r "(defn lt [] (throw (ex-info \"none\" {})))")
          (ret r "(def lz 1)")
          (let [{:keys [recorded skipped]}
                (ask! c {:op :record-calls :vars ["lc/la" "lc/lt" "lc/lz" "clojure.core/inc"] :id 1})]
            (is (= ["lc/la" "lc/lt"] recorded))
            (is (= [["lc/lz" "not a function"]
                    ["clojure.core/inc" "called everywhere, or what recording is made of"]]
                   (mapv (juxt :var :why) skipped))))
          (testing "a call made in no call recorded is only made"
            (is (= "2" (ret r "(la 1)")))
            (is (= [] (roots "lc/la"))))
          (let [f (record 1 "(defn lf [x] (+ (la x) (try (lt) (catch Exception _ 0))))")
                _ (is (= "2" (ret r "(lf 1)")))
                [lf] (roots "lf")
                made (:calls (ask! c {:op :debug-calls :unit f :call (:call lf) :id 2}))]
            (testing "one made in one is a call among the calls recorded, with what it was
                      called with and returned, or threw"
              (is (= ["lf" ["1"] "2" nil] (said lf)))
              (is (= [["lc/la" ["1"] "2" nil] ["lc/lt" nil "clojure.lang.ExceptionInfo: none" true]]
                     (mapv said made))))
            (testing "which a cursor goes through: what it was called with, then returned"
              (let [la (first made)
                    at (ask! c {:op :debug-cursor :unit (:unit la) :call (:call la) :id 3})
                    seen (fn [cur]
                           (first (:children (ask! c {:op :inspect :id 4
                                                      :source {:debug {:cursor (select-keys cur [:unit :call :at])}}}))))]
                (is (= [["call" "lc/la" 1 1] ["call" "lf" 1 1]]
                       (mapv (juxt :kind :label :index :of) (:frames at))))
                (is (= ["[x]" "[1]"] ((juxt :key :value) (seen at))))
                (is (= ["returned" "2"]
                       ((juxt :key :value) (seen (ask! c (merge {:op :debug-cursor :id 5 :move "next"}
                                                                (select-keys at [:unit :call :at])))))))))
            (testing "defined again, it is recorded still"
              (ret r "(defn la [x] (+ x 2))")
              (is (= "3" (ret r "(lf 1)")))
              (is (= ["lc/la" ["1"] "3" nil]
                     (said (first (:calls (ask! c {:op :debug-calls :unit f
                                                   :call (:call (last (roots "lf"))) :id 6})))))))
            (testing "and let go of with the last code recorded"
              (ask! c {:op :debug-uninstrument :unit f :id 7})
              (is (= "false" (wrapped? "lc/la")))
              (is (= "false" (wrapped? "lc/lt")))
              (is (= "3" (ret r "(lc/la 1)")))))
          (finally (disconnect r) (disconnect c) (.delete file)))))))

(deftest the-library-functions-code-calls-are-found
  (client/with-process [info nil]
    (let [r (client/repl-client info nil 60000)
          c (control info)
          root (source-root!)]
      (try
        (if-not (true? (:analysis (ask! c {:op :process-info :id 1})))
          (println "replique.debug-test: no model to read callees from - run with :analysis")
          (let [a (written-file! root "lcs/a.clj"
                                 (str "(ns lcs.a (:require [clojure.string :as str]))\n"
                                      "(defn low [x] (str/upper-case x))\n"
                                      "(defn mid [x] (inc (count (low x))))\n"
                                      "(defn top [] (mid \"a\"))\n"))]
            (eval! r (str "#replique/load " (pr-str {:file a})))
            (testing "the functions around one, and where they are"
              (let [{:keys [vars callers callees]}
                    (ask! c {:op :recording-scope :around "lcs.a/mid" :id 2})]
                (is (= ["lcs.a/top"] callers))
                (is (= ["lcs.a/low"] callees))
                (is (= [["lcs.a/mid" a] ["lcs.a/top" a] ["lcs.a/low" a]]
                       (mapv (juxt :var :file) vars)))))
            (testing "a library's functions they call, recorded by their calls - and never
                      clojure.core's"
              (is (= {:recorded ["clojure.string/upper-case"] :skipped []}
                     (select-keys (ask! c {:op :record-calls :callees-of ["lcs.a/low" "lcs.a/mid"] :id 3})
                                  [:recorded :skipped]))))))
        (finally
          (ask! c {:op :debug-uninstrument :id 9})
          (disconnect r) (disconnect c)
          (client/delete-recursively (.getParent (java.io.File. ^String root))))))))

(deftest a-cursor-goes-through-what-was-recorded
  (if-not (debug/instrumenting?)
    (println "replique.debug-test: no compiler to record with - run with :analysis")
    (client/with-process [info nil]
      (let [r (client/repl-client info nil 60000)
            c (control info)
            file (java.io.File/createTempFile "cursor" ".clj")
            record (fn [line code]
                     (ret r (str "#replique/src {:file " (pr-str (str file)) " :line " line
                                 " :instrument true :entry false :record true}\n" code))
                     (:unit (await-event c "debug-instrumented")))
            cursor (fn [from & {:as msg}]
                     (ask! c (merge {:op :debug-cursor :id 1}
                                    (select-keys from [:unit :call :at])
                                    msg)))
            frames (fn [cur] (mapv (juxt :kind :label :index :of) (:frames cur)))
            seen (fn [{:keys [unit call at frame]}]
                   (let [o (ask! c {:op :inspect :id 2
                                    :source {:debug {:cursor {:unit unit :call call :at at
                                                              :frame frame}}}})
                         label (first (:children o))
                         locals (some #(when (= "locals" (:key %)) %) (:children o))]
                     [(:key label) (:value label)
                      (when locals
                        (mapv (juxt :key :value)
                              (:children (ask! c {:op :inspect-children :view (:view o)
                                                  :node (:node locals) :id 3}))))]))]
        (try
          (let [g (record 1 "(defn cg [x] (* x 10))")
                f (record 2 "(defn cf [xs]
  (let [total (atom 0)]
    (doseq [x xs]
      (swap! total + (cg x)))
    @total))")
                _ (ret r "(cf [1 2 3])")
                start (cursor {:unit f})
                site-at (fn [line column]
                          (:site (some #(when (= [line column] [(:line %) (:column %)]) %)
                                       (:values (ask! c {:op :debug-recorded :unit f :id 4})))))
                cg-x (cursor {:unit f} :site (site-at 5 22))]
            (testing "a cursor starts at the first value of the last call: what it was called with"
              (is (= ["[xs]" "[[1 2 3]]" [["xs" "[1 2 3]"]]] (seen start)))
              (is (= [["call" "cf" 1 1]] (frames start))))
            (testing "and goes from value to value"
              (let [after (cursor start :move "next")]
                (is (= "(atom 0)" (first (seen after))))
                (is (= (:at start) (:at (cursor after :move "previous"))))
                (is (= "no-move" (:error (cursor start :move "previous"))))))
            (testing "at a form, at its last value - in the loops it is in"
              (is (= ["(cg x)" "30"] (take 2 (seen cg-x))))
              (is (= [["loop" "doseq x" 3 3] ["call" "cf" 1 1]] (frames cg-x)))
              (testing "each frame at it"
                (is (= [["(cg x)" (site-at 5 22)] ["(cg x)" (site-at 5 22)]]
                       (mapv (juxt :code :form) (:frames cg-x))))))
            (testing "to the same form in another iteration, with the locals of that one"
              (let [before (cursor cg-x :move "previous-iteration")]
                (is (= ["(cg x)" "20" [["xs" "[1 2 3]"] ["total" "#Atom 60"] ["x" "2"]]]
                       (seen before)))
                (is (= [["loop" "doseq x" 2 3] ["call" "cf" 1 1]] (frames before)))
                (testing "into the call made for a value, and back out of it"
                  (let [in (cursor before :move "in")]
                    (is (= ["[x]" "[2]" [["x" "2"]]] (seen in)))
                    (is (= [["call" "cg" 2 3] ["loop" "doseq x" 2 3] ["call" "cf" 1 1]] (frames in)))
                    (is (= [(:call before) (:at before)]
                           ((juxt :call :at) (cursor in :move "out"))))
                    (testing "and from a frame further out, to the same place in the call made
                              in the next iteration"
                      (let [next (cursor in :frame 1 :move "next-iteration")]
                        (is (= 1 (:frame next)))
                        (is (= ["[x]" "[3]" [["x" "3"]]] (seen (assoc next :frame 0))))
                        (is (= [["call" "cg" 3 3] ["loop" "doseq x" 3 3] ["call" "cf" 1 1]]
                               (frames next)))
                        (testing "what the frame is at is what is seen of it"
                          (is (= ["(cg x)" "30"] (take 2 (seen next)))))))
                    (testing "or the call of the same function before"
                      (let [first-call (cursor in :move "previous-call")]
                        (is (= ["[x]" "[1]" [["x" "1"]]] (seen first-call)))
                        (is (= [["call" "cg" 1 3] ["loop" "doseq x" 1 3] ["call" "cf" 1 1]]
                               (frames first-call)))
                        (is (= "no-move" (:error (cursor first-call :move "previous-call"))))))
                    (testing "past the last value of a call is the value it was made for"
                      (let [returned (cursor in :move "next")]
                        (is (= ["(* x 10)" "20" [["x" "2"]]] (seen returned)))
                        (is (= [(:call before) (:at before)]
                               ((juxt :call :at) (cursor returned :move "next"))))))
                    (testing "or a call of the same function, by its number among the ones kept"
                      (let [third (cursor in :move "call" :index 3)]
                        (is (= ["[x]" "[3]" [["x" "3"]]] (seen third)))
                        (is (= [["call" "cg" 3 3] ["loop" "doseq x" 3 3] ["call" "cf" 1 1]]
                               (frames third))))
                      (is (= "no-move" (:error (cursor in :move "call" :index 7)))))
                    (testing "what the form of a frame was in each iteration of its loop"
                      (let [l (:loop (second (:frames in)))
                            view (fn [source] (get-in (ask! c {:op :inspect :id 6
                                                                :source {:debug source}})
                                                      [:root :value]))]
                        (is (= "iteration 1 10  iteration 2 20  iteration 3 30"
                               (view {:loop l :call (:call before) :site (site-at 5 22) :only true})))
                        (testing "and the names bound in it"
                          (is (= [["iteration 1" "x 1  (cg x) 10"] ["iteration 2" "x 2  (cg x) 20"]
                                  ["iteration 3" "x 3  (cg x) 30"]]
                                 (mapv (juxt :key :value)
                                       (:children (ask! c {:op :inspect :id 6
                                                           :source {:debug {:loop l :call (:call before)
                                                                            :site (site-at 5 22)}}}))))))
                        (testing "of a name of the loop, that name"
                          (is (= "iteration 1 1  iteration 2 2  iteration 3 3"
                                 (view {:loop l :call (:call before) :site l :only true}))))))))))
            (testing "a gap in what is kept is said"
              (let [l (record 10 "(defn cl [] (dotimes [i 30] (inc i)))")
                    _ (ret r "(cl)")
                    at (fn [line column]
                         (cursor {:unit l}
                                 :site (:site (some #(when (= [line column] [(:line %) (:column %)]) %)
                                                    (:values (ask! c {:op :debug-recorded :unit l :id 5}))))))
                    ninth (cursor (at 10 29) :move "iteration" :index 9)]
                (testing "a loop bound its name once more than it went round: the last time is
                          the one that ends it"
                  (is (= [["loop" "dotimes i" 9 31] ["call" "cl" 1 1]] (frames ninth))))
                (let [next (cursor ninth :move "next-iteration")]
                  (is (= "Iterations 10 to 21 are not kept" (:note next)))
                  (is (= [["loop" "dotimes i" 22 31] ["call" "cl" 1 1]] (frames next))))
                (let [tenth (cursor (cursor ninth :move "next") :move "next")]
                  (is (= ["(inc i)" "10"] (take 2 (seen tenth))))
                  (is (= "21 values in between are not kept" (:note (cursor tenth :move "next")))))))
            (testing "a call not kept any more is said, and the last one is shown"
              (let [gone (cursor {:unit f :call 999999 :at 1} :move "next")]
                (is (= "That call is not kept any more - this is the last one" (:note gone)))
                (is (= (:call start) (:call gone))))))
          (finally (disconnect r) (disconnect c) (.delete file)))))))
