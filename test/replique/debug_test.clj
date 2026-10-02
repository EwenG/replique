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
