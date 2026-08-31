(ns replique.control-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as djson]
            [clojure.string :as string]
            [replique.control]
            [replique.core :as core]
            [replique.main]
            [replique.protocol :as protocol]
            [replique.ops]
            [replique.output]
            [replique.state :as state]
            [replique.test-client :as client
             :refer [connect send! recv request! disconnect with-process
                     temp-dir delete-recursively]])
  (:import [java.net Socket]
           [java.nio.file Files Path Paths]
           [java.nio.file.attribute FileAttribute]))

(defn- control-client [info] (client/control-client info))

;;; The exit

(def ^:private exit-watch
  "The promise the next exit delivers, when a test is watching for one."
  (atom nil))

;; Installed for the whole namespace, and never restored. The tests run inside
;; the process they test, so a real exit ends the test run - and the exit
;; happens on a thread of its own, which may still be sleeping when the test
;; that started it is over. A stub scoped to that test would already have been
;; restored by then, and the run would end in the middle of itself with the
;; status of a run that passed: the worst way a suite can fail. Nothing here
;; ever wants the real one.
(alter-var-root #'replique.ops/exit!
                (constantly (fn [] (when-let [p @exit-watch] (deliver p true)))))

(defn- watch-exit!
  "Return a promise delivered when the process would have gone."
  []
  (let [p (promise)] (reset! exit-watch p) p))

(defn- with-lock-held
  "Run f while another thread owns the connection. The lock is reentrant, so
  holding it on the calling thread would not make the connection look busy."
  [{:keys [^java.util.concurrent.locks.ReentrantLock lock]} f]
  (let [held (java.util.concurrent.CountDownLatch. 1)
        release (java.util.concurrent.CountDownLatch. 1)
        holder (future (.lock lock) (.countDown held) (.await release) (.unlock lock))]
    (.await held)
    (try (f) (finally (.countDown release) @holder))))

;;; Handshake

(deftest hello
  (with-process [info nil]
    (let [client (connect info)]
      (try
        (let [reply (request! client {:op :hello :role :control :id 1})]
          (is (= "reply" (:tag reply)))
          (is (= 1 (:id reply)))
          (is (= "hello" (:op reply)))
          (is (= "control" (:role reply)))
          (is (= (:process-id info) (:process-id reply)))
          (is (= 1 (:protocol-version reply)))
          (is (= (:port info) (:port reply)))
          (is (string? (:connection reply)))
          (is (= (clojure-version) (:clojure-version reply))))
        (finally (disconnect client))))))

(deftest hello-is-mandatory
  (with-process [info nil]
    (let [client (connect info)]
      (try
        (let [reply (request! client {:op :echo :value 1 :id 1})]
          (is (= "error" (:tag reply)))
          (is (= "expected-hello" (:error reply))))
        (is (= :eof (recv client)))
        (finally (disconnect client))))))

(deftest unknown-role
  (with-process [info nil]
    (let [client (connect info)]
      (try
        (let [reply (request! client {:op :hello :role :nope :id 1})]
          (is (= "unsupported-role" (:error reply)))
          (is (= ["control" "repl"] (:supported-roles reply))))
        (finally (disconnect client))))))

(deftest hello-must-be-alone-on-its-line
  (testing "a line holding more than the :hello is rejected rather than
  half read. What follows the handshake on a :repl connection is code, not a
  message, so a client batching the two has misunderstood the connection -
  and evaluating the batched form, or silently dropping it, would both be
  worse than saying so."
    (with-process [info nil]
      (doseq [line ["{:op :hello :role :control :id 1}{:op :echo :id 2 :value 1}"
                    "{:op :hello :role :repl :id 1} (+ 40 2)"]]
        (let [client (connect info)]
          (try
            (send! client line)
            (is (= "invalid-message" (:error (recv client))) line)
            (is (= :eof (recv client)) line)
            (finally (disconnect client))))))))

(deftest process-id-mismatch
  (with-process [info nil]
    (let [client (connect info)]
      (try
        (let [reply (request! client {:op :hello :role :control
                                      :process-id "some-other-process" :id 1})]
          (is (= "process-id-mismatch" (:error reply)))
          (is (= (:process-id info) (:process-id reply))))
        (finally (disconnect client))))))

(deftest client-provided-process-id
  (with-process [info {:process-id "my-project"}]
    (is (= "my-project" (:process-id info)))
    (let [client (connect info)]
      (try
        (let [reply (request! client {:op :hello :role :control
                                      :process-id "my-project" :id 1})]
          (is (= "reply" (:tag reply))))
        (finally (disconnect client)))))
  (testing "a process id must be usable as a file name"
    (is (thrown? clojure.lang.ExceptionInfo (core/normalize-opts {:process-id "../evil"})))
    (is (thrown? clojure.lang.ExceptionInfo (core/normalize-opts {:process-id ""})))))

;;; Ops

(deftest echo
  (with-process [info nil]
    (let [client (control-client info)]
      (try
        (let [reply (request! client {:op :echo :id 2 :value {:a [1 "two" :three]}})]
          (is (= "reply" (:tag reply)))
          (is (= 2 (:id reply)))
          (is (= "echo" (:op reply)))
          (testing "JSON is lossy: keywords come back as strings"
            (is (= {:a [1 "two" "three"]} (:value reply))))
          (testing ":printed is what the process read, as EDN"
            (is (= "{:a [1 \"two\" :three]}" (:printed reply)))))
        (testing "string ids are supported"
          (is (= "abc" (:id (request! client {:op :echo :id "abc" :value 1})))))
        (finally (disconnect client))))))

(deftest process-info
  (with-process [info nil]
    (let [client (control-client info)]
      (try
        (let [reply (request! client {:op :process-info :id 3})]
          (is (= (:process-id info) (:process-id reply)))
          (is (= (:pid info) (:pid reply)))
          (is (number? (:uptime reply)))
          (is (= (System/getProperty "java.version") (:java-version reply))))
        (finally (disconnect client))))))

(deftest unknown-op
  (with-process [info nil]
    (let [client (control-client info)]
      (try
        (let [reply (request! client {:op :not-an-op :id 4})]
          (is (= "error" (:tag reply)))
          (is (= 4 (:id reply)))
          (is (= "unknown-op" (:error reply)))
          (is (re-find #"not-an-op" (:message reply))))
        (finally (disconnect client))))))

;;; Framing and error recovery

(deftest invalid-messages-do-not-close-the-connection
  (with-process [info nil]
    (let [client (control-client info)]
      (try
        (testing "the reader is still in sync after a well formed, invalid message"
          (let [reply (request! client "[1 2 3]")]
            (is (= "invalid-message" (:error reply))))
          (let [reply (request! client {:id 5 :value 1})]
            (is (= "invalid-message" (:error reply)))
            (is (= 5 (:id reply))))
          (is (= "reply" (:tag (request! client {:op :echo :id 6 :value 1})))))
        (finally (disconnect client))))))

(deftest malformed-edn-does-not-close-the-connection
  (with-process [info nil]
    (let [client (control-client info)]
      (try
        (testing "the rest of the line is abandoned, so the trailing request is lost"
          (let [reply (request! client
                                "{:op :echo :value #<unreadable>} {:op :echo :id 1 :value 1}")]
            (is (= "error" (:tag reply)))
            (is (= "malformed-message" (:error reply))))
          (is (= 2 (:id (request! client {:op :echo :id 2 :value 1})))))
        (testing "a form that only fails after having consumed the newline does
        not eat the message that follows it - reading straight from the socket,
        the recovery skipped to the end of a line it had already left"
          (send! client "#\n{:op :echo :id 3 :value 1}")
          (is (= "malformed-message" (:error (recv client))))
          (is (= 3 (:id (recv client)))))
        (testing "an unterminated string cannot swallow what follows it -
        reading straight from the socket, it kept every later message as string
        content and the connection never answered again"
          (send! client "{:op :echo :id 4 :value \"oops")
          (is (= "malformed-message" (:error (recv client))))
          (is (= 5 (:id (request! client {:op :echo :id 5 :value 1})))))
        (finally (disconnect client))))))

(defn- written [^java.io.StringWriter out]
  (if (= "" (str out))
    []
    (mapv #(djson/read-str % :key-fn keyword) (string/split-lines (str out)))))

(deftest a-handler-that-does-not-answer-with-a-map-says-what-it-did-answer
  (testing "the branch is there for an op that is being written, so its
  message is read by whoever is writing that op. nil is the likeliest way to
  reach it - a handler whose body ends in a when that was false - and it is
  the one value (type v) cannot describe"
    (doseq [[res expected] [[nil #"returned nil$"]
                            ["not a map" #"returned a java.lang.String$"]
                            [:nope #"returned a clojure.lang.Keyword$"]]]
      (let [out (java.io.StringWriter.)
            conn (merge {:out out} (protocol/outbox))]
        (with-redefs [protocol/handle (fn [_ _] res)]
          (#'replique.control/handle-request conn {:op :being-written :id 1}))
        (let [[frame & more] (written out)]
          (is (nil? more))
          (is (= "error" (:tag frame)) (pr-str res))
          (is (= "invalid-handler-result" (:error frame)) (pr-str res))
          (is (= 1 (:id frame)) (pr-str res))
          (is (re-find expected (:message frame)) (pr-str res)))))))

(deftest events-never-pause-their-producer
  (testing "an event comes from a thread of the application being worked on -
  a background thread printing, a tapped value. It must never wait for
  whoever owns the connection."
    (testing "a short burst against a busy connection is not lost, it waits"
      (let [out (java.io.StringWriter.)
            conn (merge {:out out} (protocol/outbox))]
        (with-lock-held conn
          (fn [] (dotimes [i 10]
                   (protocol/emit-event! conn (protocol/event "out" {:line i})))))
        (is (= "" (str out)) "nothing was written, and nothing waited")
        (protocol/try-flush! conn)
        (let [frames (written out)]
          (is (= 10 (count frames)))
          (is (= (range 10) (map :line frames))))))

    (testing "only a client that stays behind loses events, and the loss is
    reported after everything that survived - so the gap can be rendered in
    place"
      (let [out (java.io.StringWriter.)
            conn (merge {:out out} (protocol/outbox))
            extra 500]
        (with-lock-held conn
          (fn [] (dotimes [i (+ protocol/max-queued-events extra)]
                   (protocol/emit-event! conn (protocol/event "out" {:line i})))))
        (is (= "" (str out)))
        (protocol/try-flush! conn)
        (let [frames (written out)]
          (is (= (inc protocol/max-queued-events) (count frames)))
          (is (= (range protocol/max-queued-events) (map :line (butlast frames))))
          (is (= "dropped" (:event (last frames))))
          (is (= extra (:count (last frames)))))))))

(deftest nothing-is-lost-or-torn-under-concurrency
  (testing "many producers contend for one connection: every line must be a
  whole frame, no reply may go missing, and every event must be either written
  or counted as dropped"
    (let [out (java.io.StringWriter.)
          conn (merge {:out out} (protocol/outbox))
          producers 8
          per-producer 500
          replies 200
          threads (conj (vec (for [_ (range producers)]
                               (future (dotimes [i per-producer]
                                         (protocol/emit-event! conn
                                          (protocol/event "out" {:line i}))))))
                        (future (dotimes [i replies]
                                  (protocol/write-frame! conn
                                   (protocol/reply {:id i :op :echo} {:value i})))))]
      (run! deref threads)
      (protocol/try-flush! conn)
      (let [frames (written out)
            by-tag (frequencies (map :tag frames))
            dropped (reduce + 0 (keep #(when (= "dropped" (:event %)) (:count %)) frames))
            markers (count (filter #(= "dropped" (:event %)) frames))]
        (testing "every line parsed, so no frame was torn by another writer"
          (is (= (count frames) (reduce + (vals by-tag)))))
        (testing "no reply was lost"
          (is (= replies (get by-tag "reply"))))
        (testing "every event is accounted for, written or counted"
          (is (= (* producers per-producer)
                 (+ (- (get by-tag "event" 0) markers) dropped))))))))

(deftest parked-frames-keep-the-order-they-were-produced-in
  (testing "a reply must not overtake the output that came before it: in M1 a
  REPL thread writes its out frames and then its result, and an editor showing
  the result first would be showing a lie"
    (let [out (java.io.StringWriter.)
          conn (merge {:out out} (protocol/outbox))]
      (with-lock-held conn
        (fn []
          (protocol/emit-event! conn (protocol/event "out" {:line "printed first"}))
          (protocol/write-frame! conn (protocol/reply {:id 1 :op :eval} {:value "the result"}))
          (protocol/emit-event! conn (protocol/event "out" {:line "printed last"}))))
      (protocol/try-flush! conn)
      (is (= ["event" "reply" "event"] (map :tag (written out)))))))

(deftest replies-are-parked-not-dropped
  (testing "a frame that must reach the client waits for the connection
  however long it takes, and goes out on the next write"
    (let [out (java.io.StringWriter.)
          conn (merge {:out out} (protocol/outbox))]
      (with-lock-held conn
        (fn [] (protocol/write-frame! conn (protocol/reply {:id 1 :op :echo} {:value 1}))))
      (is (= "" (str out)) "parked, not written and not dropped")
      (protocol/try-flush! conn)
      (let [frame (first (written out))]
        (is (= "reply" (:tag frame)))
        (is (= 1 (:id frame)))))))

(deftest several-messages-may-share-a-line
  (with-process [info nil]
    (let [client (control-client info)]
      (try
        (send! client "{:op :echo :id 1 :value 1}{:op :echo :id 2 :value 2}")
        (is (= 1 (:id (recv client))))
        (is (= 2 (:id (recv client))))
        (finally (disconnect client))))))

(deftest replies-come-back-in-request-order
  (with-process [info nil]
    (let [client (control-client info)]
      (try
        (doseq [i (range 50)]
          (send! client {:op :echo :id i :value i}))
        (is (= (range 50) (map (fn [_] (:id (recv client))) (range 50))))
        (finally (disconnect client))))))

(deftest unknown-tags-are-readable
  (with-process [info nil]
    (let [client (control-client info)]
      (try
        (let [reply (request! client {:op :echo :id 7 :value "#my/tag 1"})]
          (is (= "reply" (:tag reply))))
        (let [reply (request! client "{:op :echo :id 8 :value #my/tag 1}")]
          (is (= "reply" (:tag reply)))
          (is (= {:replique/unknown-tag "my/tag" :replique/value 1}
                 (read-string (:printed reply)))))
        (finally (disconnect client))))))

(deftest messages-may-not-span-several-lines
  (testing "one message per line - which is what keeps a malformed message from
  consuming the next ones. Nothing real is given up: pr-str and elisp prin1
  both escape the newlines inside strings and print collections on one line."
    (with-process [info nil]
      (let [client (control-client info)]
        (try
          (send! client "{:op :echo\n :id 9\n :value 1}")
          (is (= "malformed-message" (:error (recv client))))
          (testing "the leftover lines are read on their own and rejected in
          turn, then the connection carries on"
            (send! client {:op :echo :id 10 :value 1})
            ;; the reply to :id 10 is what stops the drain
            (let [frames (doall (take-while #(not= 10 (:id %)) (repeatedly #(recv client))))]
              (is (seq frames))
              (is (every? #(= "error" (:tag %)) frames))))
          (finally (disconnect client)))))))

(deftest a-runtime-replique-refuses-is-reported-on-stdout
  (testing "the doc promises a start-failed line on stdout whenever the
  process does not come up, and a client that spawned it reads stdout and
  nothing else. The clojure version is the one refusal replique can describe
  before it has loaded anything of its own, and reporting it to stderr alone
  would leave that client with an exit code to guess from"
    (let [seen (java.io.ByteArrayOutputStream.)
          real-out System/out
          exited (atom nil)]
      (try
        (System/setOut (java.io.PrintStream. seen true
                                             java.nio.charset.StandardCharsets/UTF_8))
        (with-redefs [replique.main/exit! (fn [status] (reset! exited status))]
          (binding [*clojure-version* {:major 1 :minor 8 :incremental 0}
                    ;; the sentence for the human, which is not what is
                    ;; under test and does not belong in the suite's output
                    *err* (java.io.StringWriter.)]
            (replique.main/-main)))
        (finally (System/setOut real-out)))
      (let [line (djson/read-str (first (string/split-lines (.toString seen "UTF-8")))
                                 :key-fn keyword)]
        (is (= "error" (:tag line)))
        (is (= "start-failed" (:error line)))
        (is (re-find #"requires clojure 1\.12" (:message line)))
        (testing "with no exception object: there is no exception, the runtime
        is simply not one replique runs on"
          (is (not (contains? line :exception)))))
      (is (= 1 @exited) "the exit code a client reads when it does not read stdout"))))

(deftest the-startup-line-is-utf8-whatever-the-terminal-encoding-is
  (testing "a client reads the startup line to find the process it started.
  Written through *out* it would follow the locale, and a process started
  without one announces a directory holding an accent as question marks"
    (let [seen (java.io.ByteArrayOutputStream.)
          ascii (java.io.PrintStream. seen true java.nio.charset.StandardCharsets/US_ASCII)
          line (str "{\"directory\":\"caf" (char 233) "\"}")]
      (#'replique.main/print-line! ascii line)
      (is (= (str line "\n") (.toString seen "UTF-8"))))))

;;; Port file

(deftest port-file-lifecycle
  (let [dir (temp-dir)
        info (core/start! {:directory dir})
        port-file (Paths/get (str dir) (into-array String [".replique" "processes"
                                                           (str (:process-id info) ".json")]))]
    (try
      (is (Files/exists port-file (make-array java.nio.file.LinkOption 0)))
      (testing "the port file describes the process"
        (is (= info (djson/read-str (slurp (str port-file)) :key-fn keyword))))
      (core/stop!)
      (is (not (Files/exists port-file (make-array java.nio.file.LinkOption 0))))
      (testing "the process can be started again"
        (core/start! {:directory dir :process-id (:process-id info)})
        (is (Files/exists port-file (make-array java.nio.file.LinkOption 0)))
        (core/stop!))
      (finally (core/stop!) (delete-recursively dir)))))

(deftest a-port-file-that-exists-is-not-taken
  (testing "the port file is what a client finds a process by, so a second
  process in one directory would take the name of the first - which would go
  on running with nothing able to reach it"
    (let [dir (temp-dir)
          port-file (Paths/get (str dir) (into-array String [".replique" "processes"
                                                             "taken.json"]))
          written "{\"process-id\":\"taken\",\"port\":1}\n"]
      (try
        (Files/createDirectories (.getParent port-file) (make-array FileAttribute 0))
        (spit (str port-file) written)
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"process-id \"taken\" is taken"
                              (core/start! {:directory dir :process-id "taken"})))
        (testing "the file of the process that is registered is left as it was"
          (is (= written (slurp (str port-file)))))
        (testing "nothing was started, so there is nothing to unwind"
          (is (not (state/started?))))
        (finally (core/stop!) (delete-recursively dir))))))

(deftest a-start-that-fails-after-the-process-is-registered-unwinds
  (testing "the server is bound and the process registered before the output
  is teed and before the port file is written. A failure in between used to
  unwind nothing: the server went on listening under a name nothing had
  written, so no client could find it, and start! refused to try again
  because as far as it could see a process was started"
    (let [dir (temp-dir)]
      (try
        (with-redefs-fn {#'replique.output/install!
                         (fn [] (throw (ex-info "the output could not be taken over" {})))}
          (fn []
            (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                  #"Could not start the process in "
                                  (core/start! {:directory dir :process-id "unwound"})))))
        (testing "nothing is left started, so the server was closed with it"
          (is (not (state/started?))))
        (testing "and the name is free: the next start is a start, not a refusal"
          (let [info (core/start! {:directory dir :process-id "unwound"})]
            (is (= "unwound" (:process-id info)))
            (is (Files/exists (Paths/get (str dir) (into-array String
                                                               [".replique" "processes"
                                                                "unwound.json"]))
                              (make-array java.nio.file.LinkOption 0)))))
        (finally (core/stop!) (delete-recursively dir))))))

(deftest shutdown-answers-and-then-the-process-goes
  (testing "a client that gets the reply knows the process accepted, and the
  exit follows it rather than the other way round. That the reply beats the
  exit over a real socket is what the editor tests check, out of process:
  here the exit is stubbed, because the tests run inside the process they
  would otherwise take with them"
    (with-process [info nil]
      (let [ctrl (control-client info)
            exited (watch-exit!)]
        (try
          ;; A delay long enough that the round trip cannot lose to it: what
          ;; is being checked is the order, not how quick a socket is
          (with-redefs [replique.ops/exit-delay-ms 1500]
            (let [reply (request! ctrl {:op :shutdown :id 1})]
              (is (= "reply" (:tag reply)))
              (is (= "shutdown" (:op reply)))
              (is (true? (:stopping reply)))
              (is (nil? (deref exited 0 nil))
                  "the reply came back first - the exit is what waits")
              (is (true? (deref exited 5000 nil)))))
          (finally (disconnect ctrl)))))))

(deftest ids-must-be-json-scalars
  (testing "an id that cannot travel back as JSON is rejected up front: it
  would either come back as something the client cannot match, or make the
  reply frame unserializable - and the request would never be answered"
    (with-process [info nil]
      (let [client (control-client info)]
        (try
          (let [reply (request! client "{:op :echo :id #inst \"2024-01-01\" :value 1}")]
            (is (= "error" (:tag reply)))
            (is (= "invalid-message" (:error reply)))
            (is (not (contains? reply :id))))
          (testing "a number JSON cannot carry is one of those ids. number?
          lets a ratio, a NaN and an infinity through, and the frame that
          would report the failure is built around that same id - so it does
          not serialize either, and what the client gets back carries no id
          at all: the request it cannot match to anything is the one it is
          waiting on"
            (doseq [id ["1/2" "##NaN" "##Inf"]]
              (let [reply (request! client (str "{:op :echo :id " id " :value 1}"))]
                (is (= "error" (:tag reply)) id)
                (is (= "invalid-message" (:error reply)) id)
                (is (not (contains? reply :id)) id)
                (is (re-find #"An :id must be" (:message reply)) id))))
          (testing "a double that JSON can carry is still an id"
            (is (= 3.5 (:id (request! client {:op :echo :id 3.5 :value 1})))))
          (testing "the connection is still usable"
            (is (= 1 (:id (request! client {:op :echo :id 1 :value 1})))))
          (finally (disconnect client)))))))

(deftest values-that-have-no-json-representation
  (testing "the op replies with a value the JSON writer rejects: the client
  gets an error frame, correlated, instead of nothing at all"
    (with-process [info nil]
      (let [client (control-client info)]
        (try
          (let [reply (request! client "{:op :echo :id 1 :value #inst \"2024-01-01\"}")]
            (is (= "error" (:tag reply)))
            (is (= 1 (:id reply)))
            (is (= "unserializable-frame" (:error reply)))
            (is (re-find #"java.util.Date" (:message reply))))
          (testing "the connection is still usable"
            (is (= 2 (:id (request! client {:op :echo :id 2 :value 1})))))
          (finally (disconnect client)))))))

(deftest replies-are-not-lost-when-the-client-half-closes
  (testing "a client that pipelines its requests and immediately closes its
  side of the socket must still get every reply"
    (with-process [info nil]
      (let [client (connect info)]
        (try
          (send! client (str (pr-str {:op :hello :role :control :id 1}) "\n"
                             (pr-str {:op :echo :id 2 :value 2}) "\n"
                             (pr-str {:op :echo :id 3 :value 3})))
          (.shutdownOutput ^Socket (:socket client))
          (let [frames (doall (take-while (complement #{:eof}) (repeatedly #(recv client))))]
            (is (= 3 (count frames)))
            (is (= #{1 2 3} (set (map :id frames)))))
          (finally (disconnect client)))))))

(deftest port-file-location
  (testing "a relative :port-file is resolved against the working directory,
  once, so that the port file does not move if the process chdirs"
    (let [{:keys [port-file]} (core/normalize-opts {:directory (temp-dir)
                                                    :port-file "replique-test.port"})]
      (is (.isAbsolute ^Path port-file))
      (is (= (str (Paths/get (System/getProperty "user.dir")
                             (into-array String ["replique-test.port"])))
             (str port-file)))))
  (testing "the permissions of a directory replique did not create are left alone"
    (let [dir (temp-dir)
          shared (Paths/get dir (into-array String ["shared"]))
          _ (Files/createDirectories shared (make-array FileAttribute 0))
          _ (Files/setPosixFilePermissions
             shared (java.nio.file.attribute.PosixFilePermissions/fromString "rwxr-xr-x"))
          info (core/start! {:directory dir
                             :port-file (str shared "/replique.json")})]
      (try
        (is (= "rwxr-xr-x"
               (java.nio.file.attribute.PosixFilePermissions/toString
                (Files/getPosixFilePermissions shared (make-array java.nio.file.LinkOption 0)))))
        (finally (core/stop!) (delete-recursively dir))))))

(deftest a-relative-directory-is-made-absolute
  (testing "the directory travels to the editor, in the port file and in the
  reply to every handshake. A \".\" there names the working directory of
  whoever reads it rather than the one the process was started in"
    (let [here (str (.normalize (.toAbsolutePath
                                 (Paths/get (System/getProperty "user.dir")
                                            (into-array String [])))))
          {:keys [directory port-file]} (core/normalize-opts {:directory "."
                                                              :process-id "rel"})]
      (is (= here directory))
      (testing "and the port file under it has no . left in its name either -
      it is the string a client compares to know which process it is looking at"
        (is (= (str (Paths/get here (into-array String [".replique" "processes"
                                                        "rel.json"])))
               (str port-file)))))))

(deftest an-option-that-is-not-one-is-refused
  (testing "dropping it would start a process, successfully, under a name the
  client did not ask for - a misspelt :process-id gives a random uuid - and
  the editor would then wait for a process that is running and cannot be
  found"
    (let [t (try (core/normalize-opts {:proces-id "my-project"})
                 nil
                 (catch clojure.lang.ExceptionInfo t t))]
      (is (some? t))
      (is (re-find #"Unknown option: :proces-id" (.getMessage ^Throwable t)))
      (is (= [:proces-id] (:unknown-options (ex-data t))))
      (testing "and it says what the options are, which is what the client
      needs to find its mistake"
        (is (re-find #":process-id" (.getMessage ^Throwable t))))))
  (testing "options that are not a map at all. A string reads as a map of
  nothing, so this used to start a process on every default"
    (let [t (try (core/normalize-opts "{:process-id \"quoted-by-mistake\"}")
                 nil
                 (catch clojure.lang.ExceptionInfo t t))]
      (is (some? t))
      (is (re-find #"options must be a map" (.getMessage ^Throwable t)))))
  (testing "the options themselves are still options"
    (is (= "ok" (:process-id (core/normalize-opts {:process-id "ok" :host "127.0.0.1"
                                                   :port 0 :directory "/tmp"}))))
    (is (string? (:process-id (core/normalize-opts nil))))))

(deftest a-host-that-is-not-a-host-is-refused
  (testing "the one option whose value used to reach java unchecked. It cannot
  be stringified the way a process id or a directory is, because every number
  is an address to getByName - 42 reads as 0.0.0.42, and 0 as every interface
  of a machine running a process that has no authentication"
    (let [t (try (core/normalize-opts {:host 42})
                 nil
                 (catch clojure.lang.ExceptionInfo t t))]
      (is (some? t))
      (is (re-find #"Invalid :host: 42" (.getMessage ^Throwable t)))
      (is (= 42 (:host (ex-data t)))))
    (testing "and the refusal names the option, where the cast it replaces
    said java.lang.Long and nothing a client could act on"
      (let [dir (temp-dir)]
        (try
          (let [t (try (core/start! {:directory dir :host 42})
                       nil
                       (catch Throwable t t))]
            (is (some? t))
            (is (re-find #"Invalid :host" (.getMessage ^Throwable t)))
            (is (not (state/started?))))
          (finally (core/stop!) (delete-recursively dir)))))
    (testing "a host that is a string is still a host"
      (is (= "0.0.0.0" (:host (core/normalize-opts {:host "0.0.0.0"}))))
      (is (= "127.0.0.1" (:host (core/normalize-opts nil)))))))

(deftest framing-keys-cannot-be-overridden
  (testing "a request cannot inject its own tag - framing must stay trustworthy
  whatever a client, or an op, puts in the map"
    (with-process [info nil]
      (let [client (control-client info)]
        (try
          (let [reply (request! client {:op :echo :id 1 :value 1 :tag "nope" :error "nope"})]
            (is (= "reply" (:tag reply)))
            (is (= "echo" (:op reply)))
            (is (= 1 (:id reply)))
            (is (not (contains? reply :error))))
          (finally (disconnect client)))))))

(deftest a-client-that-stops-reading-stalls-only-its-own-connection
  (testing "each connection has its own socket and its own lock, so a client
  that stops reading stalls its own connection and nothing else"
    (with-process [info nil]
      (let [a (control-client info)
            b (control-client info)
            big (apply str (repeat 20000 \x))]
        (try
          ;; a sends and never reads. From another thread, because once the
          ;; replies fill a's socket buffer a's own send! blocks too.
          (let [flood (future (try (dotimes [i 200] (send! a {:op :echo :id i :value big}))
                                   :sent
                                   (catch Exception _ :disconnected)))]
            (try
              (is (= "reply" (:tag (request! b {:op :echo :id "b" :value 1}))))
              (is (= 42 (:id (request! b {:op :echo :id 42 :value 1}))))
              (finally
                ;; closing a unblocks the flooding thread
                (disconnect a) (disconnect b) (deref flood 5000 :timeout))))
          (finally (disconnect a) (disconnect b)))))))

(deftest concurrent-connections
  (with-process [info nil]
    (let [clients (repeatedly 4 #(control-client info))]
      (try
        (is (= 4 (count (distinct (map (comp :connection :hello) clients)))))
        (doseq [[i client] (map-indexed vector clients)]
          (is (= i (:id (request! client {:op :echo :id i :value i})))))
        (finally (run! disconnect clients))))))

(defn- blocking-writer
  "A writer that never finishes a write, the way a socket whose client stopped
  reading does not: the send buffer fills, and BufferedWriter.write waits for
  room java gives no way to stop waiting for. Returns [writer release!]."
  []
  (let [gate (java.util.concurrent.CountDownLatch. 1)]
    [(proxy [java.io.Writer] []
       (write [& _] (.await gate))
       (flush [])
       (close []))
     (fn [] (.countDown gate))]))

(deftest the-exit-does-not-wait-for-a-client-that-stopped-reading
  (testing "the guarantee the doc makes. What the client that asked to be rid
  of this process does next is stop reading, and the write of the reply then
  never returns - there is no timeout to set on a write in java. This goes
  through the path that really writes it, so what blocks is the real write"
    (let [[out release!] (blocking-writer)
          conn (merge {:out out} (protocol/outbox))
          exited (watch-exit!)
          handle-request #'replique.control/handle-request]
      (try
        (with-redefs [replique.ops/exit-delay-ms 20]
          ;; on a thread of its own, because answering the request is what
          ;; does not come back here
          (let [answering (future (handle-request conn {:op :shutdown :id 1}))]
            (is (= :never-returned (deref answering 1000 :never-returned))
                "a connection that can be written to would make this test
                prove nothing")
            (is (true? (deref exited 5000 nil))
                "the process went, though the reply is still being written")))
        (finally (release!))))))

(deftest a-port-file-is-claimed-rather-than-overwritten
  (testing "start! looks before it writes, and between the look and the write
  is where another process fits. The write is what refuses, so the loser of
  that race fails rather than takes a name the winner is running under"
    (let [dir (temp-dir)
          port-file (Paths/get (str dir) (into-array String [".replique" "processes"
                                                             "claimed.json"]))
          write! #'core/write-port-file!]
      (try
        (write! port-file {:process-id "claimed" :port 1})
        (is (= 1 (:port (djson/read-str (slurp (str port-file)) :key-fn keyword))))
        (is (thrown? java.nio.file.FileAlreadyExistsException
                     (write! port-file {:process-id "claimed" :port 2})))
        (testing "the process that is registered keeps the file it wrote"
          (is (= 1 (:port (djson/read-str (slurp (str port-file)) :key-fn keyword)))))
        (testing "and nothing of the write that failed is left behind"
          (is (= ["claimed.json"]
                 (sort (map str (.list (.toFile (.getParent port-file))))))))
        (finally (delete-recursively dir))))))

(deftest a-start-that-loses-the-claim-leaves-the-winner-alone
  (testing "the refusal is checked before the server binds, and between that
  check and the claim is where another process fits. Losing there must cost
  the loser its start and nothing else: a loser that deleted the file it
  failed to take would leave the winner running with nothing able to reach it,
  which is worse than the overwriting the refusal replaced"
    (let [dir (temp-dir)
          port-file (Paths/get (str dir) (into-array String [".replique" "processes"
                                                             "contested.json"]))
          winner "{\"process-id\":\"contested\",\"port\":1}\n"]
      (try
        (with-redefs-fn
          {#'core/write-port-file!
           (fn [^Path pf _]
             ;; the process that won the race, writing in the window this
             ;; start walked into
             (Files/createDirectories (.getParent pf) (make-array FileAttribute 0))
             (spit (str pf) winner)
             (throw (java.nio.file.FileAlreadyExistsException. (str pf))))}
          (fn []
            (let [t (try (core/start! {:directory dir :process-id "contested"})
                         nil
                         (catch clojure.lang.ExceptionInfo t t))]
              (is (some? t))
              (testing "and it is told what the check would have told it. The
              claim is the same refusal found late, and a client that spawns
              processes must not have to read two messages to learn one thing"
                (is (re-find #"process-id \"contested\" is taken" (.getMessage ^Throwable t)))
                (is (= "contested" (:process-id (ex-data t))))))))
        (testing "the winner's port file is where the winner left it"
          (is (Files/exists port-file (make-array java.nio.file.LinkOption 0))
              "the loser deleted the file it failed to claim")
          (is (= winner (when (Files/exists port-file
                                            (make-array java.nio.file.LinkOption 0))
                          (slurp (str port-file))))))
        (testing "and the start that lost unwound itself"
          (is (not (state/started?))))
        (finally (core/stop!) (delete-recursively dir))))))

(deftest a-filesystem-without-hard-links-still-claims
  (testing "the link is what makes the claim atomic, and a filesystem that has
  no hard links answers it with a refusal rather than a link. The claim falls
  back to a move there - weaker, because a move refuses a name that is taken by
  looking before it renames rather than in one operation - but refuse it does,
  and that part must not depend on the filesystem"
    (let [dir (temp-dir)
          zip (Paths/get (str dir) (into-array String ["archive.zip"]))
          env (doto (java.util.HashMap.) (.put "create" "true"))
          claim! #'core/claim!
          bytes-of (fn [^Path p] (String. (Files/readAllBytes p) "UTF-8"))
          write! (fn [^Path p ^String s]
                   (Files/write p (.getBytes s "UTF-8")
                                (make-array java.nio.file.OpenOption 0)))]
      (try
        (with-open [fs (java.nio.file.FileSystems/newFileSystem zip env)]
          (let [first-tmp (.getPath fs "first.tmp" (into-array String []))
                second-tmp (.getPath fs "second.tmp" (into-array String []))
                target (.getPath fs "taken.json" (into-array String []))]
            (write! first-tmp "first\n")
            (write! second-tmp "second\n")
            (is (thrown? UnsupportedOperationException (Files/createLink target first-tmp))
                "a filesystem that has links would make this test prove nothing")
            (testing "the claim goes through anyway"
              (claim! first-tmp target)
              (is (= "first\n" (bytes-of target))))
            (testing "and the second process is still refused the name"
              (is (thrown? java.nio.file.FileAlreadyExistsException
                           (claim! second-tmp target)))
              (is (= "first\n" (bytes-of target))))))
        (finally (delete-recursively dir))))))
