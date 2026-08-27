(ns replique.control-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as djson]
            [clojure.string :as string]
            [replique.core :as core]
            [replique.protocol :as protocol]
            [replique.test-client :as client
             :refer [connect send! recv request! disconnect with-process
                     temp-dir delete-recursively]])
  (:import [java.net Socket]
           [java.nio.file Files Path Paths]
           [java.nio.file.attribute FileAttribute]))

(defn- control-client [info] (client/control-client info))

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
  (testing "what follows the :hello on its line would be lost - a :repl
  connection reads the rest of the stream itself"
    (with-process [info nil]
      (let [client (connect info)]
        (try
          (send! client "{:op :hello :role :control :id 1}{:op :echo :id 2 :value 1}")
          (is (= "invalid-message" (:error (recv client))))
          (is (= :eof (recv client)))
          (finally (disconnect client)))))))

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
