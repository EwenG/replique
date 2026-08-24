(ns replique.control-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as djson]
            [replique.core :as core])
  (:import [java.io BufferedReader BufferedWriter InputStreamReader OutputStreamWriter]
           [java.net Socket]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files Path Paths]
           [java.nio.file.attribute FileAttribute]))

;;; Test client - the reference implementation of what an editor does:
;;; write EDN, read newline delimited JSON.

(defn connect [{:keys [host port]}]
  (let [socket (doto (Socket. ^String host (int port))
                 (.setSoTimeout 10000))]
    {:socket socket
     :in (BufferedReader. (InputStreamReader. (.getInputStream socket)
                                              StandardCharsets/UTF_8))
     :out (BufferedWriter. (OutputStreamWriter. (.getOutputStream socket)
                                                StandardCharsets/UTF_8))}))

(defn send! [{:keys [^BufferedWriter out]} msg]
  (.write out (if (string? msg) msg (pr-str msg)))
  (.write out "\n")
  (.flush out)
  nil)

(defn recv
  "Read one frame. Returns :eof when the process closed the connection."
  [{:keys [^BufferedReader in]}]
  (if-let [line (.readLine in)]
    (djson/read-str line :key-fn keyword)
    :eof))

(defn request! [client msg]
  (send! client msg)
  (recv client))

(defn disconnect [{:keys [^Socket socket]}]
  (try (.close socket) (catch Exception _)))

(defn- temp-dir []
  (str (Files/createTempDirectory "replique-test" (make-array FileAttribute 0))))

(defn- delete-recursively [dir]
  (doseq [p (reverse (iterator-seq (.iterator (Files/walk (Paths/get (str dir) (make-array String 0))
                                                          (make-array java.nio.file.FileVisitOption 0)))))]
    (try (Files/deleteIfExists ^Path p) (catch Exception _))))

(defmacro with-process
  "Start a process, bind its info, stop it - and clean up its directory."
  [[info-sym opts] & body]
  `(let [dir# (temp-dir)
         ~info-sym (core/start! (merge {:directory dir#} ~opts))]
     (try ~@body
          (finally (core/stop!) (delete-recursively dir#)))))

(defn- control-client [info]
  (let [client (connect info)
        hello (request! client {:op :hello :role :control :id 0})]
    (assoc client :hello hello)))

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

(deftest repl-role-is-not-implemented-yet
  (with-process [info nil]
    (let [client (connect info)]
      (try
        (let [reply (request! client {:op :hello :role :repl :id 1})]
          (is (= "not-implemented" (:error reply))))
        (finally (disconnect client))))))

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
        ;; What is left of the line is dropped: the trailing request is lost
        (let [reply (request! client
                              "{:op :echo :value #<unreadable>} {:op :echo :id 1 :value 1}")]
          (is (= "error" (:tag reply)))
          (is (= "malformed-message" (:error reply))))
        ;; ... but the connection is still usable
        (let [reply (request! client {:op :echo :id 2 :value 1})]
          (is (= "reply" (:tag reply)))
          (is (= 2 (:id reply))))
        ;; The next line is not swallowed
        (send! client "#<garbage>\n{:op :echo :id 3 :value 1}")
        (is (= "malformed-message" (:error (recv client))))
        (is (= 3 (:id (recv client))))
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

(deftest messages-may-span-several-lines
  (with-process [info nil]
    (let [client (control-client info)]
      (try
        (send! client "{:op :echo\n :id 9\n :value 1}")
        (is (= 9 (:id (recv client))))
        (finally (disconnect client))))))

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
  (testing "requests are handled on a worker pool: a client that sends its
  requests and immediately closes its side of the socket must still get its
  replies"
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
  (testing "a relative :port-file is resolved once, when the process starts"
    (let [dir (temp-dir)
          info (core/start! {:directory dir :port-file "replique-test.port"})]
      (try
        (is (.exists (java.io.File. (System/getProperty "user.dir") "replique-test.port")))
        (finally
          (core/stop!)
          (.delete (java.io.File. (System/getProperty "user.dir") "replique-test.port"))
          (delete-recursively dir)))))
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

(deftest concurrent-connections
  (with-process [info nil]
    (let [clients (repeatedly 4 #(control-client info))]
      (try
        (is (= 4 (count (distinct (map (comp :connection :hello) clients)))))
        (doseq [[i client] (map-indexed vector clients)]
          (is (= i (:id (request! client {:op :echo :id i :value i})))))
        (finally (run! disconnect clients))))))
