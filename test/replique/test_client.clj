(ns replique.test-client
  "The reference implementation of what an editor does: write EDN, read
  newline delimited JSON."
  (:require [clojure.data.json :as djson]
            [replique.core :as core])
  (:import [java.io BufferedReader BufferedWriter InputStreamReader OutputStreamWriter]
           [java.net Socket]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files Path Paths]
           [java.nio.file.attribute FileAttribute]))

(defn connect
  ([info] (connect info 10000))
  ([{:keys [host port]} timeout]
  (let [socket (doto (Socket. ^String host (int port))
                 (.setSoTimeout (int timeout)))]
    {:socket socket
     :in (BufferedReader. (InputStreamReader. (.getInputStream socket)
                                              StandardCharsets/UTF_8))
     :out (BufferedWriter. (OutputStreamWriter. (.getOutputStream socket)
                                                StandardCharsets/UTF_8))})))

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

(defn temp-dir []
  (str (Files/createTempDirectory "replique-test" (make-array FileAttribute 0))))

(defn delete-recursively [dir]
  (doseq [p (reverse (iterator-seq (.iterator (Files/walk (Paths/get (str dir) (make-array String 0))
                                                          (make-array java.nio.file.FileVisitOption 0)))))]
    (try (Files/deleteIfExists ^Path p) (catch Exception _))))

(defmacro with-process
  "Start a process, bind its info, stop it - and clean up its directory.

  The body prints to the streams the process replaced: a test runs inside
  the process it is testing, which no editor does, and what clojure.test
  prints would otherwise be broadcast as output events - arriving on the
  control connections the test is reading frames from."
  [[info-sym opts] & body]
  `(let [dir# (temp-dir)
         out# *out*
         err# *err*
         ~info-sym (core/start! (merge {:directory dir#} ~opts))]
     (try (binding [*out* out# *err* err#] ~@body)
          (finally (core/stop!) (delete-recursively dir#)))))

(defn control-client [info]
  (let [client (connect info)
        hello (request! client {:op :hello :role :control :id 0})]
    (assoc client :hello hello)))

;;; Repl connections
;;;
;;; A repl connection is not a message channel: the client writes code and
;;; reads the frames it produces, until the prompt says the repl is ready
;;; again.

(defn repl-client
  "A repl connection, handshaken and standing at its first prompt.

  `extra' is merged into the :hello - :dialect and :target for a ClojureScript
  one. A ClojureScript handshake compiles cljs.core and starts a runtime before
  it answers, which is seconds rather than milliseconds, so the timeout is the
  caller's to raise."
  ([info] (repl-client info nil))
  ([info extra] (repl-client info extra 10000))
  ([info extra timeout]
   (let [client (connect info timeout)
         hello (request! client (merge {:op :hello :role :repl :id 0} extra))
         prompt (when (= "reply" (:tag hello)) (recv client))]
     (assoc client :hello hello :prompt prompt))))

(defn recv-until
  "Read frames until one of them has that tag, and return them all."
  [client tag]
  (loop [frames []]
    (let [f (recv client)]
      (cond
        (= :eof f) (conj frames f)
        (= tag (:tag f)) (conj frames f)
        :else (recur (conj frames f))))))

(defn eval!
  "Send code and read everything it produces, up to the next prompt."
  [client code]
  (send! client code)
  (recv-until client "prompt"))

(defn frames-tagged [frames tag]
  (filterv #(= tag (:tag %)) frames))

(defn frame-tagged [frames tag]
  (first (frames-tagged frames tag)))

(defn printed
  "What the frames say was printed on that stream."
  [frames tag]
  (apply str (map :string (frames-tagged frames tag))))
