(ns replique.protocol
  "Wire protocol: EDN in, newline delimited JSON out.

  Clients send one EDN map per message. Replique answers with one JSON object
  per line - JSON strings escape newlines, thus a raw newline is always a frame
  boundary and a client can look for it without invoking its JSON parser.

  Frames are tagged:
    reply - answer to a request, correlated by :id
    error - the request could not be handled
    event - unsolicited (tap, output, ...)"
  (:require [clojure.edn :as edn]
            [replique.json :as json])
  (:import [clojure.lang LineNumberingPushbackReader]
           [java.io IOException PushbackReader StringReader Writer]
           [java.util.concurrent.locks ReentrantLock]))

(def ^:private eof ::eof)

(def ^:private read-opts
  {:eof eof
   ;; Do not fail on tags we don't know about, the op handler will complain
   ;; about the unexpected value instead - which is a better error message and
   ;; keeps the connection alive.
   :default (fn [tag value] {:replique/unknown-tag (str tag) :replique/value value})})

(defn eof? [msg]
  (identical? eof msg))

(defn read-line!
  "Read one line from the connection. Returns ::eof when the client is gone.

  Messages are framed by newlines: the EDN reader is handed one line at a
  time, never the socket itself. A malformed message can then not reach past
  its own newline."
  [{:keys [^LineNumberingPushbackReader in]}]
  (try (or (.readLine in) eof)
       ;; The client is gone, or the connection is being closed
       (catch IOException _ eof)))

(defn read-messages
  "Read the EDN messages a line holds - usually exactly one. Returns
  [messages error]: error is nil when the whole line could be read, otherwise
  it is what the reader threw and the rest of the line is abandoned. Messages
  read before an error are kept."
  [^String line]
  (let [r (PushbackReader. (StringReader. line))]
    (loop [messages []]
      (let [msg (try (edn/read read-opts r) (catch Throwable t t))]
        (cond
          (eof? msg) [messages nil]
          (instance? Throwable msg) [messages msg]
          :else (recur (conj messages msg)))))))

(defn read-error-message
  "The text of a reader failure. StackOverflowError, on a deeply nested value,
  has no message."
  ^String [^Throwable t]
  (or (.getMessage t) (.getName (class t))))

(defn valid-id?
  "Correlation ids travel back to the client as JSON. Anything else than a
  string or a number would either come back as something the client cannot
  match - a keyword becomes a string - or not come back at all."
  [id]
  (or (nil? id) (string? id) (number? id)))

;;; Frames

(defn frame
  "Build a frame, dropping the keys whose value is nil. The protocol relies on
  the absence of a key rather than on null - emacs's json parser maps both
  null and false to sentinel objects, absent keys are simpler to handle."
  [m]
  (persistent!
   (reduce-kv (fn [acc k v] (if (nil? v) acc (assoc! acc k v)))
              (transient {}) m)))

(defn exception->data
  ([t] (exception->data t 0))
  ([^Throwable t depth]
   (frame
    {:class (.getName (class t))
     :message (.getMessage t)
     :data (when (instance? clojure.lang.IExceptionInfo t)
             (pr-str (ex-data t)))
     :trace (mapv str (take 64 (.getStackTrace t)))
     :cause (when (and (< depth 8) (.getCause t))
              (exception->data (.getCause t) (inc depth)))})))

(defn reply
  "A reply frame for the request msg. m is merged into the frame - the framing
  keys win, an op cannot corrupt them."
  [msg m]
  (frame (merge m {:tag "reply" :id (:id msg) :op (:op msg)})))

(defn error
  "An error frame for the request msg. kind is a machine readable keyword."
  ([msg kind message] (error msg kind message nil))
  ([msg kind message m]
   (frame (merge m {:tag "error"
                    :id (:id msg)
                    :op (:op msg)
                    :error kind
                    :message message}))))

(defn exception-error [msg ^Throwable t]
  (error msg :exception (or (.getMessage t) (.getName (class t)))
         {:exception (exception->data t)}))

(defn event [event-name m]
  (frame (merge m {:tag "event" :event event-name})))

;;; Writing

(defn- frame->line
  "Serializing may fail - an op returning a value that has no JSON
  representation. The failure is itself reported as a frame, which must be
  serializable whatever the request was: its id is kept only when it is a
  scalar."
  ^String [f]
  (try
    (json/write-str f)
    (catch Throwable t
      (let [id (:id f)
            fallback (error {:id (when (valid-id? id) id)}
                            :unserializable-frame
                            (str "Could not serialize a " (:tag f) " frame: "
                                 (.getMessage t)))]
        (try (json/write-str fallback)
             (catch Throwable _
               (json/write-str {:tag "error"
                                :error "unserializable-frame"
                                :message "Could not serialize a frame"})))))))

(defn write-frame!
  "Serialize the frame, then write it - followed by a newline - to the
  connection. Serializing before taking the lock guarantees that a frame that
  cannot be serialized does not leave a truncated line on the wire."
  [{:keys [^Writer out ^ReentrantLock lock] :as conn} f]
  (let [^String line (frame->line f)]
    (.lock lock)
    (try
      (.write out line)
      (.write out "\n")
      (.flush out)
      ;; The client is gone or is not reading anymore. Nothing useful can be
      ;; done here, the connection loop will notice on its next read.
      (catch java.io.IOException _)
      (finally (.unlock lock)))))

;;; Dispatch

(defn normalize-op [op]
  (cond
    (keyword? op) op
    (string? op) (keyword op)
    (symbol? op) (keyword (str op))
    :else nil))

(defmulti handle
  "Handle a request. Returns the map to be merged into the reply frame, or
  ::no-reply when the op answers by itself. Exceptions are turned into error
  frames by the caller."
  (fn [conn msg] (:op msg)))

(def no-reply ::no-reply)

(defmethod handle :default [_ msg]
  (throw (ex-info (str "Unknown op: " (pr-str (:op msg)))
                  {:replique/error :unknown-op})))

(defmulti accept-role
  "Take over a connection once the handshake has been validated. The method is
  responsible for sending the reply to the :hello message."
  (fn [conn hello] (:role hello)))
