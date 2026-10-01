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
           [java.util.concurrent ConcurrentLinkedQueue]
           [java.util.concurrent.atomic AtomicInteger AtomicLong]
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

(defn drain!
  "Consume the input the client has already sent. Closing a socket that still
  holds unread input resets the connection, and the client then loses the
  error frame that was just written to it - which is the frame that says why
  it is being closed. Never waits for more input, and gives up on a client
  that keeps sending."
  [{:keys [^LineNumberingPushbackReader in]}]
  (try
    (loop [n 0]
      (when (and (< n 1000000) (.ready in) (not (neg? (.read in))))
        (recur (inc n))))
    (catch IOException _ nil)))

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

(defn- json-number?
  "The numbers the json writer accepts. A ratio has no JSON representation,
  and neither has a NaN or an infinity - number? alone lets all three through."
  [x]
  (cond
    (ratio? x) false
    (or (instance? Double x) (instance? Float x)) (Double/isFinite (double x))
    :else (number? x)))

(defn valid-id?
  "Correlation ids travel back to the client as JSON. Anything else than a
  string or a number JSON can carry would either come back as something the
  client cannot match - a keyword becomes a string - or not come back at all:
  the reply frame would not serialize, and neither would the error frame that
  says so, which is built around that same id."
  [id]
  (or (nil? id) (string? id) (json-number? id)))

(defn invalid-id-message
  "Said in one place: the handshake and the control loop both check the id,
  and a client told two different things about one rule has to guess which of
  them is the rule."
  ^String [id]
  (str "An :id must be a string or a number JSON can carry, got: " (pr-str id)))

;;; Frames

(defn frame
  "Build a frame, dropping the keys whose value is nil. The protocol relies on
  the absence of a key rather than on null - emacs's json parser maps both
  null and false to sentinel objects, absent keys are simpler to handle."
  [m]
  (persistent!
   (reduce-kv (fn [acc k v] (if (nil? v) acc (assoc! acc k v)))
              (transient {}) m)))

(def ^:private max-printed-length
  "How much of an ex-data is printed into an error frame when the repl has
  set no limit of its own. The printer marks what it left out."
  1000)

(def ^:private max-printed-level 25)

(defn- printed-ex-data
  "ex-data travels as a string printed by the clojure printer, and printing a
  value can fail: a lazy seq that throws when it is realized, an object whose
  toString throws, a closed resource. That failure must not take the place of
  the exception being reported - what the editor would then show is the
  trouble replique had describing the problem rather than the problem."
  [t]
  (when (instance? clojure.lang.IExceptionInfo t)
    (try
      ;; Bounded, unless the repl was told otherwise. clojure.main does not
      ;; print ex-data when it reports an exception, so an ex-data holding a
      ;; seq that never ends - (ex-info "..." {:rows (map parse lines)}) is an
      ;; ordinary thing to write - would wedge the connection here where a
      ;; terminal repl prints the message and moves on.
      (binding [*print-length* (or *print-length* max-printed-length)
                *print-level* (or *print-level* max-printed-level)]
        (pr-str (ex-data t)))
      (catch Throwable t2
        (str "Could not be printed: "
             (or (.getMessage t2) (.getName (class t2))))))))

(def ^:private max-trace
  "How many stack frames of one exception travel in a frame. The top of the
  stack is where it was thrown, which is what a trace is read for."
  64)

(def ^:private max-cause-depth 8)

(defn exception->data
  ([t] (exception->data t 0))
  ([^Throwable t depth]
   (let [trace (.getStackTrace t)
         dropped (- (count trace) max-trace)
         cause (.getCause t)
         cut? (>= depth max-cause-depth)]
     (frame
      {:class (.getName (class t))
       :message (.getMessage t)
       :data (printed-ex-data t)
       :trace (mapv str (take max-trace trace))
       ;; What was left out is said rather than quietly dropped: an editor
       ;; showing 64 frames of a 300 frame trace, or a chain whose root cause
       ;; - the one the reported message names - was cut off, would otherwise
       ;; show them as if they were whole.
       :trace-dropped (when (pos? dropped) dropped)
       :cause (when (and cause (not cut?)) (exception->data cause (inc depth)))
       :cause-dropped (when (and cause cut?) true)}))))

(defn exception->text
  "T and the chain under it as text, the way a JVM prints one.

  THE SHAPE THE OTHER FRAME CARRIES. A ClojureScript `exception' frame has no
  `exception' object in it - a form that failed failed in another process and
  left no Throwable here - so what it carries instead is a stack as TEXT, which
  for a failure in the runtime is the string V8 printed, read back into
  ClojureScript files and lines. A failure on THIS side of the wire - a `:main'
  the compiler would not compile - does have a Throwable, and it goes in the
  same field: a client reading an exception frame knows one place to look for a
  stack, and a second shape for the same question would be one more thing to
  know rather than one more thing to show.

  BOUNDED BY THE SAME TWO NUMBERS as `exception->data', and it says what it left
  out for the same reason. Sixty four frames of a three hundred frame trace,
  printed as though they were the whole of it, is a trace that lies about where
  it ends - and the top is what a trace is read for, so the top is what is
  kept."
  ([t] (exception->text t 0))
  ([^Throwable t depth]
   (let [trace (.getStackTrace t)
         dropped (- (count trace) max-trace)
         cause (.getCause t)
         cut? (>= depth max-cause-depth)]
     (str (when (pos? depth) "Caused by: ")
          (.getName (class t))
          (when-let [m (.getMessage t)] (str ": " m))
          "\n"
          (apply str (for [e (take max-trace trace)] (str "\tat " e "\n")))
          ;; NOT THE JVM'S "... 23 more", which counts the frames a cause
          ;; has in COMMON with the trace above it. This counts the ones left
          ;; out, and two conventions one word apart would be read as one.
          (when (pos? dropped)
            (str "\t... " dropped " more frames were left out\n"))
          (cond
            (and cause (not cut?)) (exception->text cause (inc depth))
            (and cause cut?) "Caused by: ... the rest of the chain was left out\n")))))

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

(def ^:private max-buffered-output
  "Flush a buffering writer once it grows past this. A form that prints a lot
  without ever emitting a newline must not hold everything in memory."
  8192)

(defn buffering-writer
  "A Writer that hands what is written to it to EMIT!, one flush at a time.

  Buffered, and handed over per flush rather than per write: clojure flushes at
  the end of every println, so a line of output is one call to EMIT!. A form
  that prints without ever emitting a newline is handed over by size instead.

  WHAT IT IS FOR is a frame, or an event, or whichever of the two the
  destination turns out to be - which is why EMIT! is a function and not a
  connection. A repl writes its output to the client that asked for it; a
  ClojureScript runtime writes to whoever is evaluating in it at the time, and
  to nobody in particular the rest of the time.

  Thread safe, because the thing writing is the developer's own code and
  nothing says it is one thread."
  ^Writer [emit!]
  (let [sb (StringBuilder.)
        take-buffer!
        (fn [mid-output?]
          (locking sb
            (let [n (.length sb)
                  ;; A surrogate pair must never be split across two frames:
                  ;; each half alone is not valid text, and both would go out
                  ;; as U+FFFD. clojure prints a string one char at a time, so
                  ;; a long string holding an emoji lands on this. Only worth
                  ;; holding back mid-output: a flush is the end of what was
                  ;; printed, and a lone surrogate there is what the code
                  ;; really wrote.
                  n (if (and mid-output? (pos? n)
                             (Character/isHighSurrogate (.charAt sb (dec n))))
                      (dec n)
                      n)]
              (when (pos? n)
                (let [n (int n)
                      s (.substring sb 0 n)]
                  (.delete sb (int 0) n)
                  s)))))
        emit-buffer! (fn [mid-output?]
                       (when-let [s (take-buffer! mid-output?)]
                         (emit! s)))
        append! (fn [x]
                  (locking sb
                    (cond
                      (instance? String x) (.append sb ^String x)
                      (integer? x) (.append sb (char (int x)))
                      :else (.append sb ^chars x))
                    (.length sb)))
        append-range! (fn [x off len]
                        (locking sb
                          (if (instance? String x)
                            (.append sb ^String x (int off) (int (+ (int off) (int len))))
                            (.append sb ^chars x (int off) (int len)))
                          (.length sb)))]
    (proxy [Writer] []
      (write
        ([x] (when (>= (long (append! x)) max-buffered-output) (emit-buffer! true)))
        ([x off len] (when (>= (long (append-range! x off len)) max-buffered-output)
                       (emit-buffer! true))))
      (flush [] (emit-buffer! false))
      (close [] (emit-buffer! false)))))

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

(def max-queued-events
  "How many events may wait for a busy connection before they start being
  dropped. Replies are never dropped, they are not bounded."
  1024)

(defn outbox
  "The state a connection needs to be written to. There is no writer thread:
  whichever thread produces a frame writes it, if it can take the lock, and
  parks it otherwise - whoever takes the lock next writes what is waiting."
  []
  {:lock (ReentrantLock.)
   ;; frames that found the connection busy, in the order they were produced.
   ;; One queue for both kinds, because a reply must not overtake the output
   ;; that came before it - only the events in it count against the bound.
   :queue (ConcurrentLinkedQueue.)
   :queued-events (AtomicInteger. 0)
   :dropped (AtomicLong. 0)})

(defn- write-line! [{:keys [^Writer out]} ^String line]
  (.write out line)
  (.write out "\n")
  (.flush out))

(defn- flush-locked!
  "Write what is waiting. Must be called with the lock held."
  [{:keys [^ConcurrentLinkedQueue queue ^AtomicInteger queued-events
           ^AtomicLong dropped] :as conn}]
  (loop []
    (when-let [[kind ^String line] (.poll queue)]
      (when (identical? :event kind)
        (.decrementAndGet queued-events))
      (write-line! conn line)
      (recur)))
  ;; last, which is where the gap is: everything that survived has just been
  ;; written, so the client can render the loss in place
  (let [n (.getAndSet dropped 0)]
    (when (pos? n)
      (write-line! conn (frame->line (event "dropped" {:count n}))))))

(defn- writable? [{:keys [^ConcurrentLinkedQueue queue ^AtomicLong dropped]}]
  (or (not (.isEmpty queue)) (pos? (.get dropped))))

(defn try-flush!
  "Write the frames that are waiting, if the connection is free. The
  connection thread calls it before blocking on its next read, so that a frame
  parked while a producer held the lock does not wait for the next request.

  Loops, because a frame parked while this very flush was running would
  otherwise be stranded until something else happens on the connection. Gives
  up as soon as the lock is contended: whoever holds it flushes in turn."
  [{:keys [^ReentrantLock lock] :as conn}]
  (loop []
    (when (and (writable? conn) (.tryLock lock))
      (let [written? (try (flush-locked! conn)
                          true
                          ;; The client is gone or is not reading anymore. Stop
                          ;; rather than retry every queued frame: the
                          ;; connection loop will notice on its next read.
                          (catch IOException _ false)
                          (finally (.unlock lock)))]
        (when written? (recur))))))

(defn write-frame!
  "Write a frame that must reach the client - a reply, an error. When another
  thread owns the connection the frame is parked rather than dropped, and goes
  out with the next write."
  [{:keys [^ReentrantLock lock ^ConcurrentLinkedQueue queue] :as conn} f]
  (let [^String line (frame->line f)]
    (if (.tryLock lock)
      (do (try (flush-locked! conn)
               (write-line! conn line)
               (catch IOException _ nil)
               (finally (.unlock lock)))
          ;; a producer may have parked something while we held the lock
          (try-flush! conn))
      (do (.add queue [:reply line])
          ;; Parking is not enough on its own: the lock may have been released
          ;; between the tryLock that failed and this, and the flush the
          ;; holder does on its way out then ran on a queue this frame was not
          ;; in yet. Nothing else would write it - the connection thread
          ;; flushes before its next read, and it is already blocked in that
          ;; read - so it would wait for a request that may never come.
          ;;
          ;; Flushing from here closes that window rather than narrowing it:
          ;; either this takes the lock and writes, or it fails, and whoever
          ;; holds it took it before the frame was parked and so flushes it on
          ;; the way out.
          (try-flush! conn)))))

(defn emit-event!
  "Write an unsolicited frame if the connection is free, drop and count it
  otherwise. The producer is a thread of the application being worked on - a
  background thread printing, a tapped value - and dropping is the only thing
  that never pauses it."
  [{:keys [^ReentrantLock lock ^ConcurrentLinkedQueue queue
           ^AtomicInteger queued-events ^AtomicLong dropped] :as conn} f]
  ;; The bound is looked at before the frame is serialized: a queue that is
  ;; already full belongs to a client that is durably behind, which is a
  ;; client this event is going to be dropped for, and a flood must stay cheap
  ;; for the thread producing it. Looked at rather than taken - the slot is
  ;; reserved below, where the frame is parked, so that what this counts stays
  ;; the number of events waiting in the queue.
  (if (>= (.get queued-events) max-queued-events)
    (.incrementAndGet dropped)
    ;; Serialized before the connection is taken, as write-frame! does it:
    ;; holding the connection for the time a frame takes to turn into json is
    ;; holding it against every other producer, and against the thread that
    ;; owns it.
    (let [^String line (frame->line f)]
      (if (.tryLock lock)
        (try (flush-locked! conn)
             (write-line! conn line)
             (catch IOException _ nil)
             (finally (.unlock lock)))
        ;; The connection is busy - usually for the moment it takes to write
        ;; one frame. Wait in the queue rather than be lost, and count the
        ;; loss only when the client is durably behind.
        (if (<= (.incrementAndGet queued-events) max-queued-events)
          (.add queue [:event line])
          (do (.decrementAndGet queued-events)
              (.incrementAndGet dropped))))))
  ;; Whatever happened above. A producer may have parked something while we
  ;; held the lock, and - see write-frame! - what we parked ourselves while
  ;; the lock was being released is parked into a queue nobody is about to
  ;; look at. The count of what was dropped is written by the same flush, and
  ;; is stranded the same way.
  ;;
  ;; This does not make the producer wait for a client that is not reading:
  ;; the lock is taken with tryLock here as everywhere, and a connection whose
  ;; owner is blocked writing to it is a connection this returns from
  ;; immediately.
  (try-flush! conn))

;;; Dispatch

(defn as-keyword
  "The keyword a client wrote, or nil when it wrote something that is not one.

  A client with an EDN printer writes a keyword, one without writes a string,
  and one that prints its own symbols writes a symbol - the three spell the
  same name. Said once because it is asked of every named thing a message
  carries: the op of a request, the role of a handshake, the position a
  completion is asked at."
  [x]
  (cond
    (keyword? x) x
    (string? x) (keyword x)
    (symbol? x) (keyword (str x))
    :else nil))

(defn as-name
  "The name a client wrote, or nil when it wrote something that is not one.

  The same three spellings `as-keyword' reads, answered as the name itself:
  what is being asked for here is a name rather than a keyword, and a
  qualified one keeps its namespace - clojure.core/let is one name and not
  two."
  [x]
  (cond
    (string? x) x
    (symbol? x) (str x)
    (keyword? x) (subs (str x) 1)
    :else nil))

(defmulti handle
  "Handle a request. Returns the map to be merged into the reply frame, or
  ::no-reply when the op answers by itself. Exceptions are turned into error
  frames by the caller."
  (fn [conn msg] (:op msg)))

;;; The printing a repl is started with

(defn invalid-params
  "Why PARAMS cannot be what a repl is started with, or nil when they can.

  KINDS maps each param a repl takes to what it takes - :count or :boolean -
  and is the repl's to say: a Clojure repl takes `:warn-on-reflection' and a
  ClojureScript one has no such thing. The names are the ones every prompt
  reports them under, so what a prompt said can be handed back as it is.

  Nil is a value of every param: a count of nil is no limit, and a boolean of
  nil is false - which is what a client reading the prompt's JSON with null
  and false as one value hands back."
  [kinds params]
  (cond
    (nil? params) nil
    (not (map? params)) "they are not a map"
    :else
    (some (fn [[k v]]
            (case (get kinds k)
              nil (str (pr-str k) " is not one of them")
              :count (when-not (or (nil? v) (nat-int? v))
                       (str (pr-str k) " is a count or nil, and was "
                            (pr-str v)))
              :boolean (when-not (or (nil? v) (boolean? v))
                         (str (pr-str k) " is a boolean, and was "
                              (pr-str v)))))
          params)))

(defn params-error
  "The error frame refusing the handshake HELLO, whose :params are wrong
  because of WHY - see `invalid-params'."
  [hello kinds why]
  (error hello :invalid-params
         (str "A repl is started with these params, each one optional, and"
              " :params " why ": " (pr-str (:params hello)))
         {:params (mapv name (sort (keys kinds)))}))

(def no-reply ::no-reply)

(defmethod handle :default [_ msg]
  (throw (ex-info (str "Unknown op: " (pr-str (:op msg)))
                  {:replique/error :unknown-op})))

(defmulti accept-role
  "Take over a connection once the handshake has been validated. The method is
  responsible for sending the reply to the :hello message."
  (fn [conn hello] (:role hello)))
