(ns replique.output
  "Everything that is not printed by the thread of a REPL: a background thread
  printing, a logging framework, a thread dying - is produced by the application
  and belongs to no repl. It is broadcast to the control connections as events,
  which are best effort: an application thread is never paused by an editor that
  is not reading."
  (:require [replique.protocol :as protocol]
            [replique.state :as state])
  (:import [java.io OutputStream OutputStreamWriter PrintStream PrintWriter]
           [java.nio ByteBuffer CharBuffer]
           [java.nio.charset CharsetDecoder CodingErrorAction StandardCharsets]))

(defn control-connections []
  (->> (state/connections)
       vals
       (filter #(identical? :control @(:role %)))))

(defn broadcast-event!
  "Send an event to every control connection. Never throws."
  [f]
  (doseq [conn (control-connections)]
    (try (protocol/emit-event! conn f) (catch Throwable _ nil))))

;;; Teeing stdout and stderr

(def ^:private max-buffered-output 8192)

(defn- tee-stream
  "A PrintStream that reports what goes through it as an event, and passes it
  on to original.

  It encodes in UTF-8 whatever the terminal's encoding is, so that what the
  editor is told is the text that was printed - a process whose
  stdout.encoding is US-ASCII, which is what an unset locale gives, would
  otherwise report every accent and every emoji as a question mark. The
  terminal keeps its own encoding: what is written there is re-encoded by
  original, so replique changes nothing about what a terminal shows.

  The bytes are decoded as one stream and not one write at a time. A write is
  whatever chunk the caller happened to hold - io/copy hands over its buffer,
  a socket hands over what arrived - and a character that straddles two of
  them is, to a decoder shown them apart, two broken halves rather than one
  character. What is left over at the end of a write is kept and finished by
  the bytes that follow it.

  What goes through here is text, and what is not text does not survive it:
  bytes that are not UTF-8 are replaced by U+FFFD, on the terminal as in the
  event. An editor is told strings, so there is no version of this that also
  carries arbitrary bytes through unchanged."
  ^PrintStream [^PrintStream original ^String event-name]
  (let [^CharsetDecoder decoder (doto (.newDecoder StandardCharsets/UTF_8)
                                  (.onMalformedInput CodingErrorAction/REPLACE)
                                  (.onUnmappableCharacter CodingErrorAction/REPLACE))
        lock (Object.)
        ;; the text waiting to go out as an event, and the bytes that are not
        ;; text yet - at most the three a UTF-8 character can be short of
        sb (StringBuilder.)
        tail (volatile! (byte-array 0))
        decode!
        (fn [^bytes b off len end?]
          ;; Called with the lock held. One char per byte is an upper bound
          ;; for UTF-8 - the four byte characters are the ones that make two -
          ;; so this never overflows and never has to be resumed.
          (let [^bytes t @tail
                in (ByteBuffer/allocate (int (+ (alength t) (int len))))]
            (.put in t)
            (when (pos? (int len)) (.put in b (int off) (int len)))
            (.flip in)
            (let [out (CharBuffer/allocate (int (inc (.remaining in))))]
              (.decode decoder in out (boolean end?))
              (when end? (.flush decoder out))
              (.flip out)
              (let [left (byte-array (.remaining in))]
                (.get in left)
                (vreset! tail left))
              (.toString out))))
        take-buffer! (fn []
                       (locking lock
                         (when (pos? (.length sb))
                           (let [s (.toString sb)]
                             (.setLength sb 0)
                             s))))
        emit! (fn []
                (when-let [s (take-buffer!)]
                  (try (broadcast-event!
                        (protocol/event event-name {:string s}))
                       (catch Throwable _ nil))))
        accept!
        (fn [^bytes b off len end?]
          (let [n (locking lock
                    (let [s (decode! b off len end?)]
                      (when (pos? (.length ^String s))
                        (.append sb ^String s)
                        ;; Forwarded as it is written, not at the next flush,
                        ;; so that a line printed without a newline still
                        ;; shows up when it used to. Decoded and re-printed
                        ;; rather than copied: original encodes it the way it
                        ;; always did, so a terminal that could not show an
                        ;; accent still shows what it showed before.
                        (try (.print original ^String s) (catch Throwable _ nil)))
                      (.length sb)))]
            (when (>= n max-buffered-output) (emit!))))
        stream (proxy [OutputStream] []
                 (write
                   ([x]
                    (if (integer? x)
                      (let [b (byte-array 1)]
                        (aset-byte b 0 (unchecked-byte (int x)))
                        (accept! b 0 1 false))
                      (let [^bytes b x] (accept! b 0 (alength b) false))))
                   ([b off len] (accept! b off len false)))
                 (flush []
                   (try (.flush original) (catch Throwable _ nil))
                   ;; What is held back is an unfinished character, and a
                   ;; flush is not what finishes it - only the bytes that
                   ;; complete it are.
                   (emit!))
                 (close []
                   ;; Nothing more is coming, so what is held back is a broken
                   ;; character rather than an unfinished one: it is reported
                   ;; as broken rather than swallowed.
                   (accept! nil 0 0 true)
                   (try (.flush original) (catch Throwable _ nil))
                   (emit!)))]
    ;; autoflush: a PrintStream flushes on println and on any newline, so an
    ;; event is one line of output rather than one write
    (PrintStream. ^OutputStream stream true StandardCharsets/UTF_8)))

(defn- print-writer ^PrintWriter [^PrintStream stream]
  (PrintWriter. (OutputStreamWriter. stream (.charset stream)) true))

;;; Uncaught exceptions

(defn- uncaught-exception-handler
  "Replique owns the process, so there is no handler of the application's to
  chain to - and none to put back that would mean anything."
  [^PrintStream err]
  (reify Thread$UncaughtExceptionHandler
    (uncaughtException [_ thread ex]
      (try
        (broadcast-event!
         (protocol/event "uncaught-exception"
                         {:thread (.getName ^Thread thread)
                          :message (or (.getMessage ^Throwable ex)
                                       (.getName (class ex)))
                          :exception (protocol/exception->data ex)}))
        (catch Throwable _ nil))
      ;; What the jvm would have done, on the stderr replique replaced, so
      ;; that the trace is not also reported as an err event
      (.print err (str "Exception in thread \"" (.getName ^Thread thread) "\" "))
      (.printStackTrace ^Throwable ex err)
      (.flush err))))

;;; Install / uninstall

(defonce ^:private installed (atom nil))

(defn install!
  "Take over the process wide output: stdout and stderr are reported to the
  control connections as events, and passed on to the streams they replaced.

  Undone by uninstall!, which is what lets a test start and stop several
  processes in one jvm - a real replique process is stopped by exiting."
  []
  (when-not @installed
    (let [out System/out
          err System/err
          state {:out out
                 :err err
                 :out-var (.getRawRoot #'*out*)
                 :err-var (.getRawRoot #'*err*)
                 :handler (Thread/getDefaultUncaughtExceptionHandler)}]
      (reset! installed state)
      (let [tee-out (tee-stream out "out")
            tee-err (tee-stream err "err")]
        (System/setOut tee-out)
        (System/setErr tee-err)
        ;; The root bindings of *out* and *err* wrap the streams that were
        ;; captured when clojure booted, so replacing System/out is not
        ;; enough for (future (println ...)) to be seen.
        (alter-var-root #'*out* (constantly (print-writer tee-out)))
        (alter-var-root #'*err* (constantly (print-writer tee-err))))
      (Thread/setDefaultUncaughtExceptionHandler
       (uncaught-exception-handler err)))))

(defn uninstall! []
  (when-let [{:keys [^PrintStream out ^PrintStream err out-var err-var handler]} @installed]
    (when-not (identical? out System/out)
      (.flush ^PrintStream System/out)
      (System/setOut out)
      (alter-var-root #'*out* (constantly out-var)))
    (when-not (identical? err System/err)
      (.flush ^PrintStream System/err)
      (System/setErr err)
      (alter-var-root #'*err* (constantly err-var)))
    (Thread/setDefaultUncaughtExceptionHandler handler)
    (reset! installed nil)
    nil))
