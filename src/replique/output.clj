(ns replique.output
  "Everything that is not printed by the thread of a REPL: a background thread
  printing, a logging framework, a thread dying - is produced by the application
  and belongs to no repl. It is broadcast to the control connections as events,
  which are best effort: an application thread is never paused by an editor that
  is not reading."
  (:require [replique.protocol :as protocol]
            [replique.state :as state])
  (:import [java.io ByteArrayOutputStream OutputStream OutputStreamWriter
            PrintStream PrintWriter]
           [java.nio.charset StandardCharsets]))

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
  original, so replique changes nothing about what a terminal shows."
  ^PrintStream [^PrintStream original ^String event-name]
  (let [buf (ByteArrayOutputStream.)
        take-buffer! (fn []
                       (locking buf
                         (when (pos? (.size buf))
                           (let [b (.toByteArray buf)]
                             (.reset buf)
                             b))))
        emit! (fn []
                (when-let [^bytes bytes (take-buffer!)]
                  ;; always decodable: the buffer is only ever cut where a
                  ;; write ended, and an encoder does not split a character
                  ;; across two writes
                  (try (broadcast-event!
                        (protocol/event event-name
                                        {:string (String. bytes StandardCharsets/UTF_8)}))
                       (catch Throwable _ nil))))
        buffer! (fn [^long n] (when (>= n max-buffered-output) (emit!)))
        stream (proxy [OutputStream] []
                 (write
                   ([x]
                    (if (integer? x)
                      (let [b (int x)]
                        ;; a raw byte passes through as it always did
                        (.write original b)
                        (buffer! (locking buf (.write buf b) (.size buf))))
                      (let [^bytes b x]
                        (.write ^OutputStream this b 0 (alength b)))))
                   ([b off len]
                    (let [^bytes b b]
                      ;; Forwarded as it is written, not at the next flush, so
                      ;; that a line printed without a newline still shows up
                      ;; when it used to. Decoded and re-printed rather than
                      ;; copied: original encodes it the way it always did, so
                      ;; a terminal that could not show an accent still shows
                      ;; what it showed before.
                      (try (.print original (String. b (int off) (int len)
                                                     StandardCharsets/UTF_8))
                           (catch Throwable _ nil))
                      (buffer! (locking buf
                                 (.write buf b (int off) (int len))
                                 (.size buf))))))
                 (flush []
                   (try (.flush original) (catch Throwable _ nil))
                   (emit!))
                 (close []
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
  "Take over the process wide output. opts:
    :tee-output  report stdout and stderr to the control connections

  Undone by uninstall!, which is what lets a test start and stop several
  processes in one jvm - a real replique process is stopped by exiting."
  [{:keys [tee-output]}]
  (when-not @installed
    (let [out System/out
          err System/err
          state {:out out
                 :err err
                 :out-var (.getRawRoot #'*out*)
                 :err-var (.getRawRoot #'*err*)
                 :handler (Thread/getDefaultUncaughtExceptionHandler)}]
      (reset! installed state)
      (when tee-output
        (let [tee-out (tee-stream out "out")
              tee-err (tee-stream err "err")]
          (System/setOut tee-out)
          (System/setErr tee-err)
          ;; The root bindings of *out* and *err* wrap the streams that were
          ;; captured when clojure booted, so replacing System/out is not
          ;; enough for (future (println ...)) to be seen.
          (alter-var-root #'*out* (constantly (print-writer tee-out)))
          (alter-var-root #'*err* (constantly (print-writer tee-err)))))
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
