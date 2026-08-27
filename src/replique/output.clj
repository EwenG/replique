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
           [java.nio.charset Charset]))

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
  "A PrintStream that writes to original and, at every flush, reports what
  went through it as an event. The process keeps behaving as it did - what is
  printed still reaches the terminal - the editor just gets to see it too."
  ^PrintStream [^PrintStream original ^String event-name]
  (let [charset (.charset original)
        buf (ByteArrayOutputStream.)
        emit! (fn []
                (let [^bytes bytes (locking buf
                                     (when (pos? (.size buf))
                                       (let [b (.toByteArray buf)]
                                         (.reset buf)
                                         b)))]
                  (when bytes
                    (try
                      (broadcast-event!
                       (protocol/event event-name
                                       {:string (String. bytes ^Charset charset)}))
                      (catch Throwable _ nil)))))
        stream (proxy [OutputStream] []
                 (write
                   ([x]
                    (if (integer? x)
                      (let [b (int x)]
                        (.write original b)
                        (when (>= (long (locking buf (.write buf b) (.size buf)))
                                  max-buffered-output)
                          (emit!)))
                      (let [^bytes b x]
                        (.write ^OutputStream this b 0 (alength b)))))
                   ([b off len]
                    (let [^bytes b b]
                      (.write original b (int off) (int len))
                      (when (>= (long (locking buf
                                        (.write buf b (int off) (int len))
                                        (.size buf)))
                                max-buffered-output)
                        (emit!)))))
                 (flush []
                   (.flush original)
                   (emit!))
                 (close []
                   (.flush original)
                   (emit!)))]
    ;; autoflush: a PrintStream flushes on println and on any newline, so an
    ;; event is one line of output rather than one write
    (PrintStream. ^OutputStream stream true ^Charset charset)))

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
