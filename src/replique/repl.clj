(ns replique.repl
  "The :repl role.

  A repl connection is not a message channel. Once the handshake is done the
  client writes plain clojure code - not EDN messages - and the process
  answers with frames:

    out / err  what the evaluation printed
    ret        the printed result of one form
    exception  the form threw
    prompt     the repl is ready, and this is the state it is ready in

  Handing the repl the reader the handshake was read from, rather than a
  queue of eval requests, is what makes nested repls, (read-line) and
  debuggers work: *in* is a real stream the code being evaluated can read
  from."
  (:require [clojure.string :as string]
            [replique.protocol :as protocol]
            [replique.server :as server]
            [replique.state :as state])
  (:import [clojure.lang LineNumberingPushbackReader]
           [java.io IOException Writer]))

(def ^:private default-file "NO_SOURCE_PATH")
(def ^:private default-source "NO_SOURCE_FILE")

;;; Output

(def ^:private max-buffered-output
  "Flush the output buffer once it grows past this. A form that prints a lot
  without ever emitting a newline must not hold everything in memory."
  8192)

(defn- frame-writer
  "A Writer that turns what is written to it into frames on conn.

  Buffered, and flushed into one frame per flush rather than per write:
  clojure flushes at the end of every println, so a line of output is a
  frame. Frames go out through write-frame!, which never drops them - unlike
  the events of the control connection, this output is the answer to what the
  developer just asked for, and the thread producing it is the repl itself."
  ^Writer [conn tag]
  (let [sb (StringBuilder.)
        take-buffer! (fn []
                       (locking sb
                         (when (pos? (.length sb))
                           (let [s (.toString sb)]
                             (.setLength sb 0)
                             s))))
        emit! (fn []
                (when-let [s (take-buffer!)]
                  (protocol/write-frame! conn (protocol/frame {:tag tag :string s}))))
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
        ([x] (when (>= (long (append! x)) max-buffered-output) (emit!)))
        ([x off len] (when (>= (long (append-range! x off len)) max-buffered-output)
                       (emit!))))
      (flush [] (emit!))
      (close [] (emit!)))))

;;; Source metadata

;; A repl reads code from a socket, so the line numbers and the file the
;; compiler records are those of the socket - which means nothing to an
;; editor. A client that sends a form taken from a buffer says where it comes
;; from, in band, right before the form:
;;
;;   #replique/src {:file "/home/me/src/foo.clj" :line 42}
;;   (defn foo [] ...)
;;
;; The directive applies to the next form only. In band rather than a process
;; wide atom set by another connection: it cannot race with another repl, and
;; it stays in order with the code it describes.
(defrecord SourceDirective [file line])

(defn- source-directive [m]
  (if (map? m)
    (->SourceDirective (:file m) (:line m))
    (throw (ex-info (str "#replique/src takes a map, got: " (pr-str m))
                    {:replique/error :invalid-source-directive}))))

(defn- make-repl-read
  "The :read step: clojure.main/repl-read, plus #replique/src.

  clojure.main/repl-read renumbers every form to line 1 - a socket repl has
  no meaningful line numbers - which is exactly the knob the directive needs:
  the form is read as if it started where the client says it did.

  The directive is remembered until a form consumes it, so that a blank line
  between the two does not drop it."
  []
  (let [pending (volatile! nil)]
    (fn [request-prompt request-exit]
      (try
        (loop []
          (case (clojure.main/skip-whitespace *in*)
            :line-start request-prompt
            :stream-end request-exit
            (let [{:keys [file line]} @pending
                  input (clojure.main/renumbering-read {:read-cond :allow} *in*
                                                       (if (integer? line) line 1))]
              (clojure.main/skip-if-eol *in*)
              (cond
                (instance? SourceDirective input)
                (do (vreset! pending input) (recur))

                ;; the way a socket repl is ended, as in clojure.core.server
                (identical? :repl/quit input) request-exit

                :else
                (do (vreset! pending nil)
                    ;; the directive applies to this form only
                    (set! *file* (or file default-file))
                    ;; what the compiler records as the source file of the
                    ;; classes it emits, which is what a stack trace shows
                    (set! *source-path* (if file
                                          (.getName (java.io.File. ^String file))
                                          default-source))
                    input)))))
        ;; The connection is gone. End the repl rather than let
        ;; clojure.main/repl report the failure and read again, forever.
        (catch IOException _ request-exit)))))

;;; Frames

(defn- repl-ns [] (str (ns-name *ns*)))

(defn- prompt-frame [conn]
  (protocol/frame
   {:tag "prompt"
    :connection (:id conn)
    :ns (repl-ns)
    ;; What the editor needs to describe the repl without asking: the
    ;; namespace the next form will be read in, and the printing the last
    ;; result went through.
    :params {:print-length *print-length*
             :print-level *print-level*
             :print-meta *print-meta*
             :warn-on-reflection *warn-on-reflection*}}))

(defn- ret-frame [value]
  (protocol/frame {:tag "ret" :ns (repl-ns) :value (pr-str value)}))

(defn- exception-frame
  "The message is the one a terminal repl would print - clojure.main knows how
  to tell a reader failure from a macroexpansion failure from a real
  exception, and says so much better than the exception itself, whose message
  is often nil. The phase it found comes along: the editor can tell the code
  that could not be read from the code that ran and threw."
  [^Throwable t]
  (let [triage (try (clojure.main/ex-triage (Throwable->map t))
                    (catch Throwable _ nil))]
    (protocol/frame {:tag "exception"
                     :ns (repl-ns)
                     :phase (:clojure.error/phase triage)
                     :message (or (some-> triage clojure.main/ex-str string/trim)
                                  (.getMessage t)
                                  (.getName (class t)))
                     :exception (protocol/exception->data t)})))

;;; The repl

(defn- interruptible
  "Run f with the calling thread registered as what :interrupt targets on this
  connection. Reading is deliberately left out: interrupting a repl that is
  waiting for the next form would break the connection rather than the
  evaluation."
  [conn f]
  (server/evaluating! conn)
  (try (f)
       (finally (server/done-evaluating! conn))))

(defn repl
  "Run a repl on conn until the client disconnects."
  [conn]
  (let [out (frame-writer conn "out")
        err (frame-writer conn "err")
        ;; Output is flushed before every frame that concludes something, so
        ;; that a result never comes out before what the form printed.
        flush-output! (fn [] (.flush out) (.flush err))]
    (binding [*in* (:in conn)
              *out* out
              *err* err
              *file* default-file
              *source-path* default-source]
      (try
        (clojure.main/repl
         :init (fn []
                 (in-ns 'user)
                 ;; assoc rather than a fixed map: the data readers of the
                 ;; project being worked on must keep working
                 (set! *data-readers* (assoc *data-readers*
                                             'replique/src #'source-directive)))
         :read (make-repl-read)
         :eval (fn [form] (interruptible conn #(eval form)))
         ;; The frame is built before the output is flushed: printing a value
         ;; may itself print - a print-method that says something - and that
         ;; output belongs before the result, not after it.
         :print (fn [value]
                  (let [f (interruptible conn #(ret-frame value))]
                    (flush-output!)
                    (protocol/write-frame! conn f)))
         :caught (fn [t]
                   (let [f (exception-frame t)]
                     (flush-output!)
                     (protocol/write-frame! conn f)))
         ;; One prompt per read: the editor knows the repl is ready, and
         ;; learns the namespace the next form will be read in.
         :need-prompt (constantly true)
         :prompt (fn []
                   (flush-output!)
                   (protocol/write-frame! conn (prompt-frame conn)))
         :flush flush-output!)
        (finally (flush-output!))))))

(defmethod protocol/accept-role :repl [conn hello]
  (server/set-role! conn :repl)
  (protocol/write-frame! conn (protocol/reply hello (assoc (state/info)
                                                           :role "repl"
                                                           :connection (:id conn))))
  (repl conn))
