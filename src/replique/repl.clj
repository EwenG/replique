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
        emit! (fn [mid-output?]
                (when-let [s (take-buffer! mid-output?)]
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
        ([x] (when (>= (long (append! x)) max-buffered-output) (emit! true)))
        ([x off len] (when (>= (long (append-range! x off len)) max-buffered-output)
                       (emit! true))))
      (flush [] (emit! false))
      (close [] (emit! false)))))

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
  (let [bad (fn [message]
              (throw (ex-info message {:replique/error :invalid-source-directive})))]
    (cond
      (not (map? m))
      (bad (str "#replique/src takes a map, got: " (pr-str m)))

      ;; Checked here rather than left to fail later: *file* and the file name
      ;; the compiler records are both strings, and a client sending anything
      ;; else would otherwise get a ClassCastException pointing inside
      ;; replique instead of at the message it sent.
      (not (or (nil? (:file m)) (string? (:file m))))
      (bad (str "#replique/src :file must be a string, got: " (pr-str (:file m))))

      ;; The range a line number really has, rather than integer? alone. It
      ;; ends up in LineNumberingPushbackReader.setLineNumber, which takes an
      ;; int, so a client that counted lines into a long gets an integer
      ;; overflow thrown from inside clojure - which is the cast error pointing
      ;; at replique's own code that this check is here to replace, and it
      ;; arrives as an execution failure rather than the read failure it is.
      ;; The low end is the same rule read the other way: an editor counts
      ;; lines from 1, and a 0 or a negative one would be written into the
      ;; metadata of the var it names as if it were a place in a file.
      (not (or (nil? (:line m))
               (and (integer? (:line m)) (<= 1 (:line m) Integer/MAX_VALUE))))
      (bad (str "#replique/src :line must be an integer between 1 and "
                Integer/MAX_VALUE ", got: " (pr-str (:line m))))

      :else (->SourceDirective (:file m) (:line m)))))

;;; The namespace

;; Code taken from a buffer belongs to the namespace that buffer is in, and
;; the repl is wherever it was left. A client that sends a form says which
;; namespace to read it in, in band, the way it says where it came from:
;;
;;   #replique/ns foo.bar
;;   (defn foo [] ...)
;;
;; Unlike #replique/src this is not about the next form only. It is in-ns
;; without the evaluation: the repl stays there, which is what makes switching
;; to the repl after evaluating something land at a prompt of the namespace
;; that was being worked in. Without the evaluation because an in-ns sent as a
;; form is a form - it has a result, and a prompt after it, and both appear in
;; the transcript as something the developer did not write.
(defrecord NsDirective [ns])

(defn- ns-directive [sym]
  (if (simple-symbol? sym)
    (->NsDirective sym)
    ;; A namespace name is a symbol with no namespace of its own. Qualified
    ;; ones are the mistake worth naming: #replique/ns foo/bar is what comes
    ;; out of a client that took the symbol at point rather than the namespace
    ;; around it
    (throw (ex-info (str "#replique/ns takes an unqualified symbol naming a "
                         "namespace, got: " (pr-str sym))
                    {:replique/error :invalid-ns-directive}))))

(defn- enter-ns!
  "Read and evaluate in the namespace SYM from here on, creating it if needed.

  clojure.core is referred into a namespace this creates, which in-ns alone
  does not do: a namespace it made holds nothing at all, not even def, and a
  repl that answered a buffer's namespace with one where defn does not resolve
  would be a repl making that buffer look broken. A namespace that already
  exists is left as it is - what it refers is its own business, and a file
  that excluded something from clojure.core meant it.

  A namespace created here is empty apart from clojure.core: what a file
  requires is required by its ns form, and until that form has been evaluated
  the code in it will not find what it depends on."
  [sym]
  (let [existing (find-ns sym)]
    (in-ns sym)
    (when-not existing (refer-clojure))
    nil))

(defn- make-repl-read
  "The :read step: clojure.main/repl-read, plus the directives.

  clojure.main/repl-read renumbers every form to line 1 - a socket repl has
  no meaningful line numbers - which is exactly the knob #replique/src needs:
  the form is read as if it started where the client says it did. That one is
  remembered until a form consumes it, so that a blank line between the two
  does not drop it.

  #replique/ns is not remembered, it is done: it has to take effect before the
  next form is read rather than before it is evaluated, because ::keyword and
  the reader conditionals resolve against *ns* while reading. Applied any
  later, the form would be read in the namespace being left."
  [conn]
  (let [pending (volatile! nil)]
    (fn [request-prompt request-exit]
      ;; A frame parked by a producer that found the connection busy must not
      ;; wait for the next form - the repl is about to block on the socket,
      ;; possibly for a long time, and the parked frame may well be the
      ;; prompt that says it is ready.
      (protocol/try-flush! conn)
      (try
        (loop []
          (case (clojure.main/skip-whitespace *in*)
            :line-start request-prompt
            :stream-end request-exit
            (let [{:keys [file line]} @pending
                  input (clojure.main/renumbering-read {:read-cond :allow} *in*
                                                       (or line 1))]
              (clojure.main/skip-if-eol *in*)
              (cond
                (instance? SourceDirective input)
                (do (vreset! pending input) (recur))

                ;; pending is left alone: a #replique/src read before this one
                ;; is about the form still to come, and entering a namespace
                ;; is not that form
                (instance? NsDirective input)
                (do (enter-ns! (:ns input)) (recur))

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
                                             'replique/src #'source-directive
                                             'replique/ns #'ns-directive)))
         :read (make-repl-read conn)
         :eval (fn [form] (interruptible conn #(eval form)))
         ;; The frame is built before the output is flushed: printing a
         ;; value may itself print - a print-method warning about what it was
         ;; handed - and that output belongs before the result, not after it.
         ;; Only *err* is reachable that way: ret-frame prints through pr-str,
         ;; which binds *out* to a StringWriter, so what a print-method prints
         ;; there ends up inside the value.
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
  (protocol/write-frame! conn (protocol/reply hello (assoc (state/info)
                                                           :role "repl"
                                                           :connection (:id conn))))
  ;; after the reply, as for a control connection
  (server/set-role! conn :repl)
  (repl conn))
