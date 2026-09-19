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
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [replique.analysis :as analysis]
            [replique.protocol :as protocol]
            [replique.server :as server]
            [replique.state :as state])
  (:import [clojure.lang LineNumberingPushbackReader]
           [java.io File IOException Reader Writer]
           [java.util.jar JarFile]))

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
;;
;; It is also how a client moves the repl on its own, with no form after it:
;;
;;   #replique/ns foo.bar
;;   <blank line>
;;
;; and that blank line is answered with a prompt, since nothing else would
;; say where the repl now is.
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

;;; Loading

;; Evaluating a buffer form by form is not the same as loading the file those
;; forms are in.  A file is loaded as one unit, its ns form first and its
;; definitions in the order they are written, which is how the compiler will
;; see it and how the application will see it - and that is what somebody
;; means by "load this".  So a client asks for it in band, the way it asks for
;; everything else the repl is to do rather than evaluate:
;;
;;   #replique/load {:file "/home/me/src/foo.clj"}
;;
;; Asked here rather than as an op on the control connection, where the rest
;; of what an editor asks for goes.  What loading a file produces is the
;; developer's own output - the compiler's reflection warnings, a "WARNING:
;; foo already refers to" - and it belongs in the repl they asked from, in
;; order with the result, rather than broadcast to every control connection as
;; something the application happened to print.  Asked here it is also this
;; repl's exception when it throws, with the phase clojure.main triages and a
;; trace pointing into the file; and it is interruptible, because it goes
;; through the same eval step as any other form.
;;
;; A namespace read out of a dependency is not a file: it is an entry inside a
;; jar, and jumping into one and loading what is there is most of the point of
;; being able to jump into one.  So the entry travels beside the jar:
;;
;;   #replique/load {:file "/home/me/.m2/.../clojure-1.12.5.jar"
;;                   :entry "clojure/string.clj"}
;;
;; Which is how a file inside a jar is already written in this protocol - it
;; is what the :symbol op answers where a definition was written, and that
;; answer is exactly what an editor holds when somebody asks to load what they
;; jumped into.  A url spelling the two of them together would be a second way
;; to say the same thing, and one with escaping in it: a jar under a directory
;; with a space in its name is a %20 in a url and is not in a path.
(defrecord LoadDirective [file entry])

(defn- load-directive [m]
  (let [bad (fn [message]
              (throw (ex-info message {:replique/error :invalid-load-directive})))]
    ;; Before anything asks what it holds, and not as a branch of the cond
    ;; below: contains? does not answer about a number, it throws about one,
    ;; and the reader failure a client would then see is about the type of a
    ;; hash map rather than about the directive it wrote.
    (when-not (map? m)
      (bad (str "#replique/load takes a map, got: " (pr-str m))))
    (cond
      (not (string? (:file m)))
      (bad (str "#replique/load takes the :file to load, as a string, got: "
                (pr-str (:file m))))

      ;; Absent rather than nil is not distinguished: a client building the
      ;; map out of what :symbol answered has an entry or has nothing, and
      ;; both of those are what nothing means here.
      (not (or (nil? (:entry m)) (string? (:entry m))))
      (bad (str "The :entry of a #replique/load must be a string, got: "
                (pr-str (:entry m))))

      :else (->LoadDirective (:file m) (:entry m)))))

(defn- load-reader!
  "Load what RDR holds, as the code of PATH."
  [^Reader rdr ^String path]
  (clojure.lang.Compiler/load rdr path (.getName (File. path))))

(defn- load-entry-from-jar!
  "Load ENTRY of the jar FILE, by opening that jar.

  Loaded as the entry rather than as the jar: the inner path is what the
  compiler is given, which is what a stack trace shows and what the code being
  loaded reads as *file*.

  The jar is opened here rather than left to a jar: url, so that an entry that
  is not in it is said to be missing by name.  What a url does about that is
  throw a FileNotFoundException whose message is the whole url, which reads as
  though the jar were the thing that could not be found."
  [^String file ^String entry]
  (with-open [jar (JarFile. (File. file))]
    (let [found (or (.getEntry jar entry)
                    (throw (ex-info (str "No entry " entry " in " file)
                                    {:replique/error :invalid-load-directive})))]
      ;; UTF-8 said rather than left to the platform: clojure source is UTF-8,
      ;; and a jvm started under a latin-1 locale would otherwise read every
      ;; accent in it as two characters.
      (with-open [rdr (io/reader (.getInputStream jar found) :encoding "UTF-8")]
        (load-reader! rdr entry)))))

(defn- load-entry!
  "Load ENTRY of the jar FILE, analysed where this process can analyse it.

  Through the classpath where that reads this same jar, since a load that is
  recorded has to be a load the model can name - see `replique.analysis/load!'
  for the whole of why.  By opening the jar where it does not, which is a jar
  that is not on the classpath at all or one shadowed by another version of
  itself, and is the reading that is certainly of the file that was named."
  [^String file ^String entry]
  (or (analysis/load-entry! file entry)
      (load-entry-from-jar! file entry)))

(defn load!
  "Load what a #replique/load directive named, holding the require lock.

  The lock is clojure.lang.RT/REQUIRE_LOCK, the one clojure's own
  serialized-require takes, and holding it is what keeps two loads from
  interleaving.  Nothing in clojure.core takes it for a plain require - a
  process is assumed to have one repl in it - and replique is exactly what
  makes that untrue: it accepts as many repl connections as an editor opens,
  each evaluating on a thread of its own.  Two of them loading files that
  require the same namespace is two threads interning into one namespace, and
  the half-loaded namespace that comes out of it is a namespace nothing will
  fix but a restart.

  Held around the load and never around an evaluation.  A lock held for as
  long as an arbitrary form takes to run is a lock that stops every
  requiring-resolve in the process while somebody's (Thread/sleep 100000)
  finishes, which is a worse bargain than the race it would close."
  [{:keys [file entry]}]
  (locking clojure.lang.RT/REQUIRE_LOCK
    (if entry
      (load-entry! file entry)
      (analysis/load! file))
    ;; Nothing, whichever of those did it.  What a load leaves behind is the
    ;; namespace it defined, and the value it happens to hand back is whatever
    ;; the reading it went through happens to hand back - the last form of the
    ;; file from `load-file', the path from an analysed load, nothing from
    ;; `load'.  A repl prints that value, so answering with any of them would
    ;; be answering "this file was loaded" with a different sentence depending
    ;; on how it was read.  Which is also how it reads as a fact: the file was
    ;; loaded, and there is nothing else to say
    nil))

;;; Reloading

;; The other thing an editor asks the repl to load, and the one it cannot
;; name: everything that changed since the process read it.
;;
;;   #replique/reload {}
;;
;; Which is a question about the whole codebase and is still asked here
;; rather than as an op, for everything a load is asked here for.  It
;; compiles files, so what it produces is the compiler's warnings and the
;; code's own output, and both belong in the repl that asked; it throws where
;; a file will not compile, and that is this repl's exception, triaged, with
;; a trace into the file; and it can take a while, so it has to be
;; interruptible, which a form evaluated by the repl is and an op is not.
;;
;; A map with nothing in it, rather than nothing at all, because a tagged
;; literal reads the form after it whatever that form is - and what is asked
;; for here has somewhere to be written down the day there is something to
;; write.  What goes in it today is nothing, and a client that put something
;; there is a client asking for something this does not do, so it is told.
(defrecord ReloadDirective [])

(defn- reload-directive [m]
  (let [bad (fn [message]
              (throw (ex-info message {:replique/error :invalid-reload-directive})))]
    (when-not (map? m)
      (bad (str "#replique/reload takes a map, got: " (pr-str m))))
    (when (seq m)
      (bad (str "#replique/reload takes nothing in its map yet, got: " (pr-str m))))
    (->ReloadDirective)))

(defn reload!
  "Load every file that changed since this process read it, holding the lock.

  The same lock a load takes, for the same reason and rather more of it: this
  is several loads, of files that require each other, and two repls doing it
  at once is two threads compiling into one namespace - see `load!'.

  What changed, what that makes stale, and what order to load it in is
  `replique.analysis/reload!'.  The answer is the files it loaded, which a
  repl prints: what this did is not knowable in advance, so it is the result."
  []
  (locking clojure.lang.RT/REQUIRE_LOCK
    (analysis/reload!)))

;; Written from the read step, which is above the frames it writes
(declare prompt-frame)

(defn- make-repl-read
  "The :read step: clojure.main/repl-read, plus the directives.

  clojure.main/repl-read renumbers every form to line 1 - a socket repl has
  no meaningful line numbers - which is exactly the knob #replique/src needs:
  the form is read as if it started where the client says it did. That one is
  remembered until a form consumes it, so that a blank line between the two
  does not drop it.

  #replique/ns is not remembered, it is done: it has to take effect before the
  next form is read rather than before it is evaluated, because ::keyword and
  syntax quote resolve against *ns* while reading - and syntax quote resolves
  it away, into a symbol already qualified by the namespace being left, which
  nothing downstream can tell from one the code asked for. Applied any later,
  the form would be read where it was not written.

  One sent with no form after it is answered with a prompt, and one sent with
  a form is not: the form's own prompt says where the repl is, and two prompts
  for one evaluation is what the client cannot read - see the :line-start
  branch."
  [conn]
  (let [pending (volatile! nil)
        ;; A namespace directive has been read and nothing has been read
        ;; since. What acknowledges it is the blank line after it - see the
        ;; :line-start branch.
        moved (volatile! false)]
    (fn [request-prompt request-exit]
      ;; A frame parked by a producer that found the connection busy must not
      ;; wait for the next form - the repl is about to block on the socket,
      ;; possibly for a long time, and the parked frame may well be the
      ;; prompt that says it is ready.
      (protocol/try-flush! conn)
      (try
        (loop []
          (case (clojure.main/skip-whitespace *in*)
            ;; Read past rather than answered. clojure.main hands back
            ;; request-prompt here, which is what a terminal wants - return on
            ;; an empty line gives you a fresh prompt - and the opposite of
            ;; what an editor wants: it delimits an evaluation by the prompt
            ;; that ends it, and a blank line between two top level forms,
            ;; which is what most files look like, would end the first one
            ;; twice and lose where the second began. :need-prompt is
            ;; (constantly true), so every form still gets a prompt - and now
            ;; exactly one.
            ;;
            ;; Except after a namespace directive that no form followed, which
            ;; is a client asking to be moved rather than to have something
            ;; evaluated. Nothing else will answer it: the prompt of a form is
            ;; what says where the repl is, and there is no form. So the blank
            ;; line is what the client ends a bare move with, and this is the
            ;; prompt that says it happened.
            :line-start (do (when @moved
                              (vreset! moved false)
                              (protocol/write-frame! conn (prompt-frame conn)))
                            (recur))
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
                (do (enter-ns! (:ns input)) (vreset! moved true) (recur))

                ;; Answered with a form rather than done here, so that loading
                ;; a file is evaluated the way everything else is: its output
                ;; framed in order, its failure this repl's exception frame,
                ;; its running interruptible, and one prompt after it.
                ;;
                ;; Where the code comes from is the file's own business - the
                ;; compiler reads it from there, and binds *file* to it for as
                ;; long as the load lasts - so a #replique/src above this one
                ;; was about a form that never came. Dropped here rather than
                ;; left pending, which would place the next form the client
                ;; sends at a line of a file it has nothing to do with.
                (instance? LoadDirective input)
                (do (vreset! pending nil)
                    (vreset! moved false)
                    (list `load! {:file (:file input) :entry (:entry input)}))

                ;; A form for the same reasons, and a #replique/src above it
                ;; dropped for the same one: what this loads is files, each
                ;; read from where it is
                (instance? ReloadDirective input)
                (do (vreset! pending nil)
                    (vreset! moved false)
                    (list `reload!))

                ;; the way a socket repl is ended, as in clojure.core.server
                (identical? :repl/quit input) request-exit

                :else
                (do (vreset! pending nil)
                    ;; the form is what the directive above it was about, and
                    ;; its own prompt is the one that follows
                    (vreset! moved false)
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
                                             'replique/ns #'ns-directive
                                             'replique/load #'load-directive
                                             'replique/reload #'reload-directive)))
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
