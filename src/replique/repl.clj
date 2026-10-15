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
            [replique.cljs-repl :as cljs-repl]
            [replique.debug :as debug]
            [replique.directives :as directives]
            [replique.hooks :as hooks]
            [replique.inspect :as inspect]
            [replique.protocol :as protocol]
            [replique.server :as server]
            [replique.state :as state])
  (:import [clojure.lang LineNumberingPushbackReader]
           [java.io File IOException Reader Writer]
           [java.util.jar JarFile]
           [replique.debug Instrumented]
           [replique.directives FrameDirective LoadDirective NsDirective ReloadDirective
            SourceDirective]))

(def ^:private default-file "NO_SOURCE_PATH")
(def ^:private default-source "NO_SOURCE_FILE")

;;; Output

(defn- frame-writer
  "A Writer that turns what is written to it into frames on conn.

  Frames go out through write-frame!, which never drops them - unlike the
  events of the control connection, this output is the answer to what the
  developer just asked for, and the thread producing it is the repl itself."
  ^Writer [conn tag]
  (protocol/buffering-writer
   (fn [s] (protocol/write-frame! conn (protocol/frame {:tag tag :string s})))))

;;; The namespace

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

(defn reload!
  "Load every file that changed since this process read it, holding the lock.

  The same lock a load takes, for the same reason and rather more of it: this
  is several loads, of files that require each other, and two repls doing it
  at once is two threads compiling into one namespace - see `load!'.

  What changed, what that makes stale, and what order to load it in is
  `replique.analysis/reload!'.  The answer is the files it loaded, which a
  repl prints: what this did is not knowable in advance, so it is the result.

  AND IT SAYS WHAT IT IS LOADING WHILE IT LOADS IT - `replique.analysis/telling*'
  - because the answer arrives when it stops being useful.  A reload of forty
  files is half a minute of a repl that looks stopped, and the file it is inside
  is the one thing worth knowing about it, both while it is working and when it
  is not coming back.

  Here rather than in `replique.analysis/reload!' because it is this repl that
  has somewhere to print: `*out*' is the connection that asked, and what it is
  bound to is what makes these lines this client's rather than the process's."
  []
  (locking clojure.lang.RT/REQUIRE_LOCK
    (analysis/telling* analysis/reload!)))

;; Called from the read step, which is above them
(declare prompt-frame point-at-frame!)

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
  branch.

  #replique/frame is done too, and the same way: it points the repl at the
  frame IN-FRAME holds, and moves it to the namespace of that frame's code."
  [conn in-frame]
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
                              (protocol/write-frame! conn (prompt-frame conn in-frame)))
                            (recur))
            :stream-end request-exit
            (let [{:keys [file line instrument column entry record]} @pending
                  ;; to be stepped through, which is read with the span of
                  ;; every form, from where it was written - see replique.debug
                  input (if instrument
                          (debug/read-instrumented *in* (or line 1) (or column 1) entry record)
                          (clojure.main/renumbering-read {:read-cond :allow} *in* (or line 1)))
                  instrumented (when (instance? Instrumented input) input)
                  input (if instrumented (:form input) input)]
              (clojure.main/skip-if-eol *in*)
              (cond
                (instance? SourceDirective input)
                (do (vreset! pending input) (recur))

                ;; pending is left alone: a #replique/src read before this one
                ;; is about the form still to come, and entering a namespace
                ;; is not that form
                (instance? NsDirective input)
                (do (enter-ns! (:ns input)) (vreset! moved true) (recur))

                ;; pending left alone, as for a namespace
                (instance? FrameDirective input)
                (do (point-at-frame! in-frame input) (vreset! moved true) (recur))

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
                    (or instrumented input))))))
        ;; The connection is gone. End the repl rather than let
        ;; clojure.main/repl report the failure and read again, forever.
        (catch IOException _ request-exit)))))

;;; Frames

(defn- repl-ns [] (str (ns-name *ns*)))

(defn- prompt-frame [conn in-frame]
  (protocol/frame
   {:tag "prompt"
    :connection (:id conn)
    :ns (repl-ns)
    ;; the frame what is read is evaluated in, where the repl is pointed at one
    :frame (some-> @in-frame (select-keys [:thread :frame]))
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

;;; Printing params

(def ^:private param-kinds
  "The params a handshake may set, and what each one takes. The same four the
  prompt reports, under the same names, so that what a prompt said can be
  handed back as it is - which is what an editor reopening a repl does."
  {:print-length :count
   :print-level :count
   :print-meta :boolean
   :warn-on-reflection :boolean})

(defn- set-params!
  "Set the params named in params, inside the bindings clojure.main/repl
  establishes. One left out is left as clojure.main sets it."
  [params]
  (when (contains? params :print-length)
    (set! *print-length* (:print-length params)))
  (when (contains? params :print-level)
    (set! *print-level* (:print-level params)))
  (when (contains? params :print-meta)
    (set! *print-meta* (boolean (:print-meta params))))
  (when (contains? params :warn-on-reflection)
    (set! *warn-on-reflection* (boolean (:warn-on-reflection params)))))

(defn- point-at-frame!
  "Point the repl at the frame a #replique/frame directive, or a handshake,
  names - FRAME, a FrameDirective - or at none, where it names none. IN-FRAME
  is where the repl keeps it.

  To the namespace of the frame too, which is what its code was read in -
  where the thread is stopped already. Where it is not, the repl says so when
  it is asked to evaluate something - see `replique.debug/eval-in-frame'."
  [in-frame frame]
  (if (:thread frame)
    (do (vreset! in-frame {:thread (:thread frame) :frame (:frame frame)})
        (when-let [ns (try (debug/frame-ns (:thread frame) (:frame frame))
                           (catch Exception _ nil))]
          (in-ns (ns-name ns))))
    (vreset! in-frame nil)))

(defn- evaluated-in-frame?
  "Whether FORM, read by a repl pointed at a frame, is evaluated there.

  What a client asks the repl to do, rather than to evaluate, is done where
  the repl is: a definition to step through or record is compiled, and a file
  loaded, as anywhere else."
  [form]
  (not (or (instance? Instrumented form)
           (and (seq? form) (#{`load! `reload!} (first form))))))

(defn repl
  "Run a repl on conn until the client disconnects.

  params are set before the first prompt, which reports them - see
  `set-params!'. With FRAME, a FrameDirective, the repl starts pointed at
  that frame - see `replique.directives/frame-directive'."
  [conn params frame]
  (let [out (frame-writer conn "out")
        err (frame-writer conn "err")
        ;; Output is flushed before every frame that concludes something, so
        ;; that a result never comes out before what the form printed.
        flush-output! (fn [] (.flush out) (.flush err))
        ;; nil, or the {:thread :frame} what is read is evaluated in
        in-frame (volatile! nil)]
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
                 (set! *data-readers* (merge *data-readers*
                                             directives/data-readers))
                 (set-params! params)
                 (when frame (point-at-frame! in-frame frame)))
         :read (make-repl-read conn in-frame)
         ;; ROUND THE EVALUATION AND NOT ROUND A DIRECTIVE, because what defines
         ;; something is not only a load: a `defn' typed here, a `require', a
         ;; `#replique/reload' of forty files, and a `load-file' somebody wrote
         ;; out by hand all replace code that may be running, and the compiler
         ;; says which in every one of those cases - see `replique.hooks'.
         ;; Inside `interruptible', so that a hook is part of the evaluation it
         ;; follows: its output is this connection's and a hook that will not
         ;; come back can be interrupted like anything else.
         :eval (fn [form]
                 (interruptible conn #(hooks/around*
                                       (fn [] (cond
                                                (and @in-frame (evaluated-in-frame? form))
                                                (debug/eval-in-frame @in-frame form)

                                                (instance? Instrumented form)
                                                (debug/eval-instrumented form)

                                                :else (eval form))))))
         ;; The frame is built before the output is flushed: printing a
         ;; value may itself print - a print-method warning about what it was
         ;; handed - and that output belongs before the result, not after it.
         ;; Only *err* is reachable that way: ret-frame prints through pr-str,
         ;; which binds *out* to a StringWriter, so what a print-method prints
         ;; there ends up inside the value.
         :print (fn [value]
                  (let [f (interruptible conn #(ret-frame value))]
                    (flush-output!)
                    (protocol/write-frame! conn f)
                    ;; what *1 is to this repl, which no other thread can
                    ;; read - and a view of this repl's results is asked
                    ;; for on the control connection's
                    (inspect/record-result! (:id conn) value)))
         :caught (fn [t]
                   (let [f (exception-frame t)]
                     (flush-output!)
                     (protocol/write-frame! conn f)))
         ;; One prompt per read: the editor knows the repl is ready, and
         ;; learns the namespace the next form will be read in.
         :need-prompt (constantly true)
         :prompt (fn []
                   (flush-output!)
                   (protocol/write-frame! conn (prompt-frame conn in-frame)))
         :flush flush-output!)
        (finally (flush-output!))))))

(defn- accept-clj!
  "Take over conn as a Clojure repl."
  [conn hello]
  (if-let [why (protocol/invalid-params param-kinds (:params hello))]
    (protocol/write-frame! conn (protocol/params-error hello param-kinds why))
    ;; :frame, the frame of a stopped thread the repl starts pointed at - as
    ;; #replique/frame would point it, and before its first prompt
    (let [frame (try (some-> (:frame hello) directives/frame-directive)
                     (catch clojure.lang.ExceptionInfo e e))]
      (if (instance? Throwable frame)
        (protocol/write-frame! conn (protocol/error hello :invalid-frame (.getMessage ^Throwable frame)))
        (do
          (protocol/write-frame! conn (protocol/reply hello (assoc (state/info)
                                                                   :role "repl"
                                                                   :connection (:id conn))))
          ;; after the reply, as for a control connection
          (server/set-role! conn :repl)
          (repl conn (:params hello) frame))))))

(defmethod protocol/accept-role :repl [conn hello]
  ;; ONE ROLE AND TWO DIALECTS, rather than two roles. A repl connection is one
  ;; kind of connection - source in, frames out, code and not messages - and
  ;; which language that source is in is what `:dialect' says, here as in every
  ;; other message of this protocol. A second role would be a second way to say
  ;; the same thing, and a client would have to be told which of the two to use
  ;; where.
  ;;
  ;; Absent means Clojure, which is the same rule and keeps every client that
  ;; predates ClojureScript working unchanged.
  (case (protocol/as-keyword (:dialect hello))
    (nil :clj) (accept-clj! conn hello)
    :cljs (cljs-repl/accept! conn hello)
    (protocol/write-frame!
     conn (protocol/error hello :invalid-dialect
                          (str "A repl is in one of these dialects, and"
                               " :dialect named none of them: "
                               (pr-str (:dialect hello)))
                          {:dialects ["clj" "cljs"]}))))
