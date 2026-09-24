(ns replique.cljs-repl
  "The :repl role, for ClojureScript.

  Same connection, same frames, same directives as `replique.repl' - a client
  writes source and reads out / err / ret / exception / prompt - and a
  different everything underneath. What a handshake says to get here is
  `:dialect :cljs', which is how every message in this protocol says
  ClojureScript, and `:target' says which runtime the code is to run in.

  ## What is not the same, and why

  THE READER IS THE COMPILER'S. A ClojureScript form is not a Clojure form that
  happens to be evaluated elsewhere: its symbols resolve in another world, its
  reader conditionals take the other branch, and #js is a tag clojure has never
  heard of. So reading goes through `replique.cljs/read-form', and the
  directives are carried into that reader rather than into clojure's.

  THERE IS NO THROWABLE. A form that fails fails in another process, and what
  comes back is a message and a stack as text. So an `exception' frame here
  carries `stacktrace' - symbolicated, ClojureScript file and line - where the
  Clojure one carries a structured `exception' object, and there is no
  `clojure.main' to triage a phase out of: the phase is whichever side noticed.

  THE OUTPUT IS NOT ON THIS THREAD. A println inside an evaluated form prints
  in the runtime, arrives on the transport's own thread, and is routed back to
  whoever is evaluating by `replique.cljs/runtime-writer'. What *this* thread
  prints is the compiler talking - a warning about an undeclared var - and that
  is framed the ordinary way.

  ONE RUNTIME PER TARGET, shared by every repl connection that asked for that
  target. Two repl buffers on the browser are two views of the one page you
  have open, as two Clojure repls are two views of one JVM."
  (:require [clojure.main]
            [replique.cljs :as cljs]
            [replique.protocol :as protocol]
            [replique.server :as server]
            [replique.state :as state])
  (:import [clojure.lang LineNumberingPushbackReader]
           [java.io IOException Writer]
           [replique.directives LoadDirective NsDirective ReloadDirective
            SourceDirective]))

(def ^:private start-ns
  "Where a repl stands before anything moved it. The compiler's own default,
  and a namespace that exists in every environment because making one declares
  it."
  'cljs.user)

;;; Frames

(defn- frame-writer
  "A Writer that turns what is written to it into frames on conn."
  ^Writer [conn tag]
  (protocol/buffering-writer
   (fn [s] (protocol/write-frame! conn (protocol/frame {:tag tag :string s})))))

(defn- prompt-frame
  "The repl is ready, and this is the state it is ready in.

  NO `params', which the Clojure prompt carries: what it carries there is the
  *print-* the last result went through, and here the value was printed in
  another process by a printer this one does not set. What replaces it is
  `target', which is the thing about a ClojureScript repl that a client cannot
  work out and has to be told - and `dialect', so that a client reading frames
  need not remember which of its connections was which."
  [conn]
  (protocol/frame {:tag "prompt"
                   :connection (:id conn)
                   :ns (str (cljs/current-ns))
                   :dialect "cljs"
                   :target (name cljs/*target*)}))

(defn- ret-frame [result]
  (protocol/frame {:tag "ret"
                   :ns (str (cljs/current-ns))
                   :value (:value result)}))

(defn- exception-frame
  "A form failed, and this is what there is to say about it.

  `message' is what a terminal repl would have printed, which for ClojureScript
  is the message the runtime sent or the one the compiler threw. `phase' says
  which side noticed: :read and :compile happened here, anything else happened
  there.

  `stacktrace' is text and not a structure, because a stack is the runtime's and
  arrives as the string V8 printed - read back into ClojureScript file and line
  by `clojure.cljs.stacktrace' before it gets here. `js-stacktrace' is beside it
  and is not a fallback: it is the answer when the mapping is what you doubt,
  which is the one question the mapped stack cannot be asked."
  [result]
  (protocol/frame {:tag "exception"
                   :ns (str (cljs/current-ns))
                   :phase (some-> (:phase result) name)
                   :message (:value result)
                   :stacktrace (not-empty (:stacktrace result))
                   :js-stacktrace (not-empty (:js-stacktrace result))}))

;;; The directives

(defn- refused
  "A directive this repl will not do, in the shape a failed evaluation has, so
  that it reaches the client as the same frame and interrupts nothing."
  [message]
  {:status :error :phase :repl :value message})

(defn- load-input
  "What a #replique/load becomes: the form that loads that file.

  A FORM, rather than something done here, so that loading is evaluated the way
  everything else is - its output framed in order, its failure this repl's
  exception frame, one prompt after it - which is the reason the Clojure repl
  answers the same directive with a call to its own `load!'. `load-file' is a
  repl special of the compiler's and never reaches the compiler as a form.

  A jar entry is refused rather than loaded. The Clojure repl opens the jar and
  loads what is in it, because a namespace you jumped into is a namespace you
  may want to load; the ClojureScript driver reads its sources off source paths
  and has no way to be handed an entry, so what this could do is pretend - and
  the pretence would be a file compiled from a copy nothing else can see."
  [^LoadDirective d]
  (if (:entry d)
    (refused (str "This repl cannot load an entry out of a jar: the"
                  " ClojureScript driver reads its sources off the source"
                  " paths. Put the jar's sources on them and load "
                  (:entry d) " by name."))
    (list 'load-file (:file d))))

(def ^:private reload-refusal
  (refused (str "This repl cannot reload ClojureScript yet: nothing here"
                " watches which .cljs files changed. Reload what you know"
                " changed with (require 'the.namespace :reload) or"
                " :reload-all.")))

;;; Reading

(def ^:private eof (Object.))

(defn- skip-line!
  "Discard the rest of the current line.

  A malformed form leaves the reader inside it, and this is the recovery the
  compiler's reader declines to choose - the same one clojure.main takes,
  because a repl user types one form per line and expects the bad one to be
  gone."
  [^LineNumberingPushbackReader rdr]
  (loop []
    (let [c (.read rdr)]
      (when-not (or (== c -1) (== c (int \newline)))
        (recur)))))

(defn- enter-ns!
  "Read and evaluate in NS from here on, creating it if it is new.

  Through the compiler's own `in-ns', which declares the namespace so that the
  driver treats it as one that exists rather than demanding a source file for
  it, and moves the cursor. Nothing is shipped to the runtime: a namespace
  object is made there by whatever first assigns to it.

  Nothing is framed either - that is what makes this a directive rather than a
  form. An in-ns sent as a form has a result and a prompt after it, and both
  appear in the transcript as something the developer did not write."
  [ns]
  (cljs/eval-form (list 'in-ns (list 'quote ns))))

(defn- read-input!
  "The next thing to evaluate: [form opts], ::eof, or a result to report.

  The directives are handled here and never reach the compiler. The loop is
  `clojure.main/repl-read's, for the same reasons and with the same two knobs:

  A BLANK LINE IS READ PAST rather than answered. An editor delimits an
  evaluation by the prompt that ends it, and a blank line between two top level
  forms - which is what most files look like - would end the first one twice
  and lose where the second began.

  EXCEPT AFTER A BARE #replique/ns, which is a client asking to be moved rather
  than to have something evaluated. Nothing else would answer it, since the
  prompt of a form is what says where the repl is and there is no form.

  #replique/src IS HONOURED ON BOTH HALVES. The line is set on the reader
  before the form is read, so the positions the reader records are the buffer's;
  the file is bound around the evaluation, because that is where a def reads it
  (clojure.cljs.analyzer/*source-file*). It applies to the next form only, and
  is remembered until a form consumes it so that a blank line between the two
  does not drop it."
  [conn ^LineNumberingPushbackReader rdr]
  (let [pending (volatile! nil)
        moved (volatile! false)]
    (loop []
      ;; A frame parked by a producer that found the connection busy must not
      ;; wait for the next form - the repl is about to block on the socket,
      ;; possibly for a long time, and the parked frame may well be the prompt
      ;; that says it is ready.
      (protocol/try-flush! conn)
      (case (clojure.main/skip-whitespace rdr)
        :line-start (do (when @moved
                          (vreset! moved false)
                          (protocol/write-frame! conn (prompt-frame conn)))
                        (recur))
        :stream-end ::eof
        (let [{:keys [file line]} @pending
              _ (when line (.setLineNumber rdr (int line)))
              [form text] (cljs/read-form rdr eof)]
          (clojure.main/skip-if-eol rdr)
          (cond
            (identical? form eof) ::eof

            (instance? SourceDirective form)
            (do (vreset! pending form) (recur))

            ;; pending is left alone: a #replique/src read before this one is
            ;; about the form still to come, and entering a namespace is not
            ;; that form
            (instance? NsDirective form)
            (do (enter-ns! (:ns form)) (vreset! moved true) (recur))

            ;; What this loads is a file, read from where it is, so a
            ;; #replique/src above it was about a form that never came. Dropped
            ;; rather than left pending, which would place the next form the
            ;; client sends at a line of a file it has nothing to do with.
            (instance? LoadDirective form)
            (do (vreset! pending nil) (vreset! moved false)
                (let [r (load-input form)]
                  ;; :load is the namespace this is loading, and it is the one
                  ;; thing `replique.cljs/env-hooks' fires after. Carried on the
                  ;; opts rather than worked out from the form below, because
                  ;; this is the only place that KNOWS: by the time it is a
                  ;; (load-file ...) it looks like any other form somebody could
                  ;; have typed, and loading does not move the repl, so nothing
                  ;; afterwards says what was loaded either.
                  (if (seq? r)
                    [r {:load (cljs/declared-namespace (:file form))}]
                    r)))

            (instance? ReloadDirective form)
            (do (vreset! pending nil) (vreset! moved false) reload-refusal)

            ;; the way a socket repl is ended, as in any clojure socket repl
            (identical? :repl/quit form) ::eof

            :else
            (do (vreset! pending nil)
                ;; the form is what the directive above it was about, and its
                ;; own prompt is the one that follows
                (vreset! moved false)
                [form {:text text :file file}])))))))

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

(defn- report!
  "Frame one result, after everything it printed."
  [conn flush-output! result]
  (let [f (if (= :error (:status result))
            (exception-frame result)
            (ret-frame result))]
    (flush-output!)
    (protocol/write-frame! conn f)))

(defn- load-main!
  "Compile `main' before the first prompt, and put it in the runtime if there is
  one to put it in.

  THE COMPILE IS THE PROMISE, and it is a promise about the OUTPUT DIRECTORY
  rather than about the runtime. A repl started on a namespace is started on the
  program in it: the compile is the slow half - your whole dependency graph - so
  doing it here means the first form you type is not the one that pays for it,
  and the directory the runtime fetches its modules from holds the program
  before anybody is told the repl is ready. It needs no runtime at all, which is
  the point of doing it separately - replique master's `ensure-compiled' did
  exactly this, and did it before the line that says it is waiting for the
  browser to connect.

  THE LOAD IS NOT THE PROMISE, because on the browser it is not replique's to
  make. The page is opened when you get to it and THE PAGE LOADS THE PROGRAM:
  the application's own page asks this process's server for the modules its
  namespaces were compiled into, as it did under master. So a `:main' given
  before a page is open is not a failure and is not framed as one - it did the
  half that was its to do, and the other half is waiting on a human. On node
  there is no page and nobody else to do it - `runtime!' does not return until
  the process has dialled back - so the require happens and the program is in
  the runtime by the first prompt.

  QUIET WHEN IT WORKED, because nothing asked. A `ret' frame here would arrive
  before any prompt and under no form, and a client would have nowhere to put
  it. A FAILURE TO COMPILE has somewhere to go and has to go there, since a repl
  whose :main did not compile is a repl standing in a program that is not on
  disk - and that failure is still framed whether or not anything is connected,
  which is the half a liveness test must not swallow."
  [conn flush-output! main]
  ;; The target lock and not `with-evaluation', which would claim this target's
  ;; output for CONN while the compile ran: a compile makes no runtime print, so
  ;; the only thing that could arrive is a page logging a failed fetch of its
  ;; own - and that line would come out framed as this repl's.
  (if-let [failed (try (cljs/with-target-lock* #(cljs/compile-namespace! main))
                       nil
                       (catch Throwable t
                         {:status :error :phase :repl
                          :value (or (ex-message t) (.getName (class t)))}))]
    (report! conn flush-output! failed)
    (when (cljs/runtime-connected?)
      (let [result (try
                     (cljs/with-evaluation conn
                       (cljs/eval-form (list 'require (list 'quote main))))
                     (catch Throwable t
                       {:status :error :phase :repl
                        :value (or (ex-message t) (.getName (class t)))}))]
        (when (= :error (:status result))
          (report! conn flush-output! result))))))

(defn repl
  "Run a ClojureScript repl on conn until the client disconnects. `main' is a
  namespace to compile - and, where there is a runtime for it, load - before the
  first prompt, or nil. See `load-main!'."
  ([conn] (repl conn nil))
  ([conn main]
   (let [out (frame-writer conn "out")
         err (frame-writer conn "err")
         ;; Output is flushed before every frame that concludes something, so
         ;; that a result never comes out before what the form printed. What
         ;; goes through these two is the COMPILER's output - a warning about an
         ;; undeclared var - since the program's own printing happens in the
         ;; runtime and is routed by replique.cljs.
         flush-output! (fn [] (.flush out) (.flush err))]
     (binding [*out* out *err* err]
       (cljs/with-ns* start-ns
         (fn []
           (let [rdr (cljs/reader (:in conn))]
             (try
               (when main (load-main! conn flush-output! main))
               (loop []
                 (flush-output!)
                 (protocol/write-frame! conn (prompt-frame conn))
                 (let [input (try (read-input! conn rdr)
                                  (catch IOException e (throw e))
                                  (catch Throwable t
                                    (skip-line! rdr)
                                    {:status :error :phase :read
                                     :value (or (ex-message t) (.getName (class t)))}))]
                   (cond
                     (identical? ::eof input) nil

                     ;; a directive that answered by itself, or a read that failed
                     (map? input) (do (report! conn flush-output! input) (recur))

                     :else
                     (let [[form opts] input
                           result (try
                                    ;; The interrupt first and the lock second.
                                    ;; A repl queued behind another repl's long
                                    ;; evaluation has sent a form and had no
                                    ;; prompt back, so it is evaluating as far as
                                    ;; its client is concerned - and :interrupt
                                    ;; answering "idle" there would be a lie. It
                                    ;; is the wait this can get you out of; the
                                    ;; JavaScript already running it cannot.
                                    (interruptible conn
                                      #(cljs/with-evaluation conn
                                         (let [r (cljs/eval-form form opts)]
                                           ;; INSIDE the evaluation, because a
                                           ;; hook may evaluate and what it
                                           ;; prints belongs beside what the
                                           ;; form printed - this target's
                                           ;; output is still this connection's
                                           ;; here and is not once this returns.
                                           ;;
                                           ;; AFTER A LOAD AND AFTER NOTHING
                                           ;; ELSE, and only where it worked: a
                                           ;; file that would not compile did
                                           ;; not replace the program that is
                                           ;; running.
                                           (when (and (:load opts)
                                                      (not= :error (:status r)))
                                             (cljs/run-hooks! (:load opts)))
                                           r)))
                                    ;; The flag is left CLEARED, which is what
                                    ;; `done-evaluating!' just did and what a
                                    ;; repl about to block on a socket read
                                    ;; needs: re-raising it here would leak the
                                    ;; interrupt into the next form.
                                    (catch InterruptedException _
                                      {:status :error :phase :repl
                                       :value "Interrupted."})
                                    (catch Throwable t
                                      {:status :error :phase :repl
                                       :value (or (ex-message t)
                                                  (.getName (class t)))}))]
                       (report! conn flush-output! result)
                       (recur)))))
               ;; The client is gone, or the connection is being closed
               (catch IOException _ nil)
               (finally (flush-output!))))))))))

;;; The handshake

(defn accept!
  "Take over conn as a ClojureScript repl, or say why it cannot be one.

  The runtime is started HERE, before the reply, rather than by the first form:
  starting it can fail - node may not be on PATH, a port may be taken - and a
  handshake is where a client can be told that in one frame and have the
  connection closed, instead of watching every form it sends come back with the
  same message. It is also what the reply's `url' comes from, which is the whole
  of what a browser repl needs a client to do: open that page.

  `:main' is a namespace to compile before the first prompt - see load-main!.
  Read here rather than there so that a client that wrote something that is not
  a name learns it from the handshake, where every other malformed field is
  answered."
  [conn hello]
  (let [target (cljs/as-target (or (:target hello) cljs/default-target))
        main   (protocol/as-name (:main hello))]
    (cond
      (not (cljs/available?))
      (protocol/write-frame!
       conn (protocol/error hello :no-cljs
                            (str "This process cannot run a ClojureScript repl:"
                                 " there is no ClojureScript compiler on its"
                                 " classpath. Start it with clojure.cljs on the"
                                 " classpath, on a clojure whose namespaces can"
                                 " live in a world of their own.")))

      (nil? target)
      (protocol/write-frame!
       conn (protocol/error hello :invalid-target
                            (str "A ClojureScript repl runs in one of these"
                                 " targets, and :target named none of them: "
                                 (pr-str (:target hello)))
                            {:targets (mapv name (sort cljs/targets))}))

      (and (some? (:main hello)) (nil? main))
      (protocol/write-frame!
       conn (protocol/error hello :invalid-main
                            (str "A repl started on a namespace is started on"
                                 " one this can read as a name, and :main was"
                                 " not one: " (pr-str (:main hello)))))

      :else
      (binding [cljs/*target* target]
        (let [runtime (try (cljs/runtime!)
                           (catch Throwable t t))]
          (if (instance? Throwable runtime)
            (protocol/write-frame! conn (protocol/exception-error hello runtime))
            (do
              (protocol/write-frame!
               conn (protocol/reply hello (assoc (state/info)
                                                 :role "repl"
                                                 :dialect "cljs"
                                                 :target (name target)
                                                 :connection (:id conn)
                                                 ;; nil on node, and dropped
                                                 :url (:url runtime)
                                                 ;; SAID BACK, because a client
                                                 ;; that did not ask may be the
                                                 ;; one reading this: a second
                                                 ;; editor attaching to a repl
                                                 ;; it did not start has the
                                                 ;; reply and nothing else, and
                                                 ;; which program the repl is
                                                 ;; standing on is a thing to
                                                 ;; show. Master said the same
                                                 ;; in every repl-meta; here
                                                 ;; the handshake is where a
                                                 ;; connection's facts are, and
                                                 ;; this one does not change.
                                                 ;; Absent when none was named,
                                                 ;; and dropped like `url'.
                                                 :main main)))
              ;; after the reply, as for every other connection
              (server/set-role! conn :repl)
              (repl conn (some-> main symbol)))))))))
