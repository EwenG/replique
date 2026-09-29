(ns replique.cljs-analysis
  "Where a ClojureScript name is used, and what the compiler has fallen behind.

  `replique.analysis' asked of the other compiler, and the same two questions:
  where is this used, and what on disk has moved on since this process read it.
  The ClojureScript model is a model of its own - `clojure.cljs.analysis' - and
  not `clojure.analysis' fed a second language, for the reason that namespace
  argues: a .cljc file is a Clojure load AND a ClojureScript compile under one
  path, and cljs.core/+ is a JVM macro AND a ClojureScript function under one
  name, so the two cannot share keys. Two models, one question each, and this
  is the ClojureScript one's side of the wire.

  ## What is not here, and why there is so little

  NO NAMING. `replique.analysis/source-path' exists because a Clojure file is
  recorded under whatever `*file*' was bound to while it was read, which for a
  load from an editor is an absolute path - so a file loaded twice by two
  different names is two codebases in one model. The ClojureScript driver has
  no such problem: it names a file after the namespace the file declares
  (`clojure.cljs.driver/source-label' - my_lib/core.cljs), so a file compiled
  from an absolute path and the same file reached through a require are one key
  by construction. A `#replique/load' of a .cljs file needs no translation and
  gets none.

  NO LOADING EITHER. A Clojure file is loaded by `replique.analysis/load!',
  which has to choose between a load that is recorded and one that is not; a
  ClojureScript file is loaded by the compiler's own `load-file' special,
  which compiles under whatever sink the thread is carrying. So all a load
  needs here is that the sink be on the thread - see `with-analysis*' - and
  every compile replique asks for is under one.

  ## Written twice, like everything about the compiler

  `replique.cljs's shape and for its reason: the compiler is not a dependency
  of replique, so one answer says whether the process can be asked at all and
  an op that cannot be answered says so, and says what to start the process on
  instead. Separate from `replique.cljs/available?' although the two travel
  together - the analysis ships inside the compiler - because a build of the
  compiler from before it was written is a process that compiles ClojureScript
  perfectly well and cannot say where a name is used."
  (:require [replique.cljs :as cljs]))

;;; Whether this process can answer at all

(def ^:private subsystem
  "The ClojureScript analysis of this process's compiler, or nil where there is
  none.

  Resolved once and kept, for `replique.cljs/subsystem's reasons, and looked up
  in full rather than by one name that stands for the rest: a build carrying
  half of this - an older checkout, a name that moved - is the same answer as
  one carrying none of it, because a partial subsystem would be reported as
  available and fail at the one op that needed the missing piece.

  `requiring-resolve' loads the compiler where there is one, which is why this
  is a delay: a process nobody asks a ClojureScript question of never pays it."
  (delay
    (try
      (let [named (fn [sym]
                    (or (requiring-resolve (symbol "clojure.cljs.analysis" (str sym)))
                        (throw (ex-info (str "No clojure.cljs.analysis/" sym) {}))))]
        {:run-analysis (named 'run-analysis)
         :find-usages (named 'find-usages)
         :find-macro-usages (named 'find-macro-usages)
         :find-keyword-usages (named 'find-keyword-usages)
         :find-host-usages-where (named 'find-host-usages-where)
         :analysed-files (named 'analysed-files)
         :changed-files (named 'changed-files)
         :changed-macro-files (named 'changed-macro-files)
         :stale-macro-files (named 'stale-macro-files)
         :stale-files (named 'stale-files)
         :deleted-files (named 'deleted-files)})
      (catch Throwable _ nil))))

(defn available?
  "Whether this process's ClojureScript compiler records what it resolved."
  []
  (some? @subsystem))

(defn- of
  "The analysis function called NAMED, or nil where there is no subsystem."
  [named]
  (get @subsystem named))

(defn refuse-unless-available!
  "Refuse the request where this process's compiler records nothing, and say why.

  WHAT is what the compiler does not do, written as the sentence it goes in -
  `replique.analysis/refuse-unless-available!' takes it the same way and for
  the same reason: a process that records nothing has no usages of anything, so
  the answer must not depend on what was asked about. A name that happens to
  resolve to something the model would hold none of would otherwise come back
  as an empty list and be read as \"this is used nowhere\".

  Two refusals can be owed, and the other one is asked first: a process with no
  compiler at all cannot answer a ClojureScript question of any kind, and
  `replique.names/with-dialect' has already said so by the time anything here
  is reached."
  [what]
  (when-not (available?)
    (throw (ex-info (str "The ClojureScript compiler of this process does not "
                         what ". Put a build of it that does on the classpath - "
                         "see clojure.cljs.analysis.")
                    {:replique/error :no-cljs-analysis}))))

;;; Recording what is compiled

(defn with-analysis*
  "Call F with the model's sink installed on this thread.

  WHICH IS THE WHOLE OF HOW A FILE GETS RECORDED. The sink is a thread binding
  the driver reads (`clojure.cljs.driver/*sink*'), so every file compiled under
  F replaces its own slice of the model as it finishes, and a file that throws
  keeps what it had.

  ALWAYS, rather than on request, for `replique.analysis/load!'s reason: what
  is in the model is what the compiler resolved while it was compiling, so the
  only moment to record a file is the moment it is compiled. Asking somebody to
  compile their program twice - once to run it and once to know about it - is
  asking them to wait twice for the same work.

  It costs a repl nothing. The model is disk-only: a form typed at a prompt is
  compiled inside no file, so the sink has no frame to file it under and drops
  it. What the model holds is the code, not the session.

  A no-op where this process cannot analyse, so that a caller which merely
  wants the compile to happen does not have to ask whether it will be
  recorded."
  [f]
  (if-let [run (of :run-analysis)] (run f) (f)))

;;; What was found

(defn usages
  "Every place the var QSYM names is used, as the model records a span.

  BOTH INDEXES, because one name can be two things. `cljs.core/str' is a macro
  when it is called and a ClojureScript function when it is passed to `map',
  and the model keys those separately - a macro under the JVM var that expanded
  it, a function under the ClojureScript var - precisely so that a tool asking
  about one is not handed the other. A person asking where a name is used is
  asking about the name, so this is their union.

  Which is also why one lookup would not do. A macro has no ClojureScript var
  to be used by, and a function is never expanded, so whichever index a single
  lookup chose would be empty for half the names anybody asks about.

  Refused where there is nothing to read, rather than answered with none: a
  name used nowhere and a process that cannot say are not the same answer, and
  which one it is has to come back as which one it is.

  AND THE MACRO HALF IS MARKED ON THE WAY IN. In this model a place is a macro
  call by virtue of which index it is filed under, and the union is where that
  stops being visible - so what the index says is written onto each place as it
  is merged, which is where Clojure keeps it anyway: there the flag is the var's
  own :macro, so every place a macro is named carries it, a :refer in an `ns'
  form included. A client showing a list of call sites can then say which are
  expansions in either dialect, rather than in one."
  [qsym]
  (refuse-unless-available! "record where names are used")
  (into (or ((of :find-usages) qsym) #{})
        (map #(assoc % :macro true))
        (or ((of :find-macro-usages) qsym) #{})))

(defn keyword-usages
  "Every place KW is written, as the model records a span.

  An auto-resolved keyword is recorded fully qualified - ::x in app.core is
  found as :app.core/x - which is what makes this answerable at all: what ::x
  means is whatever namespace the file is, and no reader of the text knows
  that.

  Refused where there is nothing to read, for `usages's reason."
  [kw]
  (refuse-unless-available! "record where names are used")
  (or ((of :find-keyword-usages) kw) #{}))

;;; What has moved on

(defn host-usages
  "Every reference to the host whose ref satisfies PRED, with the places it is
  used: {ref #{span}}, the ref in the model's host-ref shape -

    {:kind :global    :name js/console.log}
    {:kind :goog-var  :name goog.string/trim}
    {:kind :goog-ns   :name goog.math.Long}
    {:kind :js-module :specifier \"react\" :export \"useState\"}

  plus the :written symbol each place spelled it with.

  BY REF AND NOT AS ONE SET, because the question is often a package: every
  export of react, every var of goog.string - and which of them each place is,
  which the places do not carry and only the key they are filed under says.

  Refused where there is nothing to read, for `usages's reason."
  [pred]
  (refuse-unless-available! "record where names are used")
  (or ((of :find-host-usages-where) pred) {}))

(defn stale
  "What a reload would compile: {:changed #{source} :stale #{source}
  :deleted [source] :analysed n}.

  Sources as the model names them - my_lib/core.cljs - and the first two
  disjoint, the way `replique.analysis/stale' answers the Clojure question:

    :changed  the file on disk is newer than the version this process compiled
    :stale    the file has not changed, and what this process holds of it is
              out of date all the same
    :deleted  this process compiled the file and the disk no longer has it

  THE SECOND HALF IS A DIFFERENT SHAPE OF FACT HERE. A Clojure file goes stale
  because it expands a macro of a file that changed, and the macros live in the
  same files as the code; a ClojureScript file's macros live in Clojure files
  on the JVM, so what makes one stale is an edit to a .clj it expands from - and
  an edit to a .clj that the macro file itself expands from, through the
  Clojure model's own graph. It can also go stale with nothing edited anywhere
  near it: it was compiled against a var's `:tag' or its arities, and the var
  in the compile environment no longer has them, which is what a def typed at
  this very repl can do.

  THE COMPILE ENVIRONMENT IS ASKED AND SO IT IS MADE. `:stale' is the one
  reading op that can want a compile environment it has no other reason for,
  since the metadata half of the question is about vars rather than about
  files - and making one compiles cljs.core, which is seconds. It is the same
  environment every other ClojureScript question uses and is made once per
  process per target.

  No cascade among the .cljs files themselves, and that is right rather than
  missing: a var is a property looked up at run time, so editing the file a
  var is defined in leaves the code that calls it correct.

  AND THE MACRO FILES ARE IN BOTH LISTS, although neither is a file this
  compiles. They are Clojure files, loaded on the jvm before anything is
  recompiled, and a reload loads them - so they are part of what a reload
  would do, which is the question. Leaving them out named the effect and not
  the cause: a .cljs file with nothing in it touched appeared as stale beside
  an empty changed list, and the one file that would have explained it - the
  .clj whose macro it expands - was in neither. Which is the case this op
  exists for, since it is the half nobody can work out from their buffers.

  The one that was edited is changed and the ones that expand its macros in
  turn are stale, the same way round as on the ClojureScript side. A macro
  file this process also loaded through the Clojure model is named by the
  Clojure question too, and that is not a duplicate: the two ops answer about
  two compiles, and it is out of date in both."
  []
  (refuse-unless-available! "record what it compiled")
  (let [{:keys [cenv]} (cljs/environment)
        ;; ONE READING OF THE DISK FOR ALL THREE ANSWERS. What is gone is what a
        ;; reload would drop and what the other two must not name, so
        ;; `stale-files' is handed it rather than asking again: asking costs a
        ;; classpath lookup per compiled file, and two askings are two moments
        ;; of a disk free to have moved in between.
        gone ((of :deleted-files))
        changed (into (set ((of :changed-files))) ((of :changed-macro-files)))]
    {:changed changed
     :stale (into #{} (remove changed)
                  (concat ((of :stale-files) cenv gone) ((of :stale-macro-files))))
     ;; And the files this process compiled that the disk no longer has, which is
     ;; neither of the two above and is what a branch switch mostly does. They are
     ;; not stale - there is nothing to compile them from - and a reload does not
     ;; leave them alone either: it retracts them and takes their definitions out
     ;; of the compile environment. So the one question this op asks - what would a
     ;; reload do - has no answer without them.
     ;;
     ;; As the model names them, which every list here is, and here it is the
     ;; only way there is: what is gone is what nothing answers for.
     :deleted (vec (sort gone))
     ;; And whether anything has been compiled here at all - see
     ;; `replique.analysis/stale', where the same key answers the same
     ;; question about the JVM side and for the same reason. It is rarer to
     ;; be false here, since a repl compiles its main on the way up, but a
     ;; main that would not compile leaves exactly this state.
     :analysed (count ((of :analysed-files)))}))
