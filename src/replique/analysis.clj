(ns replique.analysis
  "Where a name is used, which the compiler is the only thing that knows.

  A var is not written where it is called. clojure.core/let is written let
  where core is referred, c/let where it is aliased, something else again
  where a :rename gave it another name - and a macro writes it in code nobody
  typed at all. Reading that back out of the text is guessing; the compiler
  resolved every one of them while it was compiling the file, and the only
  question is whether anything wrote it down.

  This process's clojure can, when it is the one that was built to. A
  compiler-integrated analysis subsystem - `clojure.analysis' and the sink it
  installs - folds what the compiler resolved into a model of the whole
  codebase: every usage of every var, keyword and class, with the span it was
  written at and the namespace it was written from. It is not in clojure, it
  is in a fork of it, and a process may or may not be running that one.

  So everything here is written twice: what to do when the process can
  answer, and what to do when it cannot. A process on stock clojure loads
  files the way it always did and says plainly that it has no usages to
  offer, rather than failing at the point somebody asks - see `available?'.

  ## Two models, one question

  A process running the ClojureScript compiler has a second model of its own -
  `clojure.cljs.analysis', reached through `replique.cljs-analysis' - and the
  two cannot be one. A .cljc file is a Clojure load AND a ClojureScript compile
  under one path, and cljs.core/+ is a JVM macro AND a ClojureScript function
  under one name, so a single model keyed by either would have each of them
  retracting the other.

  Which of the two a question is about is the `:dialect' the request carries,
  the way it is for every other op that reads a name - `replique.names' - and
  the switch is `replique.names/cljs?'. It is read in the two places where the
  two models differ, `usages-of' and `stale'; everything around them is one
  piece of code, because a span is a span and a client opens what it points at
  the same way whichever compiler wrote it down.

  The naming that the whole first half of this namespace is about is Clojure's
  problem alone. The ClojureScript driver names a file after the namespace the
  file declares, so `source-path' has no ClojureScript counterpart and needs
  none - see `replique.cljs-analysis'.

  ## What a file is called

  The model names a file the way the classpath does: app/util.clj, which is
  what `*file*' is bound to while the compiler reads it, and what every span
  recorded out of that file carries. An editor names a file the way the
  filesystem does: /home/me/proj/src/app/util.clj. These are the same file
  and not the same string, and the model has no idea of that.

  Which matters more than it sounds, because loading a file by its absolute
  path - what `clojure.core/load-file' does - binds `*file*' to the absolute
  path. A project loaded once by a require and once from an editor would be
  two codebases in the model, each holding half the usages, and reloading
  either would retract neither. So a load that is to be analysed is a load
  through the classpath, and a file that cannot be named that way is loaded
  the old way and not analysed - see `load!'."
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [replique.classpath :as classpath]
            [replique.cljs :as cljs]
            [replique.cljs-analysis :as cljs-analysis]
            [replique.names :as names]
            [replique.symbol :as sym])
  (:import [java.io File]
           [java.net JarURLConnection URL]
           [java.nio.file Path Paths]))

;;; Whether this process can answer at all

(def ^:private subsystem
  "The analysis subsystem of this process's clojure, or nil where it has none.

  Resolved once and kept, because the answer cannot change under a running
  process: it is a property of the clojure this jvm started with, and no
  library added later brings one.

  Everything is looked up rather than only what says it is there, so that a
  clojure carrying half of this - an older fork, one where a name moved - is
  the same answer as one carrying none of it. A partial subsystem is worse
  than an absent one: it would be reported as available and fail at the one
  op that needed the missing piece.

  `requiring-resolve' loads `clojure.analysis' where there is one, which is
  wanted: the model lives in a defonce of that namespace, so loading it is
  what there is to have loaded before the first file is analysed. Loading it
  installs nothing - the sink is a thread binding `run-analysis' pushes and
  nothing else - so a process that never analyses anything is not changed by
  having asked."
  (delay
    (try
      (let [named (fn [sym]
                    (or (requiring-resolve (symbol "clojure.analysis" (str sym)))
                        (throw (ex-info (str "No clojure.analysis/" sym) {}))))]
        {:load-file! (named 'load-file!)
         :stale-reload! (named 'stale-reload!)
         :analysed-files (named 'analysed-files)
         :changed-files (named 'changed-files)
         :stale-files (named 'stale-files)
         :classpath-changed! (named 'classpath-changed!)
         ;; A VAR RATHER THAN A FUNCTION, and the only one here: what is wanted
         ;; of it is to be bound, not to be called.  `named' answers vars
         ;; throughout - `of' hands back what `requiring-resolve' found and
         ;; every other entry is then called as a function - so this costs a
         ;; line and nothing else.
         :reload-progress (named '*reload-progress*)
         :find-usages (named 'find-usages)
         :find-keyword-usages (named 'find-keyword-usages)
         :find-class-usages (named 'find-class-usages)})
      (catch Throwable _ nil))))

(defn available?
  "Whether this process's clojure records what the compiler resolved.

  Answered in a `:process-info', for somebody looking at a process and
  wondering why it will not say where a name is used. Nothing needs it to
  ask: an op that cannot be answered says so, and says what to start the
  process on instead."
  []
  (some? @subsystem))

(defn- of
  "The analysis function called NAMED, or nil where there is no subsystem."
  [named]
  (get @subsystem named))

(defn- refuse-unless-available!
  "Refuse the request where this process records nothing, and say why.

  WHAT is what this process's clojure does not do, written as the sentence
  it goes in: a process that records nothing cannot say where a name is used
  and cannot say what it compiled either, and which of those was asked for
  is what somebody reading the refusal needs to know.

  Asked once, at the top of an op, rather than where each kind of name is
  looked up. A process on stock clojure has no usages of anything, so the
  answer must not depend on what was asked about: a name that happens to
  resolve to a namespace - which nothing records usages of either way - would
  otherwise be answered with an empty list, and read as \"this is used
  nowhere\" rather than as \"this process cannot say\".

  Named as what a client would have to change rather than as a missing var.
  Nothing is wrong with the request, and nothing about this process will make
  it work: it is running a clojure that does not record this."
  [what]
  (when-not (available?)
    (throw (ex-info (str "This process runs clojure " (clojure-version)
                         ", which does not " what ". Start it on a clojure whose "
                         "compiler does - see clojure.analysis.")
                    {:replique/error :no-analysis}))))

(defn- refuse-unless-recording!
  "Refuse the request where the compiler this thread's dialect is about records
  nothing, and say why.

  Two compilers and two answers, because the two are missing for unrelated
  reasons and are fixed by unrelated things: a Clojure question goes unanswered
  on a process running stock clojure, and a ClojureScript one on a process whose
  compiler is a build from before the analysis was written. A refusal naming the
  wrong one of those would send somebody to change the wrong half of their
  classpath.

  WHAT is the sentence-fragment both of them take, which is what lets one call
  stand in for two: it says what the compiler does not do, and neither refusal
  needs to know anything else about the request."
  [what]
  (if (names/cljs?)
    (cljs-analysis/refuse-unless-available! what)
    (refuse-unless-available! what)))

(def ^:private told-of
  "Which reading of the classpath the analysis has been told about."
  (atom nil))

(defn- tell-of-the-classpath!
  "Tell the analysis when the classpath has been read again since it heard.

  What it does with being told is forget which of the files it holds were
  read out of a jar. It skips those when it is asked what changed - a file
  inside a jar is not a file anybody edits, and resolving every one of them
  to find that out again is most of the work of asking - and a jar is only
  where a file is until a directory earlier on the classpath holds one by the
  same name. Which is a thing somebody does on purpose: a file written over a
  library's namespace is a file they then want loaded instead of it.

  Told rather than asked, because what the classpath is doing is this
  process's business and not its compiler's. Told by a number rather than
  every time, because forgetting costs the classpath being asked about every
  file it forgot, and paying that on each asking would be paying for the
  saving.

  Told on the ClojureScript path too, and that is not a mistake: what makes a
  .cljs file stale is a macro of a .clj file, and which .clj files have changed
  is the Clojure model's answer - see `replique.cljs-analysis/stale'. So the
  refusal that was made there was the other model's, and this one may have
  nothing to tell: the Clojure analysis is asked whether it is there rather than
  assumed to be."
  []
  (when (available?)
    (let [reading (classpath/reading)]
      (when-not (= reading @told-of)
        (reset! told-of reading)
        ((of :classpath-changed!))))))

;;; Naming a file the way the classpath does

(defn- canonical
  "FILE with every symbolic link and every dot resolved away, or nil.

  Which is what two paths have to be reduced to before they can be compared:
  /tmp on a mac is a link to /private/tmp, and a classpath written one way
  and a file opened the other are the same file under two names."
  ^File [^File file]
  (try (.getCanonicalFile file) (catch Exception _ nil)))

(defn- canonical-path
  "PATH with every symbolic link and every dot resolved away, or nil."
  ^Path [^Path path]
  (when-let [^File f (canonical (.toFile path))] (.toPath f)))

(defn- same-file? [^File a ^File b]
  (when-let [a (canonical a)]
    (= a (canonical b))))

(defn- above-resolved
  "TARGET with the way up to it resolved and its own name left alone, or nil.

  Which is the one file a link can stand in front of that neither of the
  other two readings of a path names - see `source-path'. Resolving nothing
  keeps a link INTO a tree and loses a tree reached THROUGH one; resolving
  everything does the opposite; and a file that is both is under a source
  root by neither name. It is under one by this one, which is the file as it
  was opened with the directories above it made into the names the classpath
  holds."
  ^Path [^Path target]
  (when-let [^Path parent (.getParent target)]
    (when-let [^Path real (canonical-path parent)]
      (.resolve real (.getFileName target)))))

(defn- reachable-as?
  "Whether reading RESOURCE off the classpath reads the file at PATH.

  The question the translation has to end on, because relativizing a path
  against a classpath root answers what the file would be called and not
  whether that name reaches it. A project with two source roots holding the
  same relative path - src/core.clj and test/core.clj, which is an ordinary
  way to lay out a fixture - has one of them shadowing the other, and the
  shadowed one is a file the classpath cannot name. Loading it under that
  name would load the other file, which is a good deal worse than not
  analysing it."
  [^String resource ^Path path]
  (when-let [^URL url (io/resource resource)]
    (and (= "file" (.getProtocol url))
         (same-file? (File. (.toURI url)) (.toFile path)))))

(defn- named-under
  "What ANCHORS call the file at TARGET, or nil.

  An anchor is a directory and what being under THAT directory contributes to
  a name - nothing for an entry of the classpath, and the way down to it for a
  directory the classpath reaches by crossing a link. Which is why the name is
  built rather than relativized: a file under a linked package is named
  `pkg/x.clj' and reached as `<root>/pkg/x.clj', and the piece in front of it
  is the piece no path arithmetic on the two ends can recover. See
  `replique.classpath/naming-anchors'.

  The first anchor holding it wins, and the name it gives is then held up
  against the classpath - an anchor can hold a file and still not be where
  that name is read from, see `reachable-as?'. Which is also what makes an
  anchor that has moved harmless: a link re-pointed under a running process
  leaves anchors naming a tree the classpath no longer reads, and a name that
  does not reach the file it was made from is thrown away rather than given
  out."
  [^Path target anchors]
  (some (fn [[^Path root ^String prefix]]
          (when (and (.startsWith target root) (not= target root))
            (let [resource (str prefix
                                (string/join "/" (map str (.relativize root target))))]
              (when (reachable-as? resource target) resource))))
        anchors))

(defn source-path
  "What the classpath calls the file at PATH, or nil when it calls it nothing.

  The path of the file under the classpath directory it is in, written with
  slashes - app/util.clj - which is the name the compiler records while it
  reads that file and the name every span out of it carries.

  ASKED OF ANCHORS RATHER THAN OF ENTRIES, which is what lets the first of
  the three questions below answer for a link BELOW a source root - a
  package directory that is a link into somewhere else, which is how one
  process reads a source tree that is somewhere else and can be made to be
  somewhere else again without being restarted. Neither end of such a path
  can be resolved into the other: the entry is a real directory and stays
  where it is, and the file resolves out of the tree altogether. What joins
  them is the pair the classpath walk wrote down as it crossed the link, and
  a name built out of that pair is a name the first question answers with.
  See `replique.classpath/naming-anchors'.

  Asked three times where it has to be, because a path is a name for a file
  and not the file, and a link puts two names on one file. First as the two
  were written, then with both of them resolved, then with the way up to the
  file resolved and the file itself left alone.

  Written first, because resolving is not the safer answer: a file linked
  INTO a source tree - which is how one file is shared between several
  worktrees - is under a source root by the name it was opened and loaded
  under, and somewhere else entirely by the name it resolves to. Resolving
  that one loses it.

  Resolved second, for the tree reached THROUGH a link. The directory a
  process was started in is resolved by the kernel before the process ever
  sees it, and an editor naming a file resolves nothing - so a project worked
  on through a link to it is a project whose every file the classpath cannot
  name, and every load from that editor records nothing at all. Which is a
  thing to be got right rather than to be quiet about: the second question is
  only asked when the first found nothing, where the answer today is no
  analysis, and it costs one resolution per source root of a load that was
  going to be given up on.

  AND THE TWO OF THEM AT ONCE, third, which is neither of the first two and
  is not rare: a project reached through a link, holding a file linked into
  it from somewhere the classpath has never heard of. Written, the file is
  under no root, because the project is spelt by the link. Resolved, it is
  under no root either, because the file resolves out of the tree
  altogether. It is under one by the reading in between - the directories
  above it resolved, its own name left as it was - which is `above-resolved'.

  Last because the first two answer on their own where only one link stands
  between the name and the file, and because the roots it asks against are
  the ones the second question has already resolved.

  Nothing for a file that is under no directory of the classpath, which is an
  ordinary thing for a file to be: a scratch buffer, a file in a directory a
  project has not added to its paths, a file being written before it is saved
  where it belongs. Nothing too for one that is shadowed by another of the
  same name earlier on the classpath - see `reachable-as?'."
  [^String path]
  (let [target (try (.normalize (.toAbsolutePath (Paths/get path (make-array String 0))))
                    (catch Exception _ nil))]
    (when target
      (let [anchors (classpath/naming-anchors)
            ;; Resolved once for the two questions that ask against them, and
            ;; not at all for the load that the first question answers. The
            ;; prefix is carried through untouched: what is being resolved is
            ;; where the directory is, not what it is called.
            resolved (delay (into [] (keep (fn [[^Path root prefix]]
                                             (when-let [real (canonical-path root)]
                                               [real prefix])))
                                  anchors))]
        (or (named-under target anchors)
            (when-let [real (canonical-path target)]
              (named-under real @resolved))
            (when-let [above (above-resolved target)]
              (named-under above @resolved)))))))

(defn entry-path
  "What the classpath calls ENTRY of the jar at FILE, or nil.

  Which is the entry itself, since an entry of a jar is already written the
  way the classpath names what is in it - but only where that jar is the one
  the classpath reads it out of. Two versions of a library on one classpath
  is a broken build and also a thing that happens, and the one somebody
  jumped into is not always the one a require would reach."
  [^String file ^String entry]
  (when-let [^URL url (io/resource entry)]
    (when (= "jar" (.getProtocol url))
      (let [connection (.openConnection url)]
        (when (instance? JarURLConnection connection)
          (when (same-file? (File. (.toURI (.getJarFileURL ^JarURLConnection connection)))
                            (File. file))
            entry))))))

;;; Loading

(defn- not-analysed!
  "Say that the file at PATH was loaded without being recorded, and why.

  Only where this process could have recorded it, since there is nothing to
  tell somebody running a clojure that records nothing: the ops that would
  have read it refuse themselves and say so.

  Said at all because the fallback is otherwise invisible. The file loads,
  the code runs, and what is missing is an answer nobody has asked for yet -
  so the first sign of it is a name reported as used nowhere, long after the
  load that would have explained it. A file under no directory of the
  classpath is an ordinary thing to load, and this is one line about it
  rather than a refusal.

  On the error stream, which a repl shows apart from what a program printed:
  it is a note about the load rather than something the file wrote."
  [^String path]
  (binding [*out* *err*]
    (println (str "Loaded " path " without analysing it: no directory of the "
                  "classpath holds it, so nothing it defines or uses was recorded."))))

(defn load!
  "Load the file at PATH, analysed where this process can analyse it.

  Always analysed rather than on request, because there is no other moment to
  do it in. What is in the model is what the compiler resolved while it was
  compiling, so the only way to record a file is to compile it - and asking
  somebody to load their project twice, once to run it and once to know about
  it, is asking them to wait twice for the same work.

  It costs nothing to a repl: a form evaluated at a prompt is compiled with
  no file around it, and the sink drops a form that is inside no file. So the
  model holds what was loaded from disk and never what somebody tried at the
  prompt, which is what makes it a model of the code rather than of the
  session.

  Loaded through the classpath where the file can be named that way, and by
  its path where it cannot - and the second is the plain `load-file' this
  always did, right down to which file it reads. Said rather than done
  quietly, where this process could have analysed it: a load that records
  nothing looks exactly like one that records everything until something is
  asked about the file - see `not-analysed!'. The difference the first one
  makes to anything but the model is `*file*': a var defined by a load
  through the classpath carries app/util.clj where one defined by a load from
  a path carries the path. Both are what `replique.symbol/source-of' reads,
  and the first is what a require would have left there anyway - so a file
  loaded from an editor and a file loaded by the application that requires it
  now say the same thing about where they came from."
  [^String path]
  (let [analyse (of :load-file!)
        ;; only where there is something to analyse with: naming a path the
        ;; way the classpath does asks the filesystem about every entry of it,
        ;; and there is nothing to do with the answer here
        source (when analyse (source-path path))]
    (if source
      (analyse source)
      (let [value (clojure.core/load-file path)]
        (when analyse (not-analysed! path))
        value))))

(defn load-entry!
  "Load ENTRY of the jar at FILE through the classpath, analysed, or nil.

  Nil where that is not what reading ENTRY off the classpath would read, or
  where this process cannot analyse anything - and the caller loads the entry
  out of the jar it named, which is the reading that is certainly right."
  [^String file ^String entry]
  (when-let [analyse (of :load-file!)]
    (when-let [source (entry-path file entry)]
      (analyse source)
      source)))

;;; Saying what a reload is doing while it does it

;; A RELOAD IS THE ONE THING HERE THAT IS WORTH WATCHING.  Every other load is
;; asked for by naming a file, so the person who asked knows what it is going to
;; do and the only news is whether it worked; a reload is asked for precisely
;; because nobody knows which files it will load, and it can be forty of them
;; and take half a minute.  The value it ends with is the list, and the value
;; arrives at the moment it stops being useful.
;;
;; SO THE MODEL SAYS WHAT IT IS DOING AS IT DOES IT, through
;; `clojure.analysis/*reload-progress*', and the wording is here because the
;; wording is replique's.  The model emits maps; what a line looks like in a
;; repl is not a question a compiler should have an opinion about.
;;
;; ONE CHANNEL FOR BOTH DIALECTS.  A ClojureScript reload loads Clojure macro
;; files on the jvm before it compiles anything, through the same
;; `reload-files!' a Clojure reload goes through, so the two halves report
;; through one var and arrive in the order they happen - see
;; `clojure.cljs.analysis/told!'.  `:dialect' says which half an event is from,
;; and the file name says it again.

(defn- progress-text
  "What EVENT prints, or nil where it is worth no line.

  ONE LINE PER FILE AND NOTHING ELSE, which is what makes this readable in a
  transcript that also has forms and values in it.  The count is on every line -
  `3/12' - so the first line already says how big this is going to be, and no
  separate announcement of the plan is owed.  What the whole list is, before any
  of it has happened, is a question with a command of its own: the `:stale' op
  answers it and compiles nothing.

  NAMED BEFORE IT IS LOADED, which is the property the whole thing is for: the
  file that never finishes compiling is the last line on the screen.  A line
  printed after a load would name every file except that one.

  A PLAN IS SILENT except where the loading lines would misrepresent it.  The
  second pass and a further ClojureScript round both restart the count, and
  somebody watching 1/3 appear under 12/12 is owed the reason rather than left
  to wonder whether it began again."
  ;; `:nth' and `:of' are bound to names of their own rather than destructured
  ;; under theirs: one is `clojure.core/nth' and the other is this namespace's
  ;; own `of', and a local that shadows either is a trap for whatever is written
  ;; here next.
  [{:keys [event file pass round files] n :nth total :of}]
  (case event
    :loading (format "  %d/%d %s" n total file)
    :plan    (cond
               (and (= 2 pass) (seq files))
               "  and again, in the order the first pass revealed:"

               (and round (seq files))
               (format "  and %d more, compiled against metadata that just changed:"
                       (count files)))
    :deleted (when (seq files)
               (str "  gone: " (string/join ", " files)))
    nil))

(defn- print-progress!
  "Write what EVENT is worth on `*out*'.

  Flushed, because what this is for is being read while it is happening: a line
  sitting in a buffer until the reload ends is the line the reload ended
  without."
  [event]
  (when-let [line (progress-text event)]
    (println line)
    (flush)))

(defn telling*
  "Call F with every reload under it saying on `*out*' what it is loading.

  WHAT A REPL WRAPS ITS RELOAD IN, and a repl rather than this namespace because
  the binding is a decision about who is watching: the same reload asked for by
  a tool that only wants the answer should print nothing, and the thread doing
  it is the only thing that knows which it is.  Dynamic for the same reason -
  two repls reloading at once are two threads, each printing into its own
  connection.

  Nothing at all where this process has no analysis subsystem: there is then no
  var to bind, and no reload to report either.  F is called all the same, and
  fails the way it was always going to."
  [f]
  (if-let [v (of :reload-progress)]
    (with-bindings {v print-progress!} (f))
    (f)))

(defn reload!
  "Load every file that changed on disk since this process read it.

  Which files those are is something only the compiler knows, and only where
  it was writing down what it compiled. A file is in the model because
  something loaded it - a load from an editor, and everything that load
  required on the way - and the model kept the time each one was last
  modified when it read it. What changed since is the difference between that
  and what the disk says now.

  And not only what changed. A macro is expanded where it is used, so a file
  that uses one holds the old expansion until that file is compiled again:
  editing a macro leaves every file that expands it wrong, and not one of
  those files changed. The model records which form expanded which macro, so
  what is loaded is the files that changed plus the files that expand a macro
  of one of them, and they are loaded macro first - a file before the files
  that expand what it defines.

  And not only what a macro reaches. A `defprotocol' generates an interface,
  and a `deftype' or a `defrecord' generates a class; what names one of them
  was compiled against the one that existed then, and loading the file that
  generates it again generates another. A type then stops satisfying the
  protocol it was written to implement - the call fails with no implementation
  of a method of a protocol, which reads like a missing `extend-type' and is
  nothing of the sort - and two classes of one name disagree about what
  `instance?' means. So a file that names a class another file generates is
  loaded after that file, the same way a file that expands a macro is. Nothing
  in the macro edges says so: `deftype' is clojure.core's macro, and the file
  whose protocol is being implemented is not in that graph at all.

  A file that merely calls a function of a changed file is not loaded, and
  needs not to be: a call goes through the var every time it runs, so the
  definition it finds is the new one. A protocol's METHODS are calls like any
  other - it is the interface, and not the method, that a file holds. It is
  the compile time dependency that goes stale, and that is the one this
  follows.

  A definition a file no longer has is unmapped. Loading a file defines what
  the file says and cannot un-define what it no longer says, so a var whose
  def form was deleted would stay interned and go on answering for a name the
  codebase does not have - and the next thing to resolve it would find the
  old function where it should have found the error that says the code is
  behind. What a reloaded file stopped defining is therefore taken away as
  the file is loaded, which is the answer the ClojureScript side gives too.

  Taken away narrowly. A var goes where the model recorded this file defining
  it, where no def of it is left anywhere - moved to another file is moved
  rather than deleted - and where the var interned now is the same object that
  was interned before the load, so a def deleted and written again under a
  fresh var is left alone. Everything a def form makes is a candidate, the
  ones written by an expansion included: a `defonce', a `defmulti', a
  `declare', a `deftype's `->Name' and `map->Name', a protocol's own var.

  And taken away everywhere it was put. A `:refer' is a mapping of its own,
  in the namespace that took it and under whatever name it took it as, so a
  var unmapped only where it was defined would go on resolving in every file
  that referred it - one name half gone. Every namespace that maps the var
  loses it, matched by the var itself and never by the name, so a namespace
  with a var of its own under that name keeps it. Which is what `:remove-var'
  does to a var named by hand; the two now mean the same thing by removing a
  definition.

  A protocol's METHODS are the exception, and they are taken away by their
  protocol rather than by the model: they are interned and never def-ed, so
  nothing recorded them to be missed, and what says one has gone is the
  protocol itself - a method dropped from a `defprotocol' is a method the
  protocol has stopped naming, and a `defprotocol' deleted takes all of them.
  Which is worth having because of what the alternative looks like: a method
  nothing names any more still resolves, and the call fails at run time with
  no implementation of a method of a protocol, which reads like a missing
  `extend-type' rather than like code that is behind.

  A var an `intern' made is the one thing left where it was. Nothing recorded
  it and no protocol speaks for it, so the direction is the chosen one there:
  something that should have gone stays, rather than something live being
  taken away - and `:remove-var' is how it goes, by name.

  A file the disk no longer has is dropped. Nothing can load a file that is
  gone, so it is not a file to load again - and not one to leave alone
  either, since what it defined is still defined here and nothing else will
  ever notice that the file behind it went away. What it defined is unmapped,
  the way a deleted def is, with the whole file playing the part of the def;
  the model stops answering for it, so nothing points at a file no editor can
  open; and its namespaces stop being loaded libs, so requiring one says what
  the disk says rather than finding the entry the `ns' form left behind and
  doing nothing. A namespace another file still defines keeps its entry, and
  the namespace object is left standing either way - with its aliases, its
  imports and whatever an `intern' made in it, none of which this speaks for.

  Dropped before the changed files are loaded, so that one of them still
  using a definition of a file that is gone fails where it uses it rather
  than compiling clean against a file nobody can open. Not answered, though:
  the answer is the files that were loaded, and a file that is gone is not
  one of them. Nor is a file between two writes told from one that is gone -
  which is why this is asked for by hand and nothing here watches the disk.

  A var something still uses goes all the same, and says so on this repl's
  error stream, naming where the usages were. What uses it may be something
  nothing recorded - a call made by reflection, or a file this process
  compiled before anything was watching - so a usage is a thing to be told
  about rather than a veto. A protocol method's usages are its call sites and
  not the types that implement it, so a method whose implementations are all
  `deftype's goes without a word here; those files say so the next time they
  compile, which is later and elsewhere.

  Answers the files it loaded, in the order it loaded them, named the way the
  classpath names them. Which is the one thing here that is news: this is
  asked for without knowing what it will do, where every other load is asked
  for by naming the file.

  Fails the way a load fails. The first file that will not compile throws,
  and the files after it are not loaded - and nothing is lost by that, since
  a file that failed to load keeps the time it had before and is therefore
  still changed. Asking again carries on from where this stopped."
  []
  (refuse-unless-available! "keep track of what it compiled")
  (tell-of-the-classpath!)
  ((of :stale-reload!) :prune true))

;; NO CLOJURESCRIPT COUNTERPART OF `reload!' HERE, and the asymmetry with
;; `stale' below is deliberate. A Clojure reload ends when the files have been
;; loaded on this JVM; a ClojureScript one has a second half - the bodies of the
;; recompiled namespaces have to be run in the runtime this repl is talking to,
;; in the order the recompile decided - and that half belongs to the repl rather
;; than to the model. The compiler's own `stale-reload' special is where it is
;; written, and `replique.cljs-repl/reload-input' is the `#replique/reload' that
;; evaluates it.

(defn- by-name
  "The sources SOURCES point at, as a client opens them, in name order.

  Which is not the order they would be loaded in. That order is decided
  while the loading happens - a file edited to use a macro it did not use
  before adds an edge that nothing knew about until it has been compiled
  once - so a list made beforehand claiming to be it would be claiming to
  know something it cannot. What this is is a list to read and to open
  things from, and the order to read a list of files in is their names.

  One that reaches no file is left out, the way a usage of a deleted file
  is: the model holds what it read, and the disk is free to have moved on."
  [sources]
  (vec (sort-by (juxt #(str (:entry %)) #(str (:file %)))
                (keep sym/source-of sources))))

(defn stale
  "What would be loaded if this process were asked to load what changed.

  The same question `reload!' answers by doing it, asked without doing it:
  the file times are read and the macro graph is walked, and nothing is
  compiled. Which is a question worth asking on its own - what a reload is
  about to do is a thing to look at before it does it, and \"these two files
  make those five need compiling\" is not something anybody can work out by
  looking at their buffers.

  Answered as two lists, because they are two different facts:

    :changed  the file on disk is newer than what this process read
    :stale    the file has not changed, and what this process holds of it is
              out of date all the same - it expands a macro of a file that
              did change, and holds the expansion the old one made

  Disjoint, and together they are what a reload would load. The second is
  the half nothing but the compiler can know.

  And `:analysed', how many files this process has read. Two empty lists
  have two meanings - up to date, or nothing here to be out of date - and a
  client that cannot tell them apart has to guess which it is showing.

  A file a jar answered to is in neither list until the classpath has been
  read again, which is the one thing here that is not a fact about the disk -
  see `tell-of-the-classpath!'.

  Both are read off one reading of the disk: the files that changed are what
  the stale ones are worked out from, so asking for them separately would be
  asking the filesystem about every analysed file twice, and would leave the
  two answers free to disagree about a file saved in between."
  []
  (refuse-unless-recording! "keep track of what it compiled")
  (tell-of-the-classpath!)
  (if (names/cljs?)
    ;; The two lists come back already parted there, because what parts them is
    ;; not one subtraction: a ClojureScript file can be stale for a reason that
    ;; is not a file at all - a var whose metadata it was compiled against has
    ;; changed - and the model is the only thing that can say so
    (let [{:keys [changed stale analysed]} (cljs-analysis/stale)]
      {:changed (by-name changed)
       :stale (by-name stale)
       ;; Whether anything has been compiled here at all, which the Clojure
       ;; branch answers too and for the same reason - see it.
       :analysed analysed
       ;; AND WHETHER THERE IS ANYWHERE TO PUT IT, which is half of what would
       ;; happen if a reload were asked for and is a half the Clojure question
       ;; does not have.  A Clojure reload ends when the files have been loaded
       ;; on this jvm; a ClojureScript one has a second act - the bodies have to
       ;; be run in the runtime - and a browser runtime with no page connected
       ;; is a reload that would compile everything and land nowhere.  So it
       ;; belongs in the answer to "what would a reload do", which is what this
       ;; op is.
       ;;
       ;; Asked without starting anything - see `replique.cljs/runtime-connected?'
       ;; - so a client asking what is stale does not open a port or start a
       ;; node process by asking.  Which also means false where no repl has ever
       ;; been opened on this target, and that is the honest answer: there is
       ;; nowhere to run anything.
       :connected (cljs/runtime-connected?)})
    (let [changed ((of :changed-files))]
      {:changed (by-name changed)
       :stale (by-name (remove changed ((of :stale-files) changed)))
       ;; AND WHETHER THIS PROCESS HAS READ ANYTHING AT ALL, because the two
       ;; ways the lists come back empty are not the same fact and read the
       ;; same.  "Nothing has changed" says the program is up to date;
       ;; "nothing has been loaded here" says this process knows of no files
       ;; and would go on saying nothing whatever was edited.  Which is a
       ;; state a Clojure repl is in more often than it looks: what the model
       ;; holds is what the compiler read UNDER THE SINK, so a namespace that
       ;; arrived by `require' - at a prompt, or from an init script - is
       ;; loaded and is not in it.  A client with an empty answer and no way
       ;; to tell them apart can only report the wrong one of the two.
       ;;
       ;; A COUNT AND NOT A FLAG, so that a client reading an answer with no
       ;; such key in it - an older process than itself - can tell that from
       ;; a process saying it has read nothing, and go on saying what it used
       ;; to say rather than announcing an empty model that is not there.
       :analysed (count ((of :analysed-files)))})))

;;; What was found

(defn- located
  "The span SPAN, as a client opens what it points at.

  The file in the two halves the protocol writes one in - a path, or the jar
  and the entry inside it - which is what `replique.symbol' answers a
  definition with, and what an editor already knows how to open.

  Nothing where the source reaches no file. A span is written down when the
  compiler reads a file and read back long afterwards, and by then the file
  may have been deleted, moved out of the classpath, or replaced by a jar.

  `:dead' IS CARRIED, and it is the one thing here that a client cannot work
  out by opening the file: a name written inside a `#_' or a `(comment ...)'
  is `:discard' or `:comment', and one in code that runs carries nothing. The
  model has the distinction because the compiler resolves dead code without
  compiling it, and this process already acts on it - a prune says nothing
  about a var whose only use is in a comment block - so a list of places that
  did not carry it would be this process knowing which of them are real and
  not saying.

  And `:declaration', which is the other half of the same courtesy: `:refer'
  where the place is the name written in the `ns' form's own `:refer' or
  `:only', and `:import' where it is the name written in its `:import'. Those
  are places a rename has to rewrite and are not uses of anything, and a
  client showing a list of call sites wants to say which is which."
  [{:keys [source line column end-line end-column from-ns macro dead declaration]}]
  (when-let [found (sym/source-of source)]
    (cond-> (assoc found :line line :column column)
      end-line (assoc :end-line end-line)
      end-column (assoc :end-column end-column)
      from-ns (assoc :from-ns (str from-ns))
      macro (assoc :macro true)
      dead (assoc :dead dead)
      declaration (assoc :declaration declaration))))

(defn- in-reading-order
  "The usages sorted the way somebody reads them: by file, and down each file.

  The model holds them in a set, which has an order of its own that is not
  anybody's. A list an editor shows has to come back the same way twice -
  what is in it is a list of places to walk through, and one that shuffles
  between two askings is one nothing can be walked through."
  [usages]
  (vec (sort-by (juxt #(or (:entry %) "") #(or (:file %) "") :line :column) usages)))

(defn- usages-of
  "Every place FOUND is used, as `located' writes one.

  FOUND is what `:symbol' answers - the var, keyword or class the name at
  point resolves to - rather than the name as it was written, which is the
  whole point: the question is about the thing, and what a namespace calls it
  is a different question with its own op.

  A name that resolves to something no usage was recorded for is answered
  with none, which is also what a codebase nothing has loaded yet answers.
  The two are not told apart here: what the model holds is what has been
  loaded, and saying so is the client's business - it is the half that knows
  whether anything has been loaded at all.

  THE MODEL IS THE DIALECT'S, and the three kinds are not the same three. A var
  and a keyword are asked of both, each of its own model. A CLASS IS ASKED OF
  CLOJURE ONLY, and answering nothing for ClojureScript is the honest answer
  rather than a gap: what a .cljs file refers to is a JavaScript global, a
  Closure namespace or an npm export, and the ClojureScript model records those
  as host references keyed by what they name and by the name the source wrote -
  a different question, whose answer has more in it than a list of places, and
  which wants an op of its own rather than to be squeezed through this one.

  A PROTOCOL ANSWERS WHAT IMPLEMENTS IT as well, and nothing here does that: it
  is the Clojure model that puts the two together, because a `deftype', a
  `defrecord' or a `reify' naming a protocol names the interface the protocol
  generated by the time the compiler sees it, and the place is recorded against
  that interface. Which makes who implements a protocol the same question as
  where it is used, asked with the same op - and leaves a protocol method the
  question it looks like, its call sites, since a type need not implement every
  method it could. ClojureScript records a type's protocols another way and has
  no such answer yet."
  [{:keys [type name ns package] :as found}]
  (when found
    (let [cljs (names/cljs?)]
      (case type
        ("function" "macro" "var")
        (when ns
          (let [qualified (symbol ns name)]
            (if cljs
              (cljs-analysis/usages qualified)
              ((of :find-usages) qualified))))

        "keyword"
        (let [kw (if ns (keyword ns name) (keyword name))]
          (if cljs
            (cljs-analysis/keyword-usages kw)
            ((of :find-keyword-usages) kw)))

        "class"
        (when-not cljs
          ((of :find-class-usages) (if package (str package "." name) name)))

        nil))))

(defn usages
  "Where the name MSG carries is used, as the reply frame carries it.

  The name is resolved the way `:symbol' resolves it and answered along with
  the usages, for two reasons. A client asking this has a name at point and
  not a var, so something has to do that resolving anyway; and what it has to
  show above the list is what the name turned out to be - \"12 usages of
  clojure.core/let\" rather than \"12 usages of let\", which would be a
  heading that does not say which let."
  [msg]
  (refuse-unless-recording! "record where names are used")
  (let [found (:symbol (sym/named msg))]
    {:symbol found
     :usages (in-reading-order (keep located (usages-of found)))}))
