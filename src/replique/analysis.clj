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
         :changed-files (named 'changed-files)
         :stale-files (named 'stale-files)
         :classpath-changed! (named 'classpath-changed!)
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
  "What the directories ROOTS call the file at TARGET, or nil.

  The first root holding it wins, and the name it gives is then held up
  against the classpath - a root can hold a file and still not be where that
  name is read from, see `reachable-as?'."
  [^Path target roots]
  (some (fn [^Path root]
          (when (and (.startsWith target root) (not= target root))
            (let [resource (string/join "/" (map str (.relativize root target)))]
              (when (reachable-as? resource target) resource))))
        roots))

(defn source-path
  "What the classpath calls the file at PATH, or nil when it calls it nothing.

  The path of the file under the classpath directory it is in, written with
  slashes - app/util.clj - which is the name the compiler records while it
  reads that file and the name every span out of it carries.

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
      (let [roots (classpath/directories)
            ;; Resolved once for the two questions that ask against them, and
            ;; not at all for the load that the first question answers
            resolved (delay (keep canonical-path roots))]
        (or (named-under target roots)
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

  A file that merely calls a function of a changed file is not loaded, and
  needs not to be: a call goes through the var every time it runs, so the
  definition it finds is the new one. It is the compile time dependency that
  goes stale, and that is the one this follows.

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
  fresh var is left alone. A var nothing recorded as a def is never a
  candidate at all, which is what keeps a `defprotocol's methods, interned
  rather than def-ed; the cost of the same rule is that a deleted `defonce',
  which a reload does not re-def either, lingers until the model is built
  again from cold. The direction is chosen: something that should have gone
  stays, rather than something live being taken away.

  A var something still uses goes all the same, and says so on this repl's
  error stream, naming where the usages were. What uses it may be something
  nothing recorded - a call made by reflection, or a file this process
  compiled before anything was watching - so a usage is a thing to be told
  about rather than a veto, and `:remove-var' is still how a definition is
  taken away by name, by somebody who knows it is gone.

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
    (let [{:keys [changed stale]} (cljs-analysis/stale)]
      {:changed (by-name changed)
       :stale (by-name stale)})
    (let [changed ((of :changed-files))]
      {:changed (by-name changed)
       :stale (by-name (remove changed ((of :stale-files) changed)))})))

;;; What was found

(defn- located
  "The span SPAN, as a client opens what it points at.

  The file in the two halves the protocol writes one in - a path, or the jar
  and the entry inside it - which is what `replique.symbol' answers a
  definition with, and what an editor already knows how to open.

  Nothing where the source reaches no file. A span is written down when the
  compiler reads a file and read back long afterwards, and by then the file
  may have been deleted, moved out of the classpath, or replaced by a jar."
  [{:keys [source line column end-line end-column from-ns macro]}]
  (when-let [found (sym/source-of source)]
    (cond-> (assoc found :line line :column column)
      end-line (assoc :end-line end-line)
      end-column (assoc :end-column end-column)
      from-ns (assoc :from-ns (str from-ns))
      macro (assoc :macro true))))

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
  which wants an op of its own rather than to be squeezed through this one."
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
