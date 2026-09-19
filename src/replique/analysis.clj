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

;;; Naming a file the way the classpath does

(defn- canonical
  "FILE with every symbolic link and every dot resolved away, or nil.

  Which is what two paths have to be reduced to before they can be compared:
  /tmp on a mac is a link to /private/tmp, and a classpath written one way
  and a file opened the other are the same file under two names."
  ^File [^File file]
  (try (.getCanonicalFile file) (catch Exception _ nil)))

(defn- same-file? [^File a ^File b]
  (when-let [a (canonical a)]
    (= a (canonical b))))

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

(defn source-path
  "What the classpath calls the file at PATH, or nil when it calls it nothing.

  The path of the file under the classpath directory it is in, written with
  slashes - app/util.clj - which is the name the compiler records while it
  reads that file and the name every span out of it carries.

  Nothing for a file that is under no directory of the classpath, which is an
  ordinary thing for a file to be: a scratch buffer, a file in a directory a
  project has not added to its paths, a file being written before it is saved
  where it belongs. Nothing too for one that is shadowed by another of the
  same name earlier on the classpath - see `reachable-as?'."
  [^String path]
  (let [target (try (.normalize (.toAbsolutePath (Paths/get path (make-array String 0))))
                    (catch Exception _ nil))]
    (when target
      (some (fn [^Path root]
              (when (and (.startsWith target root) (not= target root))
                (let [resource (string/join "/" (map str (.relativize root target)))]
                  (when (reachable-as? resource target) resource))))
            (classpath/directories)))))

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
  always did, right down to which file it reads. The difference the first one
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
      (clojure.core/load-file path))))

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

  Nothing is unmapped. A definition deleted from a file leaves its var
  behind, because loading a file defines what the file says and cannot
  un-define what it no longer says - and replique already answers that with
  `:remove-var', a definition taken away by name, by somebody who knows it is
  gone. Unmapping it here instead would be a second answer to the same
  question, given silently, in a process where what still uses that var may
  be something nothing recorded: a call made by reflection, or a file this
  process compiled before anything was watching.

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
  ((of :stale-reload!)))

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
  whether anything has been loaded at all."
  [{:keys [type name ns package] :as found}]
  (when found
    (case type
      ("function" "macro" "var")
      (when ns (mapv located ((of :find-usages) (symbol ns name))))

      "keyword"
      (mapv located ((of :find-keyword-usages) (if ns (keyword ns name) (keyword name))))

      "class"
      (mapv located ((of :find-class-usages) (if package (str package "." name) name)))

      nil)))

(defn usages
  "Where the name MSG carries is used, as the reply frame carries it.

  The name is resolved the way `:symbol' resolves it and answered along with
  the usages, for two reasons. A client asking this has a name at point and
  not a var, so something has to do that resolving anyway; and what it has to
  show above the list is what the name turned out to be - \"12 usages of
  clojure.core/let\" rather than \"12 usages of let\", which would be a
  heading that does not say which let."
  [msg]
  (refuse-unless-available! "record where names are used")
  (let [found (:symbol (sym/named msg))]
    {:symbol found
     :usages (in-reading-order (remove nil? (usages-of found)))}))
