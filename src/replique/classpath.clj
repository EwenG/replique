(ns replique.classpath
  "What is on the classpath: the namespaces that could be required, the
  classes that could be imported, the resources that could be read.

  The classpath rather than what the process has loaded, because a require is
  written for a namespace that has not been loaded - writing one is what loads
  it - and `all-ns` knows only the ones that have. The same holds for a class,
  which nothing loads until something names it.

  Read once, when this namespace is loaded, and kept until something asks for
  it to be read again. Reading it walks every jar and every directory on it,
  which costs a fifth of a second on a classpath of a hundred and fifty jars,
  and a directory is walked whole - a source tree is cheap and a large
  resources directory is not. That is not work to do behind a keystroke.

  What it costs is a file written afterwards, which is not found until the
  reading is done again - and a namespace somebody has just created is exactly
  the namespace they are about to require. The :update-classpath op is what
  says the reading is due, and an editor that watches the files of a project
  knows when to send it."
  (:require [clojure.string :as string])
  (:import [java.io File]
           [java.lang.module ModuleDescriptor$Exports ModuleFinder ModuleReader
            ModuleReference]
           [java.net URL URLClassLoader]
           [java.nio.file FileVisitOption FileVisitResult FileVisitor Files LinkOption Path Paths]
           [java.util.jar JarEntry JarFile]))

;;; Reading a name out of a resource

(def ^:private source-extensions
  "The extensions a Clojure namespace is written in."
  [".clj" ".cljc"])

(def ^:private cljs-source-extensions
  "The extensions a ClojureScript namespace is written in.

  A .cljc is in both lists, and that is the point of there being two: one
  file provides a namespace to each of the two worlds ClojureScript compiles
  with, and which of them a client is asking about is the thing only the
  client knows. Read into two lists here rather than sorted out at the
  question, because the classpath is read once and the question is asked on a
  keystroke."
  [".cljs" ".cljc"])

(defn- stem-of
  "The path of RESOURCE without whichever of EXTENSIONS it ends in, or nil
  when it ends in none of them."
  ^String [^String resource extensions]
  (some (fn [^String extension]
          (when (.endsWith resource extension)
            (subs resource 0 (- (.length resource) (.length extension)))))
        extensions))

(defn- source-stem
  "The path of RESOURCE without the extension that makes it a Clojure
  namespace, or nil when it is not one."
  ^String [^String resource]
  (stem-of resource source-extensions))

(defn- cljs-source-stem
  "The path of RESOURCE without the extension that makes it a ClojureScript
  namespace, or nil when it is not one."
  ^String [^String resource]
  (stem-of resource cljs-source-extensions))

(defn- namespace-name
  "The namespace the source at STEM provides.

  The underscores a file name carries where the namespace has dashes are put
  back: it is the namespace that gets written in a require, and the file name
  is what the loader makes of it."
  ^String [^String stem]
  (-> stem (.replace \/ \.) (.replace \_ \-)))

(def ^:private synthetic-class
  "A class the compiler made up rather than one anybody wrote. What follows a
  dollar in the name of one starts with a digit, which no name written in
  source can do, so this tells the two apart without knowing which compiler
  produced it."
  #"\$\d")

(defn- class-name
  "The class the resource at RESOURCE is, or nil when it is not one.

  The descriptors are not classes and neither is a synthetic one: none of the
  three is a name that can be imported, and every one of them would be offered
  under the package somebody is importing from."
  ^String [^String resource]
  (when (and (.endsWith resource ".class")
             (not (.endsWith resource "module-info.class"))
             (not (.endsWith resource "package-info.class")))
    (let [name (.replace (subs resource 0 (- (.length resource) 6)) \/ \.)]
      (when-not (re-find synthetic-class name)
        name))))

(defn prefixes
  "Every name above these, a piece at a time.

  The packages of a set of classes, and the same answer for a set of
  namespaces: what is above java.util.Date is java.util and java, and what is
  above clojure.core.specs.alpha is clojure.core.specs and clojure.core and
  clojure.

  All the way up rather than the one directly above, because such a name is
  written a piece at a time: somebody who typed java.u is writing java.util,
  and what is offered there has to be a name that holds nothing of its own."
  [names]
  (persistent!
   (reduce (fn [acc ^String name]
             (loop [acc acc index (.indexOf name (int \.))]
               (if (neg? index)
                 acc
                 (recur (conj! acc (subs name 0 index))
                        (.indexOf name (int \.) (inc index))))))
           (transient #{}) names)))

(defn- collect
  "What the resources named by RESOURCES provide, as the kinds of name they
  can be asked for.

  Everything that is neither a class nor a source is a resource: a name
  `clojure.java.io/resource' answers to and nothing else here does. A class
  is left out of them because it is answered as the class it is, and a source
  because it is answered as the namespace it provides - both of them are on
  the classpath under a name of their own, and a path to one is not how
  either is asked for."
  [resources]
  (loop [resources (seq resources)
         namespaces (transient [])
         cljs-namespaces (transient [])
         classes (transient [])
         paths (transient [])
         found (transient [])]
    (if resources
      (let [^String resource (first resources)
            resources (next resources)
            ;; Read beside the rest rather than instead of it. A .cljc is a
            ;; namespace of both worlds, and a .cljs is a resource the way
            ;; any other file on the classpath is - so what this adds is one
            ;; more list and not a name taken out of another.
            cljs-namespaces (if-let [stem (cljs-source-stem resource)]
                              (conj! cljs-namespaces (namespace-name stem))
                              cljs-namespaces)]
        (if (.endsWith resource ".class")
          (recur resources
                 namespaces
                 cljs-namespaces
                 (if-let [class (class-name resource)] (conj! classes class) classes)
                 paths
                 found)
          (if-let [stem (source-stem resource)]
            (recur resources
                   (conj! namespaces (namespace-name stem))
                   cljs-namespaces
                   classes
                   ;; what a load takes is a path and not a name, so the
                   ;; underscores stay: it names the file rather than what
                   ;; the file provides
                   (conj! paths (str "/" stem))
                   found)
            ;; A jar holds an entry for each directory in it, written with a
            ;; slash at the end. A name with nothing at the end of it is not
            ;; a resource anybody reads.
            (if (.endsWith resource "/")
              (recur resources namespaces cljs-namespaces classes paths found)
              (recur resources namespaces cljs-namespaces classes paths
                     (conj! found resource))))))
      (let [classes (persistent! classes)]
        {:namespaces (persistent! namespaces)
         :cljs-namespaces (persistent! cljs-namespaces)
         :classes classes
         :packages (vec (prefixes classes))
         :paths (persistent! paths)
         :resources (persistent! found)}))))

;;; The classes the runtime brings

(defn- resource-package
  "The package of the resource at RESOURCE, as a package is written."
  ^String [^String resource]
  (let [index (.lastIndexOf resource (int \/))]
    (if (neg? index) "" (.replace (subs resource 0 index) \/ \.))))

(defn- exported-packages
  "The packages MODULE exports to everything.

  A package exported to named modules only is exported to something that is
  not this process, and a class of one cannot be imported here - offering it
  would be offering a name that does not compile."
  [^ModuleReference module]
  (into #{}
        (comp (remove (fn [^ModuleDescriptor$Exports export] (.isQualified export)))
              (map (fn [^ModuleDescriptor$Exports export] (.source export))))
        (.exports (.descriptor module))))

(defn- module-resources [^ModuleReference module]
  (let [exported (exported-packages module)]
    (with-open [^ModuleReader reader (.open module)
                resources (.list reader)]
      (into [] (filter (fn [^String resource]
                         (and (.endsWith resource ".class")
                              (contains? exported (resource-package resource)))))
            (iterator-seq (.iterator resources))))))

(def ^:private runtime-classes
  "The classes of the java runtime. Read once: they are the one part of the
  classpath that is fixed before the process starts."
  (delay (collect (into [] (mapcat module-resources)
                        (.findAll (ModuleFinder/ofSystem))))))

;;; The entries of the classpath

(defn- loader-urls
  "The urls the class loaders hold.

  Walked rather than read from java.class.path alone, because a library added
  to a running process - which is what `add-libs' does - is added to a loader
  and to no property. The application loader is not one of these since jdk 9,
  which is why the property is read as well."
  []
  (loop [loader (.getContextClassLoader (Thread/currentThread))
         urls []]
    (if loader
      (recur (.getParent loader)
             (if (instance? URLClassLoader loader)
               (into urls (.getURLs ^URLClassLoader loader))
               urls))
      urls)))

(defn- url->path
  "The path URL names, or nil when it names something that is not a file."
  ^Path [^URL url]
  (when (= "file" (.getProtocol url))
    (try (Paths/get (.toURI url)) (catch Exception _ nil))))

(defn- entries
  "The entries of the classpath, each of them once.

  A relative entry is made absolute: it is relative to the directory the
  process was started in, and nothing here is walked from there."
  []
  (let [property (or (System/getProperty "java.class.path") "")
        named (into [] (comp (remove string/blank?)
                             (map (fn [^String entry]
                                    (try (.toAbsolutePath (Paths/get entry (make-array String 0)))
                                         (catch Exception _ nil))))
                             (remove nil?))
                    (string/split property (re-pattern (java.util.regex.Pattern/quote File/pathSeparator))))
        loaded (into [] (comp (map url->path) (remove nil?) (map #(.toAbsolutePath ^Path %)))
                     (loader-urls))]
    (into [] (comp (map #(.normalize ^Path %)) (distinct)) (concat named loaded))))

;;; Reading

(defn- jar-resources [^Path path]
  (with-open [jar (JarFile. (.toFile path))]
    (into [] (map (fn [^JarEntry entry] (.getName entry)))
          (enumeration-seq (.entries jar)))))

(defn- unnameable?
  "Whether DIRECTORY is one nothing on the classpath can be named under.

  A name is read from the path of a resource, a piece of it for each directory
  the resource is in, so what is under a directory whose name starts with a
  dot is a name with an empty piece: .git.objects.ff, which is neither a
  namespace anybody wrote nor a class anybody can import.

  Skipping them is also what keeps the walk to the size of a source tree. A
  project root finds its way onto a classpath from time to time, and .git
  alone holds more files than everything else in one put together - twenty
  thousand of them cost a tenth of a second of every reading."
  [^Path directory]
  (when-let [name (.getFileName directory)]
    (.startsWith (str name) ".")))

(defn- real-path
  "PATH with every link resolved away, or nil where it cannot be read.

  Which is the other half of the pair an anchor is made of: a directory the
  classpath reaches through a link has a second name, and it is the second
  name that an editor - which resolves nothing, and is naming a file where
  the file really is - hands to a load."
  ^Path [^Path path]
  (try (.toRealPath path (make-array LinkOption 0)) (catch Exception _ nil)))

(defn- directory-resources
  "The resources under ROOT, and the anchors the walk crossed to reach them.

  LINKS ARE FOLLOWED, which is what puts a tree reached through one into the
  reading at all. Without that a linked directory arrives as a leaf - one
  resource named after the link, and nothing under it - so a source root whose
  packages are links reads as a source root holding no namespaces.

  AND WHAT IS CROSSED IS WRITTEN DOWN, because this is the only place that
  knows. A directory that is a link is handed to `preVisitDirectory' as the
  link and not as what it points at, so the walk holds both of that
  directory's names at the moment it steps across: the way the classpath
  spells it, and where it really is. That pair is an anchor, and it is what
  `replique.analysis/source-path' needs to name a file somebody opened by the
  second name. Nothing can work it out afterwards from the entries alone - an
  entry is a directory rather than a tree, and which of the directories under
  it are links is known only to whoever walked it.

  One question per directory, and none per file."
  [^Path root]
  (let [found (java.util.ArrayList.)
        anchors (java.util.ArrayList.)]
    (Files/walkFileTree
     root
     (java.util.EnumSet/of FileVisitOption/FOLLOW_LINKS)
     Integer/MAX_VALUE
     (reify FileVisitor
       (preVisitDirectory [_ directory _]
         (if (and (not= root directory) (unnameable? directory))
           FileVisitResult/SKIP_SUBTREE
           (do
             ;; The root itself is left out on purpose even when it is a link:
             ;; an entry that is one is already answered by resolving the two
             ;; sides of the question, which is what `source-path' asks second.
             ;; What is here is the link no resolution can find - one BELOW an
             ;; entry, where the entry is spelt one way and the file another.
             (when (and (not= root directory) (Files/isSymbolicLink directory))
               (when-let [real (real-path directory)]
                 (.add anchors
                       [real (str (string/join "/" (map str (.relativize root directory)))
                                  "/")])))
             FileVisitResult/CONTINUE)))
       (visitFile [_ path _]
         ;; Joined rather than printed: a path prints with the separator of
         ;; the machine, and a resource is named with a slash wherever it is
         ;; read. Nothing is asked of the file itself - what is wanted is its
         ;; name, and asking whether it is a regular one is a system call for
         ;; every file of every source tree on every request.
         (.add found (string/join "/" (map str (.relativize root path))))
         FileVisitResult/CONTINUE)
       ;; A file that cannot be read loses that file and a directory that
       ;; cannot be read loses what is under it, where letting it out would
       ;; lose the whole entry - a source tree answered as if it were empty.
       ;; A tree is walked while something else is writing to it, so a file
       ;; that was listed and then removed is an ordinary thing to meet. With
       ;; links followed it is also where a tree that links back into itself
       ;; arrives: skipping ends that walk rather than losing the entry.
       (visitFileFailed [_ _ _] FileVisitResult/SKIP_SUBTREE)
       (postVisitDirectory [_ _ _] FileVisitResult/CONTINUE)))
    {:resources (vec found) :anchors (vec anchors)}))

(defn- entry-scan
  "What the classpath entry at PATH provides, or nil when it provides nothing.

  An entry that cannot be read is one of them. A classpath names directories
  that were never created and jars that were moved, and a completion is not
  the place to report it: the request was about names, and the names of the
  entries that could be read are still the answer.

  A directory says so, and says where it is. It is asked here anyway, to know
  which way to read the entry, so carrying the answer out costs nothing - and
  it is the half of the classpath a path on disk can be named against, which
  is a question asked of a whole classpath at a time. See `naming-anchors'."
  [^Path path]
  (try
    (let [options (make-array LinkOption 0)]
      (cond
        (Files/isDirectory path options) (let [{:keys [resources anchors]}
                                               (directory-resources path)]
                                           (assoc (collect resources)
                                                  :directory path
                                                  :anchors anchors))
        (Files/isRegularFile path options) (collect (jar-resources path))))
    (catch Exception _ nil)))

(defn- read-classpath
  "Read every entry of the classpath, the classes of the runtime included.

  Those are not read again: the classes java came with are the one part of
  this that cannot change under a running process."
  []
  (let [scans (into [@runtime-classes] (comp (map entry-scan) (remove nil?)) (entries))
        namespaces (vec (mapcat :namespaces scans))
        cljs-namespaces (vec (mapcat :cljs-namespaces scans))]
    {:namespaces namespaces
     :namespace-prefixes (vec (prefixes namespaces))
     :cljs-namespaces cljs-namespaces
     :cljs-namespace-prefixes (vec (prefixes cljs-namespaces))
     :classes (vec (mapcat :classes scans))
     :packages (vec (mapcat :packages scans))
     :paths (vec (mapcat :paths scans))
     :resources (vec (mapcat :resources scans))
     ;; The classes the runtime brings are no entry of the classpath and
     ;; carry none of these
     :naming-anchors (into (vec (for [{:keys [directory]} scans :when directory]
                                  [directory ""]))
                           (mapcat :anchors)
                           scans)}))

;; Read here, which is process startup: replique.control loads the ops and the
;; ops load this. A first completion would otherwise wait a fifth of a second
;; for what a process that answers completions was always going to read.
(defonce ^:private scanned (atom {:reading 0 :names (read-classpath)}))

(defn scan
  "The names on the classpath, as the kinds of name they can be asked for.

  Each value may name the same thing twice - a namespace is on the classpath
  as many times as an entry provides it - and is in no particular order.
  Whoever answers with them is filtering them down to a few, and sorting a few
  is cheaper than keeping a hundred thousand of them sorted.

  What was read when the process started, until `rescan!' says otherwise."
  []
  (:names @scanned))

(defn reading
  "Which reading of the classpath this is.

  A number that changes when the classpath is read again and never otherwise,
  for something holding an answer that was worked out from a reading and
  needing to know whether it was worked out from this one - see
  `replique.analysis/stale'.

  A number rather than the reading itself, which is what there is to compare
  otherwise: the reading is a hundred thousand names, and keeping one to hold
  the next up against is keeping a copy of the classpath to answer a question
  about whether it moved."
  []
  (:reading @scanned))

(defn naming-anchors
  "The places a path on disk can be given a name under, each with what being
  under it contributes to that name.

  A pair, and the two kinds of pair are the two kinds of place. An entry of
  the classpath that is a directory contributes NOTHING, because a name under
  an entry is the whole name; a directory the walk reached by crossing a link
  contributes the way down to it, because the classpath spells that directory
  under an entry and whoever opened the file spelt it where it really is. See
  `replique.analysis/source-path', which is what asks.

  A jar is in neither kind because nothing under one has a path: what is in
  there is an entry, and an entry is already written the way the classpath
  names it.

  Entries first, so that a file under one is named by the entry rather than
  by a link that happens to reach the same tree - the shorter answer, and the
  one that does not depend on a link staying where it is.

  Out of the same reading everything else here comes out of, and stale in the
  same way until `rescan!'. Which is the point of it being here rather than
  read afresh per call: an entry is asked whether it is a directory to know
  which way to read it, and the links under it are crossed on the way through
  it, so both answers are already paid for. A classpath this one has not read
  is a classpath it says nothing else about either, and the two agreeing is
  worth more than one of them being fresher - what a load does with an
  unrecognised directory is load the file without analysing it, which is the
  same thing it does for a file that is on no classpath at all.

  Nothing puts a directory on the classpath without a reading: `:add-libs'
  and `:sync-deps' both end in one, and `:update-classpath' is a client
  asking for one by itself. A LINK UNDER ONE IS MOVED WITHOUT A READING,
  which is the one way this goes stale that the entries do not - a worktree
  swapped under a running process is a reading due, and until it happens the
  anchors name where the files were. Never the wrong file, though: a name is
  held up against the classpath before it is given out, so an anchor that has
  moved answers nothing rather than answering something else. See
  `replique.analysis/reachable-as?'."
  []
  (:naming-anchors (scan)))

(defn rescan!
  "Read the classpath again, and return what is on it now.

  The names and the number of the reading are set together, in one swap,
  because what the number is for is saying which names these are - and two
  writes would leave a reader free to see the new number beside the old names
  and conclude that nothing had happened."
  []
  (let [names (read-classpath)]
    (:names (swap! scanned (fn [was] {:reading (inc (:reading was)) :names names})))))
