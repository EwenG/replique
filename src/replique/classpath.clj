(ns replique.classpath
  "What is on the classpath: the namespaces that could be required, the
  classes that could be imported.

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
           [java.nio.file FileVisitResult FileVisitor Files LinkOption Path Paths]
           [java.util.jar JarEntry JarFile]))

;;; Reading a name out of a resource

(def ^:private source-extensions
  "The extensions a namespace of this process is written in. A .cljs file
  names a namespace of the other of the two worlds ClojureScript compiles
  with, and this process is the Clojure one."
  [".clj" ".cljc"])

(defn- source-stem
  "The path of RESOURCE without the extension that makes it a namespace, or
  nil when it is not one."
  ^String [^String resource]
  (some (fn [^String extension]
          (when (.endsWith resource extension)
            (subs resource 0 (- (.length resource) (.length extension)))))
        source-extensions))

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
  "What the resources named by RESOURCES provide, as the four kinds of name
  they can be asked for."
  [resources]
  (loop [resources (seq resources)
         namespaces (transient [])
         classes (transient [])
         paths (transient [])]
    (if resources
      (let [^String resource (first resources)
            resources (next resources)]
        (if-let [class (class-name resource)]
          (recur resources namespaces (conj! classes class) paths)
          (if-let [stem (source-stem resource)]
            (recur resources
                   (conj! namespaces (namespace-name stem))
                   classes
                   ;; what a load takes is a path and not a name, so the
                   ;; underscores stay: it names the file rather than what
                   ;; the file provides
                   (conj! paths (str "/" stem)))
            (recur resources namespaces classes paths))))
      (let [classes (persistent! classes)]
        {:namespaces (persistent! namespaces)
         :classes classes
         :packages (vec (prefixes classes))
         :paths (persistent! paths)}))))

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

(defn- directory-resources [^Path root]
  (let [found (java.util.ArrayList.)]
    (Files/walkFileTree
     root
     (reify FileVisitor
       (preVisitDirectory [_ directory _]
         (if (and (not= root directory) (unnameable? directory))
           FileVisitResult/SKIP_SUBTREE
           FileVisitResult/CONTINUE))
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
       ;; that was listed and then removed is an ordinary thing to meet.
       (visitFileFailed [_ _ _] FileVisitResult/SKIP_SUBTREE)
       (postVisitDirectory [_ _ _] FileVisitResult/CONTINUE)))
    (vec found)))

(defn- entry-scan
  "What the classpath entry at PATH provides, or nil when it provides nothing.

  An entry that cannot be read is one of them. A classpath names directories
  that were never created and jars that were moved, and a completion is not
  the place to report it: the request was about names, and the names of the
  entries that could be read are still the answer."
  [^Path path]
  (try
    (let [options (make-array LinkOption 0)]
      (cond
        (Files/isDirectory path options) (collect (directory-resources path))
        (Files/isRegularFile path options) (collect (jar-resources path))))
    (catch Exception _ nil)))

(defn- read-classpath
  "Read every entry of the classpath, the classes of the runtime included.

  Those are not read again: the classes java came with are the one part of
  this that cannot change under a running process."
  []
  (let [scans (into [@runtime-classes] (comp (map entry-scan) (remove nil?)) (entries))
        namespaces (vec (mapcat :namespaces scans))]
    {:namespaces namespaces
     :namespace-prefixes (vec (prefixes namespaces))
     :classes (vec (mapcat :classes scans))
     :packages (vec (mapcat :packages scans))
     :paths (vec (mapcat :paths scans))}))

;; Read here, which is process startup: replique.control loads the ops and the
;; ops load this. A first completion would otherwise wait a fifth of a second
;; for what a process that answers completions was always going to read.
(defonce ^:private scanned (atom (read-classpath)))

(defn scan
  "The names on the classpath, as the kinds of name they can be asked for.

  Each value may name the same thing twice - a namespace is on the classpath
  as many times as an entry provides it - and is in no particular order.
  Whoever answers with them is filtering them down to a few, and sorting a few
  is cheaper than keeping a hundred thousand of them sorted.

  What was read when the process started, until `rescan!' says otherwise."
  []
  @scanned)

(defn rescan!
  "Read the classpath again, and return what is on it now."
  []
  (reset! scanned (read-classpath)))
