(ns replique.classpath
  "What is on the classpath: the namespaces that could be required, the
  classes that could be imported.

  The classpath rather than what the process has loaded, because a require is
  written for a namespace that has not been loaded - writing one is what loads
  it - and `all-ns` knows only the ones that have. The same holds for a class,
  which nothing loads until something names it.

  Jars are read once and kept. A jar cannot change under a running process
  without breaking it, so what was read of one stays true for as long as the
  process lasts. Directories are walked again every time, and they are the
  ones that change: a namespace gains a file the moment somebody writes one,
  and that is the namespace they are about to require. A source tree is a few
  hundred files, and walking it costs less than the round trip that asked."
  (:require [clojure.string :as string])
  (:import [java.io File]
           [java.lang.module ModuleDescriptor$Exports ModuleFinder ModuleReader
            ModuleReference]
           [java.net URL URLClassLoader]
           [java.nio.file FileVisitOption Files LinkOption Path Paths]
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

(defn- packages-of
  "Every package the classes are in, and every package those are in.

  All the way up rather than the one a class sits in directly, because a
  package is written a piece at a time: somebody who has typed java.u is
  writing java.util, and what is offered there has to be a package that holds
  no class of its own."
  [classes]
  (persistent!
   (reduce (fn [acc ^String class]
             (loop [acc acc index (.indexOf class (int \.))]
               (if (neg? index)
                 acc
                 (recur (conj! acc (subs class 0 index))
                        (.indexOf class (int \.) (inc index))))))
           (transient #{}) classes)))

(defn- collect
  "What the resources named by RESOURCES provide, as the four kinds of name
  they can be asked for."
  [resources]
  (let [namespaces (transient [])
        classes (transient [])
        paths (transient [])]
    (doseq [^String resource resources]
      (if-let [class (class-name resource)]
        (conj! classes class)
        (when-let [stem (source-stem resource)]
          (conj! namespaces (namespace-name stem))
          ;; what a load takes is a path and not a name, so the underscores
          ;; stay: it names the file rather than what the file provides
          (conj! paths (str "/" stem)))))
    (let [classes (persistent! classes)]
      {:namespaces (persistent! namespaces)
       :classes classes
       :packages (vec (packages-of classes))
       :paths (persistent! paths)})))

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

;;; Scanning

;; What was read of each jar, by its path. See the namespace docstring for why
;; a jar is remembered and a directory is not.
(defonce ^:private jars (atom {}))

(defn- jar-resources [^Path path]
  (with-open [jar (JarFile. (.toFile path))]
    (into [] (map (fn [^JarEntry entry] (.getName entry)))
          (enumeration-seq (.entries jar)))))

(defn- directory-resources [^Path root]
  (let [options (make-array LinkOption 0)]
    (with-open [found (Files/walk root (make-array FileVisitOption 0))]
      (into [] (comp (filter (fn [^Path path] (Files/isRegularFile path options)))
                     ;; joined rather than printed: a path prints with the
                     ;; separator of the machine, and a resource is named with
                     ;; a slash wherever it is read
                     (map (fn [^Path path]
                            (string/join "/" (map str (.relativize root path))))))
            (iterator-seq (.iterator found))))))

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
        (Files/isRegularFile path options)
        (let [key (str path)]
          (or (get @jars key)
              (let [scan (collect (jar-resources path))]
                (swap! jars assoc key scan)
                scan)))))
    (catch Exception _ nil)))

(defn scan
  "The names on the classpath, as the four kinds of name they can be asked for.

  Each value is a seq that may name the same thing twice - a namespace is on
  the classpath as many times as an entry provides it - and is in no
  particular order. Whoever answers with them is filtering them down to a few,
  and sorting a few is cheaper than keeping a hundred thousand of them sorted."
  []
  (let [scans (into [@runtime-classes] (comp (map entry-scan) (remove nil?)) (entries))]
    {:namespaces (mapcat :namespaces scans)
     :classes (mapcat :classes scans)
     :packages (mapcat :packages scans)
     :paths (mapcat :paths scans)}))
