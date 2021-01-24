(ns replique.files
  (:require [replique.utils :as utils])
  (:import [java.io File]
           [java.nio.file Path Files FileVisitor FileVisitResult
            FileVisitOption FileSystemLoopException NoSuchFileException]
           [java.nio.file.attribute FileAttribute]
           [java.util EnumSet]
           [java.util.jar JarFile JarEntry]))

(def ^:dynamic *root-path* nil)

(defn visit-jar [^JarFile jar-file visitor-fn-jar]
  (doseq [^JarEntry entry (enumeration-seq (.entries jar-file))
          :when (not (.isDirectory entry))]
    (visitor-fn-jar entry)))

(defn relativize-path [^Path root-path ^Path path]
  (if (= root-path path)
    (if-let [parent-path (.getParent path)]
      (.normalize (.relativize parent-path path))
      path)
    (.normalize (.relativize root-path path))))

(defn make-file-visitor [visitor-fn]
  (reify FileVisitor
    (postVisitDirectory [_ dir exception]
      FileVisitResult/CONTINUE)
    (preVisitDirectory [_ dir attrs]
      FileVisitResult/CONTINUE)
    (visitFile [_ path attrs]
      (visitor-fn (relativize-path *root-path* path) attrs)
      FileVisitResult/CONTINUE)
    (visitFileFailed [_ file exception]
      (cond (instance? FileSystemLoopException exception)
            FileVisitResult/SKIP_SUBTREE
            (instance? NoSuchFileException exception)
            FileVisitResult/SKIP_SUBTREE
            :else (throw exception)))))

(defn make-file-visitor-with-limit [visitor-fn]
  (reify FileVisitor
    (postVisitDirectory [_ dir exception]
      FileVisitResult/CONTINUE)
    (preVisitDirectory [_ dir attrs]
      FileVisitResult/CONTINUE)
    (visitFile [_ path attrs]
      (visitor-fn (relativize-path *root-path* path) attrs))
    (visitFileFailed [_ file exception]
      (cond (instance? FileSystemLoopException exception)
            FileVisitResult/SKIP_SUBTREE
            (instance? NoSuchFileException exception)
            FileVisitResult/SKIP_SUBTREE
            :else (throw exception)))))

(defn visit-files
  ([paths visitor-fn]
   (let [file-visitor (make-file-visitor visitor-fn)]
     (doseq [path paths]
       (binding [*root-path* path]
         (Files/walkFileTree path (EnumSet/of FileVisitOption/FOLLOW_LINKS) Integer/MAX_VALUE
                             file-visitor)))))
  ([paths visitor-fn visitor-fn-jar]
   (let [file-visitor (make-file-visitor visitor-fn)]
     (doseq [^Path path paths]
       (let [file-name (str (.getFileName ^Path path))]
         (if (.endsWith file-name ".jar")
           (let [file (File. (str path))]
             (when (.exists file)
               (visit-jar (JarFile. file) visitor-fn-jar))))
         (binding [*root-path* path]
           (Files/walkFileTree path (EnumSet/of FileVisitOption/FOLLOW_LINKS) Integer/MAX_VALUE
                               file-visitor)))))))

(defn visit-with-limit [path level-limit follow-links? visitor-fn]
  (let [file-visitor (make-file-visitor-with-limit visitor-fn)]
    (binding [*root-path* path]
      (Files/walkFileTree path
                          (if follow-links? (EnumSet/of FileVisitOption/FOLLOW_LINKS) #{})
                          level-limit file-visitor))))

(comment
  (import '[java.nio.file Paths])
  (def the-path (Paths/get "." (make-array String 0)))
  )

(defn create-file [path]
  (let [^Path path (if (string? path)
                     (utils/make-path path)
                     path)]
    (Files/createDirectories path (make-array FileAttribute 0))
    (Files/createFile path (make-array FileAttribute 0))))

(defn create-symbolic-link
  ([link target]
   (create-symbolic-link link target nil))
  ([link target file-attributes]
   (let [^Path link (if (string? link)
                      (utils/make-path link)
                      link)
         target (if (string? target)
                  (utils/make-path target)
                  target)
         file-attributes (if file-attributes
                           (into-array FileAttribute file-attributes)
                           (make-array FileAttribute 0))]
     (Files/createDirectories (.getParent link) (make-array FileAttribute 0))
     (Files/createSymbolicLink link target file-attributes))))
