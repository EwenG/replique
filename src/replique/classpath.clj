(ns replique.classpath
  (:refer-clojure :exclude [add-classpath])
  (:import [clojure.lang DynamicClassLoader]
           [java.net URL URI URLClassLoader]
           [java.io File]
           [java.nio.file Paths Path]
           [java.lang.reflect Method]))

(defn dynamic-classloader? [cl]
  (instance? DynamicClassLoader cl))

(defn root-dynamic-classloader []
  (let [context-classloader (.getContextClassLoader (Thread/currentThread))]
    (->> context-classloader
         (iterate #(.getParent ^ClassLoader %))
         (cons context-classloader)
         (take-while dynamic-classloader?)
         last)))

(defn add-classpath [^DynamicClassLoader cl urls]
  (doseq [url urls]
    (.addURL cl url)))

(defn classpath->paths [classpath]
  (when classpath
    (for [path (-> classpath
                   clojure.string/trim
                   (.split File/pathSeparator))]
      (Paths/get path (make-array String 0)))))

(defn paths->urls [paths]
  (->> paths
       (map #(.toUri ^Path %))
       (map #(.toURL ^URI %))))

(defn urls->paths [urls]
  (->> urls
       (map #(.toURI ^URL %))
       (map #(Paths/get ^URI %))))

(defn paths
  ([] (paths true))
  ([include-boot-classpath?]
   (let [dynamic-classloader ^DynamicClassLoader (root-dynamic-classloader)
         dynamic-classloader-paths (when dynamic-classloader
                                     (urls->paths (.getURLs dynamic-classloader)))]
     (distinct
      (concat (classpath->paths (System/getProperty "java.class.path"))
              dynamic-classloader-paths
              ;; nil under jdk9
              (when include-boot-classpath?
                (classpath->paths (System/getProperty "sun.boot.class.path"))))))))

(defn update-classpath [classpath]
  (add-classpath (root-dynamic-classloader) (paths->urls (classpath->paths classpath))))

(def system-module-resources
  (try
    (require 'replique.classpath-jdk9+)
    @(resolve 'replique.classpath-jdk9+/system-module-resources)
    (catch Exception e nil)))
