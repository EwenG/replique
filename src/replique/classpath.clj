(ns replique.classpath
  (:refer-clojure :exclude [add-classpath])
  (:require [replique.tooling-msg :as tooling-msg])
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

(defn paths []
  (concat (classpath->paths (System/getProperty "java.class.path"))
          ;; nil under jdk9
          (classpath->paths (System/getProperty "sun.boot.class.path"))))


(defmethod tooling-msg/tooling-msg-handle [:replique/clj :classpath]
  [{:keys [classpath] :as msg}]
  (tooling-msg/with-tooling-response msg
    (add-classpath (root-dynamic-classloader) (paths->urls (classpath->paths classpath)))))
