(ns replique.classpath
  (:refer-clojure :exclude [add-classpath])
  (:require [replique.tooling-msg :as tooling-msg])
  (:import [clojure.lang DynamicClassLoader]
           [java.net URL URI URLClassLoader]
           [java.nio.file Paths Path]
           [java.lang.reflect Method]))

(defprotocol AddableClasspath
  (can-add? [cl])
  (add-classpath-url [cl url]))

(when-not (extends? AddableClasspath URLClassLoader)
  (let [addURL (try
                 (-> URLClassLoader
                     (.getDeclaredMethod "addURL" (into-array Class [URL]))
                     (doto (.setAccessible true)))
                 (catch Exception _))]
    (extend URLClassLoader
      AddableClasspath
      {:can-add? (fn [_] (boolean addURL))
       :add-classpath-url (fn [cl url]
                            (when addURL
                              (.invoke ^Method addURL cl (into-array URL [url]))))}))

  (extend DynamicClassLoader
    AddableClasspath
    {:can-add? (fn [_] (boolean true))
     :add-classpath-url (fn [^DynamicClassLoader cl url]
                          (.addURL cl url))})

  ;; on < java 9, the boot classloader is a URLClassLoader, but
  ;; modifying it can have dire consequences
  (when (try (resolve 'sun.misc.Launcher$ExtClassLoader)
             (catch Exception _))
    (extend sun.misc.Launcher$ExtClassLoader
      AddableClasspath
      {:can-add? (constantly false)})))

(defn classloader-hierarchy
  ([] (classloader-hierarchy (.. Thread currentThread getContextClassLoader)))
  ([tip] (->> tip
              (iterate #(.getParent ^ClassLoader %))
              (take-while boolean))))

(defn addable-classloader []
  (let [classloaders (classloader-hierarchy)]
    (last (filter can-add? classloaders))))

(defn add-classpath [cl urls]
  (doseq [url urls]
    (add-classpath-url cl url)))

(defn classpath->urls [classpath]
  (->> (clojure.string/split (clojure.string/trim classpath) #":")
       (map #(Paths/get % (make-array String 0)))
       (map #(.toUri ^Path %))
       (map #(.toURL ^URI %))))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :classpath]
  [{:keys [classpath] :as msg}]
  (tooling-msg/with-tooling-response msg
    (if-let [cl (addable-classloader)]
      (do (add-classpath cl (classpath->urls classpath))
          {})
      {:error "Could not find a suitable classloader to update the classpath"})))

