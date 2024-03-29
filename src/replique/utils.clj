(ns replique.utils
  (:refer-clojure :exclude [delay])
  (:require [clojure.core.server :as server])
  (:import [java.util.concurrent.locks ReentrantLock]
           [java.net URL]
           [java.net ServerSocket]
           [java.nio.file Paths Path]))

(defonce version "1.1.0-SNAPSHOT")

(defonce host "localhost")
(defonce port 0)
(defonce http-host "localhost")
(defonce http-port 0)
(defonce cljs-compile-path "target/cljs")

(defonce process-out nil)
(defonce process-err nil)
(defonce repl-server nil)
(defonce http-server nil)

(defonce root-dynamic-classloader nil)

(defn server-port [server]
  (.getLocalPort ^ServerSocket server))

(defn server-host [server]
  (.getHostName (.getInetAddress ^ServerSocket server)))

(defn- is-1-10-0+? [clojure-version]
  (let [{:keys [major minor incremental]} clojure-version]
    (or (> major 1)
        (and (= 1 major) (> minor 10))
        (and (= 1 major) (= minor 10) (>= incremental 0)))))

(defmacro with-1-10-0+ [& body]
  (when (is-1-10-0+? *clojure-version*)
    `(do ~@body)))

(defn cljs-env?
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (contains? env :ns))

(def ^:dynamic *repl-env* :replique/clj)
(def ^:dynamic *main-ns* nil)

(defmulti repl-type identity)
(defmulti repl-ns identity)
(defmulti repl-params identity)

;; Same as Delay but don't realize the Delay on exception.
;; This would not be possible with clojure Delay because it makes its function ^:once because
;; of local clearings. Replique Delay is not expected to be used in situations where local
;; clearing is needed

(deftype Dynaload [^:unsynchronized-mutable f ^:unsynchronized-mutable val
                   ^:unsynchronized-mutable exception]
  clojure.lang.IDeref
  (deref [this]
    (locking this
      (when f
        (let [res (try {:val (f)}
                       (catch Exception e
                         {:exception e}))]
          (when (:val res)
            (set! val (:val res))
            (set! f nil)
            (set! exception nil))
          (when (:exception res)
            (set! exception (:exception res)))))
      (when exception
        (throw (clojure.lang.Util/sneakyThrow exception)))
      val))
  clojure.lang.IPending
  (isRealized [this] (nil? f)))

(defmacro delay [& body]
  (list 'new 'replique.utils.Dynaload `(~'fn [] ~@body) nil nil))

(defn dynaload [s]
  (Dynaload. (fn []
               (let [ns (namespace s)]
                 (assert ns)
                 (require (symbol ns))
                 (let [v (resolve s)]
                   (if v
                     @v
                     (throw (RuntimeException. (str "Var " s " is not on the classpath")))))))
             nil nil))

(defmacro with-lock
  [lock-expr & body]
  `(let [lockee# ~(with-meta lock-expr {:tag 'java.util.concurrent.locks.ReentrantLock})]
     (.lock lockee#)
     (try
       ~@body
       (finally
         (.unlock lockee#)))))

(defmacro maybe-locking [x & body]
  (if (resolve 'clojure.core/locking)
    `(clojure.core/locking ~x ~@body)
    `(do ~@body)))

(defn ^Path make-path
  "Returns a java.nio.file.Path constructed from the provided String(s)."
  [path & paths]
  (Paths/get (str path) (into-array String (map str paths))))

(defn jar-url->path [url]
  (let [url-str (str url)
        path (when (.contains url-str "!/")
               (last (.split url-str "!/")))]
    path))

(defn file-url->path [url]
  (.getFile ^URL url))

(defmulti url->path (fn [url] (.getProtocol ^URL url)))

(defmethod url->path "file" [url]
  (file-url->path url))

(defmethod url->path "jar" [url]
  (jar-url->path url))

;; post eval hooks
(defonce clj-env-hooks (atom nil))
(defonce cljs-env-hooks (atom nil))
