(ns replique.utils
  (:refer-clojure :exclude [delay])
  (:import [java.util.concurrent.locks ReentrantLock]
           [java.net URL]))

(defonce process-out nil)
(defonce process-err nil)
(defonce project-map nil)
(defonce version nil)
(defonce cljs-compile-path nil)

(defmacro with-1.9+ [& body]
  (let [{:keys [major minor]} *clojure-version*]
    (when (or (> major 1) (and (= 1 major) (>= minor 9)))
      `(do ~@body))))

(defn cljs-env?
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (contains? env :ns))

(def ^:dynamic *repl-env* :replique/clj)

(defmulti repl-type identity)
(defmulti repl-ns identity)

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
