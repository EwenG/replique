(ns replique.utils
  (:refer-clojure :exclude [delay])
  (:import [java.util.concurrent.locks ReentrantLock]))

(defonce cljs-compile-path nil)

(defmacro with-1.9+ [& body]
  (let [{:keys [major minor]} *clojure-version*]
    (when (or (> major 1) (and (= 1 major) (>= minor 9)))
      `(do ~@body))))

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

(defmacro with-err-str [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*err* s#]
       ~@body
       (str s#))))

(defn repl-caught-str [e]
  (with-err-str (clojure.main/repl-caught e)))


