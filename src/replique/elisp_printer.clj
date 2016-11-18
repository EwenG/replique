(ns replique.elisp-printer
  "Print to a format that can be read by the elisp reader. Not everything is made printable, 
  only the most common Clojure datastructures. This is because elisp reader is not extensible,
  and thus cannot read everything."
  (:refer-clojure :exclude [print-method pr prn])
  (:import [java.io Writer]))

(defn- print-sequential [^String begin, print-one, ^String sep, ^String end, sequence, ^Writer w]
  (.write w begin)
  (when-let [xs (seq sequence)]
    (loop [[x & xs] xs]
      (print-one x w)
      (when xs
        (.write w sep)
        (recur xs))))
  (.write w end))

(declare pr-on)

(defmulti print-method (fn [x writer]
                         (let [t (get (meta x) :type)]
                           (if (keyword? t) t (class x)))))

(defmethod print-method :default [o, ^Writer w]
  (throw (IllegalArgumentException.
          (format "%s cannot be printed to elisp format" (clojure.core/pr-str o)))))

(defmethod print-method nil [o, ^Writer w]
  (.write w "nil"))

(defmethod print-method clojure.lang.Keyword [o, ^Writer w]
  (.write w (str o)))

(defmethod print-method Number [o, ^Writer w]
  (.write w (str o)))

(defmethod print-method Boolean [o, ^Writer w]
  (.write w (str o)))

(defmethod print-method clojure.lang.Symbol [o, ^Writer w]
  (.write w (str o)))

(defmethod print-method clojure.lang.ISeq [o, ^Writer w]
  (print-sequential "(" pr-on " " ")" o w))

(defmethod print-method String [^String s, ^Writer w]
  (.append w \")
  (dotimes [n (count s)]
    (let [c (.charAt s n)
          e (char-escape-string c)]
      (if e (.write w e) (.append w c))))
  (.append w \")
  nil)

(defmethod print-method clojure.lang.IPersistentVector [v, ^Writer w]
  (print-sequential "[" pr-on " " "]" v w))

(defn- print-prefix-map [m print-one w]
  (print-sequential
   (format "#s(hash-table test equal size %d data (" (count m))
   (fn [e ^Writer w]
     (do (print-one (key e) w) (.append w \space) (print-one (val e) w)))
   " "
   "))"
   (seq m) w))

(defn- print-map [m print-one w]
  (print-prefix-map m print-one w))

(defmethod print-method clojure.lang.IPersistentMap [m, ^Writer w]
  (print-map m pr-on w))

(defmethod print-method clojure.lang.IPersistentSet [s, ^Writer w]
  (print-sequential "(" pr-on " " ")" (seq s) w))

(defmethod print-method StackTraceElement [^StackTraceElement o ^Writer w]
  (print-method [(symbol (.getClassName o)) (symbol (.getMethodName o)) (.getFileName o) (.getLineNumber o)] w))

(defn- print-throwable [^Throwable o ^Writer w]
  (.write w "#s(hash-table test equal data (\n :cause ")
  (let [{:keys [cause data via trace]} (Throwable->map o)
        print-via #(do (.write w "#s(hash-table test equal data (\n :type ")
                       (print-method (:type %) w)
                       (.write w "\n   :message ")
                       (print-method (:message %) w)
                       (when-let [data (:data %)]
                         (.write w "\n   :data ")
                         (print-method (clojure.core/pr-str data) w))
                       (when-let [at (:at %)]
                         (.write w "\n   :at ")
                         (print-method (:at %) w))
                       (.write w "))"))]
    (print-method cause w)
    (when data
      (.write w "\n :data ")
      (print-method (clojure.core/pr-str data) w))
    (when via
      (.write w "\n :via\n [")
      (when-let [fv (first via)]
        (print-via fv)
        (doseq [v (rest via)]
          (.write w "\n  ")
          (print-via v)))
      (.write w "]"))
    (when trace
      (.write w "\n :trace\n [")
      (when-let [ft (first trace)]
        (print-method ft w)
        (doseq [t (rest trace)]
          (.write w "\n  ")
          (print-method t w)))
      (.write w "]")))
  (.write w "))"))

;; Throwable will print almost like Clojure, but directly as a map, without the tag reader.
;; Also :data is returned as a string because we don't control what it contains.
(defmethod print-method Throwable [^Throwable o ^Writer w]
  (print-throwable o w))

(defn pr-on [x w]
  (print-method x w)
  nil)

(defn pr
  ([] nil)
  ([x]
   (pr-on x *out*))
  ([x & more]
   (pr x)
   (. *out* (append \space))
   (if-let [nmore (next more)]
     (recur (first more) nmore)
     (apply pr more))))

(defn prn [& more]
    (apply pr more)
    (newline)
    (when *flush-on-newline*
      (flush)))
