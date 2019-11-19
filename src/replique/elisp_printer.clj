(ns replique.elisp-printer
  "Print to a format that can be read by the elisp reader. Prints to a transit-like format."
  (:refer-clojure :exclude [print-method pr prn print-simple])
  (:import [java.io Writer]
           [clojure.core Eduction]))

(defn escape-symbol [symbol-str]
  (.replaceAll symbol-str "(?=[\\\\?\\[\\]\\(\\)\\{\\}\\+\\-#;\\.\\'])"  "\\\\"))

(defn print-more-level [w]
  (.write w "\"~+level\""))

(defn print-more-length-one [w]
  (.write w "\"~+length\""))

(defn print-more-length-map [w]
  (print-more-length-one w)
  (.write w " ")
  (print-more-length-one w))

(defn- print-sequential [^String begin, print-one, ^String sep, ^String end, sequence, ^Writer w
                         print-more-length]
  (binding [*print-level* (when (number? *print-level*) (dec *print-level*))]
    (if (and (number? *print-level*) (neg? *print-level*))
      (print-more-level w)
      (do
        (.write w begin)
        (when-let [xs (seq sequence)]
          (if (number? *print-length*)
            (loop [[x & xs] xs
                   print-length *print-length*]
              (if (zero? print-length)
                (print-more-length w)
                (do
                  (print-one x w)
                  (when xs
                    (.write w sep)
                    (recur xs (dec print-length))))))
            (loop [[x & xs] xs]
              (print-one x w)
              (when xs
                (.write w sep)
                (recur xs)))))
        (.write w end)))))

(declare pr-on)
(declare pr)

(defmulti print-method (fn [x writer]
                         (let [t (get (meta x) :type)]
                           (if (keyword? t) t (class x)))))

(defmacro print-with-meta [o w & body]
  `(let [m# (meta ~o)]
     (if (and (pos? (count m#)) *print-meta*)
       (do
         (.write ~w "[\"~#with-meta\" [")
         ~@body
         (.write ~w " ")
         (pr-on m# ~w)
         (.write ~w "]]"))
       (do ~@body))))

(defn print-simple [o, ^Writer w]
  (print-with-meta
   o w
   (.write w (str o))))

(defmethod print-method :default [o, ^Writer w]
  (if (instance? clojure.lang.IObj o)
    (print-method (vary-meta o #(dissoc % :type)) w)
    (print-simple o w)))

(defmethod print-method nil [o, ^Writer w]
  (.write w "nil"))

(defn- print-tagged-object* [o rep ^Writer w]
  (.write w "[\"~#object\" [")
  (let [c (class o)]
    (if (.isArray c)
      (print-method (.getName c) w)
      (.write w (.getName c))))
  (.write w " ")
  (.write w (format "0x%x " (System/identityHashCode o)))
  (print-method rep w)
  (.write w "]]"))

(defn- print-tagged-object [o rep ^Writer w]
  (if (instance? clojure.lang.IMeta o)
    (print-with-meta
     o w
     (print-tagged-object* o rep w))
    (print-tagged-object* o rep w)))

(defn- print-object [o, ^Writer w]
  (print-tagged-object o (str o) w))

(defmethod print-method Object [o, ^Writer w]
  (print-object o w))

(defmethod print-method clojure.lang.Keyword [o, ^Writer w]
  (.write w (escape-symbol (str o))))

(defmethod print-method Number [o, ^Writer w]
  (.write w (str o)))

(defmethod print-method Double [o, ^Writer w]
  (cond
    (= Double/POSITIVE_INFINITY o) (.write w "\"~zInf\"")
    (= Double/NEGATIVE_INFINITY o) (.write w "\"~z-Inf\"")
    (.isNaN ^Double o) (.write w "\"~zNaN\"")
    :else (.write w (str o))))

(defmethod print-method Float [o, ^Writer w]
  (cond
    (= Float/POSITIVE_INFINITY o) (.write w "\"~zInf\"")
    (= Float/NEGATIVE_INFINITY o) (.write w "\"~z-Inf\"")
    (.isNaN ^Float o) (.write w "\"~zNaN\"")
    :else (.write w (str o))))

(defmethod print-method Boolean [o, ^Writer w]
  (.write w "\"~?")
  (if o
    (.write w "t")
    (.write w "f"))
  (.write w "\""))

(defmethod print-method clojure.lang.Symbol [o, ^Writer w]
  (print-with-meta
   o w
   (.write w (escape-symbol (str o)))))

(defmethod print-method clojure.lang.Var [o, ^Writer w]
  (print-with-meta
   o w
   (.write w "\"~v")
   (.write w (str (.-ns o) "/" (.-sym o)))
   (.write w "\"")))

(defmethod print-method clojure.lang.ISeq [o, ^Writer w]
  (print-sequential "(" pr-on " " ")" o w print-more-length-one))

(prefer-method print-method clojure.lang.ISeq clojure.lang.IPersistentCollection)
(prefer-method print-method clojure.lang.ISeq java.util.Collection)

(defmethod print-method String [^String s, ^Writer w]
  (.append w \")
  (dotimes [n (count s)]
    (let [c (.charAt s n)
          e (if (and (= 0 n) (= \~ c))
              "~~"
              (char-escape-string c))]
      (if e (.write w e) (.append w c))))
  (.append w \")
  nil)

(defmethod print-method clojure.lang.IPersistentVector [v, ^Writer w]
  (print-with-meta
   v w
   (print-sequential "[" pr-on " " "]" v w print-more-length-one)))

(defn- print-prefix-map [m print-one w]
  (print-sequential
   (format "#s(hash-table test equal size %d data (" (count m))
   (fn [e ^Writer w]
     (do (print-one (key e) w) (.append w \space) (print-one (val e) w)))
   " "
   "))"
   (seq m) w
   print-more-length-map))

(defn- print-map [m print-one w]
  (print-prefix-map m print-one w))

(defmethod print-method clojure.lang.IPersistentMap [m, ^Writer w]
  (print-with-meta
   m w
   (print-map m pr-on w)))

(prefer-method print-method clojure.lang.IPersistentCollection java.util.Collection)
(prefer-method print-method clojure.lang.IPersistentCollection java.util.RandomAccess)
(prefer-method print-method java.util.RandomAccess java.util.List)
(prefer-method print-method clojure.lang.IPersistentCollection java.util.Map)

(defmethod print-method java.util.List [c, ^Writer w]
  (print-with-meta
   c w
   (print-sequential "(" pr-on " " ")" c w print-more-length-one)))

(defmethod print-method java.util.RandomAccess [v, ^Writer w]
  (print-with-meta
   v w
   (print-sequential "[" pr-on " " "]" v w print-more-length-one)))

(defmethod print-method java.util.Map [m, ^Writer w]
  (print-with-meta
   m w
   (print-map m pr-on w)))

(defmethod print-method java.util.Set [s, ^Writer w]
  (print-with-meta
   s w
   (.write w "[\"~#set\" ")
   (print-sequential "[" pr-on " " "]" (seq s) w print-more-length-one)
   (.write w "]")))

(defmethod print-method clojure.lang.IRecord [r, ^Writer w]
  (print-with-meta
   r w
   (.write w "[\"~#")
   (.write w (.getName (class r)))
   (.write w "\" ")
   (print-map r pr-on w)
   (.write w "]")))

(prefer-method print-method clojure.lang.IRecord java.util.Map)
(prefer-method print-method clojure.lang.IRecord clojure.lang.IPersistentMap)

(defmethod print-method clojure.lang.IPersistentSet [s, ^Writer w]
  (print-with-meta
   s w
   (.write w "[\"~#set\" ")
   (print-sequential "[" pr-on " " "]" (seq s) w print-more-length-one)
   (.write w "]")))

(defmethod print-method Character [^Character c, ^Writer w]
  (.write w "\"~c")
  (if-let [c-string (get char-name-string c)]
    (.write w c-string)
    (.write w (str c)))
  (.write w "\""))

(defmethod print-method Class [^Class c, ^Writer w]
  (.write w (.getName c)))

(defmethod print-method java.math.BigDecimal [b, ^Writer w]
  (.write w "\"~f")
  (.write w (str b))
  (.write w "\""))

(defn print-bigint [b, ^Writer w]
  (.write w "\"~n")
  (.write w (str b))
  (.write w "\""))

(defmethod print-method clojure.lang.BigInt [b, ^Writer w]
  (print-bigint b w))

(def ^:const MAX_INTEGER (dec (bit-shift-left 2 28)))
(def ^:const MIN_INTEGER (- (bit-shift-left 2 28)))

(defmethod print-method Integer [n, ^Writer w]
  (if (or (> n MAX_INTEGER) (< n MIN_INTEGER))
    (print-bigint n w)
    (.write w (str n))))

(comment
  (print-bigint (java.math.BigInteger. "99999999999999999999999999999999999999999999999999999999999999999999") w)
  )

(defmethod print-method Long [n, ^Writer w]
  (if (or (> n MAX_INTEGER) (< n MIN_INTEGER))
    (print-bigint n w)
    (.write w (str n))))

(defmethod print-method java.util.regex.Pattern [p ^Writer w]
  (.write w "\"~p")
  (.write w (.pattern p))
  (.write w "\""))

(defn- deref-as-map [^clojure.lang.IDeref o]
  (let [pending (and (instance? clojure.lang.IPending o)
                     (not (.isRealized ^clojure.lang.IPending o)))
        [ex val]
        (when-not pending
          (try [false (deref o)]
               (catch Throwable e
                 [true e])))]
    {:status
     (cond
       (or ex
           (and (instance? clojure.lang.Agent o)
                (agent-error o)))
       :failed

       pending
       :pending

       :else
       :ready)

     :val val}))

(defmethod print-method clojure.lang.IDeref [o ^Writer w]
  (print-tagged-object o (deref-as-map o) w))

(defmethod print-method StackTraceElement [^StackTraceElement o ^Writer w]
  (print-method [(symbol (.getClassName o)) (symbol (.getMethodName o)) (.getFileName o) (.getLineNumber o)] w))

(defmethod print-method clojure.lang.TaggedLiteral [o ^Writer w]
  (.write w "[\"~#taggedliteral\" [")
  (print-method (:tag o) w)
  (.write w " ")
  (print-method (:form o) w)
  (.write w "]]"))

(defn- print-throwable [^Throwable o ^Writer w]
  (.write w "[\"~#error\" ")
  (.write w "#s(hash-table test equal data (:cause ")
  (let [{:keys [cause data via trace]} (Throwable->map o)
        print-via #(do (.write w "#s(hash-table test equal data (:type ")
                       (print-method (:type %) w)
                       (.write w " :message ")
                       (print-method (:message %) w)
                       (when-let [data (:data %)]
                         (.write w " :data ")
                         (print-method data w))
                       (when-let [at (:at %)]
                         (.write w " :at ")
                         (print-method (:at %) w))
                       (.write w "))"))]
    (print-method cause w)
    (when data
      (.write w " :data ")
      (print-method data w))
    (when via
      (.write w " :via [")
      (when-let [fv (first via)]
        (print-via fv)
        (doseq [v (rest via)]
          (.write w " ")
          (print-via v)))
      (.write w "]"))
    (when trace
      (.write w " :trace [")
      (when-let [ft (first trace)]
        (print-method ft w)
        (doseq [t (rest trace)]
          (.write w " ")
          (print-method t w)))
      (.write w "]")))
  (.write w "))]"))

(defmethod print-method Throwable [^Throwable o ^Writer w]
  (print-throwable o w))

(defmethod print-method java.util.UUID [uuid ^java.io.Writer w]
  (.write w "\"~u")
  (.write w (str uuid))
  (.write w "\""))

(defn- print-calendar
  "Print a java.util.Calendar as RFC3339 timestamp, preserving timezone."
  [^java.util.Calendar c, ^java.io.Writer w]
  (let [calstr (format "%1$tFT%1$tT.%1$tL%1$tz" c)
        offset-minutes (- (.length calstr) 2)]
    ;; calstr is almost right, but is missing the colon in the offset
    (.write w "\"~i")
    (.write w calstr 0 offset-minutes)
    (.write w ":")
    (.write w calstr offset-minutes 2)
    (.write w "\"")))

(defmethod print-method java.util.Calendar
  [^java.util.Calendar c, ^java.io.Writer w]
  (print-calendar c w))

(defn- print-date
  "Print a java.util.Date as RFC3339 timestamp, always in UTC."
  [^java.util.Date d, ^java.io.Writer w]
  (let [^java.text.DateFormat utc-format (.get @#'clojure.instant/thread-local-utc-date-format)]
    (.write w "\"~i")
    (.write w (.format utc-format d))
    (.write w "\"")))

(defmethod print-method java.util.Date
  [^java.util.Date d, ^java.io.Writer w]
  (print-date d w))

(defmethod print-method Eduction [c, ^Writer w]
  (print-sequential "(" pr-on " " ")" c w print-more-length-one))

(defn- print-timestamp
  "Print a java.sql.Timestamp as RFC3339 timestamp, always in UTC."
  [^java.sql.Timestamp ts, ^java.io.Writer w]
  (let [^java.text.DateFormat utc-format (.get @#'clojure.instant/thread-local-utc-timestamp-format)]
    (.write w "\"~i")
    (.write w (.format utc-format ts))
    ;; add on nanos and offset
    ;; RFC3339 says to use -00:00 when the timezone is unknown (+00:00 implies a known GMT)
    (.write w (format ".%09d-00:00" (.getNanos ts)))
    (.write w "\"")))

(defmethod print-method java.sql.Timestamp
  [^java.sql.Timestamp ts, ^java.io.Writer w]
  (print-timestamp ts w))

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


(comment
  (with-out-str (pr #'clojure.core/prn))

  (with-out-str (pr (tagged-literal 'e 44)))

  (with-out-str (binding [*print-level* 1]
                  (pr 
                   {:e [{:f "f"}]})))
  )

(comment
  (keys (methods print-method))
  (keys (methods clojure.core/print-method))

  ;; core protocol CollReduce ReduceKV -- exclude Object
  ;; find-protocol-impl
  )

;; zero copy (for things serialized from cljs)
(deftype ElispString [s])

(defmethod print-method ElispString [^String s ^java.io.Writer w]
  (.write w (.-s s)))
