(ns replique.json
  "Minimal, dependency free JSON writer.

  Values are written to a java.io.Writer. Types that have no JSON
  representation (Ratio, non finite doubles, ...) are rejected instead of being
  silently coerced: protocol frames must never carry them. Data coming from the
  user (evaluation results, tapped values, ...) is printed by the Clojure
  printer and travels as a JSON string.

  Dispatch is an explicit cond, not a protocol, because clojure maps implement
  both java.util.Map and Iterable - protocol dispatch would be ambiguous."
  (:import [java.io Writer StringWriter]))

(declare write-json)

(defn- write-hex-escape [^Writer w ^long c]
  (.write w "\\u")
  (let [hex (Integer/toHexString c)]
    (dotimes [_ (- 4 (.length hex))]
      (.write w "0"))
    (.write w hex)))

;; Unpaired surrogates are valid in java/clojure strings but not in JSON. They
;; are replaced by the unicode replacement character - emacs's json parser (and
;; most others) rejects them, even when escaped.
(def ^:private ^String replacement-char "\\ufffd")

(defn write-string [^String s ^Writer w]
  (.write w "\"")
  (let [len (.length s)]
    (loop [i 0]
      (when (< i len)
        (let [c (.charAt s i)
              code (int c)]
          (cond
            (= c \") (do (.write w "\\\"") (recur (inc i)))
            (= c \\) (do (.write w "\\\\") (recur (inc i)))
            (= c \newline) (do (.write w "\\n") (recur (inc i)))
            (= c \return) (do (.write w "\\r") (recur (inc i)))
            (= c \tab) (do (.write w "\\t") (recur (inc i)))
            (= c \backspace) (do (.write w "\\b") (recur (inc i)))
            (= c \formfeed) (do (.write w "\\f") (recur (inc i)))
            (< code 0x20) (do (write-hex-escape w code) (recur (inc i)))
            (Character/isHighSurrogate c)
            (let [low (when (< (inc i) len) (.charAt s (inc i)))]
              (if (and low (Character/isLowSurrogate low))
                (do (.write w code) (.write w (int low)) (recur (+ i 2)))
                (do (.write w replacement-char) (recur (inc i)))))
            (Character/isLowSurrogate c)
            (do (.write w replacement-char) (recur (inc i)))
            :else (do (.write w code) (recur (inc i))))))))
  (.write w "\""))

(defn- key->string ^String [k]
  (cond
    (string? k) k
    (keyword? k) (subs (str k) 1)
    (symbol? k) (str k)
    (integer? k) (str k)
    :else (throw (IllegalArgumentException.
                  (str "Invalid JSON object key: " (pr-str k))))))

(defn- write-map [m ^Writer w]
  (.write w "{")
  (loop [entries (seq m)
         first? true]
    (when entries
      (let [e (first entries)]
        (when-not first? (.write w ","))
        (write-string (key->string (key e)) w)
        (.write w ":")
        (write-json (val e) w)
        (recur (next entries) false))))
  (.write w "}"))

(defn- write-array [coll ^Writer w]
  (.write w "[")
  (loop [xs (seq coll)
         first? true]
    (when xs
      (when-not first? (.write w ","))
      (write-json (first xs) w)
      (recur (next xs) false)))
  (.write w "]"))

(defn- write-number [x ^Writer w]
  (cond
    (or (instance? Double x) (instance? Float x))
    (let [d (double x)]
      (if (or (Double/isNaN d) (Double/isInfinite d))
        (throw (IllegalArgumentException. (str "Cannot write " d " as JSON")))
        (.write w (str d))))
    (ratio? x)
    (throw (IllegalArgumentException. (str "Cannot write the ratio " (pr-str x) " as JSON")))
    :else
    (.write w (.toString ^Object x))))

(defn write-json
  "Write x, as JSON, to the java.io.Writer w."
  [x ^Writer w]
  (cond
    (nil? x) (.write w "null")
    (string? x) (write-string x w)
    (keyword? x) (write-string (subs (str x) 1) w)
    (symbol? x) (write-string (str x) w)
    (instance? Boolean x) (.write w (if x "true" "false"))
    (number? x) (write-number x w)
    (instance? Character x) (write-string (str x) w)
    (instance? java.util.Map x) (write-map x w)
    (instance? Iterable x) (write-array x w)
    (.isArray (class x)) (write-array (seq x) w)
    :else (throw (IllegalArgumentException.
                  (str "Cannot write an instance of " (.getName (class x)) " as JSON")))))

(defn write-str
  "Return x, as a JSON string."
  ^String [x]
  (let [w (StringWriter.)]
    (write-json x w)
    (.toString w)))
