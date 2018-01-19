(ns replique.cljs-env.elisp-printer
  "Print to a format that can be read by the elisp reader. Prints to a transit-like format."
  (:refer-clojure :exclude [IPrintWithWriter -pr-writer IWriter -write -flush
                            StringBufferWriter ->StringBufferWriter
                            pr-str print-prefix-map print-map])
  (:require [clojure.string :as s]
            [goog.string :as gstring])
  (:require-macros [replique.cljs-env.elisp-printer :refer [print-with-meta custom-unchecked-get]]
                   [replique.cljs :refer [with-version]])
  (:import [goog.string StringBuffer]))

(defprotocol IWriter
  (-write [writer s])
  (-append [writer c]))

(deftype StringBufferWriter [sb]
  IWriter
  (-write [_ s] (.append sb s))
  (-append [_ c] (.append sb c)))

(defn escape-symbol [symbol-str]
  (s/replace symbol-str #"(?=[\\?\[\]\(\)\{\}\+\-#;\.])" "\\"))

(defn- print-sequential [^String begin print-one ^String sep ^String end sequence w]
  (-write w begin)
  (when-let [xs (seq sequence)]
    (loop [[x & xs] xs]
      (print-one x w)
      (when xs
        (-write w sep)
        (recur xs))))
  (-write w end))

(defprotocol IPrintWithWriter
  (-pr-writer [o writer]))

(defn- print-tagged-object* [o rep w]
     (-write w "[\"~#object\" [")
     (-write w rep)
  (-write w "]]"))

(defn- print-tagged-object [o rep w]
  (if (implements? IMeta o)
    (print-with-meta
     o w
     (print-tagged-object* o rep w))
    (print-tagged-object* o rep w)))

(defn- print-object [obj w]
  (if (some-> obj .-constructor .-cljs$lang$ctorStr)
    (print-tagged-object obj (.replace (.. obj -constructor -cljs$lang$ctorStr)
                                       (js/RegExp. "/" "g") ".") w)
    (let [name (some-> obj .-constructor .-name)
          name (if (or (nil? name) (gstring/isEmpty name))
                 "Object"
                 name)]
      (if (nil? (. obj -constructor))
        (print-tagged-object obj name w)
        (print-tagged-object obj (str name " " (str obj)) w)))))

(defn- print-function [f w]
  (let [name (.-name f)
        name (if (or (nil? name) (gstring/isEmpty name))
               "Function"
               name)]
    (print-tagged-object f name w)))

(def char-escape-string
  (js-obj \newline "\\n"
          \tab  "\\t"
          \return "\\r"
          \" "\\\""
          \\  "\\\\"
          \formfeed "\\f"
          \backspace "\\b"))

(defn print-string [s w]
  (-append w \")
  (dotimes [n (count s)]
    (let [c (.charAt s n)
          e (if (and (= 0 n) (= \~ c))
              "~~"
              (custom-unchecked-get char-escape-string c))]
      (if e (-write w e) (-append w c))))
  (-append w \")
  nil)

(defn print-boolean [o w]
  (-write w "\"~?")
  (if o
    (-write w "t")
    (-write w "f"))
  (-write w "\""))

(def ^:const MAX_INTEGER (dec (bit-shift-left 2 28)))
(def ^:const MIN_INTEGER (- (bit-shift-left 2 28)))

(defn print-bigint [b w]
  (-write w "\"~n")
  (-write w (str b))
  (-write w "\""))

(defn print-bigdec [b w]
  (-write w "\"~f")
  (-write w (str b))
  (-write w "\""))

(defn print-number [o w]
  (cond
    ^boolean (js/isNaN o) (-write w "\"~zNaN\"")
    (identical? o js/Number.POSITIVE_INFINITY) (-write w "\"~zInf\"")
    (identical? o js/Number.NEGATIVE_INFINITY) (-write w "\"~z-Inf\"")
    (and (int? o) (or (> o MAX_INTEGER) (< o MIN_INTEGER))) (print-bigint o w)
    :else (-write w (str o))))

(defn- print-prefix-map [m print-one w]
  (print-sequential
   (str "#s(hash-table test equal size " (count m) " data (")
   (fn [e ^Writer w]
     (do (print-one (key e) w) (-append w \space) (print-one (val e) w)))
   " "
   "))"
   (seq m) w))

(defn- print-map [m print-one w]
  (print-prefix-map m print-one w))

(defn print-js-object [o w]
  (-write w "[\"~#js\" ")
  (print-prefix-map
   (map (fn [k]
          [(cond-> k (some? (re-matches #"[A-Za-z_\*\+\?!\-'][\w\*\+\?!\-']*" k)) keyword)
           (custom-unchecked-get o k)])
        (js-keys o))
   -pr-writer w)
  (-write w "]"))

(defn print-js-array [o w]
  (cljs.core/prn o)
  (-write w "[\"~#js\" ")
  (print-sequential "[" -pr-writer " " "]" o w)
  (-write w "]"))

(defn print-regexp [o w]
  (-write w "\"~p")
  (-write w (.-source o))
  (-write w "\""))

(defn print-date
  [o w]
  (let [normalize (fn [n len]
                    (loop [ns (str n)]
                      (if (< (count ns) len)
                        (recur (str "0" ns))
                        ns)))]
    (-write w "\"~i")
    (-write w (str (.getUTCFullYear o) "-"
                   (normalize (inc (.getUTCMonth o)) 2) "-"
                   (normalize (.getUTCDate o) 2) "T"
                   (normalize (.getUTCHours o) 2) ":"
                   (normalize (.getUTCMinutes o) 2) ":"
                   (normalize (.getUTCSeconds o) 2) "."
                   (normalize (.getUTCMilliseconds o) 3) "-"))
    (-write w "00:00\"")))

(defn print-var [o w]
  (-write w "\"~v")
  (-write w (.-sym o))
  (-write w "\""))

(defn print-uuid [o w]
  (-write w "\"~u")
  (-write w (str o))
  (-write w "\""))

(defn print-ex-info [o w]
  (-write w "[\"~#error\" ")
  (-write w "#s(hash-table test equal data (:message ")
  (-pr-writer (.-message o) w)
  (when (.-data o)
    (-write w " :data ")
    (-pr-writer (.-data o) w))
  (when (.-cause o)
    (-write w " :cause ")
    (-pr-writer (.-cause o) w))
  (-write w "))]"))

(defn print-tagged-literal [o w]
  (-write w "[\"~#taggedliteral\" [")
  (-pr-writer (:tag o) w)
  (-write w " ")
  (-pr-writer (:form o) w)
  (-write w "]]"))

(defn print-queue [o w]
  (print-with-meta
   o w
   (-write w "[\"~#queue\" ")
   (print-sequential "[" -pr-writer " " "]" o w)
   (-write w "]")))

(defn print-set [o w]
  (print-with-meta
   o w
   (-write w "[\"~#set\" ")
   (print-sequential "[" -pr-writer " " "]" o w)
   (-write w "]")))

(defn print-record [o w]
  (print-with-meta
   o w
   (-write w "[\"~#")
   (-write w (cljs.core/pr-str (type o)))
   (-write w "\" ")
   (print-map o -pr-writer w)
   (-write w "]")))

(extend-protocol IPrintWithWriter
  nil
  (-pr-writer [o w]
    (-write w "nil"))
  function
  (-pr-writer [o w] (print-function o w))
  default
  (-pr-writer [o w] (print-object o w))
  string
  (-pr-writer [o w] (print-string o w))
  boolean
  (-pr-writer [o w] (print-boolean o w))
  number
  (-pr-writer [o w] (print-number o w))
  Keyword
  (-pr-writer [o w] (-write w (str o)))
  object
  (-pr-writer [o w] (cond
                      (instance? js/Date o)
                      (print-date o w)
                      (record? o)
                      (print-record o w)
                      (regexp? o)
                      (print-regexp o w)
                      (object? o)
                      (print-js-object o w)
                      :else (print-object o w)))
  array
  (-pr-writer [o w] (print-js-array o w))
  Symbol
  (-pr-writer [o w] (print-with-meta
                     o w
                     (-write w (escape-symbol (str o)))))
  List
  (-pr-writer [o w] (print-with-meta
                     o w
                     (print-sequential "(" -pr-writer " " ")" o w)))
  Var
  (-pr-writer [o w] (print-with-meta
                     o w
                     (print-var o w)))
  Eduction
  (-pr-writer [o w] (print-sequential "(" -pr-writer " " ")" o w))
  UUID
  (-pr-writer [o w] (print-uuid o w))
  ExceptionInfo
  (-pr-writer [o w] (print-ex-info o w))
  TaggedLiteral
  (-pr-writer [o w] (print-tagged-literal o w))
  LazySeq
  (-pr-writer [o w]
    (print-with-meta
     o w
     (print-sequential "(" -pr-writer " " ")" o w)))
  IndexedSeq
  (-pr-writer [o w]
    (print-with-meta
     o w
     (print-sequential "(" -pr-writer " " ")" o w)))
  RSeq
  (-pr-writer [o w]
    (print-with-meta
     o w
     (print-sequential "(" -pr-writer " " ")" o w)))
  PersistentQueue
  (-pr-writer [o w] (print-queue o w))
  PersistentQueueSeq
  (-pr-writer [o w]
    (print-with-meta
     o w
     (print-sequential "(" -pr-writer " " ")" o w)))
  PersistentTreeMapSeq
  (-pr-writer [o w]
    (print-with-meta
     o w
     (print-sequential "(" -pr-writer " " ")" o w)))
  NodeSeq
  (-pr-writer [o w]
    (print-with-meta
     o w
     (print-sequential "(" -pr-writer " " ")" o w)))
  ArrayNodeSeq
  (-pr-writer [o w]
    (print-with-meta
     o w
     (print-sequential "(" -pr-writer " " ")" o w)))
  Cons
  (-pr-writer [o w]
    (print-with-meta
     o w
     (print-sequential "(" -pr-writer " " ")" o w)))
  EmptyList
  (-pr-writer [o w] (-write w "()"))
  PersistentVector
  (-pr-writer [o w]
    (print-with-meta
     o w
     (print-sequential "[" -pr-writer " " "]" o w)))
  ChunkedCons
  (-pr-writer [o w]
    (print-with-meta
     o w
     (print-sequential "(" -pr-writer " " ")" o w)))
  ChunkedSeq
  (-pr-writer [o w]
    (print-with-meta
     o w
     (print-sequential "(" -pr-writer " " ")" o w)))
  Subvec
  (-pr-writer [o w]
    (print-with-meta
     o w
     (print-sequential "[" -pr-writer " " "]" o w)))
  BlackNode
  (-pr-writer [o w]
    (print-with-meta
     o w
     (print-sequential "[" -pr-writer " " "]" o w)))
  RedNode
  (-pr-writer [o w]
    (print-with-meta
     o w
     (print-sequential "[" -pr-writer " " "]" o w)))
  ObjMap
  (-pr-writer [o w] (print-with-meta o w (print-map o -pr-writer w)))
  KeySeq
  (-pr-writer [o w]
    (print-with-meta
     o w
     (print-sequential "(" -pr-writer " " ")" o w)))
  ValSeq
  (-pr-writer [o w]
    (print-with-meta
     o w
     (print-sequential "(" -pr-writer " " ")" o w)))
  PersistentArrayMapSeq
  (-pr-writer [o w]
    (print-with-meta
     o w
     (print-sequential "(" -pr-writer " " ")" o w)))
  PersistentArrayMap
  (-pr-writer [o w] (print-with-meta o w (print-map o -pr-writer w)))
  PersistentHashMap
  (-pr-writer [o w] (print-with-meta o w (print-map o -pr-writer w)))
  PersistentTreeMap
  (-pr-writer [o w] (print-with-meta o w (print-map o -pr-writer w)))
  PersistentHashSet
  (-pr-writer [o w] (print-set o w))
  PersistentTreeSet
  (-pr-writer [o w] (print-set o w))
  Range
  (-pr-writer [o w]
    (print-with-meta
     o w
     (print-sequential "(" -pr-writer " " ")" o w)))
  ES6IteratorSeq
  (-pr-writer [o w]
    (print-with-meta
     o w
     (print-sequential "(" -pr-writer " " ")" o w)))
  Atom
  (-pr-writer [o w]
    (print-with-meta
     o w
     (-write w "[\"~#object\" [cljs.core.Atom ")
     (-pr-writer {:val (.-state o)} w)
     (-write w "]]")))
  Volatile
  (-pr-writer [o w]
    (print-with-meta
     o w
     (-write w "[\"~#object\" [cljs.core.Volatile ")
     (-pr-writer {:val (.-state o)} w)
     (-write w "]]"))))

(with-version
  [1 9 562]
  [nil nil nil]
  (extend-protocol IPrintWithWriter
    TransformerIterator
    (-pr-writer [o w]
      (print-with-meta
       o w
       (print-sequential "(" -pr-writer " " ")" o w)))))

(defn pr-seq [objs writer]
  (-pr-writer (first objs) writer)
  (doseq [obj (next objs)]
    (-write writer " ")
    (-pr-writer obj writer)))

(defn pr-sb [objs]
  (let [sb (StringBuffer.)
        writer (StringBufferWriter. sb)]
    (pr-seq objs writer)
    sb))


(defn pr-str [& objs]
  (if (empty? objs)
    ""
    (str (pr-sb objs))))

(comment
  (.-cljs$lang$type ArrayList)
  
  default

  (pr-str nil)

  (pr-str (with-meta (fn e []) {:e "e"}))

  (pr-str "~ee~e")

  (pr-str js/window)

  (pr-str (/ 3 (js-obj "e" 33)))

  (pr-str 1111111111111111111111111111111111.333333333333333333333)

  (pr-str '?tt)
  )
