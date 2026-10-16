(ns replique.shape
  "What a value looks like rather than what it is: the keys of its maps, how
  many things its collections hold and what those look like, the types of the
  rest - the same on the JVM and in a ClojureScript runtime, which each work
  out the shapes of the values they keep.

  A SHAPE IS WRITTEN AS DATA THAT READS LIKE A DESCRIPTION, so that it can be
  printed as it is:

    {:id uuid, :status :draft, :tags (vector keyword 3)}

  - a map whose keys are all keywords is the map of its keys to the shapes of
    their values - a record, (record my.ns.Foo {...}); any other map is
    (map-of KEY-SHAPE VALUE-SHAPE COUNT)
  - a collection is (KIND ELEMENT-SHAPE COUNT), KIND one of vector, list,
    set, seq, array, and lazy for a seq some of which is not realized yet:
    COUNT is then how much of it is, and a lazy seq none of which is, is
    `lazy'. An empty one's ELEMENT-SHAPE is `none'
  - a keyword is itself, which is what tells a status from another
  - nil is nil, and anything else is the name of its type: string, long,
    uuid, fn, ... or its class's name

  SHAPES MERGE: the shape of many values is the shape that says what each of
  them looked like. A key that was not in all the maps is (optional SHAPE), a
  COUNT that varied is [MIN MAX], and things that were of more than one kind
  are (or SHAPE ...) - where keywords give way to `keyword' once there are
  more of them than say anything.

  NOTHING IS REALIZED OR DEREFERENCED to find out its shape: a lazy seq is
  as much of it as was realized, and an atom is an atom - what is in it now
  is not what was in it when the value was recorded. Nor is all of a big
  collection looked at: the shape of its elements is that of its first
  ELEMENTS ones. And past DEPTH collections in, a collection is `...'.")

(def ^:private keywords-said
  "How many different keywords a shape says before saying `keyword'."
  5)

(def ^:private fields-said
  "How many keys a map can have and still be said key by key."
  40)

(def ^:private counted-up-to
  "How much of a seq that does not know its count is walked to count it."
  10000)

(defn- unrealized-seq? [x]
  (and (seq? x)
       #?(:clj (instance? clojure.lang.IPending x) :cljs (satisfies? IPending x))
       (not (realized? x))))

(defn- realized
  "Of the seq S, what is realized, up to COUNTED-UP-TO things: [things end],
  END :end where that was all of it, :lazy where what follows is not
  realized, :more where there was more to count."
  [s]
  (loop [s s, things []]
    (cond
      (unrealized-seq? s) [things :lazy]
      (= counted-up-to (count things)) [things :more]
      ;; realized, which `seq' and `rest' leave as it is
      :else (if-let [s (seq s)]
              (recur (rest s) (conj things (first s)))
              [things :end]))))

(defn- type-name [x]
  #?(:clj (symbol (.getName (class x)))
     ;; a constructor of JavaScript's, which prints as an #object
     :cljs (if-let [t (type x)]
             (if (.-cljs$lang$type t) (symbol (pr-str t)) (symbol "js" (or (not-empty (.-name t)) "Object")))
             'object)))

(defn- typed
  "The shape of X, which holds nothing a shape looks into."
  [x]
  #?(:clj (condp instance? x
            String 'string
            Long 'long
            Double 'double
            Boolean 'boolean
            Integer 'int
            Float 'float
            Character 'char
            clojure.lang.Ratio 'ratio
            clojure.lang.BigInt 'bigint
            BigDecimal 'bigdec
            clojure.lang.Symbol 'symbol
            java.util.UUID 'uuid
            java.util.Date 'inst
            java.util.regex.Pattern 'regex
            clojure.lang.Var 'var
            clojure.lang.Atom 'atom
            clojure.lang.Volatile 'volatile
            clojure.lang.Ref 'ref
            clojure.lang.Agent 'agent
            clojure.lang.Delay 'delay
            java.util.concurrent.Future 'future
            clojure.lang.MultiFn 'multi-fn
            clojure.lang.Fn 'fn
            ;; `promise' is a reify, whose class has no name to say
            (if (and (instance? clojure.lang.IBlockingDeref x)
                     (instance? clojure.lang.IPending x))
              'promise
              (type-name x)))
     :cljs (cond
             (string? x) 'string
             (number? x) 'number
             (boolean? x) 'boolean
             (symbol? x) 'symbol
             (uuid? x) 'uuid
             (inst? x) 'inst
             (regexp? x) 'regex
             (var? x) 'var
             (instance? Atom x) 'atom
             (volatile? x) 'volatile
             (delay? x) 'delay
             (instance? MultiFn x) 'multi-fn
             (fn? x) 'fn
             (instance? js/Promise x) 'promise
             (object? x) 'object
             :else (type-name x))))

(declare merged)

(defn- of-all
  "The shape of the things XS, merged - `none' for no things."
  [xs shape-of]
  (reduce (fn [s x] (merged s (shape-of x))) 'none xs))

(defn- an-array? [x]
  #?(:clj (.isArray ^Class (class x)) :cljs (array? x)))

(defn- keyed
  "An empty map whose keys come in order - by how they print, which keys
  that are not all keywords do too."
  []
  (sorted-map-by #(compare (str %1) (str %2))))

(defn shape
  "The shape of X - see this namespace's docstring. Its collections are
  looked into DEPTH deep, and the shape of their elements is that of their
  first ELEMENTS ones."
  ([x] (shape x {}))
  ([x {:keys [depth elements] :or {depth 6 elements 100}}]
   (letfn [(inner [x] (shape x {:depth (dec depth) :elements elements}))
           (fields [m] (into (keyed) (map (fn [[k v]] [k (inner v)])) m))
           (coll [kind things n] (list kind (of-all (take elements things) inner) n))]
     (cond
       (nil? x) nil
       (keyword? x) x
       (unrealized-seq? x) 'lazy
       (not (or (coll? x) (an-array? x))) (typed x)
       (<= depth 0) '...
       (record? x) (list 'record (type-name x) (fields x))
       (map? x) (if (and (<= (count x) fields-said) (every? keyword? (keys x)))
                  (fields x)
                  (list 'map-of
                        (of-all (take elements (keys x)) inner)
                        (of-all (take elements (vals x)) inner)
                        (count x)))
       (vector? x) (coll 'vector x (count x))
       (set? x) (coll 'set x (count x))
       (an-array? x) (coll 'array x (count x))
       (seq? x) (let [[things end] (if (counted? x) [x :end] (realized x))
                      n (if (counted? x) (count x) (count things))]
                  (case end
                    :lazy (coll 'lazy things n)
                    :more (coll 'seq things '?)
                    (coll (if (list? x) 'list 'seq) things n)))
       :else (typed x)))))

;;; Merging

(defn- or? [s] (and (seq? s) (= 'or (first s))))

(defn- optional? [s] (and (seq? s) (= 'optional (first s))))

(defn- alternatives [s] (if (or? s) (rest s) [s]))

(defn- kind
  "What S is, as far as merging goes: two shapes of one kind merge into one,
  two of different kinds are two alternatives."
  [s]
  (cond
    (nil? s) :nil
    (or (keyword? s) (= 'keyword s)) :keyword
    (map? s) :map
    (= 'lazy s) 'lazy
    (seq? s) (if (= 'record (first s)) [:record (second s)] (first s))
    :else s))

(defn- collection? [s] (or (map? s) (seq? s) (= 'lazy s)))

(defn- counts [a b]
  (if (or (= '? a) (= '? b))
    '?
    (let [[a1 a2] (if (vector? a) a [a a])
          [b1 b2] (if (vector? b) b [b b])
          lo (min a1 b1)
          hi (max a2 b2)]
      (if (= lo hi) lo [lo hi]))))

(defn- fields-merged [a b]
  (let [bare #(if (optional? %) (second %) %)]
    (into (keyed)
          (map (fn [k]
                 [k (if (and (contains? a k) (contains? b k))
                      (let [s (merged (bare (get a k)) (bare (get b k)))]
                        (if (or (optional? (get a k)) (optional? (get b k)))
                          (list 'optional s)
                          s))
                      (list 'optional (bare (get a k (get b k)))))]))
          (distinct (concat (keys a) (keys b))))))

(defn- merged-alike
  "The shapes A and B, of one kind, as one."
  [a b]
  (let [as-seq #(if (= 'lazy %) '(lazy none 0) %)
        [a b] (if (= a b) [a b] [(as-seq a) (as-seq b)])]
    (cond
      (= a b) a
      (map? a) (fields-merged a b)
      (= 'record (first a)) (list 'record (second a) (fields-merged (nth a 2) (nth b 2)))
      (= 'map-of (first a)) (let [[_ ka va na] a, [_ kb vb nb] b]
                              (list 'map-of (merged ka kb) (merged va vb) (counts na nb)))
      :else (let [[k sa na] a, [_ sb nb] b]
              (list k (merged sa sb) (counts na nb))))))

(defn- with-alternative
  "The alternatives ALTS, and S - merged into the one of its kind, if any."
  [alts s]
  (if (= :keyword (kind s))
    (let [kws (filter #(= :keyword (kind %)) alts)
          others (remove #(= :keyword (kind %)) alts)
          kws (distinct (concat kws [s]))]
      (if (or (some #{'keyword} kws) (> (count kws) keywords-said))
        (conj (vec others) 'keyword)
        (into (vec others) kws)))
    (if-let [i (first (keep-indexed (fn [i a] (when (= (kind a) (kind s)) i)) alts))]
      (update alts i merged-alike s)
      (conj alts s))))

(defn merged
  "The shape of the values of shapes A and B - see this namespace's
  docstring. `none', no value, is the shape that merging starts from."
  ([] 'none)
  ([a] a)
  ([a b]
   (cond
     (= 'none a) b
     (= 'none b) a
     :else
     (let [alts (reduce with-alternative [] (concat (alternatives a) (alternatives b)))
           ;; what was too deep to say is said by what was not
           alts (if (and (some #{'...} alts) (some #(and (collection? %) (not= '... %)) alts))
                  (remove #{'...} alts)
                  alts)
           ;; and nil last: (or string nil)
           alts (concat (remove nil? alts) (filter nil? alts))]
       (if (next alts) (apply list 'or alts) (first alts))))))
