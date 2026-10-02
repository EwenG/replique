(ns replique.inspector
  "A value, browsed a piece at a time.

  What an editor shows of a value it is inspecting is a tree it opens one node
  at a time, and what this is, is the half of that tree that holds the value:
  a node is a place in the value, its children are asked for a page at a time,
  and each one comes back as a line to show - a short printing of it, what it
  is, how many things it holds - with the id of its node, and whether there is
  more in it than the line shows.

  THE VALUE STAYS HERE, and that is the whole of what this does better than
  printing it. A printed value is cut by *print-length* and *print-level* all
  at once, and whatever was cut is gone; here nothing is cut, it is only not
  asked for yet. A key that would not read back - an object, a function, a
  map - is a key all the same, because a node is found by the key itself and
  not by its printing. And a lazy seq is realized one page at a time, so an
  infinite one is a seq like any other.

  A NODE IS A PATH, and its id stands for that path across every value the
  view is given. A refresh puts a new value at the root; a node is found again
  in it by walking its path down from there, so what was open stays open, and
  what is no longer there says so. Whether what a node holds changed since it
  was last looked at is `identical?', which is what persistent data makes
  nearly free: what was not touched is the same object.

  OBJECTS ARE BROWSED AS clojure.datafy SAYS: an exception, a class, a
  namespace, and whatever a library extended it to, are shown as the data
  `datafy' makes of them, and a child is what `nav' says it is.

  Written once, for both dialects. The ClojureScript half runs in the runtime,
  where the value is - see replique/inspect.cljs - and the Clojure half in this
  process - see replique/inspect.clj."
  (:require [clojure.core.protocols :as p]
            [clojure.datafy :as d]
            [clojure.string :as string])
  #?(:clj (:import [clojure.lang IDeref IPending]
                   [java.io Writer]
                   [java.util Collection Map RandomAccess Set])))

(def gone
  "What a node is when its path no longer leads anywhere."
  ::gone)

;;; Printing, within a bound

(def ^:private full
  "What a printing that went past its bound is stopped with."
  (ex-info "The printing went past its bound" {::full true}))

(defn- full? [e]
  (true? (::full (ex-data e))))

#?(:clj
   (defn- bounded-writer
     "A Writer that stops the printing once SB holds more than LIMIT chars.

     Stopped rather than cut afterwards: a seq that does not end prints until
     it is stopped, and so does one whose every element costs something to
     realize."
     ^Writer [^StringBuilder sb limit]
     (let [check! (fn [] (when (> (.length sb) (long limit)) (throw full)))]
       (proxy [Writer] []
         (write
           ([x]
            (cond
              (instance? String x) (.append sb ^String x)
              (integer? x) (.append sb (char (int x)))
              :else (.append sb ^chars x))
            (check!))
           ([x off len]
            (if (instance? String x)
              (.append sb ^String x (int off) (int (+ (int off) (int len))))
              (.append sb ^chars x (int off) (int len)))
            (check!)))
         (flush [])
         (close [])))))

#?(:cljs
   (deftype BoundedWriter [^:mutable text limit]
     IWriter
     (-write [_ s]
       (set! text (str text s))
       (when (> (count text) limit) (throw full)))
     (-flush [_] nil)))

(defn- print-within
  "X printed under PRINT-LENGTH and PRINT-LEVEL, and stopped past LIMIT chars:
  [text whole?], whole? being whether nothing was left out. What was printed
  before the bound is the text either way.

  A string is cut before it is printed, which is what the bound is for when
  the value is a log file somebody slurped."
  [x limit print-length print-level meta?]
  (if (and (string? x) (> (count x) limit))
    [(subs (pr-str (subs x 0 limit)) 0 limit) false]
    #?(:clj (let [sb (StringBuilder.)]
              (try
                (binding [*print-length* print-length
                          *print-level* print-level
                          *print-meta* meta?
                          *print-readably* true
                          *out* (bounded-writer sb limit)]
                  (pr x))
                [(str sb) true]
                (catch Throwable e
                  (if (full? e)
                    [(subs (str sb) 0 limit) false]
                    ;; A lazy seq that throws when it is realized, a toString
                    ;; that throws: the value is still there, and the line
                    ;; says what printing it did
                    [(str "#<unprintable: " (or (ex-message e) (str e)) ">") true]))))
       :cljs (let [w (BoundedWriter. "" limit)]
               (try
                 (binding [*print-length* print-length
                           *print-level* print-level
                           *print-meta* meta?]
                   (pr-seq-writer [x] w {:flush-on-newline false
                                         :readably true
                                         :meta meta?
                                         :dup false
                                         :print-length print-length}))
                 [(.-text w) true]
                 (catch :default e
                   (if (full? e)
                     [(subs (.-text w) 0 limit) false]
                     [(str "#<unprintable: " (or (ex-message e) (str e)) ">") true])))))))

(defn- summary
  "X as one line of at most WIDTH chars: {:text :whole}.

  Printed whole where it fits, which is what a small value is: shown, and not
  something to open. Where it does not fit, printed short - the first few of
  everything, two levels down - so that the line says what kind of thing is
  inside without being the thing."
  [x width meta?]
  (let [[text whole?] (print-within x width nil nil meta?)
        one-line (fn [s] (string/replace s #"\s*\n\s*" " "))]
    (if whole?
      {:text (one-line text) :whole true}
      {:text (one-line (first (print-within x width 8 2 meta?))) :whole false})))

;;; What a value is

(defn- pending-unrealized?
  "Whether X is a value still to come - a delay, a future, a promise - which
  deref would wait for."
  [x]
  #?(:clj (and (instance? IPending x) (not (realized? x)))
     :cljs (and (satisfies? IPending x) (not (realized? x)))))

#?(:cljs
   (defn- plain-object?
     "A JavaScript object of no class of its own: what #js {} is."
     [x]
     (and (some? x) (object? x))))

(defn shape
  "How X holds what it holds: :map, :set, :seq, :ref, :object or nil.

  The host's collections are collections: a java.util.Map is browsed as a map
  and an array as a seq."
  [x]
  #?(:clj (cond
            (nil? x) nil
            (or (map? x) (instance? Map x)) :map
            (or (set? x) (instance? Set x)) :set
            (or (sequential? x) (instance? Collection x) (.isArray (class x))) :seq
            (instance? IDeref x) :ref
            :else nil)
     :cljs (cond
             (nil? x) nil
             (map? x) :map
             (set? x) :set
             (or (sequential? x) (array? x) (seq? x)) :seq
             (satisfies? IDeref x) :ref
             (plain-object? x) :object
             :else nil)))

(defn kind
  "What X is, in a word an editor can show it by."
  [x]
  (cond
    (nil? x) "nil"
    (boolean? x) "boolean"
    (string? x) "string"
    (number? x) "number"
    (keyword? x) "keyword"
    (symbol? x) "symbol"
    #?@(:clj [(char? x) "char"])
    (record? x) "record"
    (var? x) "var"
    :else (case (shape x)
            :map "map"
            :set "set"
            :seq (cond (vector? x) "vector"
                       (list? x) "list"
                       #?(:clj (.isArray (class x)) :cljs (array? x)) "array"
                       (seq? x) "seq"
                       :else "collection")
            :ref "ref"
            :object "object"
            (if (fn? x) "fn" "object"))))

(defn type-name
  "The name of X's type, the short way, or nil for nil."
  [x]
  #?(:clj (when (some? x)
            (let [c (class x)]
              (cond
                (record? x) (.getName c)
                (fn? x) (clojure.lang.Compiler/demunge (.getName c))
                :else (.getSimpleName c))))
     :cljs (when (some? x)
             ;; What the compiler named the constructor: cljs$core$ExceptionInfo,
             ;; or PersistentVector$fn - the part that is the type's own name
             (let [n (some-> (type x) .-name)
                   n (when (seq n) (string/replace n #"\$fn$" ""))]
               (if (seq n) (last (string/split n #"\$")) "Object")))))

(defn- counted
  "How many things X holds, where that is known without walking it."
  [x]
  #?(:clj (cond
            (counted? x) (count x)
            ;; a seq is a java.util.List, and asking one its size walks it
            (or (seq? x) (instance? IPending x)) nil
            (or (instance? Collection x) (instance? Map x)) (count x)
            (and (some? x) (.isArray (class x))) (count x)
            :else nil)
     :cljs (cond
             (counted? x) (count x)
             (array? x) (alength x)
             (plain-object? x) (count (js-keys x))
             :else nil)))

#?(:clj
   (def ^:private undatafied
     "What `datafy' does to a value nobody extended it to."
     (find-protocol-impl p/Datafiable (Object.))))

(defn- datafiable?
  "Whether `datafy' makes something else of X: an object it knows how to show
  as data, or a value carrying a datafy of its own.

  Not a collection, nor a reference, which are browsed as what they are -
  clojure.datafy shows a reference as a vector of its value, which is a step
  of nothing between the two."
  [x]
  (and (some? x)
       (or (contains? (meta x) `p/datafy)
           (and (nil? (shape x))
                #?(:clj (not (identical? undatafied (find-protocol-impl p/Datafiable x)))
                   :cljs (and (not (or (string? x) (number? x) (boolean? x)
                                       (keyword? x) (symbol? x) (fn? x)))
                              (not (identical? x (d/datafy x)))))))))

(defn- browsed
  "What the children of X are taken from: what `datafy' makes of it, where it
  makes something else, and X itself otherwise."
  [x]
  (if (datafiable? x) (d/datafy x) x))

(defn- navigable?
  "Whether the children of B lead somewhere by `nav' - which is what a
  library says by putting a nav on the metadata of what it hands out."
  [b]
  (contains? (meta b) `p/nav))

;;; Finding a child

(defn- map-get [b k]
  (try
    #?(:clj (if (instance? Map b)
              (if (.containsKey ^Map b k) (.get ^Map b k) gone)
              gone)
       :cljs (if (contains? b k) (get b k) gone))
    ;; a sorted map asked about a key it cannot compare
    (catch #?(:clj Exception :cljs :default) _ gone)))

(defn- indexed-like? [b]
  #?(:clj (or (instance? RandomAccess b) (indexed? b) (.isArray (class b)))
     :cljs (or (indexed? b) (array? b))))

(defn- nth-of [b i]
  (if (indexed-like? b)
    (if (< -1 i (count b)) (nth b i) gone)
    (if-let [s (nthnext (seq b) i)] (first s) gone)))

(defn- set-get [b x]
  (try
    #?(:clj (cond
              (set? b) (if (contains? b x) (get b x) gone)
              (instance? Set b) (if (.contains ^Set b x) x gone)
              :else gone)
       :cljs (if (contains? b x) (get b x) gone))
    (catch #?(:clj Exception :cljs :default) _ gone)))

#?(:cljs
   (defn- prop-get [b k]
     (if (.call (.-hasOwnProperty (.-prototype js/Object)) b k)
       (unchecked-get b k)
       gone)))

(defn- lookup
  "What STEP leads to from a node whose value is V and whose children are
  taken from B, or `gone'."
  [b v [via k]]
  (case via
    :key (map-get b k)
    :index (nth-of b k)
    :elem (set-get b k)
    :deref (if (pending-unrealized? b) gone @b)
    :meta (if-let [m (meta v)] m gone)
    #?@(:cljs [:prop (prop-get b k)])
    gone))

(defn- navigated
  "What the child X found by STEP from B is, by `nav'."
  [b [via k] x]
  (case via
    :key (d/nav b k x)
    :index (d/nav b (when (indexed-like? b) k) x)
    :elem (d/nav b nil x)
    x))

;;; The view

(defn view
  "A view whose root holds ROOT. What the functions below take, in an atom."
  [root]
  {:gen 0
   :nodes {0 {:raw root :gen 0 :value root :browse (browsed root) :valued 0}}
   :index {}
   :next 1})

(defn- seen
  "REC, the node found by its path to hold RAW in generation GEN.

  Whether it changed is decided the first time a generation sees it, against
  the last one that did, and kept for the rest of that generation."
  [rec gen raw]
  (if (= gen (:gen rec))
    rec
    (assoc rec
           :gen gen
           :raw raw
           :changed (and (contains? rec :raw)
                         ;; A collection by identity, which is what makes this
                         ;; cheap and what says a value was replaced. Anything
                         ;; else by value: the 1000th of something is a long
                         ;; that is a new object every time it is computed
                         (not (if (shape raw)
                                (identical? raw (:raw rec))
                                (= raw (:raw rec))))))))

(defn root!
  "Put ROOT at the root of VIEW. A root that is the one already there changes
  nothing; any other starts a new generation, in which every node is found
  again from it."
  [view root]
  (swap! view
         (fn [{:keys [gen nodes] :as v}]
           (if (identical? root (get-in nodes [0 :raw]))
             v
             (let [gen (inc gen)]
               (-> v
                   (assoc :gen gen)
                   (assoc-in [:nodes 0]
                             (-> (seen (get nodes 0) gen root)
                                 (assoc :value root :browse (browsed root) :valued gen)))))))))

(defn- current
  "The node ID of VIEW as of its generation, with its :value and :browse - or
  nil where its path no longer leads anywhere, or there is no such node."
  [view id]
  (let [{:keys [gen nodes]} @view
        rec (get nodes id)]
    (cond
      (nil? rec) nil
      (= gen (:valued rec)) rec
      :else
      (when-let [parent (current view (:parent rec))]
        (let [raw (lookup (:browse parent) (:value parent) (:step rec))]
          (when-not (identical? gone raw)
            (let [value (navigated (:browse parent) (:step rec) raw)
                  rec (-> (seen rec gen raw)
                          (assoc :value value :browse (browsed value) :valued gen))]
              (swap! view assoc-in [:nodes id] rec)
              rec)))))))

(defn value
  "What the node ID of VIEW holds now, or `gone'."
  [view id]
  (if-let [rec (current view id)] (:value rec) gone))

(defn path
  "The steps from the root of VIEW to its node ID, or nil where there is no
  such node. A step is [:key k], [:index i], [:elem x], [:deref], [:meta],
  or - in ClojureScript - [:prop name]."
  [view id]
  (let [nodes (:nodes @view)]
    (when (contains? nodes id)
      (loop [id id steps ()]
        (if (zero? id)
          (vec steps)
          (let [{:keys [parent step]} (get nodes id)]
            (recur parent (cons step steps))))))))

(defn- node!
  "The id of the child of PARENT found by STEP to hold RAW, made the first
  time it is asked for. The same path is the same id, which is what lets an
  editor keep a node open across values."
  [view parent step raw]
  (let [k [parent step]
        made (volatile! nil)]
    (swap! view
           (fn [{:keys [gen index next] :as v}]
             (if-let [id (get index k)]
               (do (vreset! made id)
                   (update-in v [:nodes id] seen gen raw))
               (do (vreset! made next)
                   (-> v
                       (assoc-in [:index k] next)
                       (assoc-in [:nodes next] (seen {:parent parent :step step} gen raw))
                       (assoc :next (inc next)))))))
    @made))

;;; What an editor is shown

(def ^:private sorted-up-to
  "How many keys a map can have and still be shown sorted. Past that its own
  order, which costs nothing to page through."
  1000)

(defn- in-order
  "XS sorted where they can be compared with each other, and as they are
  otherwise."
  [xs]
  (if (<= (count xs) sorted-up-to)
    (try (sort xs) (catch #?(:clj Exception :cljs :default) _ xs))
    xs))

(defn- in-its-order?
  "Whether the map M is shown in its own order, sorted or not: what it
  holds was put in it in an order that is worth reading in - the locals of
  a frame are, in the order they were bound in."
  [m]
  (boolean (::in-its-order (meta m))))

(defn- page-of
  "The children of a node whose children are taken from B: the LIMIT of them
  from OFFSET, as [step raw] pairs, one more than asked for where there is one
  - which is how it is known that there are more."
  [b offset limit]
  (let [n (inc limit)]
    (case (shape b)
      :map (if (and (<= (or (counted b) (inc sorted-up-to)) sorted-up-to)
                    (not (in-its-order? b)))
             (->> (in-order (keys b))
                  (drop offset) (take n)
                  (map (fn [k] [[:key k] (map-get b k)])))
             (->> (seq b)
                  (drop offset) (take n)
                  (map (fn [e] [[:key (key e)] (val e)]))))
      :set (->> (if (<= (or (counted b) (inc sorted-up-to)) sorted-up-to)
                  (in-order (seq b))
                  (seq b))
                (drop offset) (take n)
                (map (fn [x] [[:elem x] x])))
      :seq (if (indexed-like? b)
             (for [i (range offset (min (count b) (+ offset n)))]
               [[:index i] (nth b i)])
             (->> (seq b)
                  (drop offset) (take n)
                  (map-indexed (fn [i x] [[:index (+ offset i)] x]))))
      :ref (when (and (zero? offset) (not (pending-unrealized? b)))
             [[[:deref] @b]])
      #?@(:cljs [:object (->> (in-order (js-keys b))
                              (drop offset) (take n)
                              (map (fn [k] [[:prop k] (unchecked-get b k)])))])
      nil)))

(defn- label
  "What the line of the child found by STEP is labelled with."
  [[via k] width]
  (case via
    :key {:via "key" :key (:text (summary k (max 20 (quot width 2)) false))}
    :index {:via "index" :key (str k)}
    :elem {:via "elem"}
    :deref {:via "deref" :key "@"}
    :meta {:via "meta" :key "^"}
    :prop {:via "prop" :key (str k)}))

(defn- expandable?
  "Whether a line holding X has more in it than it shows: what does not fit
  on it, a reference, an object `datafy' shows as data, and the child of
  something a library made navigable."
  [x whole? b meta?]
  (or (case (shape x)
        :ref (not (pending-unrealized? x))
        (:map :set :seq :object) (and (or (not whole?) (and meta? (some? (meta x))))
                                      (not (zero? (or (counted x) 1))))
        false)
      (datafiable? x)
      (and (some? b) (navigable? b))))

(defn- describe
  "The line showing X, the child of PARENT found by STEP - or the root, where
  STEP is nil."
  [view parent b step x {width :width meta? :meta}]
  (let [width (or width 80)
        {:keys [text whole]} (summary x width meta?)
        ;; A node for every line and not only for the ones that open, so that
        ;; a line can say it changed whatever it holds
        id (if (nil? step) 0 (node! view parent step x))
        ;; The root is open whatever it holds, small or not: it is what the
        ;; view is of, and a line per child is what says which of them changed
        expandable (expandable? x (and whole (some? step)) (when step b) meta?)
        rec (get-in @view [:nodes id])
        n (when (shape x) (counted x))]
    (cond-> {:value text :kind (kind x)}
      (type-name x) (assoc :type (type-name x))
      n (assoc :count n)
      (not whole) (assoc :truncated true)
      true (assoc :node id)
      expandable (assoc :expandable true)
      (:changed rec) (assoc :changed true)
      step (merge (label step width)))))

(defn describe-root
  "The line showing the root of VIEW, whose node is 0."
  [view opts]
  (let [rec (get-in @view [:nodes 0])]
    (describe view nil nil nil (:value rec) opts)))

(defn children
  "A page of the children of the node ID of VIEW: {:children [line ...]
  :total n :more bool}, :total where it is known without walking the node,
  and {:gone true} where the node is no longer there.

  OPTS are :offset and :limit, the :width of a line, and :meta - which shows
  metadata as the first child of what has some."
  [view id {offset :offset limit :limit meta? :meta :or {offset 0 limit 100} :as opts}]
  (if-let [{v :value b :browse} (current view id)]
    (let [page (page-of b offset limit)
          more? (> (count page) limit)
          page (cond->> (take limit page)
                 (and meta? (zero? offset) (some? (meta v)))
                 (cons [[:meta] (meta v)]))]
      (cond-> {:children (mapv (fn [[step x]] (describe view id b step x opts)) page)
               :more more?}
        (counted b) (assoc :total (counted b))
        (= :ref (shape b)) (assoc :total (if (pending-unrealized? b) 0 1))))
    {:gone true}))

(defn printed
  "The node ID of VIEW printed whole, up to LIMIT chars: {:value :whole}, or
  {:gone true}."
  [view id limit meta?]
  (if-let [{v :value} (current view id)]
    (let [[text whole?] (print-within v limit nil nil meta?)]
      {:value text :whole whole?})
    {:gone true}))

(defn- readable?
  "Whether X prints as something that reads back as X - which a key has to,
  to be written into code."
  [x]
  (cond
    (or (nil? x) (boolean? x) (string? x) (keyword? x) (symbol? x)) true
    (number? x) #?(:clj (not (and (float? x) (or (Double/isNaN x) (Double/isInfinite x))))
                   :cljs (js/isFinite x))
    (record? x) false
    (map? x) (every? readable? (mapcat identity x))
    (or (vector? x) (set? x) (list? x)) (every? readable? x)
    :else false))

(defn path-form
  "The code that reaches the node ID of VIEW from BASE, the code that reaches
  its root - or nil where a step is one code cannot write, a key that does not
  read back.

  Where there is no BASE, the root is a map of names to what they name - a
  repl's *1, *2 and *3 - and the first step is the name.

  What `nav' made of a child is not in it: this is the path through the
  value, and a library that navigates somewhere else is navigating outside
  of it."
  [view id base]
  (when-let [steps (path view id)]
    (let [[base steps] (if (some? base)
                         [base steps]
                         (let [[[via k] & more] steps]
                           (when (and (= :key via) (symbol? k))
                             [k more])))
          forms (map (fn [[via k]]
                       (case via
                         :key (cond (keyword? k) k
                                    (readable? k) (list 'get k))
                         :index (list 'nth k)
                         :elem (when (readable? k) (list 'get k))
                         :deref 'deref
                         :meta 'meta
                         :prop (list 'unchecked-get k)))
                     steps)]
      (when (and (some? base) (every? some? forms))
        (if (seq forms) (list* '-> base forms) base)))))

(defn base-form
  "The code that reaches the root of a session watching the var NAME: the var,
  and a deref for every reference under it - DEREFS of them."
  [name derefs]
  (nth (iterate #(list 'deref %) (symbol name)) derefs))

(defn derefs
  "How many references the session S looks through to its root."
  [s]
  (count @(:refs s)))

;;; Watching

(defn- watchable? [x]
  #?(:clj (instance? clojure.lang.IRef x)
     :cljs (satisfies? IWatchable x)))

(defn- refs-of
  "X and the references under it, outermost first: a var holding an atom is
  watched as both, since `def'ing it again is a change too, and so is the atom
  being swapped. Nil where X is no reference."
  [x]
  (loop [refs [] x x]
    (if (and (watchable? x) (< (count refs) 8))
      (recur (conj refs x) @x)
      refs)))

(defn session
  "A view of what TOP - a function of nothing - answers: the root is the value
  of the last reference under it, and TOP's answer itself where there is none.

  HISTORY is how many of the values it held are kept, where it is a
  reference: each change is one more, and the oldest goes. Nil or 0 keeps
  none."
  [top history]
  (let [refs (refs-of (top))
        root (if (seq refs) @(peek refs) (top))]
    {:top top
     :refs (atom [])
     :model (atom (view root))
     :history (when (and history (pos? history) (seq refs))
                (atom {:values [root] :size history :at nil}))}))

(defn- root-now [{:keys [top]}]
  (let [refs (refs-of (top))]
    (if (seq refs) @(peek refs) (top))))

(defn watch!
  "Watch what the session's root comes from, under KEY, calling CHANGED with
  nothing on every change - and stop watching what it no longer comes from,
  which is what moves a watch to the atom a var holds once it is `def'd
  again. Called again on every change, for that reason."
  [{:keys [top refs]} key changed]
  (let [now (refs-of (top))
        was @refs
        in? (fn [x xs] (some #(identical? x %) xs))]
    (doseq [r was :when (not (in? r now))] (remove-watch r key))
    (doseq [r now :when (not (in? r was))]
      (add-watch r key (fn [_ _ _ _] (changed))))
    (reset! refs now)))

(defn unwatch! [{:keys [refs]} key]
  (doseq [r @refs] (remove-watch r key))
  (reset! refs []))

(defn record!
  "Keep the value the session's root holds now, where it keeps any. A value
  shown from the history keeps being shown: the one that went is the oldest,
  and where that was the one shown it is the next oldest that is."
  [{:keys [history] :as s}]
  (when history
    (let [root (root-now s)]
      (swap! history
             (fn [{:keys [values size at] :as h}]
               (if (identical? root (peek values))
                 h
                 (let [values (conj values root)
                       drop? (> (count values) size)]
                   (assoc h
                          :values (if drop? (subvec values 1) values)
                          :at (when at (max 0 (if drop? (dec at) at)))))))))))

(defn snapshot!
  "Show AT: nil for what the root holds now, an index of the history for a
  value it held."
  [{:keys [model history] :as s} at]
  (if (and history (some? at))
    (let [{:keys [values]} (swap! history
                                  (fn [h] (assoc h :at (max 0 (min at (dec (count (:values h))))))))]
      (root! model (nth values (:at @history))))
    (do (when history (swap! history assoc :at nil))
        (root! model (root-now s)))))

(defn shown
  "What an editor is shown of the session: the root's line, the first page of
  its children where there is more in it than the line shows, and where the
  history is."
  [{:keys [model history]} opts]
  (let [root (describe-root model opts)]
    (cond-> {:root root}
      (:expandable root) (merge (children model 0 opts))
      history (assoc :history (let [{:keys [values at]} @history]
                                (cond-> {:count (count values)}
                                  (some? at) (assoc :at at)))))))
