(ns replique.scope
  "Which locals are in scope at a place of a form, read off the form itself -
  by the names it was written with, and by where they are written.

  A form read with the span of everything in it - see the compiler's
  RECORD_POSITIONS - is code whose locals can be told by position: a param
  of a fn is in scope in the arity it is a param of, a local of a let in what
  follows it in the let. Which is what a repl evaluating in a recorded call
  binds - see `replique.debug/eval-in-call' - the place being where the
  editor's point is in the code of the call.

  What it knows of is what the source wrote: fn and defn, and the forms of
  clojure.core that bind a local to a value - let, when-let, if-let,
  when-some, if-some and with-open. Not the locals bound to something else
  than a value the code had: the element of a doseq or a for, the local of
  a loop, which is another one each time round.")

(defn span
  "Where X was written, [line column end-line end-column] - the end just past
  it - or nil where it was not read with its span."
  [x]
  (let [{:keys [line column end-line end-column]} (meta x)]
    (when (and line column end-line end-column)
      [line column end-line end-column])))

(defn in-span?
  "Whether the place POS, [line column], is inside the span SPAN."
  [[line column end-line end-column :as span] pos]
  (boolean (and span
                (not (neg? (compare pos [line column])))
                (neg? (compare pos [end-line end-column])))))

(defn inside?
  "Whether the place POS, [line column], is inside X - which was written."
  [x pos]
  (in-span? (span x) pos))

(defn marker
  "Where the first thing of FORM that was written is - FORM itself, or what
  it holds, depth first - as [line column], or nil.

  An arity of a fn as the compiler sees it is one fn and defn made: it is not
  what the source wrote, but what it holds is - its params, or the patterns
  they destructure, and its body - and is where it was written."
  [form]
  (if-let [[line column] (span form)]
    [line column]
    (when (coll? form)
      (some marker (if (map? form) (mapcat identity form) form)))))

;;; The fns of a form

(def ^:private fn-heads '#{fn fn* clojure.core/fn})

(def ^:private defn-heads '#{defn defn- clojure.core/defn clojure.core/defn-})

(def ^:private defmethod-heads '#{defmethod clojure.core/defmethod})

(defn- fn-tail
  "The arities of a fn written TAIL - what follows fn, or a defn's name: an
  optional name, then [params] and a body, or ([params] body)s - the span
  of a single arity being WHOLE, the whole fn's."
  [tail whole]
  (let [tail (if (symbol? (first tail)) (rest tail) tail)]
    (if (vector? (first tail))
      [{:params (first tail) :body (rest tail) :span whole}]
      (for [a tail :when (and (seq? a) (vector? (first a)))]
        {:params (first a) :body (rest a) :span (span a) :own true}))))

(defn arities
  "The arities FORM writes, where it is a fn - {:params :body :span}, and
  :own where the span is the arity's own rather than the fn's - or nil."
  [form]
  (when (and (seq? form) (symbol? (first form)))
    (let [head (first form)]
      (cond
        (fn-heads head) (fn-tail (rest form) (span form))
        ;; the docstring and the map of attributes before the arities
        (defn-heads head) (fn-tail (drop-while #(or (string? %) (map? %)) (nnext form))
                                   (span form))
        ;; the dispatch value before them
        (defmethod-heads head) (fn-tail (drop 3 form) (span form))))))

(defn all-arities
  "Every arity written anywhere in FORM, outermost first."
  [form]
  (concat (arities form)
          (when (and (coll? form) (not (and (seq? form) (= 'quote (first form)))))
            (mapcat all-arities (if (map? form) (mapcat identity form) form)))))

(defn entered-arity
  "Of ARITIES, the one a fn the compiler sees whose `marker' is MARKER is -
  or nil, for a fn a macro made, around code of the arity: its marker is
  somewhere in that code, where an arity's own is its params, or what they
  destructure, or the arity itself, of a fn of many."
  [arities marker]
  (last (filter (fn [{:keys [params span own]}]
                  (or (inside? params marker)
                      (and own (= marker (subvec span 0 2)))))
                arities)))

;;; The locals at a place

(def ^:private let-heads
  '#{let when-let if-let when-some if-some with-open
     clojure.core/let clojure.core/when-let clojure.core/if-let
     clojure.core/when-some clojure.core/if-some clojure.core/with-open})

(def ^:private loop-heads
  '#{loop doseq for dotimes
     clojure.core/loop clojure.core/doseq clojure.core/for clojure.core/dotimes})

(def ^:private if-heads '#{if-let if-some clojure.core/if-let clojure.core/if-some})

(defn- quoted? [form]
  (and (seq? form) (= 'quote (first form))))

(defn- children [form]
  (if (map? form) (mapcat identity form) form))

(defn- child-at
  "Of FORMS, the one the place POS is inside of, or nil."
  [forms pos]
  (some #(when (inside? % pos) %) forms))

(declare locals-at)

(defn- loop-locals
  "The locals the loop-like FORM binds at the place POS, inside it: each
  another value each time round, so not one the code had - {:pattern p
  :unbound true} - and the ones the code there binds."
  [form pos]
  (let [[_ bindings & body] form
        unbound (when (vector? bindings)
                  (for [[p _] (partition 2 bindings) :when (not (keyword? p))]
                    {:pattern p :unbound true}))]
    (into (if (inside? bindings pos) [] (vec unbound))
          (some-> (child-at (if (inside? bindings pos) [bindings] body) pos) (locals-at pos)))))

(defn- params
  "The locals the params of ARITY bind: each a {:pattern :arity :arg}, the
  value of the param numbered :arg - the rest param's being the last."
  [arity]
  (let [[fixed [_ more]] (split-with #(not= '& %) (:params arity))]
    (vec (concat (map-indexed (fn [i p] {:pattern p :arity arity :arg i}) fixed)
                 (when more [{:pattern more :arity arity :arg (count fixed)}])))))

(defn- let-locals
  "The locals the let-like FORM binds at the place POS, inside it, and the
  ones the code there binds."
  [form pos]
  (let [[head bindings & body] form
        pairs (when (vector? bindings) (partition 2 bindings))
        bound (mapv (fn [[p init]] {:pattern p :init init}) pairs)]
    (if (inside? bindings pos)
      ;; in the bindings: the ones before the place
      (loop [[[p init] & more] pairs
             bound []]
        (cond
          (or (nil? p) (inside? p pos)) bound
          (inside? init pos) (into bound (locals-at init pos))
          :else (recur more (conj bound {:pattern p :init init}))))
      (let [i (first (keep-indexed (fn [i f] (when (inside? f pos) i)) body))]
        (cond
          (nil? i) bound
          ;; the else of an if-let has nothing bound
          (and (if-heads head) (= 1 i)) (locals-at (nth body i) pos)
          :else (into bound (locals-at (nth body i) pos)))))))

(defn locals-at
  "The locals in scope at the place POS, [line column], of FORM - outermost
  first, as a let would bind them:

    {:pattern p :init form}       what a let-like form binds p to
    {:pattern p :arity a :arg i}  the param numbered i of the arity a - see
                                  `arities'
    {:pattern p :unbound true}    bound by a loop, a doseq, a for or a
                                  dotimes: to another value each time round

  Each :pattern is a name, or what destructures."
  [form pos]
  (cond
    (quoted? form) []
    (arities form) (if-let [a (some #(when (in-span? (:span %) pos) %)
                                    (arities form))]
                     (into (params a) (some-> (child-at (:body a) pos) (locals-at pos)))
                     [])
    (and (seq? form) (let-heads (first form))) (let-locals form pos)
    (and (seq? form) (loop-heads (first form))) (loop-locals form pos)
    (coll? form) (or (some-> (child-at (children form) pos) (locals-at pos)) [])
    :else []))

(defn pattern-names
  "The names the binding pattern P binds."
  [p]
  (cond
    (symbol? p) (if (= '& p) [] [p])
    (vector? p) (mapcat pattern-names p)
    (map? p) (mapcat (fn [[k v]]
                       (cond
                         (= :as k) [v]
                         (= :or k) []
                         (and (keyword? k) (#{"keys" "syms" "strs"} (name k)))
                         (map #(symbol (name %)) v)
                         :else (pattern-names k)))
                     p)
    :else []))

(defn replayed
  "FORM, as code that has the value it had - or ::unknown where that is not
  known. VALUE-OF answers the code that has the value the form of a span
  had, or nil; KNOWN? whether a name is one the code can use.

  A call the source wrote has the value it was recorded with, and does not
  run again; a name, a constant and a fn are what they are; a literal
  vector, map or set is built from what it holds."
  [form value-of known?]
  (let [all (fn [xs] (let [xs (mapv #(replayed % value-of known?) xs)]
                       (if (some #{::unknown} xs) ::unknown xs)))]
    (cond
      (quoted? form) form
      (arities form) form
      (seq? form) (or (value-of (span form)) ::unknown)
      (symbol? form) (if (known? form) form ::unknown)
      (map-entry? form) (all form)
      (vector? form) (all form)
      (map? form) (let [kvs (all (mapcat identity form))]
                    (if (= ::unknown kvs) ::unknown (apply array-map kvs)))
      (set? form) (let [xs (all form)] (if (= ::unknown xs) ::unknown (set xs)))
      :else form)))
