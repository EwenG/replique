(ns replique.iterations
  "Which of the values a call recorded are those of one iteration of a loop of
  it - the same on the JVM and in a ClojureScript runtime, which each keep the
  values of their own calls. See \"Recording\" in replique/debug.clj.

  Every value a call records is numbered, in the order it was recorded in. A
  name a loop binds being bound starts an iteration of the loop, and the
  iteration goes on until it is bound again - or until the loop it is in, for
  one in another, goes round again. What the code the loop evaluates again
  each time round evaluated to in between is that iteration's: once, or more,
  or not at all - a branch not taken.

  Of a form, the first values and the last ones are kept, numbered - what is
  kept of it is {:times n :head [[number value] ...] :tail [...]}, the first
  values and the last, the two the same where it was evaluated as many times
  as are kept, or fewer. An iteration in between is not kept, and nor are the
  values of a form evaluated more times than are kept in it, where the ones
  not kept may be among them.")

(defn- number [entry] (nth entry 0))

(defn- not-kept
  "The numbers of the values of KEPT not kept being between them, [after
  before] - or nil where none was not kept."
  [{:keys [times head tail]}]
  (let [upto (number (peek head))
        both (count (filter #(<= (number %) upto) tail))]
    (when (< (- (+ (count head) (count tail)) both) times)
      [upto (number (nth tail 0))])))

(defn- may-miss?
  "Whether a value of KEPT not kept may be numbered FROM or after, before TO -
  nil, for no end."
  [kept from to]
  (when-let [[after before] (not-kept kept)]
    (< (max from (inc after)) (if to (min to before) before))))

(defn- entries
  "The values of KEPT, numbered, in order, each once."
  [{:keys [head tail]}]
  (let [upto (number (peek head))]
    (into head (drop-while #(<= (number %) upto)) tail)))

(defn- nth-number
  "The number of the Kth value of KEPT, from 1 - or nil where it is not kept."
  [{:keys [times head tail]} k]
  (cond
    (<= 1 k (count head)) (number (nth head (dec k)))
    (< (- times (count tail)) k (inc times)) (number (nth tail (- k (- times (count tail)) 1)))))

(defn kept-iterations
  "Which iterations of a loop whose name was bound TIMES times are kept, where
  as many as KEPT values of a form are kept first and as many last: [first
  last] ranges of them, from 1 - an iteration being kept where its start is,
  and its end, the next one's start."
  [times kept]
  (if (<= times (* 2 kept))
    (if (pos? times) [[1 times]] [])
    [[1 (dec kept)] [(inc (- times kept)) times]]))

(defn- first-bound
  "Of the sites of the names a loop binds each time round, SITES, the one
  bound first each time round - the one whose first value is numbered lowest
  - or nil where none is kept. Not always the first written: a map pattern
  binds its :as before its keys."
  [kept-of sites]
  (some->> (seq (keep (fn [site]
                        (when-let [entry (first (:head (kept-of site)))]
                          [(number entry) site]))
                      sites))
           (apply min-key first)
           second))

(defn ranges
  "The numbers of the values of the iteration INDEX, from 1, of the loop
  whose name was bound at the site LOOP, from the first one to the one just
  after the last - or nil, for the last iteration of a loop in nothing - and
  of the iteration of each loop of ENCLOSING it is in, the sites their names
  were bound at: {site [from to]}. KEPT-OF answers what is kept of a site;
  STARTS, a map or [site sites] pairs, the sites of all the names each loop
  binds each time round, of which the one bound first starts an iteration.

  Nil where that is not kept."
  [kept-of {:keys [loop index enclosing starts]}]
  (let [starts (into {} starts)
        kept-of-loop (fn [site] (kept-of (or (first-bound kept-of (get starts site)) site)))
        kept (kept-of-loop loop)
        from (when kept (nth-number kept index))
        last? (and kept (= index (:times kept)))
        to (when (and kept (not last?)) (nth-number kept (inc index)))
        around (into {}
                     (map (fn [site]
                            (let [kept (kept-of-loop site)
                                  es (when kept (entries kept))
                                  start (some-> (last (take-while #(<= (number %) from) es)) number)
                                  end (some-> (first (drop-while #(<= (number %) from) es)) number)]
                              [site (when (and start (not (may-miss? kept (inc start) end)))
                                      [start end])])))
                     (when from enclosing))]
    (when (and from (or to last?) (every? some? (vals around)))
      (let [ends (keep second (cons [from to] (vals around)))]
        (assoc around loop [from (when (seq ends) (apply min ends))])))))

(defn value-in
  "What is kept of KEPT between the numbers FROM and TO, TO nil for no end:
  {:times :value}, the value the last one, or nil where there is none - or
  ::not-kept where one may not be kept. Nil for KEPT nil, nothing kept."
  [kept [from to]]
  (cond
    (nil? kept) nil
    (may-miss? kept from to) ::not-kept
    :else
    (let [in (filterv #(and (<= from (number %)) (or (nil? to) (< (number %) to)))
                      (entries kept))]
      (when (seq in)
        {:times (count in) :value (nth (peek in) 1)}))))

(defn picker
  "A fn answering, of a site, what it evaluated to in the iteration ITERATION
  says - {:loop :index :enclosing :owners :starts}, the site of a name bound
  each time round of the loop, the index of the iteration, the sites of the
  loops it is in, of each site evaluated again each time round of one of
  these, the innermost one's: a map, or [site owner] pairs, and the :starts
  `ranges' takes. Of a call of which
  KEPT-OF says what is kept: {:times :value}, or nil where the site was not
  evaluated then, or ::not-kept - or DEFAULT where it is not evaluated again
  each time round.

  Nil where the iteration is not kept."
  [kept-of {:keys [owners] :as iteration}]
  (when-let [ranges (ranges kept-of iteration)]
    (let [owners (into {} owners)]
      (fn [site default]
        (if-let [owner (get owners site)]
          (value-in (kept-of site) (get ranges owner))
          default)))))

(defn kept-indices
  "The iterations kept of a loop whose name is bound at the site LOOP, of
  which KEPT-OF says what is kept, as many values of a form as KEPT being
  kept first and as many last: their indices, from 1."
  [kept-of loop kept]
  (vec (for [[from to] (kept-iterations (:times (kept-of loop) 0) kept)
             i (range from (inc to))]
         i)))

(defn table
  "What the iterations kept of a loop of a call evaluated to, as rows - a map
  each, of :iteration, the index, and of each name of COLUMNS, [name site]
  pairs, what its site evaluated to then, where it was evaluated then and
  that is kept. ITERATION says the loop, as `picker' takes it, without an
  :index; KEPT-OF what is kept of the call, as many values of a form as KEPT
  being kept first and as many last."
  [kept-of iteration columns kept]
  (vec (for [index (kept-indices kept-of (:loop iteration) kept)
             :let [pick (picker kept-of (assoc iteration :index index))]
             :when pick]
         (apply array-map :iteration index
                (mapcat (fn [[name site]]
                          (let [v (pick site nil)]
                            (when (map? v) [name (:value v)])))
                        columns)))))

(defn first-where
  "Of INDICES, the first where (F index) is true - {:index i}, or {:index nil}
  - with the message of the first exception F threw, :error, where it threw
  one and was true for none."
  [f indices]
  (loop [[i & more] indices
         error nil]
    (if (nil? i)
      (cond-> {:index nil} error (assoc :error error))
      (let [[found thrown] (try [(f i) nil]
                                (catch #?(:clj Throwable :cljs :default) e
                                  [nil (or (ex-message e) (str e))]))]
        (if found
          {:index i}
          (recur more (or error thrown)))))))

(defn by-iteration
  "Of ROWS of a `table' of a single column, what it is in each row where it
  is there: {index value}, by index."
  [rows]
  (into (sorted-map)
        (keep (fn [row] (when-let [[_ v] (first (dissoc row :iteration))]
                          [(:iteration row) v])))
        rows))
