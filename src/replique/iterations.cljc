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

(defn ranges
  "The numbers of the values of the iteration INDEX, from 1, of the loop
  whose name was bound at the site LOOP, from the first one to the one just
  after the last - or nil, for the last iteration of a loop in nothing - and
  of the iteration of each loop of ENCLOSING it is in, the sites their names
  were bound at: {site [from to]}. KEPT-OF answers what is kept of a site.

  Nil where that is not kept."
  [kept-of loop index enclosing]
  (let [kept (kept-of loop)
        from (when kept (nth-number kept index))
        last? (and kept (= index (:times kept)))
        to (when (and kept (not last?)) (nth-number kept (inc index)))
        around (into {}
                     (map (fn [site]
                            (let [kept (kept-of site)
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
  ::not-kept where one may not be kept."
  [kept [from to]]
  (if (may-miss? kept from to)
    ::not-kept
    (let [in (filterv #(and (<= from (number %)) (or (nil? to) (< (number %) to)))
                      (entries kept))]
      (when (seq in)
        {:times (count in) :value (nth (peek in) 1)}))))
