(ns replique.debug
  "The runtime's half of recording ClojureScript - see replique/debug.clj,
  which is the half the editor talks to, and \"Recording\" there for what
  recording is.

  HERE BECAUSE THE VALUES ARE HERE, as a view's are in replique/inspect.cljs:
  what a form evaluated to is an object of the page or of the node process,
  and what crosses the wire is how it is printed, when a client asks. The JVM
  keeps what it knows from compiling - which form each site is, and where -
  and this keeps the calls and the values.

  Code compiled to be recorded (clojure.cljs.emitter/*instrument*) holds the
  `unit' of what was evaluated, and calls it:

    enter(n, outer, args)
                        on entering a fn, called with ARGS, answering the
                        frame of the call it is in - OUTER, the frame of the fn
                        it was made in, where it was made in one
    value(n, v, frame)  once the form numbered N has its value, answering V
    exit(frame)         once the call is over, returned from or thrown out of

  WHICH CALL A VALUE IS IN IS KNOWN FROM THE CODE, rather than found out from
  a stack: a frame is handed down lexically, so a fn made in a call and called
  once it is over - a callback, the rest of an async fn - records in the call
  that made it, and nothing has to tell a call that threw from one still
  running. A call starts on entering an outermost fn of a definition. A form
  in no fn, or in a fn made outside any call, is the last call's - or the
  unit's first, its evaluation, where there is none yet.

  Asked through `op', with what the JVM wants as JSON and what it gets back as
  EDN, as replique.inspect is. That a unit recorded is said with the
  runtime's notify, once every `tell-every' ms at most.")

(def ^:private calls-kept
  "How many calls of a unit are kept, the last ones."
  10)

(def ^:private values-kept
  "How many values of a form are kept in a call, the last ones."
  10)

(def ^:private tell-every
  "How often the JVM is told that a unit recorded, at most, in ms."
  200)

(defonce ^:private units
  ;; unit number -> Unit
  (atom {}))

(defonce ^:private site-units
  ;; site number -> the number of the unit it recorded in
  (js/Map.))

(defonce ^:private call-count (atom 0))

(defonce ^:private entries
  ;; the numbers of the sites that are a fn's entry, whose value is what it
  ;; was called with
  (js/Set.))

;;; Saying that a unit recorded

(defonce ^:private telling
  ;; the units the JVM is about to be told recorded
  (atom #{}))

(declare unit-calls)

(defn- notify! [content]
  (when-let [notify (.-notify (.-$CLJS js/globalThis))]
    (notify (pr-str content))))

(defn- changed!
  "Tell the JVM that the unit N recorded - once every `tell-every' ms at most:
  a function called in a loop must not cost a message a call."
  [n]
  (when-not (contains? @telling n)
    (swap! telling conj n)
    (js/setTimeout (fn []
                     (swap! telling disj n)
                     (when-let [calls (some-> (get @units n) unit-calls)]
                       (when (pos? (alength calls))
                         (notify! {:recorded [{:unit n
                                               :call (.-n (aget calls (dec (alength calls))))
                                               :calls (alength calls)}]}))))
                   tell-every)))

;;; Calls

(deftype Call [n at values])

(deftype Frame [of own])

(defn- new-call
  "A call numbered from now on, of the unit whose calls kept are CALLS."
  [calls]
  (let [c (Call. (swap! call-count inc) (js/Date.now) (js/Map.))]
    (.push calls c)
    (when (> (alength calls) calls-kept) (.shift calls))
    c))

(defn- keep-value!
  "Keep V as what the site SITE evaluated to in the call C."
  [c site v]
  (let [m (.-values c)
        e (or (.get m site)
              (let [e #js {:times 0 :values #js []}]
                (.set m site e)
                e))
        vs (.-values e)]
    (set! (.-times e) (inc (.-times e)))
    (.push vs v)
    (when (> (alength vs) values-kept) (.shift vs))))

(deftype Unit [n definition ^:mutable gone calls]
  Object
  (enter [this site outer args]
    (let [frame (cond
                  (some? outer) (Frame. (.-of outer) false)
                  gone (Frame. nil false)
                  definition (let [c (new-call calls)]
                               (changed! n)
                               (Frame. c true))
                  :else (Frame. (.latest this) false))]
      ;; what it was called with is what its entry evaluated to
      (when (and (not gone) (some? (.-of frame)) args (pos? (alength args)))
        (keep-value! (.-of frame) site (vec args))
        (.add entries site)
        (.set site-units site n))
      frame))
  (value [this site v frame]
    (when-not gone
      (when-let [c (if (some? frame) (.-of frame) (.latest this))]
        (keep-value! c site v)
        (.set site-units site n)
        (changed! n)))
    v)
  (exit [this frame]
    (when (and (.-own frame) (not gone))
      (changed! n)))
  (latest [this]
    ;; the call a value outside of any is in: the last one, or a first
    (if (pos? (alength calls))
      (aget calls (dec (alength calls)))
      (new-call calls))))

(defn- unit-calls [^Unit u] (.-calls u))

(defn unit
  "The unit numbered N, which code compiled to be recorded calls - see this
  namespace's docstring. Calls of it start on entering its outermost fns where
  it is a DEFINITION, and its forms are recorded in one call, its evaluation,
  where it is not."
  [n definition]
  (or (get @units n)
      (let [u (Unit. n (boolean definition) false #js [])]
        (swap! units assoc n u)
        u)))

(defn- forget! [n]
  (when-let [u (get @units n)]
    (set! (.-gone u) true)
    (swap! units dissoc n)
    (doseq [site (vec (es6-iterator-seq (.keys site-units)))
            :when (= n (.get site-units site))]
      (.delete site-units site))
    true))

;;; Printing

(deftype Unrealized [what]
  IPrintWithWriter
  (-pr-writer [_ w _]
    (-write w (str "#<unrealized" (when what (str " " what)) ">"))))

(defn- unrealized-seq? [x]
  (and (seq? x) (satisfies? IPending x) (not (realized? x))))

(defn- unforced
  "X as it is printed without realizing what of its lazy seqs is not realized
  yet: that part is left out, and said to be. Down to LEVEL, and as many
  things of a collection as LENGTH, and one more for printing to say there
  are more - see the function of the same name in replique/debug.clj."
  [x level length]
  (letfn [(some-of [coll] (take (inc length) coll))
          (inner [x] (unforced x (dec level) length))]
    (cond
      (unrealized-seq? x) (Unrealized. "LazySeq")
      (zero? level) x
      (seq? x) (loop [s x, n 0, acc []]
                 (cond
                   (unrealized-seq? s) (apply list (conj acc (Unrealized. nil)))
                   (> n length) (apply list acc)
                   :else (if-let [s (seq s)]
                           (recur (rest s) (inc n) (conj acc (inner (first s))))
                           (apply list acc))))
      (map-entry? x) (MapEntry. (inner (key x)) (inner (val x)) nil)
      (record? x) (reduce-kv (fn [m k v] (assoc m k (inner v))) x x)
      (map? x) (into (empty x) (map (fn [[k v]] [k (inner v)])) (some-of x))
      (or (vector? x) (set? x)) (into (empty x) (map inner) (some-of x))
      :else x)))

(defn printed-briefly
  "X printed to be shown after the form it is the value of: its beginning,
  which is what fits there - the rest is to browse."
  [x]
  (let [length 10, level 3, up-to 1000
        s (try (binding [*print-length* length *print-level* level]
                 (pr-str (unforced x level length)))
               (catch :default e
                 (str "#<unprintable " (or (ex-message e) (str e)) ">")))]
    (if (> (count s) up-to) (str (subs s 0 up-to) "...") s)))

;;; What a client sees

(defn- the-unit [n]
  (or (get @units n)
      (throw (ex-info (str "Unit " n " recorded nothing in this runtime")
                      {:kind "unknown-unit"}))))

(defn- last-of [a] (aget a (dec (alength a))))

(defn- recorded
  "What the unit N recorded: the calls of it kept, and what each form
  evaluated to in the call CALL - the last call, without it - printed: its
  last value in the call, and how many times it was evaluated in it."
  [n call]
  (let [kept (vec (unit-calls (the-unit n)))
        c (if (some? call)
            (or (some #(when (= call (.-n %)) %) kept)
                (throw (ex-info (str "Call " call " is not kept any more")
                                {:kind "unknown-call"})))
            (peek kept))]
    (cond-> {:calls (mapv (fn [c] {:call (.-n c) :at (.-at c)}) kept)
             :values (if c
                       (vec (for [site (sort (es6-iterator-seq (.keys (.-values c))))
                                  :let [e (.get (.-values c) site)
                                        v (last-of (.-values e))]]
                              (cond-> {:site site :times (.-times e)
                                       :value (printed-briefly v)}
                                (and (.has entries site) (vector? v))
                                (assoc :args (mapv printed-briefly v)))))
                       [])}
      c (assoc :call (.-n c)))))

(defn recorded-value
  "What a view of what the site SITE evaluated to in the call CALL shows: the
  last value, where it was evaluated more than once - or, without CALL, the
  last value of each call kept, one per call, the last call last. And
  :not-recorded where nothing is kept."
  [site call]
  (let [calls (some-> (get @units (.get site-units site)) unit-calls vec)
        last-in (fn [c] (some-> (.get (.-values c) site) .-values last-of))
        in? (fn [c] (some? (.get (.-values c) site)))]
    (if (some? call)
      (if-let [c (some #(when (and (= call (.-n %)) (in? %)) %) calls)]
        (last-in c)
        :not-recorded)
      (let [cs (filter in? calls)]
        (if (seq cs) (mapv last-in cs) :not-recorded)))))

(defn- handle [{:keys [op unit call] :as msg}]
  (case op
    "recorded" (recorded unit call)
    "forget" {:forgotten (vec (filter forget! (if (contains? msg :unit)
                                                [unit]
                                                (keys @units))))}
    (throw (ex-info (str "No op " (pr-str op)) {:kind "invalid-message"}))))

(defn op
  "Answer MSG, a JSON object as a string, with EDN as a string - and what goes
  wrong as {:error :kind} rather than thrown, as replique.inspect/op does."
  [msg]
  (pr-str
   (try
     (handle (js->clj (js/JSON.parse msg) :keywordize-keys true))
     (catch :default e
       {:error (or (ex-message e) (str e))
        :kind (or (:kind (ex-data e)) "exception")}))))
