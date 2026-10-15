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

  WHICH CALL MADE A CALL is the one running when it started - entered and not
  exited yet, of any unit - which is its :parent, as on the JVM: see \"Going
  through what was recorded\" in replique/debug.clj. Which calls are kept is
  as there too: see \"Recording\".

  Asked through `op', with what the JVM wants as JSON and what it gets back as
  EDN, as replique.inspect is. That a unit recorded is said with the
  runtime's notify, once every `tell-every' ms at most."
  (:require [replique.iterations :as iterations]))

(def ^:private calls-kept
  "How many calls of a unit made in no other call kept are kept, the last
  ones."
  10)

(def ^:private calls-kept-in-all
  "How many calls are kept at most, of all units - the last ones, and the
  ones running."
  10000)

(def ^:private values-kept
  "How many values of a form are kept in a call, the first ones, and as many
  of the last ones."
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

(defonce ^:private by-number
  ;; call number -> Call, of the calls kept
  (js/Map.))

(defonce ^:private running
  ;; the calls entered and not exited yet, the innermost last
  #js [])

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

(deftype Call [n unit at values ^:mutable counted parent children ^:mutable until])

(deftype Frame [of own])

(defn- remove-from! [a x]
  (let [i (.indexOf a x)]
    (when-not (neg? i) (.splice a i 1))))

(defn- forget-call!
  "Let go of the call C: its unit has it no more among its calls, and the
  call that made it among its children."
  [c]
  (.delete by-number (.-n c))
  (remove-from! running c)
  (some-> (get @units (.-unit c)) unit-calls (remove-from! c))
  (when-let [[parent] (.-parent c)]
    (when-let [p (.get by-number parent)]
      (let [cs (.-children p)]
        (loop [i (dec (alength cs))]
          (when-not (neg? i)
            (when (= (.-n c) (aget (aget cs i) 1)) (.splice cs i 1))
            (recur (dec i))))))))

(defn- forget-made!
  "Let go of the calls CS, and of the calls they made and so on."
  [cs]
  (let [todo (into-array cs)]
    (while (pos? (alength todo))
      (let [c (.pop todo)]
        (when (.has by-number (.-n c))
          (doseq [pair (vec (.-children c))]
            (when-let [made (.get by-number (aget pair 1))]
              (.push todo made)))
          (forget-call! c))))))

(defn- made-for
  "Of the calls the call C made, the ones made for its value numbered I: the
  next one it had once they were over."
  [c i]
  (into [] (comp (map #(.get by-number (aget % 1)))
                 (filter #(and (some? %) (= (dec i) (.-until %)))))
        (.-children c)))

(defn- new-call
  "A call numbered from now on, of the unit N whose calls kept are CALLS -
  made by the call running, where there is one."
  [n calls]
  (let [p (when (pos? (alength running)) (aget running (dec (alength running))))
        c (Call. (swap! call-count inc) n (js/Date.now) (js/Map.) 0
                 (when p [(.-n p) (.-counted p)]) #js [] nil)]
    (when p (.push (.-children p) #js [(.-counted p) (.-n c)]))
    (.set by-number (.-n c) c)
    (.push calls c)
    ;; of the calls made in none kept, the last ones
    (forget-made! (drop-last calls-kept (remove #(.has by-number (first (.-parent %))) calls)))
    (let [over (- (.-size by-number) calls-kept-in-all)]
      (when (pos? over)
        (forget-made! (into [] (comp (remove #(.includes running %)) (take over))
                            (es6-iterator-seq (.values by-number))))))
    c))

(defn- ended!
  "The call C is over: it was made for the next value of the call that made
  it - of the ones made for the same one, the first ones and the last ones
  are kept."
  [c]
  (when-let [p (some->> (.-parent c) first (.get by-number))]
    (when (nil? (.-until c))
      (set! (.-until c) (.-counted p))
      (let [same (made-for p (inc (.-until c)))]
        (forget-made! (take (- (count same) (* 2 values-kept)) (drop values-kept same)))))))

(defn- keep-value!
  "Keep V as what the site SITE evaluated to in the call C - numbered in it,
  see replique.iterations: the last values, numbered :numbers, and the first
  ones, :head, [number value]s."
  [c site v]
  (let [m (.-values c)
        e (or (.get m site)
              (let [e #js {:times 0 :values #js [] :numbers #js [] :head #js []}]
                (.set m site e)
                e))
        i (inc (.-counted c))
        vs (.-values e)
        numbers (.-numbers e)]
    (set! (.-counted c) i)
    (set! (.-times e) (inc (.-times e)))
    (.push vs v)
    (.push numbers i)
    (when (> (alength vs) values-kept)
      (.shift vs)
      ;; the value that is not kept any more, and the calls made for it
      (let [out (.shift numbers)]
        (when-not (some #(= out (aget % 0)) (.-head e))
          (forget-made! (made-for c out)))))
    (when (< (alength (.-head e)) values-kept)
      (.push (.-head e) #js [i v]))))

(deftype Unit [n definition ^:mutable gone calls]
  Object
  (enter [this site outer args]
    (let [frame (cond
                  (some? outer) (Frame. (.-of outer) false)
                  gone (Frame. nil false)
                  definition (let [c (new-call n calls)]
                               (.push running c)
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
    (when (.-own frame)
      (remove-from! running (.-of frame))
      (ended! (.-of frame))
      (when-not gone
        (changed! n))))
  (latest [this]
    ;; the call a value outside of any is in: the last one, or a first
    (if (pos? (alength calls))
      (aget calls (dec (alength calls)))
      (new-call n calls))))

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

;;; Recording calls only

;; The calls of a function whose code is not recorded - a library's - are
;; recorded by its var: a property of its namespace's object, which every
;; call reads at call time, made one whose value is a function that records
;; the call and calls the one it held - and that, set again, as a `def'
;; evaluated again sets it, wraps what it is set to. Of a call made in a call
;; recorded only: see "Recording calls only" in replique/debug.clj.

(defonce ^:private wrapped
  ;; unit number -> #js {:name :original :wrapper}
  (js/Map.))

(defn- function?
  "Whether X is a function of JavaScript's - which a multimethod, a keyword or
  a map, though they are called as one, is not."
  [x]
  (identical? "function" (js* "typeof ~{}" x)))

(defn- namespace-object [named]
  (let [i (.indexOf named "/")]
    (when (pos? i)
      [(.get (.-namespaces (.-$CLJS js/globalThis)) (subs named 0 i))
       ((.-munge (.-$CLJS js/globalThis)) (subs named (inc i)))])))

(defn- calls-recorder
  "What the var of the unit U holds while its calls are recorded, F being
  what it held: its sites are ENTRY, RETURNED and THREW."
  [u entry returned threw f]
  (fn [& args]
    (if (zero? (alength running))
      (.apply f nil (into-array args))
      (let [frame (.enter u entry nil (into-array args))]
        (try
          (.value u returned (.apply f nil (into-array args)) frame)
          (catch :default e
            (.value u threw e frame)
            (throw e))
          (finally (.exit u frame)))))))

(defn- wrap!
  "Record the calls of the var named :name, as the unit :unit whose sites are
  :enter, :return and :threw. Answers why it cannot be, or nil."
  [{:keys [name enter return threw] n :unit}]
  (let [[o prop] (namespace-object name)
        f (when o (unchecked-get o prop))]
    (cond
      (nil? o) "no such namespace in this runtime"
      (not (js-in prop o)) "no such var in this runtime"
      (not (function? f)) "not a function"
      :else
      (let [u (unit n true)
            wrap #(calls-recorder u enter return threw %)
            state #js {:name name :original f :wrapper (wrap f)}]
        (.set wrapped n state)
        (js/Object.defineProperty
         o prop
         #js {:configurable true
              :enumerable true
              :get (fn [] (.-wrapper state))
              ;; defined again: what it is now is recorded
              :set (fn [g]
                     (set! (.-original state) g)
                     (set! (.-wrapper state) (if (function? g) (wrap g) g)))})
        nil))))

(defn- unwrap! [n]
  (when-let [state (.get wrapped n)]
    (let [[o prop] (namespace-object (.-name state))]
      (when o
        (js/Object.defineProperty o prop #js {:configurable true :enumerable true
                                              :writable true :value (.-original state)})))
    (.delete wrapped n)))

(defn- forget! [n]
  (unwrap! n)
  (when-let [u (get @units n)]
    (set! (.-gone u) true)
    (swap! units dissoc n)
    (forget-made! (vec (.-calls u)))
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

(defn- kept-in
  "What is kept of each site of the call C, as replique.iterations takes it:
  a fn of the site, nil where nothing is."
  [c]
  (fn [site]
    (when-let [e (and c (.get (.-values c) site))]
      {:times (.-times e)
       :head (mapv vec (.-head e))
       :tail (mapv vector (.-numbers e) (.-values e))})))

(defn- in-iteration
  "A fn answering, of a site, what it evaluated to in the iteration ITERATION
  of the call C - see `iterating' in replique/debug.clj, which says it, its
  :owners as pairs - or DEFAULT, where it is not evaluated again each time
  round."
  [iteration c]
  (or (iterations/picker (kept-in c) iteration)
      (throw (ex-info (str "Iteration " (:index iteration) " is not kept")
                      {:kind "unknown-iteration"}))))

(defn- recorded
  "What the unit N recorded: the calls of it kept, and what each form
  evaluated to in the call CALL - the last call, without it - printed: its
  last value in the call, and how many times it was evaluated in it, :times
  also saying that of each site. Or, where ITERATION says one, in that
  iteration of a loop of the call: see `in-iteration'."
  [n call iteration]
  (let [kept (vec (unit-calls (the-unit n)))
        c (if (some? call)
            (or (some #(when (= call (.-n %)) %) kept)
                (throw (ex-info (str "Call " call " is not kept any more")
                                {:kind "unknown-call"})))
            (peek kept))
        pick (if iteration
               (in-iteration iteration c)
               (fn [_ default] default))]
    (cond-> {:calls (mapv (fn [c] {:call (.-n c) :at (.-at c)}) kept)
             :times (if c
                      (into {} (map (fn [[site e]] [site (.-times e)]))
                            (es6-iterator-seq (.entries (.-values c))))
                      {})
             :values (if c
                       (vec (for [site (sort (es6-iterator-seq (.keys (.-values c))))
                                  :let [e (.get (.-values c) site)
                                        picked (pick site {:times (.-times e)
                                                           :value (last-of (.-values e))})
                                        v (:value picked)]
                                  :when picked]
                              (if (= ::iterations/not-kept picked)
                                {:site site :not-kept true}
                                (cond-> {:site site :times (:times picked)
                                         :value (printed-briefly v)}
                                  (and (.has entries site) (vector? v))
                                  (assoc :args (mapv printed-briefly v))))))
                       [])}
      c (assoc :call (.-n c)))))

(defn- iteration-in
  "What the site SITE evaluated to in the iteration ITERATION of the call C -
  see `in-iteration' - or DEFAULT, nil or {:value v}, where it is not
  evaluated again each time round: the value, or :not-recorded."
  [c site iteration default]
  (let [v ((in-iteration iteration c) site default)]
    (if (map? v) (:value v) :not-recorded)))

(defn recorded-value
  "What a view of what the site SITE evaluated to in the call CALL shows: the
  last value, where it was evaluated more than once - or, without CALL, the
  last value of each call kept it was evaluated in, by the number of the call
  among the ones kept, from 1, as a loop's are by iteration: an outline, see
  replique.iterations/by-call. And :not-recorded where nothing is kept.

  With ITERATION, what it evaluated to last in that iteration of a loop of
  the call CALL, or of the last call - see `in-iteration'."
  ([site call] (recorded-value site call nil))
  ([site call iteration]
   (let [calls (some-> (get @units (.get site-units site)) unit-calls vec)
         last-in (fn [c] (some-> (.get (.-values c) site) .-values last-of))
         in? (fn [c] (some? (.get (.-values c) site)))]
     (cond
       (some? iteration)
       (if-let [c (if (some? call) (some #(when (= call (.-n %)) %) calls) (peek calls))]
         (iteration-in c site iteration (when (in? c) {:value (last-in c)}))
         :not-recorded)

       (some? call)
       (if-let [c (some #(when (and (= call (.-n %)) (in? %)) %) calls)]
         (last-in c)
         :not-recorded)

       :else
       (let [values (into []
                          (keep-indexed (fn [i c] (when (in? c) [(inc i) (last-in c)])))
                          calls)]
         (if (seq values) (iterations/by-call values) :not-recorded))))))

(defn recorded-in
  "What the site SITE evaluated to last in the iteration ITERATION of a loop
  of the call CALL of the unit UNIT, see `in-iteration' - which code evaluated
  against a call binds its locals to, see \"Code evaluated against a recorded
  call\" in replique/debug.clj: :replique/not-recorded where it was not
  evaluated then."
  [unit call site iteration]
  (let [c (some #(when (= call (.-n %)) %) (some-> (get @units unit) unit-calls vec))]
    (when-not c
      (throw (ex-info (str "Call " call " is not kept any more") {:kind "unknown-call"})))
    (let [kept (.get (.-values c) site)
          v (iteration-in c site iteration (when kept {:value (some-> kept .-values last-of)}))]
      (if (= :not-recorded v) :replique/not-recorded v))))

(defn- call-said
  "What is kept of the call CALL - the last one of the unit UNIT, CALL nil -
  as replique/debug.clj takes it: what it was made by and made, the calls of
  its unit kept, and of each site, how many times it was evaluated, and the
  numbers of its values kept, first and last - not the values, which stay
  here."
  [unit call]
  (let [c (if (some? call)
            (.get by-number call)
            (some-> (get @units unit) unit-calls vec peek))]
    (when-not c
      (throw (ex-info (if (some? call)
                        (str "Call " call " is not kept any more")
                        (str "Unit " unit " was not called yet"))
                      {:kind "unknown-call"})))
    {:call (.-n c)
     :unit (.-unit c)
     :at (.-at c)
     :parent (.-parent c)
     :children (mapv vec (.-children c))
     :until (.-until c)
     :calls (mapv #(.-n %) (some-> (get @units (.-unit c)) unit-calls))
     :sites (into {} (map (fn [[site e]]
                            [site {:times (.-times e)
                                   :head (mapv #(aget % 0) (.-head e))
                                   :tail (vec (.-numbers e))}]))
                  (es6-iterator-seq (.entries (.-values c))))}))

(defn plan-value
  "What a cursor going through what was recorded is at, as PLAN says it - see
  replique.iterations/at-outline: of the call numbered :call."
  [{:keys [call] :as plan}]
  (if-let [c (.get by-number call)]
    (let [kept-of (kept-in c)]
      (iterations/at-outline (fn [site n]
                               (if-let [v (iterations/numbered (kept-of site) n)]
                                 (:value v)
                                 ::iterations/not-kept))
                             plan))
    :not-recorded))

(defn loop-table
  "What each iteration kept of a loop of the call CALL - the last one,
  without - evaluated to, by iteration - see replique.iterations/table. TABLE
  says the loop: its :loop, the site of its name, its :iteration, as
  `in-iteration' takes it without an :index, the names bound in it,
  :columns, and how many values of a form are kept, :kept - and :only, for
  the values of a single column, by iteration."
  [call {:keys [loop iteration columns kept only]}]
  (let [calls (some-> (get @units (.get site-units loop)) unit-calls vec)
        c (if (some? call) (some #(when (= call (.-n %)) %) calls) (peek calls))]
    (if c
      (cond-> (iterations/table (kept-in c) iteration
                                (mapv (fn [[name & more]] (into [(symbol name)] more)) columns) kept)
        only iterations/by-iteration)
      :not-recorded)))

(defn calls-table
  "What each call kept of the unit UNIT evaluated to, by call - see
  replique.iterations/calls-table, COLUMNS being said with names as strings."
  [unit columns]
  (iterations/calls-table (mapv kept-in (some-> (get @units unit) unit-calls vec))
                          (mapv (fn [[name & more]] (into [(symbol name)] more)) columns)))

(defn first-where
  "Of INDICES, the first iteration where (F index) is true - see
  replique.iterations/first-where."
  [f indices]
  (iterations/first-where f indices))

(defn- calls-said
  "The calls kept made in none kept - or, with PARENT, the ones the call
  PARENT made - as replique/debug.clj takes them: what each was made by and
  made, the calls of its unit kept, and the last value of each of the sites
  SITES it has a value of, printed - {:number :value}, and the :args, of an
  entry."
  [{:keys [parent sites]}]
  (let [cs (if (some? parent)
             (keep (fn [pair] (.get by-number (aget pair 1)))
                   (some-> (.get by-number parent) .-children))
             (filter (fn [c] (not (.has by-number (first (.-parent c)))))
                     (es6-iterator-seq (.values by-number))))]
    {:calls (mapv (fn [c]
                    {:call (.-n c)
                     :unit (.-unit c)
                     :calls (mapv #(.-n %) (some-> (get @units (.-unit c)) unit-calls))
                     :at (.-at c)
                     :parent (first (.-parent c))
                     :made (alength (.-children c))
                     :last (into {} (keep (fn [site]
                                            (when-let [e (.get (.-values c) site)]
                                              (let [v (last-of (.-values e))]
                                                [site (cond-> {:number (last-of (.-numbers e))
                                                               :value (if (instance? js/Error v)
                                                                        ;; what a call threw
                                                                        (str (.-name v) ": " (.-message v))
                                                                        (printed-briefly v))}
                                                        (and (.has entries site) (vector? v))
                                                        (assoc :args (mapv printed-briefly v)))]))))
                                 sites)})
                  cs)}))

(defn- handle [{:keys [op unit call] :as msg}]
  (case op
    "recorded" (recorded unit call (:iteration msg))
    "call" (call-said unit call)
    "calls" (calls-said msg)
    "wrap" {:skipped (vec (keep (fn [w] (when-let [why (wrap! w)] {:name (:name w) :why why}))
                                (:wraps msg)))}
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
