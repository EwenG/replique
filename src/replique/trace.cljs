(ns replique.trace
  "The runtime's half of tracing ClojureScript - see replique/trace.clj, which
  is the half the editor talks to, and says what tracing is.

  A var is a property of its namespace's object, which every call reads at
  call time: traced, the property is one whose value is a function that says
  it was called and calls the one it held - and that, set again, as a `def'
  evaluated again sets it, traces what it is set to. Nothing is compiled
  again.

  A CALL IS IN THE CALL THAT MADE IT, the traced one whose code is running:
  a stack, which a call is on from its entry to its return. A call answering
  a promise is over when the promise is, and what is run once the promise it
  awaited is - the rest of an async fn, a callback - runs with no traced call
  under it: such a call is in none, and is said to be one of the calls made in
  none.

  Asked through `op', with what the JVM wants as JSON and what it gets back as
  EDN, as replique.inspect is. That traced functions were called is said with
  the runtime's notify, once every `tell-every' ms at most."
  (:require [replique.debug :as debug]))

(def ^:private calls-kept
  "How many calls are kept, the first ones: the others are counted."
  5000)

(def ^:private tell-every
  "How often the JVM is told that traced functions were called, at most, in ms."
  200)

(defonce ^:private traced
  ;; "ns/name" -> #js {:original :wrapper}
  (js/Map.))

(defonce ^:private calls
  ;; call number -> {:var :at :args :parent :made :state, and once over :ms and
  ;; :value or :thrown}
  (atom (sorted-map)))

(defonce ^:private call-count (atom 0))

(defonce ^:private dropped (atom 0))

(defonce ^:private running
  ;; the numbers of the traced calls whose code is running, the innermost last
  #js [])

;;; Telling the JVM

(defonce ^:private telling (atom false))

(defn- notify! [content]
  (when-let [notify (.-notify (.-$CLJS js/globalThis))]
    (notify (pr-str content))))

(defn- changed! []
  (when-not @telling
    (reset! telling true)
    (js/setTimeout (fn []
                     (reset! telling false)
                     (notify! {:trace-called {:calls (count @calls) :dropped @dropped}}))
                   tell-every)))

;;; A call

(defn- now-ms []
  (if (exists? js/performance) (.now js/performance) (js/Date.now)))

(defn- started! [named args]
  (let [parent (when (pos? (alength running)) (aget running (dec (alength running))))]
    (if (>= (count @calls) calls-kept)
      (do (swap! dropped inc) (changed!) nil)
      (let [c (swap! call-count inc)]
        (swap! calls (fn [m]
                       (cond-> (assoc m c (cond-> {:var named :at (js/Date.now)
                                                   :args (vec args) :made 0 :state :running}
                                            (some? parent) (assoc :parent parent)))
                         (contains? m parent) (update-in [parent :made] inc))))
        (changed!)
        c))))

(defn- ended! [c t0 state value]
  (let [ms (- (now-ms) t0)]
    (swap! calls (fn [m]
                   (if (contains? m c)
                     (update m c assoc :state state :ms ms
                             (if (= :threw state) :thrown :value) value)
                     m))))
  (changed!))

(defn- wrapped
  "What the var named NAMED holds while it is traced, F being what it held.

  A promise answered is answered as another one, which says when it is
  settled: a handler on the one F answered would have it handled whatever
  the code does with it."
  [named f]
  (fn [& args]
    (if-let [c (started! named args)]
      (let [t0 (now-ms)]
        (.push running c)
        (try
          (let [r (.apply f nil (into-array args))]
            (if (instance? js/Promise r)
              (do (swap! calls (fn [m] (cond-> m (contains? m c) (assoc-in [c :state] :pending))))
                  (.then r
                         (fn [v] (ended! c t0 :returned v) v)
                         (fn [e] (ended! c t0 :threw e) (throw e))))
              (do (ended! c t0 :returned r) r)))
          (catch :default e
            (ended! c t0 :threw e)
            (throw e))
          (finally
            (let [i (.lastIndexOf running c)]
              (when (>= i 0) (.splice running i 1))))))
      (.apply f nil (into-array args)))))

;;; Tracing a var

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

(defn- trace!
  "Trace the var named NAMED. Answers why it cannot be, or nil."
  [named]
  (when-not (.has traced named)
    (let [[o prop] (namespace-object named)
          f (when o (unchecked-get o prop))]
      (cond
        (nil? o) "no such namespace in this runtime"
        (not (js-in prop o)) "no such var in this runtime"
        (not (function? f)) "not a function"
        :else
        (let [state #js {:original f :wrapper (wrapped named f)}]
          (.set traced named state)
          (js/Object.defineProperty
           o prop
           #js {:configurable true
                :enumerable true
                :get (fn [] (.-wrapper state))
                ;; defined again: what it is now is traced
                :set (fn [g]
                       (set! (.-original state) g)
                       (set! (.-wrapper state) (if (function? g)
                                                 (wrapped named g)
                                                 g)))})
          nil)))))

(defn- untrace! [named]
  (when-let [state (.get traced named)]
    (let [[o prop] (namespace-object named)]
      (when o
        (js/Object.defineProperty o prop #js {:configurable true :enumerable true
                                              :writable true :value (.-original state)})))
    (.delete traced named)
    true))

;;; What the JVM sees

(defn- thrown-text [e]
  (if (instance? js/Error e)
    (str (.-name e) ": " (.-message e))
    (debug/printed-briefly e)))

(defn- described [all made-in c {:keys [var at ms args value thrown state made parent]}]
  (cond-> {:call c :var var :at at :state (name state) :made made
           :kept (count (get made-in c))
           :args (mapv debug/printed-briefly args)}
    ms (assoc :ms ms)
    (some? parent) (assoc :parent parent)
    (and (some? parent) (not (contains? all parent))) (assoc :made-in-gone true)
    (= :returned state) (assoc :value (debug/printed-briefly value))
    (= :threw state) (assoc :thrown (thrown-text thrown))))

(defn- traced-names []
  (vec (sort (es6-iterator-seq (.keys traced)))))

(defn- calls-said [parent limit]
  (let [all @calls
        made-in (group-by (comp :parent val) all)
        here (if (some? parent)
               (get made-in parent)
               (filter (fn [[_ {:keys [parent]}]] (or (nil? parent) (not (contains? all parent))))
                       all))
        here (cond->> (sort-by key here)
               (nil? parent) (take-last limit))]
    {:calls (mapv (fn [[c call]] (described all made-in c call)) here)
     :traced (traced-names)
     :kept (count all)
     :dropped @dropped}))

(defn call-value
  "What a view of the traced call C shows - WHAT of it: :args, :value, or the
  arg numbered :arg. And :not-recorded where the call is not kept."
  [c {:keys [args value arg]}]
  (if-let [{:keys [state] :as call} (get @calls c)]
    (cond
      args (:args call)
      (some? arg) (nth (:args call) arg :not-recorded)
      value (case state
              :returned (:value call)
              :threw (:thrown call)
              :not-recorded)
      :else :not-recorded)
    :not-recorded))

(defn args-of
  "What the traced call C was called with, as a list - to call its function
  again with, in a repl: (apply ns/f (replique.trace/args-of c)). The same
  values and not copies of them: what was mutated since is as it is now."
  [c]
  (let [args (call-value c {:args true})]
    (if (= :not-recorded args)
      (throw (ex-info (str "The call " c " is not kept") {:call c}))
      args)))

(defn- handle [{:keys [op names parent limit]}]
  (case op
    "trace" (let [skipped (vec (keep (fn [n] (when-let [why (trace! n)] {:var n :why why}))
                                     names))]
              {:traced (traced-names) :skipped skipped})
    "untrace" {:untraced (vec (filter untrace! (or names (traced-names))))}
    "clear" (let [n (count @calls)]
              (reset! calls (sorted-map))
              (reset! dropped 0)
              (changed!)
              {:cleared n})
    "calls" (calls-said parent (or limit 500))
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
