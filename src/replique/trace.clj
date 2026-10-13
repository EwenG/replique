(ns replique.trace
  "The calls of chosen functions, as a tree: what each was called with, what
  it returned or threw, how long it took, and the calls it made - one request
  handled, a test run, a button clicked, call by call.

  A FUNCTION IS TRACED BY ITS VAR, which is made to hold a function that
  says it was called and calls the one it held. Nothing is compiled again
  and nothing of the code changes: any function of any namespace can be
  traced and let go of at once - a whole namespace, the functions a branch
  changed, a function with what calls it and what it calls - and a var
  defined again while it is traced is traced again, so that editing the code
  does not end the trace. What it cannot see is a call that does not go
  through the var: a direct-linked one, a function captured as a value before
  it was traced - a handler in a map built at load time - and a protocol's
  method or a multimethod, which are not traced. Neither is clojure.core,
  which this is made of, nor replique.

  A CALL IS IN THE CALL THAT MADE IT, the traced one the thread was in -
  conveyed to the threads a call starts the way Clojure's bindings are, so
  that a future it made is its own. The calls are kept from the first, up to
  `calls-kept': what is wanted of a trace is a scenario from its start, and
  clearing is how the next one starts - see :trace-clear. A call's args and
  value are kept as they are, and printed when asked for, without realizing
  what of them is lazy.

  CLOJURESCRIPT IS TRACED IN ITS RUNTIME, where its functions are - see
  replique/trace.cljs. What is decided here is which vars: the runtime is
  handed their names."
  (:require [clojure.edn :as edn]
            [replique.cljs :as cljs]
            [replique.debug :as debug]
            [replique.json :as json]
            [replique.lint :as lint]
            [replique.names :as names]
            [replique.output :as output]
            [replique.protocol :as protocol]
            [replique.symbol :as sym])
  (:import [clojure.lang Var]))

(defn- refuse [kind & message]
  (throw (ex-info (apply str message) {:replique/error kind})))

(def ^:private calls-kept
  "How many calls are kept, the first ones: the others are counted."
  5000)

(def ^:private tell-every
  "How often clients are told that traced functions were called, at most, in
  ms."
  200)

(defonce ^:private traced
  ;; Var -> {:original the function it held :wrapper what it holds now}
  (atom {}))

(defonce ^:private calls
  ;; call number -> {:var :thread :at :args :parent :made, and once over :ms
  ;; and :value or :thrown}: :state :running, :returned or :threw
  (atom (sorted-map)))

(defonce ^:private call-numbers (atom 0))

(defonce ^:private dropped
  ;; how many calls were not kept since the last clear
  (atom 0))

(def ^:dynamic *call*
  "The number of the traced call the code running is in, or nil."
  nil)

(def ^:dynamic ^:private *quiet*
  "Whether what is called now is not to be traced: printing what was traced
  may call what is."
  false)

;;; Telling clients

(defonce ^:private telling (atom false))

(defn- changed!
  "Tell clients that traced functions were called - once every `tell-every' ms
  at most: a function called in a loop must not cost the editor a message a
  call."
  []
  (when (compare-and-set! telling false true)
    (-> (Thread/ofVirtual)
        (.name "replique-trace-called")
        (.start ^Runnable
                (fn []
                  (Thread/sleep (long tell-every))
                  (reset! telling false)
                  (output/broadcast-event!
                   (protocol/event "trace-called" {:calls (count @calls)
                                                   :dropped @dropped})))))))

;;; A call

(defn- started!
  "Keep that the function of the var named NAMED was called with ARGS, in the
  call the thread is in - and answer its number, or nil where it is not kept."
  [named args]
  (when-not *quiet*
    (let [parent *call*]
      (if (>= (count @calls) calls-kept)
        (do (swap! dropped inc) (changed!) nil)
        (let [c (swap! call-numbers inc)]
          (swap! calls (fn [m]
                         (cond-> (assoc m c (cond-> {:var named
                                                     :thread (.getName (Thread/currentThread))
                                                     :at (System/currentTimeMillis)
                                                     :args (vec args) :made 0 :state :running}
                                              parent (assoc :parent parent)))
                           (contains? m parent) (update-in [parent :made] inc))))
          (changed!)
          c)))))

(defn- ended!
  "The call C, started at T0 in ns, is over: it returned VALUE, or threw it."
  [c t0 state value]
  (let [ms (/ (- (System/nanoTime) t0) 1e6)]
    (swap! calls (fn [m]
                   (if (contains? m c)
                     (update m c assoc :state state :ms ms
                             (if (= :threw state) :thrown :value) value)
                     m))))
  (changed!))

(defn- wrapped
  "What the var V holds while it is traced, F being what it held - which
  it says, for code recorded in F to stay recorded through it."
  [^Var v f]
  (let [named (str (symbol v))]
    (with-meta
     (fn traced [& args]
       (if-let [c (started! named args)]
         (let [t0 (System/nanoTime)]
           (try
             (let [r (binding [*call* c] (apply f args))]
               (ended! c t0 :returned r)
               r)
             (catch Throwable t
               (ended! c t0 :threw t)
               (throw t))))
         (apply f args)))
     {::debug/wraps f})))

;;; Tracing a var

(defn- untraceable
  "Why the function F, the root of the var V, cannot be traced - or nil."
  [^Var v f]
  (let [ns (str (.ns v))
        m (meta v)]
    (cond
      (or (= "clojure.core" ns) (.startsWith ns "replique."))
      "what tracing is made of is not traced"
      (:macro m) "a macro"
      (.isDynamic v) "dynamic: what is bound is not the var's"
      (:protocol m) "a protocol's method"
      (not (fn? f)) "not a function"
      (some #(.startsWith (.getName ^Class %) "clojure.lang.IFn$") (supers (class f)))
      "it takes or returns primitives, which a caller calls it with directly")))

(declare rewrapped!)

(defn- trace!
  "Trace the var V. Answers why it cannot be, or nil."
  [^Var v]
  (when-not (contains? @traced v)
    (let [f (.getRawRoot v)]
      (if-let [why (untraceable v f)]
        why
        (let [w (wrapped v f)]
          (swap! traced assoc v {:original f :wrapper w})
          (alter-var-root v (constantly w))
          (add-watch v ::trace (fn [_ v _ now] (rewrapped! v now)))
          nil)))))

(defn- untrace!
  "Stop tracing the var V, which holds what it held again. Whether it was
  traced."
  [^Var v]
  (when-let [{:keys [original wrapper]} (get @traced v)]
    (remove-watch v ::trace)
    (swap! traced dissoc v)
    (when (identical? wrapper (.getRawRoot v))
      (alter-var-root v (constantly original)))
    true))

(defn- rewrapped!
  "The var V, traced, was defined again to NOW: traced again, or - defined to
  what cannot be traced - not any more."
  [^Var v now]
  (when-let [{:keys [wrapper]} (get @traced v)]
    (when-not (identical? wrapper now)
      (if (untraceable v now)
        (do (remove-watch v ::trace)
            (swap! traced dissoc v))
        (let [w (wrapped v now)]
          (swap! traced assoc v {:original now :wrapper w})
          (alter-var-root v (constantly w)))))))

;;; Which vars

(defn- var-named
  "The var of this thread's dialect QSYM names, or nil."
  ^Var [qsym]
  (when-let [^clojure.lang.Namespace n (names/find-namespace (namespace qsym))]
    (.findInternedVar n (symbol (name qsym)))))

(defn- as-qsym [x]
  (let [s (some-> x protocol/as-name symbol)]
    (if (qualified-symbol? s)
      s
      (refuse :invalid-message "A var is named by its qualified name, got: " (pr-str x)))))

(defn- chosen
  "The qualified names of the vars MSG chooses - its :vars, the vars a :ns
  defines, or the var :around and those :depth calls around it - and what is
  said of how they were found."
  [{:keys [vars ns around depth]}]
  (cond
    vars (do (when-not (sequential? vars)
               (refuse :invalid-message "The :vars of a trace are a list, got: " (pr-str vars)))
             {:names (mapv as-qsym vars)})
    ns (let [n (or (names/find-namespace (protocol/as-name ns))
                   (refuse :unknown-ns "No namespace " ns))]
         {:names (vec (for [[s ^Var v] (ns-interns n)
                            :when (not (:macro (meta v)))]
                        (symbol (str (ns-name n)) (str s))))})
    around (let [q (as-qsym around)
                 depth (or depth 1)]
             (when-not (pos-int? depth)
               (refuse :invalid-message "A :depth is a number of calls, got: " (pr-str depth)))
             (let [{:keys [callers callees]} (lint/around q depth)]
               {:names (vec (concat [q] (sort callers) (sort callees)))
                :callers (mapv str (sort callers))
                :callees (mapv str (sort callees))}))
    :else (refuse :invalid-message "A trace is of :vars, of an :ns, or :around a var")))

;;; ClojureScript

(def ^:private cljs-within
  "How long a runtime is given to answer, in ms."
  5000)

(defonce ^:private cljs-compiled
  ;; the output directories replique.trace has been compiled into
  (atom #{}))

(defn- cljs-op
  "Ask replique.trace in the runtime of `cljs/*target*' MSG, and answer what
  it said."
  [msg]
  (when-not (cljs/runtime-connected?)
    (refuse :no-runtime "No " (name cljs/*target*) " runtime is running"))
  (let [dir (str (:out-dir (cljs/environment)))]
    (when-not (contains? @cljs-compiled dir)
      (cljs/with-target-lock* #(cljs/compile-namespace! 'replique.trace))
      (swap! cljs-compiled conj dir)))
  (let [js (str "(async () => { await $CLJS.require(\"replique.trace\");"
                " return $CLJS.namespaces.get(\"replique.trace\").op("
                (json/write-str (json/write-str msg)) "); })()")
        {:keys [status value]} (cljs/eval-js-here js cljs-within)]
    (when-not (= :success status)
      (refuse :runtime value))
    (let [answer (edn/read-string (edn/read-string value))]
      (when-let [why (:error answer)]
        (refuse (keyword (or (:kind answer) "runtime")) why))
      answer)))

(cljs/on-notify!
 ::called
 (fn [target content]
   (when-let [{:keys [calls dropped]} (:trace-called (try (edn/read-string content)
                                                          (catch Exception _ nil)))]
     (output/broadcast-event!
      (protocol/event "trace-called" {:dialect "cljs" :target (name target)
                                      :calls calls :dropped dropped})))))

;;; What a client is told

(defn- thrown-text [^Throwable t]
  (str (.getName (class t)) (when-let [m (.getMessage t)] (str ": " m))))

(defn- place
  "Where the var named NAMED, of this thread's dialect, was defined."
  [named]
  (when-let [{:keys [file line column]} (some-> (var-named (symbol named)) meta)]
    (merge (when file (sym/source-of file))
           (when line {:line line})
           (when column {:column column}))))

(defn- described
  "The call C - CALL - as a client is told about it, ALL being the calls kept
  and MADE-IN them by the call they were made in."
  [all made-in c {:keys [var thread at ms args value thrown state made parent]}]
  (merge {:call c :var var :thread thread :at at :state (name state)
          :made made :kept (count (get made-in c))
          :args (mapv debug/printed-briefly args)}
         (when ms {:ms ms})
         (when parent {:parent parent})
         (when (and parent (not (contains? all parent))) {:made-in-gone true})
         (case state
           :returned {:value (debug/printed-briefly value)}
           :threw {:thrown (thrown-text thrown)}
           nil)))

(defn- jvm-calls [parent limit]
  (binding [*quiet* true]
    (let [all @calls
          made-in (group-by (comp :parent val) all)
          here (if parent
                 (get made-in parent)
                 (filter (fn [[_ {:keys [parent]}]] (or (nil? parent) (not (contains? all parent))))
                         all))
          here (cond->> (sort-by key here)
                 (nil? parent) (take-last limit))]
      {:calls (mapv (fn [[c call]] (described all made-in c call)) here)
       :traced (vec (sort (map #(str (symbol %)) (keys @traced))))
       :kept (count all)
       :dropped @dropped})))

(defn call-value
  "What a view of the traced call C shows - WHAT of it: :args, :value, or the
  arg numbered :arg. And :not-recorded where the call is not kept."
  [c {:keys [args value arg]}]
  (if-let [{:keys [state] :as call} (get @calls c)]
    (cond
      args (:args call)
      arg (nth (:args call) arg :not-recorded)
      value (case state
              :returned (:value call)
              :threw (:thrown call)
              :not-recorded)
      :else :not-recorded)
    :not-recorded))

(defn call-ns
  "The namespace of the function of the traced call C, or nil for a call not
  kept."
  [c]
  (some-> (get @calls c) :var symbol namespace symbol find-ns))

(defn args-of
  "What the traced call C was called with, as a list - to call its function
  again with, in a repl: (apply #'ns/f (replique.trace/args-of c)). The
  same values and not copies of them: what was mutated since is as it is now."
  [c]
  (let [args (call-value c {:args true})]
    (if (= :not-recorded args)
      (throw (ex-info (str "The call " c " is not kept") {:call c}))
      args)))

;;; The ops

;; Trace the vars :vars, those the namespace :ns defines, or the var :around
;; with the vars :depth calls around it, which call it or which it calls.
;; Answers how many vars were `asked', the vars `traced' now, and those
;; `skipped', each with `why'
(defmethod protocol/handle :trace [_ msg]
  (names/with-dialect msg
    (let [{:keys [names] :as found} (chosen msg)
          said (assoc (select-keys found [:callers :callees]) :asked (count names))]
      (if (names/cljs?)
        (let [ours (remove #(or (= "cljs.core" (namespace %))
                                (.startsWith (namespace %) "replique."))
                           names)
              answer (cljs-op {:op "trace" :names (mapv str ours)})]
          (merge said
                 (update answer :skipped
                         into (for [q names :when (not (some #{q} ours))]
                                {:var (str q) :why "what tracing is made of is not traced"}))))
        (let [results (mapv (fn [q]
                              (if-let [v (var-named q)]
                                [q (trace! v)]
                                [q "no such var"]))
                            names)]
          (merge said
                 {:traced (vec (sort (map #(str (symbol %)) (keys @traced))))
                  :skipped (vec (for [[q why] results :when why] {:var (str q) :why why}))}))))))

;; Stop tracing the vars :vars - or every one. What was kept stays
(defmethod protocol/handle :untrace [_ msg]
  (names/with-dialect msg
    (let [qsyms (when (:vars msg) (mapv as-qsym (:vars msg)))]
      (if (names/cljs?)
        (cljs-op (cond-> {:op "untrace"} qsyms (assoc :names (mapv str qsyms))))
        {:untraced (vec (for [^Var v (if qsyms (keep var-named qsyms) (keys @traced))
                              :when (untrace! v)]
                          (str (symbol v))))}))))

;; Forget the calls kept: the next ones are kept from the first
(defmethod protocol/handle :trace-clear [_ msg]
  (names/with-dialect msg
    (if (names/cljs?)
      (cljs-op {:op "clear"})
      (let [[before _] (reset-vals! calls (sorted-map))]
        (reset! dropped 0)
        (changed!)
        {:cleared (count before)}))))

;; The calls made in the call :parent - or, without it, those made in none
;; kept, the last :limit. Each with what it was called with and returned, or
;; threw, how long it took, and how many calls it made - of which `kept' are -
;; and where its var was defined
(defmethod protocol/handle :trace-calls [_ msg]
  (names/with-dialect msg
    (let [{:keys [parent limit]} msg
          limit (or limit 500)]
      (when-not (or (nil? parent) (integer? parent))
        (refuse :invalid-message "A call is named by its number, got: " (pr-str parent)))
      (when-not (pos-int? limit)
        (refuse :invalid-message "A :limit is a number of calls, got: " (pr-str limit)))
      (let [answer (if (names/cljs?)
                     (cljs-op (cond-> {:op "calls" :limit limit} parent (assoc :parent parent)))
                     (jvm-calls (some-> parent long) limit))
            placed (memoize place)]
        (update answer :calls (fn [cs] (mapv #(merge % (placed (:var %))) cs)))))))
