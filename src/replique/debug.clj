(ns replique.debug
  "Stopping a thread where the code says so, and working on it while it is
  stopped: its frames, the locals of each, code run on it, and a call started
  over.

    (defn total [order]
      (let [items (:items order)]
        (replique.debug/break!)
        (reduce + (map :price items))))

  A thread that reaches `break!' stops there, and the editor is told. What
  stopped is the thread and nothing else: the process goes on answering, the
  other repls go on evaluating, and the thread waits until it is told to
  continue - or to start over the call of one of its frames, which is what
  makes a fix to the code the next thing that runs.

  TWO HALVES. Where to stop is decided here, by `break!', which is a macro and
  so knows the form it is in: the locals in scope, by the names they were
  written with, and where it was written. What stops the thread is a debugger
  - `replique.debugger', in a jvm of its own, attached to this one through its
  JDWP agent - because only a debugger can do what makes a stopped thread worth
  having: read the locals of every one of its frames, run code on it with what
  it has bound, and pop frames off it. The two meet at one function,
  `pause-here', which does nothing at all: the debugger keeps a breakpoint on
  it, and calling it is what stopping is.

  SO THE PROCESS HAS TO BE STARTED FOR IT, with the agent - which is a flag of
  the jvm, and cannot be added to one that is running:

    -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=127.0.0.1:0

  STARTING A CALL OVER NEEDS ITS ARGUMENTS KEPT, which Clojure does not do by
  default: it clears a local after its last use, so that a lazy sequence it
  holds can be collected. The jvm makes the call again with the arguments the
  popped frame holds, and a frame whose compiler cleared them holds nils - as
  does a local of a frame further out read after its last use. Clearing is an
  option of the compiler rather than of the jvm, so it is turned off in a
  process that runs, for the code compiled from then on - see `:locals-clearing'
  - and is on until then: keeping every local costs the memory a lazy sequence
  held by one would have given back. `break!' sees its own locals either way.

  Without the agent `break!' does nothing, and says so once: a form left in the
  code is a form that must not take a process down.

  The debugger is started the first time a thread asks to stop, and runs as
  long as the process does."
  (:require [clojure.edn :as edn]
            [replique.cljs :as cljs]
            [replique.json :as json]
            [replique.output :as output]
            [replique.protocol :as protocol]
            [replique.state :as state]
            [replique.symbol :as sym])
  (:import [clojure.lang LineNumberingPushbackReader]
           [java.io BufferedReader InputStreamReader Writer OutputStreamWriter StringReader]
           [java.lang.management ManagementFactory]
           [java.nio.charset StandardCharsets]
           [java.util.concurrent TimeUnit]))

(defn- refuse [kind & message]
  (throw (ex-info (apply str message) {:replique/error kind})))

;;; Whether this process can stop a thread at all

(defonce ^:private agent?
  (delay
    (boolean (some #(re-find #"^-agentlib:jdwp|^-Xrunjdwp|[/\\]libjdwp\." %)
                   (.getInputArguments (ManagementFactory/getRuntimeMXBean))))))

(defn available?
  "Whether the jvm was started with the JDWP agent, which is what a debugger
  attaches to."
  []
  @agent?)

;;; The debugger

(def ^:private start-within
  "How long the debugger is given to start and attach, in ms. A jvm, then
  Clojure, then the attach."
  30000)

(def ^:private answer-within
  "How long the debugger is given to answer anything but code run on a stopped
  thread, which takes the time it takes."
  10000)

(defonce ^:private debugger
  ;; nil, or {:process :in :pending (atom {id promise}) :ids}
  (atom nil))

(defonce ^:private pauses
  ;; java thread id -> {:thread :where :locals :frames {index locals}
  ;;                    :stopped :abort}
  ;; From the moment a thread asks to stop: :stopped is set once the debugger
  ;; says it did, which is what a thread is shown as stopped from
  (atom {}))

(declare stopped!)

(defn- read-debugger!
  "Read what the debugger writes until it goes: answers, and the threads it
  stopped."
  [{:keys [^BufferedReader out pending ready]}]
  (try
    (loop []
      (when-let [line (.readLine out)]
        (let [msg (try (edn/read-string line) (catch Exception _ nil))]
          (cond
            (contains? msg :ready) (deliver ready msg)
            (contains? msg :failed) (deliver ready msg)
            (contains? msg :id) (when-let [p (get @pending (:id msg))] (deliver p msg))
            (= :paused (:event msg)) (stopped! (:thread msg))))
        (recur)))
    (catch Throwable _ nil)
    (finally
      (deliver ready {:failed "The debugger stopped before it was ready"})
      (doseq [[_ p] @pending] (deliver p {:error "The debugger stopped"}))
      (swap! debugger #(when-not (identical? out (:out %)) %)))))

(defn- start-debugger!
  "Start the debugger and wait until it has attached. Returns it, or throws
  saying why it could not."
  []
  (let [java (str (System/getProperty "java.home") "/bin/java")
        builder (doto (ProcessBuilder.
                       ^"[Ljava.lang.String;"
                       (into-array String [java
                                           ;; for the slot of a local, see
                                           ;; `replique.debugger/slot-of'
                                           "--add-opens=jdk.jdi/com.sun.tools.jdi=ALL-UNNAMED"
                                           "-cp" (System/getProperty "java.class.path")
                                           "clojure.main" "-m" "replique.debugger"
                                           (str (.pid (java.lang.ProcessHandle/current)))]))
                  (.redirectError java.lang.ProcessBuilder$Redirect/INHERIT))
        ;; What would start an agent of its own in the debugger's jvm, which
        ;; is this one's environment: a JAVA_TOOL_OPTIONS asking for JDWP is
        ;; one way of starting a process for debugging
        _ (doto (.environment builder)
            (.remove "JAVA_TOOL_OPTIONS")
            (.remove "JDK_JAVA_OPTIONS"))
        process (.start builder)
        d {:process process
           :in (OutputStreamWriter. (.getOutputStream process) StandardCharsets/UTF_8)
           :out (BufferedReader. (InputStreamReader. (.getInputStream process)
                                                     StandardCharsets/UTF_8))
           :pending (atom {})
           :ids (atom 0)
           :ready (promise)}]
    (doto (Thread. #(read-debugger! d) "replique-debugger-reader")
      (.setDaemon true)
      (.start))
    (let [said (deref (:ready d) start-within {:failed "The debugger did not start in time"})]
      (when-let [why (:failed said)]
        (.destroy process)
        (throw (ex-info (str "The debugger could not attach: " why) {:replique/error :debugger})))
      d)))

(defn- the-debugger
  "The debugger, started where it is not running."
  []
  (locking debugger
    (or (let [d @debugger]
          (when (and d (.isAlive ^Process (:process d))) d))
        (reset! debugger (start-debugger!)))))

(defn- ask!
  "Send MSG to the debugger and return what it answered, throwing what it
  said went wrong."
  ([msg] (ask! msg answer-within))
  ([msg within]
   (let [{:keys [^Writer in pending ids]}
         (or @debugger (refuse :no-debugger "The debugger is not running"))
         id (swap! ids inc)
         answer (promise)]
     (swap! pending assoc id answer)
     (try
       (locking in
         (.write in (str (pr-str (assoc msg :id id)) "\n"))
         (.flush in))
       (let [{:keys [result error] :as said} (deref answer within ::late)]
         (cond
           (= ::late said) (refuse :debugger "The debugger did not answer in time")
           error (refuse :debugger error)
           :else result))
       (finally (swap! pending dissoc id))))))

(defn release!
  "Stop the debugger, which lets go of every thread it stopped."
  []
  (locking debugger
    (when-let [{:keys [^Process process]} @debugger]
      (reset! debugger nil)
      (.destroy process)
      (.waitFor process 5 TimeUnit/SECONDS))))

;;; Stopping

(defn pause-here
  "Where a thread stops: the debugger keeps a breakpoint on this function, and
  a thread that calls it is suspended there until it is told to go on. ID is
  the thread's own id, which the debugger reads off the call to know which
  thread it stopped."
  [id]
  id)

(def ^:dynamic *in-frame*
  "Whether the code running is code the debugger is running on a stopped
  thread, which does not stop again: the debugger is waiting for it to return."
  false)

(defonce ^:private told (atom #{}))

(defn- tell-once! [k & message]
  (when-not (contains? @told k)
    (swap! told conj k)
    (binding [*out* *err*] (println (apply str "replique: " message)))))

(defn- repl-of
  "The id of the repl connection THREAD is evaluating for, or nil."
  [^Thread thread]
  (some (fn [[id conn]]
          (when (and (identical? :repl @(:role conn))
                     (identical? thread @(:eval-thread conn)))
            id))
        (state/connections)))

(def ^:private print-length
  "How many things of a collection a value printed for a client shows."
  100)

(def ^:private print-level
  "How deep into nested collections a value printed for a client goes."
  10)

(def ^:private printed-up-to
  "How much of a value printed for a client is printed, in chars."
  100000)

(defn- printed [x]
  (let [s (binding [*print-length* print-length *print-level* print-level] (pr-str x))]
    (if (> (count s) printed-up-to) (str (subs s 0 printed-up-to) "...") s)))

(deftype Unrealized [what])

(defmethod print-method Unrealized [^Unrealized u ^Writer w]
  (.write w (str "#<unrealized" (when-let [what (.-what u)] (str " " what)) ">")))

(defn- unrealized-seq? [x]
  (and (seq? x) (instance? clojure.lang.IPending x) (not (realized? x))))

(defn- unforced
  "X as it is printed without realizing what of its lazy seqs is not
  realized yet: that part is left out, and said to be. Down to LEVEL, which
  is as far as printing goes - and as many things of a collection as
  printing shows, LENGTH, and one more for it to say there are more.

  Printing a value is not what the code it came from did with it: a seq
  realized here runs code - side effects and all - on another thread than
  the one it was written for, before the code got to it, or where it never
  would have."
  ([x level] (unforced x level print-length))
  ([x level length]
   (letfn [(some-of [coll] (take (inc length) coll))
           (inner [x] (unforced x (dec level) length))]
     (cond
       (unrealized-seq? x) (Unrealized. (.getSimpleName (class x)))
       (zero? level) x
       (seq? x) (loop [s x, n 0, acc []]
                  (cond
                    (unrealized-seq? s) (apply list (conj acc (Unrealized. nil)))
                    (> n length) (apply list acc)
                    ;; realized, which `seq' and `rest' leave as it is
                    :else (if-let [s (seq s)]
                            (recur (rest s) (inc n) (conj acc (inner (first s))))
                            (apply list acc))))
       (map-entry? x) (clojure.lang.MapEntry/create (inner (key x)) (inner (val x)))
       ;; a record keeps its type, and its fields
       (record? x) (reduce-kv (fn [m k v] (assoc m k (inner v))) x x)
       (map? x) (into (empty x) (map (fn [[k v]] [k (inner v)])) (some-of x))
       (or (vector? x) (set? x)) (into (empty x) (map inner) (some-of x))
       :else x))))

(defn- printed-briefly
  "X printed for a client to show after the form it is the value of: its
  beginning, which is what fits there - the rest is to browse."
  [x]
  (let [length 10, level 3, up-to 1000
        s (binding [*print-length* length *print-level* level]
            (pr-str (unforced x level length)))]
    (if (> (count s) up-to) (str (subs s 0 up-to) "...") s)))

(defn- described
  "A pause as a client is told about it."
  [id {:keys [^Thread thread where kind] :as pause}]
  (let [{:keys [ns file line column end-line end-column cleared]} where]
    (merge {:thread id
            :name (.getName thread)
            :kind (name (or kind :break))}
           (sym/source-of file)
           (cond-> {:ns ns}
             line (assoc :line line)
             column (assoc :column column)
             end-line (assoc :end-line end-line)
             end-column (assoc :end-column end-column)
             ;; what the form the thread stopped after evaluated to
             (contains? pause :printed) (assoc :value (:printed pause))
             ;; a breakpoint's, rather than a step's
             (:break pause) (assoc :break true)
             ;; why it stopped although a condition was asked for
             (:condition-error pause) (assoc :condition-error (:condition-error pause))
             ;; whether the function that stopped clears its locals, which a
             ;; client asked to start its call over has to warn about
             cleared (assoc :locals-cleared true)
             (repl-of thread) (assoc :connection (repl-of thread))))))

(defn- shown!
  "Say that the thread ID stopped, and is the client's to work on."
  [id]
  (when-let [pause (get (swap! pauses #(cond-> % (get % id) (assoc-in [id :stopped] true))) id)]
    (output/broadcast-event! (protocol/event "debug-paused" (described id pause)))))

(declare evaluated)

(defn- holds-or-goes!
  "Show the thread ID stopped at a breakpoint where its CONDITION holds, and
  let it go on otherwise - where it does not hold, the stop was never there.
  A condition that cannot be run stops it, saying why."
  [id condition]
  (let [said (evaluated id 0 condition)]
    (cond
      (#{"nil" "false"} (:value said))
      (try (ask! {:op :resume :thread id}) (catch Throwable _ nil))
      (contains? said :value)
      (shown! id)
      :else
      (do (swap! pauses #(cond-> % (get % id)
                           (assoc-in [id :condition-error]
                                     (or (:message said)
                                         (get-in said [:exception :message])
                                         "the condition threw"))))
          (shown! id)))))

(defn- stopped!
  "Say that the thread ID stopped, as the debugger says it did - once the
  condition of the breakpoint it stopped at holds, where there is one. The
  condition is run on the thread, in its frame, which the debugger does: off
  the thread that reads what the debugger says, which is waiting for this."
  [id]
  (if-let [condition (:condition (get @pauses id))]
    (-> (Thread/ofVirtual)
        (.name "replique-debug-condition")
        (.start ^Runnable (fn [] (holds-or-goes! id condition))))
    (shown! id)))

(defn- resumed! [id]
  (output/broadcast-event! (protocol/event "debug-resumed" {:thread id})))

(def ^:private ^java.lang.StackWalker walker (java.lang.StackWalker/getInstance))

(defn- frames-here
  "The classes of the frames of the code that called into this namespace,
  innermost first. How many there are is how deep it is - which is what tells
  a step to the next form of the same call from one inside a call it makes,
  or one after it returned."
  []
  (.walk walker
         (reify java.util.function.Function
           (apply [_ frames]
             (->> (iterator-seq (.iterator ^java.util.stream.Stream frames))
                  (map #(.getClassName ^java.lang.StackWalker$StackFrame %))
                  (drop-while #(.startsWith ^String % "replique.debug$"))
                  vec)))))

(defn- depth []
  (count (frames-here)))

(defn- stop!
  "Stop the calling thread, as PAUSE says: :where, and the :locals in scope
  where they were handed over rather than to be read off the frame, and its
  :depth where it was measured already. Returns
  nil once it is told to continue, and throws where it is told to abort."
  [pause]
  (when-not *in-frame*
    (cond
      (not (available?))
      (tell-once! :agent "(break!) did not stop: the process was not started with the JDWP"
                  " agent, which is what a debugger attaches to - see replique.debug")
      :else
      (when (try (the-debugger) true
                 (catch Throwable t
                   (binding [*out* *err*]
                     (println (str "replique: the thread did not stop: " (.getMessage t))))
                   false))
        (let [thread (Thread/currentThread)
              id (.threadId thread)]
          (swap! pauses assoc id (assoc pause :thread thread :frames {}
                                        :depth (or (:depth pause) (depth))))
          (pause-here id)
          ;; Told to continue. Not reached where the call was started over
          ;; instead: the frames this would run in are gone, and whoever
          ;; restarted it took the pause away
          (let [{:keys [abort]} (get @pauses id)]
            (swap! pauses dissoc id)
            (when abort
              (throw (ex-info "Aborted from the debugger" {:replique/aborted true})))
            nil))))))

(defn break*
  "Stop the calling thread, which has LOCALS in scope, at WHERE - see
  `break!'. Returns nil once it is told to continue, and throws where it is
  told to abort."
  [locals where]
  (stop! {:locals locals :where where :kind :break}))

(defn- gensym-name? [s]
  (re-find #"__\d+" (name s)))

(def ^:private binding-index
  ;; Which local was bound first is what the compiler numbers them by, in a
  ;; field it keeps to itself - and &env is a hash map
  (let [field (try (doto (.getDeclaredField clojure.lang.Compiler$LocalBinding "idx")
                     (.setAccessible true))
                   (catch Throwable _ nil))]
    (fn [b]
      (or (when field (try (.getInt ^java.lang.reflect.Field field b) (catch Throwable _ nil)))
          0))))

(defmacro break!
  "Stop the thread here, with the locals in scope - see `replique.debug'.

  Nil once the thread is told to continue. Does nothing where the process was
  not started with the JDWP agent, and nothing while the debugger is running
  code on this thread."
  []
  (let [locals (->> &env
                    ;; in the order they were bound in, which is the order they
                    ;; read in
                    (sort-by (fn [[s b]] [(binding-index b) (name s)]))
                    (map key)
                    (remove gensym-name?))
        {:keys [line column]} (meta &form)]
    `(break* (array-map ~@(mapcat (fn [s] [(list 'quote s) s]) locals))
             ~{:ns (str *ns*) :file *file* :line line :column column
               :cleared (not (:disable-locals-clearing *compiler-options*))})))

;;; Stepping through code

;; Code evaluated to be stepped through is compiled by the compiler of
;; replique-clj to call `hook' with the value of every form the source wrote,
;; once it has it, and with the number of every fn it enters - see
;; clojure.lang.Compiler/INSTRUMENT. Which form a number is, is what `site!'
;; was told while the code was compiled. A thread stops at the entry of the
;; outermost fn of what was evaluated, every time it is called - for as long as
;; it is the code that runs, which is until it is evaluated again the ordinary
;; way, or until it is told to stop being stepped through - and from a stop it
;; goes on to the next form, into a call, or out of the one it is in. Unless
;; it was evaluated to stop at its breakpoints only: then a thread stops after
;; a form a breakpoint is on, where its condition holds - see `breaks'. Or to
;; record: then nothing stops, and what every form evaluated to is kept - see
;; "Recording".
;;
;; What was evaluated is a unit, and its sites go with it: code of a unit that
;; is gone may still run - it is still compiled to call `hook' - and nothing
;; stops it. A definition is a unit for as long as it is what its var holds,
;; and anything else for as long as it is being evaluated - or, recorded, for
;; as long as it is not let go of: what it recorded is what it is there for.

(defonce ^:private units
  ;; number -> {:ns :file :line :column :end-line :end-column :var :shown
  ;;            :entry :record :definition}: what was evaluated to be stepped
  ;; through, :shown once clients were told, whether :entry to it stops a
  ;; thread, and whether it records
  (atom {}))

(defonce ^:private unit-numbers (atom 0))

(defonce ^:private sites
  ;; number -> {:unit, :kind :value or :enter, :outermost, :ns, :file,
  ;;            :line, :column, :end-line, :end-column}
  (atom {}))

(defonce ^:private site-numbers (atom 0))

(defonce ^:private breaks
  ;; site number -> {:condition code or nil}: the breakpoints
  (atom {}))

(defonce ^:private stepping
  ;; java thread id -> {:mode :next, :in or :out, :depth :floor}: where the
  ;; thread was told to go from the stop it was at, which was :depth frames
  ;; deep, inside the call :floor - see `still-in?'
  (atom {}))

(defonce ^:private recording
  ;; site number -> {:unit :starts}: the sites of the units that record, and
  ;; whether entering there :starts a call - see "Recording"
  (atom {}))

(defn- compiler-var
  "The Var of Compiler named NAME, which only the compiler of replique-clj has
  - or nil."
  [name]
  (try (.get (.getField clojure.lang.Compiler name) nil)
       (catch Exception _ nil)))

(defn- site!
  "Number the site the compiler is about to compile, of the unit UNIT
  evaluated from FILE in the namespace NS - the form whose metadata is
  EVALUATED. See Compiler/INSTRUMENT."
  [ns file unit evaluated site]
  (let [n (swap! site-numbers inc)
        ;; an arity the source did not write, as a single arity defn's is,
        ;; is where the form that was evaluated is
        where (if (:line site) site evaluated)
        {:keys [record definition dialect]} (get @units unit)]
    (swap! sites assoc n (merge {:ns ns :file file :unit unit}
                                (select-keys site [:kind :outermost])
                                (select-keys where [:line :column :end-line :end-column])))
    ;; a ClojureScript site is recorded where its code runs - see "Recording
    ;; ClojureScript"
    (when (and record (not= :cljs dialect))
      (swap! recording assoc n {:unit unit
                                :starts (boolean (and definition (:outermost site)
                                                      (= :enter (:kind site))))}))
    n))

(defn- step-stop!
  "Stop the thread at the site SITE, DEPTH frames deep inside the call FLOOR,
  after it evaluated to VALUE where there is one. The value is printed here,
  on the thread it belongs to, before the client is told. BREAK is the
  breakpoint it stopped at, where it is that rather than a step."
  [site value depth floor break]
  (stop! (cond-> {:kind (:kind site)
                  :where (assoc (select-keys site [:ns :file :line :column :end-line :end-column])
                                :cleared false)
                  :depth depth
                  :floor floor}
           (= :value (:kind site)) (assoc :value value
                                          :printed (printed (unforced value print-level)))
           break (assoc :break true :condition (:condition break)))))

(defn- still-in?
  "Whether the thread, whose frames are FRAMES, is still inside the call
  FLOOR - {:at :class}, the frame :at frames from the bottom of the stack,
  of the class :class - which is the outermost call of code compiled to be
  stepped through that it was stepping in.

  A thread told to go to the next form after the last one of that call is
  out of the code it was stepping through, and runs free: a form of it run
  later - a fn it made, called from somewhere else - is not where it was
  going."
  [frames {:keys [at class] :as floor}]
  (or (nil? floor)
      (let [here (count frames)]
        (and (>= here at) (= class (nth frames (- here at)))))))

(defn- there?
  "Whether a thread that was told to go as GOING says is there, HERE frames
  deep: anywhere for a step into, the call it was in or one it returned to
  for a step to the next form, one it returned to for a step out."
  [{:keys [mode depth]} here]
  (case mode
    :in true
    :next (<= here depth)
    :out (< here depth)))

(defn- reached!
  "The thread got to the site numbered N, with VALUE where it has one. A site
  of a unit that is gone is none.

  The entry of the outermost fn of a unit stops it whatever it was doing,
  unless the unit stops at its breakpoints only - and is the call a step
  stays inside of, unless it was stepping inside another already. So is the
  call a breakpoint is in.

  A breakpoint whose condition does not hold is not one: a thread that was
  going there anyway stops whatever the condition says."
  [n value]
  (when-let [site (get @sites n)]
    (let [id (.threadId (Thread/currentThread))
          entry (and (:outermost site) (= :enter (:kind site))
                     (:entry (get @units (:unit site))))
          break (get @breaks n)
          going (get @stepping id)]
      (when (or entry break going)
        (let [frames (frames-here)
              here (count frames)
              going (when going
                      (if (still-in? frames (:floor going))
                        going
                        (do (swap! stepping dissoc id) nil)))
              stepped (or entry (and going (there? going here)))]
          (when (or stepped break)
            (swap! stepping dissoc id)
            (step-stop! site value here
                        (if (or entry (and break (not stepped)))
                          (or (:floor going) {:at here :class (first frames)})
                          (:floor going))
                        (when (and break (not stepped)) break))))))))

;;; Recording

;; A unit evaluated to record stops nothing: what each of its forms evaluates
;; to is kept instead, call by call - of the last `calls-kept' calls, the last
;; `values-kept' values of each form, a form in a loop being evaluated many
;; times in one call. Which is what makes the data that went through a
;; function something to look at once it ran, written over the code it went
;; through: an example of everything it was handed and made.
;;
;; A call starts on entering the outermost fn of a definition, and ends once
;; the last form of that fn has its value - the value it returns - or once the
;; thread is found out of it, having thrown out of it. A form evaluated on a
;; thread in no call of its unit - in a fn the call made, called once it
;; returned, or a lazy seq it made, realized then - is the last call's: that
;; is the call that made it. Anything else is recorded in one call, its
;; evaluation.
;;
;; The values are kept as they are, and printed when a client asks: what
;; changed since of a mutable one is shown changed, and what was realized
;; since of a lazy one, realized.

(def ^:private calls-kept
  "How many calls of a unit are kept, the last ones."
  10)

(def ^:private values-kept
  "How many values of a form are kept in a call, the last ones."
  10)

(def ^:private tell-every
  "How often clients are told that a unit recorded, at most, in ms."
  200)

(defonce ^:private calls
  ;; call number -> {:unit :thread :at :values {site {:times :values}}}: what
  ;; the thread named :thread did from :at, in ms - each site evaluated
  ;; :times times, its last :values
  (atom {}))

(defonce ^:private call-numbers (atom 0))

(defonce ^:private recorded
  ;; unit number -> [call number ...]: the calls of it kept, the last one last
  (atom {}))

(defonce ^:private in-calls
  ;; java thread id -> [{:call :unit :depth :class :return} ...]: the calls the
  ;; thread is in, the innermost last - each :depth frames deep, in a frame of
  ;; :class, and over once the site :return has its value
  (atom {}))

(defonce ^:private returns
  ;; site number of an entry -> the site of the last form of its fn, or nil
  (atom {}))

(defonce ^:private telling
  ;; the units clients are about to be told recorded
  (atom #{}))

(defn- changed!
  "Tell clients that the unit UNIT recorded - once every `tell-every' ms at
  most: a function called in a loop must not cost the editor a message a
  call."
  [unit]
  (let [[before _] (swap-vals! telling conj unit)]
    (when-not (contains? before unit)
      (-> (Thread/ofVirtual)
          (.name "replique-debug-recorded")
          (.start ^Runnable
                  (fn []
                    (Thread/sleep (long tell-every))
                    (swap! telling disj unit)
                    (let [kept (get @recorded unit)]
                      (when (and (seq kept) (:shown (get @units unit)))
                        (output/broadcast-event!
                         (protocol/event "debug-recorded"
                                         {:unit unit :call (peek kept) :calls (count kept)}))))))))))

(defn- return-of
  "The site of the last form of the fn whose entry is the site ENTER - the
  form whose value it returns, which is the one that ends last inside it -
  or nil."
  [enter]
  (if-let [[_ found] (find @returns enter)]
    found
    (let [{:keys [unit line column end-line end-column]} (get @sites enter)
          inside? (fn [s]
                    (and (= unit (:unit s)) (= :value (:kind s)) (:end-line s) (:end-column s)
                         (not (neg? (compare [(:line s) (:column s)] [line column])))
                         (neg? (compare [(:end-line s) (:end-column s)] [end-line end-column]))))
          found (when (and end-line end-column)
                  (->> @sites
                       (filter (comp inside? val))
                       ;; of the forms that end there, the outermost: it has
                       ;; its value last
                       (sort-by (fn [[_ s]] [(- (:end-line s)) (- (:end-column s))
                                             (:line s) (:column s)]))
                       ffirst))]
      (swap! returns assoc enter found)
      found)))

(defn- still-in-calls
  "Of IN, the calls a thread was in, the ones it is still in, its frames being
  FRAMES - entering another HERE frames deep, which none as deep is."
  [in frames here]
  (filterv #(and (< (:depth %) here) (still-in? frames {:at (:depth %) :class (:class %)})) in))

(defn- call-started!
  "Start a call of the unit UNIT on this thread, over once the site RETURN
  has its value - or never, without one: see `call-ended!'. Returns its
  number."
  [unit return]
  (let [thread (Thread/currentThread)
        id (.threadId thread)
        frames (frames-here)
        here (count frames)
        c (swap! call-numbers inc)]
    (swap! calls assoc c {:unit unit :thread (.getName thread)
                          :at (System/currentTimeMillis) :values {}})
    (swap! in-calls update id #(conj (still-in-calls % frames here)
                                     {:call c :unit unit :depth here :class (first frames)
                                      :return return}))
    (let [[before after] (swap-vals! recorded update unit
                                     #(vec (take-last calls-kept (conj (or % []) c))))
          kept (set (get after unit))]
      (swap! calls #(apply dissoc % (remove kept (get before unit)))))
    (changed! unit)
    c))

(defn- call-ended!
  "The call C of the unit UNIT, which this thread is in, is over - and the
  calls it is in are not."
  [unit c]
  (let [id (.threadId (Thread/currentThread))]
    (swap! in-calls (fn [m]
                      (let [in (filterv #(not= c (:call %)) (get m id))]
                        (if (seq in) (assoc m id in) (dissoc m id))))))
  (changed! unit))

(defn- record!
  "The site N of the unit UNIT has VALUE, which the call the thread is in
  keeps. Where it is the last form of a call, the call is over - and so are
  the calls the thread threw out of, which are found out here: it is not as
  deep in as they were any more, or not in the same frames."
  [unit n value]
  (let [id (.threadId (Thread/currentThread))
        in (get @in-calls id [])
        in (if (some #(and (= unit (:unit %)) (= n (:return %))) in)
             (let [frames (frames-here)
                   here (count frames)
                   still (filterv #(and (<= (:depth %) here)
                                        (still-in? frames {:at (:depth %) :class (:class %)}))
                                  in)]
               (swap! in-calls #(if (seq still) (assoc % id still) (dissoc % id)))
               still)
             in)
        call (some #(when (= unit (:unit %)) %) (rseq in))
        c (or (:call call) (peek (get @recorded unit)))]
    (when c
      (swap! calls (fn [m]
                     (if (contains? m c)
                       (update-in m [c :values n]
                                  (fn [{:keys [times values]}]
                                    (let [values (conj (or values []) value)]
                                      {:times (inc (or times 0))
                                       ;; a vector of its own rather than a
                                       ;; subvec, which holds on to all of it
                                       :values (if (> (count values) values-kept)
                                                 (vec (rest values))
                                                 values)})))
                       m)))
      (when (and call (= n (:return call)))
        (call-ended! unit c)))))

(defn recorded-value
  "What a view of what the site SITE evaluated to in the call CALL shows: the
  last value, where it was evaluated more than once - or, without CALL, the
  last value of each call kept, one per call, the last call last. And
  :not-recorded where nothing is kept."
  [site call]
  (let [all @calls]
    (if call
      (if-let [values (get-in all [call :values site :values])]
        (peek values)
        :not-recorded)
      (let [unit (:unit (get @sites site))
            values (vec (keep #(peek (get-in all [% :values site :values]))
                              (get @recorded unit)))]
        (if (seq values) values :not-recorded)))))

(defn hook
  "What code compiled to be stepped through calls: (hook n) on entering a fn,
  (hook n value) once a form has its value. As cheap as can be where no thread
  steps and nothing records, since it is called for every form."
  ([n]
   (when-not *in-frame*
     (when-let [{:keys [starts unit]} (get @recording n)]
       (when starts (call-started! unit (return-of n))))
     (reached! n nil))
   nil)
  ([n value]
   (when-let [{:keys [unit]} (get @recording n)]
     (when-not *in-frame*
       (record! unit n value)))
   ;; count rather than empty?, which would make a seq of a map that is not
   (when (and (or (pos? (count @stepping)) (contains? @breaks n))
              (not *in-frame*))
     (reached! n value))
   nil))

(defrecord Instrumented [form entry record])

(defn instrumenting?
  "Whether this process can compile code to be stepped through - which it can
  with the compiler of replique-clj only."
  []
  (boolean (compiler-var "INSTRUMENT")))

(defn read-instrumented
  "Read a form from READER, as if it started at LINE and COLUMN, recording
  where every form inside it starts and ends - which is where a step shows it
  is - and answer it, to be evaluated with `eval-instrumented': to stop on
  ENTRY, or at its breakpoints only - and to RECORD, or not.

  Read twice, as `clojure.main/renumbering-read' does: once to have its text,
  and again from the text, from where it was written."
  [reader line column entry record]
  (let [[_ text] (read+string {:read-cond :allow} reader)
        again (doto (LineNumberingPushbackReader.
                     (StringReader. (str (apply str (repeat (dec column) \space)) text)))
                (.setLineNumber line))
        read #(read {:read-cond :allow} again)]
    (->Instrumented (if-let [^clojure.lang.Var positions (compiler-var "RECORD_POSITIONS")]
                      (with-bindings {positions true} (read))
                      (read))
                    entry
                    record)))

(defn- unit-described
  "The unit numbered N as a client is told about it."
  [n {:keys [ns file line column end-line end-column entry record dialect target]}]
  (merge {:unit n :ns ns}
         ;; which runtime its code is in, where it is not this one
         (when (= :cljs dialect) {:dialect "cljs" :target (name target)})
         (sym/source-of file)
         (cond-> {}
           line (assoc :line line)
           column (assoc :column column)
           end-line (assoc :end-line end-line)
           end-column (assoc :end-column end-column)
           ;; :entry is a file's, inside a jar
           (false? entry) (assoc :breaks-only true)
           record (assoc :record true))))

(declare cljs-forget! cljs-recorded)

(defn- uninstrument!
  "Stop stepping through the unit numbered N, and forget what it recorded,
  saying so to the clients that were told about it. Whether there was one to
  stop."
  [n]
  (let [[before _] (swap-vals! units dissoc n)]
    (when-let [{:keys [var shown dialect] :as unit} (get before n)]
      (let [[sites-before _] (swap-vals! sites #(into {} (remove (fn [[_ site]] (= n (:unit site))))
                                                      %))
            gone (vec (for [[k site] sites-before :when (= n (:unit site))] k))]
        (swap! breaks #(apply dissoc % gone))
        (swap! recording #(apply dissoc % gone))
        (swap! returns #(apply dissoc % gone)))
      (let [[before _] (swap-vals! recorded dissoc n)]
        (swap! calls #(apply dissoc % (get before n))))
      (swap! in-calls (fn [m]
                        (into {} (keep (fn [[id in]]
                                         (let [in (filterv #(not= n (:unit %)) in)]
                                           (when (seq in) [id in]))))
                              m)))
      (when (var? var) (remove-watch var [::unit n]))
      (when (= :cljs dialect) (cljs-forget! n unit))
      (when shown
        (output/broadcast-event! (protocol/event "debug-uninstrumented" {:unit n})))
      true)))

(defn- definition?
  "Whether FORM defines something, which is what the compiler tells by the
  name of what it calls: the fns it makes are called later, by others."
  [form]
  (and (seq? form) (symbol? (first form)) (.startsWith (name (first form)) "def")))

(defn eval-instrumented
  "Evaluate the form of INSTRUMENTED, compiled to be stepped through: the
  thread that calls what it defines stops on entering it, and the one that
  evaluates anything else stops before it does - see `hook' - unless it is
  not to stop on :entry, but at its breakpoints only. Or to :record, see
  \"Recording\" - which is all it does where the process was not started with
  the debugger. Compiled to keep its locals, which a step shows and a restart
  needs.

  A definition stays stepped through until it is uninstrumented, or what its
  var holds is no longer what was evaluated here - and anything recorded,
  until it is uninstrumented - and clients are told so, by
  `debug-instrumented' and `debug-uninstrumented' events."
  [{:keys [form entry record]}]
  (let [^clojure.lang.Var instrument (compiler-var "INSTRUMENT")]
    (cond
      (nil? instrument)
      (refuse :unavailable "Stepping through code needs the compiler of replique-clj,"
              " which this process does not run")
      (not (or (available?) (and record (not entry))))
      (refuse :unavailable "Stepping through code needs the process to be started with the"
              " debugger - see replique.debug")
      :else
      (let [n (swap! unit-numbers inc)
            where (meta form)
            definition (definition? form)
            _ (swap! units assoc n {:entry entry :record record :definition definition})
            ;; what is evaluated outside of the calls of what it defines
            evaluation (when record (call-started! n nil))
            v (try (with-bindings {instrument {:site (partial site! (str *ns*) *file* n where)
                                               :hook #'hook}
                                   #'*compiler-options* (assoc *compiler-options*
                                                               :disable-locals-clearing true)}
                     (eval form))
                   (catch Throwable t
                     (uninstrument! n)
                     (throw t))
                   (finally
                     (when evaluation (call-ended! n evaluation))))]
        (if-not (or definition record)
          (uninstrument! n)
          (let [unit (merge {:ns (str *ns*) :file *file* :shown true :entry entry
                             :record record :definition definition}
                            (select-keys where [:line :column :end-line :end-column])
                            (when (var? v) {:var v}))]
            ;; the evaluation of a function is not one of its calls, which are
            ;; what is looked at
            (when (and evaluation (var? v) (fn? @v))
              (swap! recorded update n #(vec (remove #{evaluation} %)))
              (swap! calls dissoc evaluation))
            (swap! units assoc n unit)
            ;; defined again, the ordinary way or not: the code it holds is
            ;; not this unit's any more
            (when (var? v)
              (add-watch v [::unit n] (fn [_ _ _ _] (uninstrument! n))))
            (output/broadcast-event! (protocol/event "debug-instrumented"
                                                     (unit-described n unit)))))
        v))))

;;; Recording ClojureScript

;; ClojureScript is recorded, and nothing else: there is no debugger to stop
;; a page with. A form is compiled to be recorded by the compiler's own
;; emitter (clojure.cljs.emitter/*instrument*), numbering its sites here as a
;; Clojure form's are, and what its code records is kept where it runs - see
;; replique/debug.cljs, the runtime's half, which is compiled into the
;; program the first time a form is recorded in it. What is kept here is the
;; unit and its sites, which is what a client is told about and what places
;; a value; what a unit recorded is asked of the runtime.
;;
;; A unit is a definition for as long as nothing defines its var again: a
;; ClojureScript var has no watch, so what says so is the compiler, which
;; says what every evaluation defined - a def, or a file compiled whole - see
;; `cljs-evaluated!'. And for as long as the page it was evaluated in is
;; there: a page that reloads runs the program off disk, recorded nowhere.

(def ^:private cljs-within
  "How long a runtime is given to answer about what it recorded, in ms."
  5000)

(defonce ^:private cljs-compiled
  ;; the output directories replique.debug has been compiled into
  (atom #{}))

(defn- cljs-runtime-half!
  "Compile replique.debug into `cljs/*target*''s program, once."
  []
  (let [dir (str (:out-dir (cljs/environment)))]
    (when-not (contains? @cljs-compiled dir)
      (cljs/with-target-lock* #(cljs/compile-namespace! 'replique.debug))
      (swap! cljs-compiled conj dir))))

(defn- cljs-op
  "Ask replique.debug in the runtime of `cljs/*target*' MSG, and answer what
  it said - {:error :kind} included."
  [msg]
  (when-not (cljs/runtime-connected?)
    (refuse :no-runtime "No " (name cljs/*target*) " runtime is running"))
  (cljs-runtime-half!)
  (let [js (str "(async () => { await $CLJS.require(\"replique.debug\");"
                " return $CLJS.namespaces.get(\"replique.debug\").op("
                (json/write-str (json/write-str msg)) "); })()")
        {:keys [status value]} (cljs/eval-js-here js cljs-within)]
    (when-not (= :success status)
      (refuse :runtime value))
    (edn/read-string (edn/read-string value))))

(defn- cljs-forget!
  "Tell the runtime the unit N - UNIT - was in to forget what it recorded,
  where that page is still there. Quietly: what was to be forgotten is gone
  either way."
  [n {:keys [target page]}]
  (when page
    (try (binding [cljs/*target* target]
           (when (= page (cljs/page))
             (cljs/eval-js-here (str "$CLJS.namespaces.get(\"replique.debug\")?.op("
                                     (json/write-str (json/write-str {:op "forget" :unit n}))
                                     ")")
                                1000)))
         (catch Throwable _ nil))))

(defn- cljs-unit-gone?
  "Whether the page the ClojureScript unit UNIT was evaluated in is gone."
  [{:keys [target page]}]
  (binding [cljs/*target* target]
    (not= page (cljs/page))))

(defn- cljs-recorded
  "What the ClojureScript unit N recorded, as `:debug-recorded' answers it -
  asked of the runtime, which keeps it, and placed here, where its sites are."
  [n call]
  (let [unit (get @units n)]
    (when (cljs-unit-gone? unit)
      (uninstrument! n)
      (refuse :unknown-unit "Unit " n " was recorded in a page that is gone"))
    (let [answer (binding [cljs/*target* (:target unit)]
                   (cljs-op (cond-> {:op "recorded" :unit n} call (assoc :call call))))
          thread (name (:target unit))]
      (when-let [why (:error answer)]
        (refuse (case (:kind answer)
                  "unknown-call" :unknown-call
                  "unknown-unit" :unknown-unit
                  :runtime)
                why))
      (-> answer
          (update :calls (fn [cs] (mapv #(assoc % :thread thread) cs)))
          (update :values (fn [vs]
                            (mapv #(merge % (select-keys (get @sites (:site %))
                                                         [:line :column :end-line :end-column]))
                                  vs)))))))

(defn- cljs-hook
  "The JavaScript the code of the unit N calls, loading the runtime's half
  where it is not loaded yet - evaluated once by each form of it."
  [n definition]
  (str "(await $CLJS.require(\"replique.debug\"),"
       " $CLJS.namespaces.get(\"replique.debug\").unit(" n ", " (boolean definition) "))"))

(defn eval-cljs-recorded
  "Evaluate FORM in the runtime of `cljs/*target*', compiled to be recorded,
  with OPTS as `cljs/eval-form' takes them - and answer what it answers.

  A unit, as one of Clojure's is - see `eval-instrumented' - told to clients
  by a `debug-instrumented' event saying which runtime it is in. The answer
  carries its number, as ::unit, for `cljs-evaluated!' to tell its own
  definition from one that replaces it."
  [form opts]
  (let [n (swap! unit-numbers inc)
        where (meta form)
        ns (str (cljs/current-ns))
        file (:file opts)
        definition (definition? form)
        _ (swap! units assoc n {:dialect :cljs :record true :entry false
                                :definition definition})
        r (try (cljs-runtime-half!)
               (cljs/with-instrument* {:site (partial site! ns file n where)
                                       :hook (cljs-hook n definition)}
                                      #(cljs/eval-form form opts))
               (catch Throwable t
                 (uninstrument! n)
                 (throw t)))]
    (if (= :error (:status r))
      (do (uninstrument! n) r)
      (let [unit (merge {:dialect :cljs :target cljs/*target* :page (cljs/page)
                         :ns ns :file file :shown true :entry false :record true
                         :definition definition}
                        (select-keys where [:line :column :end-line :end-column])
                        (when (and definition (symbol? (second form)))
                          {:var (symbol ns (name (second form)))}))]
        (swap! units assoc n unit)
        (output/broadcast-event! (protocol/event "debug-instrumented"
                                                 (unit-described n unit)))
        (assoc r ::unit n)))))

(defn cljs-evaluated!
  "Let go of the ClojureScript units of `cljs/*target*' an evaluation
  replaced the code of, the compiler having said what it defined - EVENTS,
  as `replique.analysis/defining*' reports them - and of the ones whose page
  is gone. Except the unit EXCEPT, which the evaluation was."
  [events except]
  (let [events (filter #(= :cljs (:dialect %)) events)
        vars (set (keep #(when (#{:define :remove} (:op %)) (some-> (:var %) str)) events))
        nss (set (keep #(when (and (= :define (:op %)) (nil? (:var %))) (str (:ns %))) events))]
    (doseq [[n unit] @units
            :when (and (= :cljs (:dialect unit))
                       (= cljs/*target* (:target unit))
                       (not= n except)
                       (:page unit)
                       (or (cljs-unit-gone? unit)
                           (contains? vars (some-> (:var unit) str))
                           (contains? nss (:ns unit))))]
      (uninstrument! n))))

(cljs/on-notify!
 ::recorded
 (fn [target content]
   (doseq [{:keys [unit call calls]} (:recorded (try (edn/read-string content)
                                                     (catch Exception _ nil)))
           :let [u (get @units unit)]
           :when (and u (= :cljs (:dialect u)) (= target (:target u)) (:shown u))]
     (output/broadcast-event! (protocol/event "debug-recorded"
                                              {:unit unit :call call :calls calls})))))

(defn stop-value
  "What a view of the value the thread ID stopped after shows: the value,
  while the thread is stopped, and :running otherwise."
  [id]
  (if-let [pause (let [p (get @pauses id)] (when (:stopped p) p))]
    (:value pause)
    :running))

;;; What the debugger calls, on the thread it stopped

(defn- local-name
  "What a local read off a frame is called, unmunged - or nil where it is one
  the compiler made rather than one somebody wrote."
  [^String munged]
  ;; Told apart before it is unmunged, which turns the __ of a gensym into --
  (when-not (or (gensym-name? munged) (= "this" munged))
    (symbol (clojure.lang.Compiler/demunge munged))))

(defn- locals-of [^objects pairs]
  (when pairs
    ;; an array map whatever the count, which `into' would not keep: they are
    ;; shown in the order they come in
    (apply array-map (mapcat (fn [[n v]] (when-let [s (local-name n)] [s v]))
                             (partition 2 pairs)))))

(defn keep-locals!
  "Keep the locals of the frame INDEX of the stopped thread ID - read off it by
  the debugger, as name, value, name, value."
  [id index pairs]
  (swap! pauses #(cond-> % (get % id) (assoc-in [id :frames index] (locals-of pairs))))
  nil)

(def ^:dynamic *bound*
  "The locals code run in a frame sees, while it runs."
  nil)

(defn- frame-locals
  "The locals of a frame of the stopped thread ID: the ones `break!' was given
  where PAIRS is nil, and PAIRS - read off the frame by the debugger -
  otherwise."
  [id pairs]
  (if pairs
    (locals-of pairs)
    (:locals (get @pauses id))))

(defn- eval-with
  "Evaluate FORM with LOCALS bound, as code run in a frame: it stops nowhere."
  [locals form]
  (let [bound (vec (mapcat (fn [s] [s `(get *bound* '~s)]) (keys locals)))]
    (binding [*in-frame* true
              *bound* locals]
      (eval `(let ~bound ~form)))))

(defn eval-in!
  "Evaluate CODE on the stopped thread ID, which is the thread calling this,
  with the locals of a frame bound - see `frame-locals'. In the namespace NS,
  or the one the thread stopped in for a frame that has none.

  Answers what came of it as EDN, {:value printed} or {:exception data}: what
  the debugger can hand back is a string."
  [id code ns pairs]
  (pr-str
   (try
     (let [{:keys [where]} (get @pauses id)
           ;; a frame of Java code has no namespace, and locals of its own
           ns (or (find-ns (symbol (or ns (:ns where)))) *ns*)
           form (binding [*ns* ns] (read-string code))]
       (binding [*ns* ns]
         {:value (printed (eval-with (frame-locals id pairs) form))}))
     (catch Throwable t
       {:exception (protocol/exception->data t)}))))

;;; A repl evaluating in a frame

(defonce ^:private handed
  ;; number -> {:form :ns :out :err :results :said}: a form a repl pointed at
  ;; a frame hands the thread that stopped - see `eval-in-frame'
  (atom {}))

(defonce ^:private handed-numbers (atom 0))

(defn eval-handed!
  "Evaluate the form handed over as N on the stopped thread ID, which is the
  thread calling this, with the locals of a frame bound - see `frame-locals'.

  As the repl that handed it would have: in its namespace, printing to it, and
  with its *1, *2, *3 and *e. What came of it is delivered to the repl rather
  than answered, as it is - a value, or what was thrown - which the debugger
  could only have handed back printed."
  [id n _ns pairs]
  ;; gone where the repl stopped waiting for it
  (when-let [{:keys [form ns ^java.io.Writer out ^java.io.Writer err results said]}
             (get @handed n)]
    (let [[one two three e] results]
      (deliver said (try
                      (binding [*ns* ns, *out* out, *err* err
                                *1 one, *2 two, *3 three, *e e]
                        {:value (eval-with (frame-locals id pairs) form)})
                      (catch Throwable t {:thrown t})
                      (finally (.flush out) (.flush err))))))
  nil)

(defn frame-ns
  "The namespace the code of the frame INDEX of the stopped thread ID was read
  in - or nil, for a frame of Java code, or a thread that is not stopped."
  [id index]
  (let [{:keys [where] :as pause} (get @pauses id)]
    (when (:stopped pause)
      (or (when (zero? index) (some-> (:ns where) symbol find-ns))
          (some-> (nth (:frames (ask! {:op :frames :thread id})) index nil)
                  :fn symbol namespace symbol find-ns)))))

(defn eval-in-frame
  "Evaluate FORM, read by a repl pointed at the frame FRAME of the thread
  THREAD - see `#replique/frame' - on that thread, which has to be stopped,
  with the locals of the frame bound.

  Its value is answered and what it threw thrown, as the eval of the repl
  would: what is done with them is the repl's. Interrupting the repl
  interrupts the thread running the code, and waits for it to come back -
  which interrupting again does not."
  [{:keys [thread frame]} form]
  (let [pause (get @pauses thread)]
    (when-not (:stopped pause)
      (throw (ex-info (str "Thread " thread " is not stopped - this repl evaluates in its frame "
                           frame " once it stops, and here after #replique/frame nil")
                      {:replique/error :not-paused})))
    (let [n (swap! handed-numbers inc)
          said (promise)]
      (swap! handed assoc n {:form form :ns *ns* :out *out* :err *err*
                             :results [*1 *2 *3 *e] :said said})
      (try
        (try (ask! {:op :eval :thread thread :frame frame :handed n
                    ;; read off the frame, where break! did not hand them over
                    :read (not (and (zero? frame) (:locals pause)))}
                   Long/MAX_VALUE)
             (catch InterruptedException _
               (.interrupt ^Thread (:thread pause))
               @said))
        (let [{:keys [value thrown] :as done} (deref said 0 nil)]
          (cond
            (nil? done) (throw (ex-info "The debugger did not run the code"
                                        {:replique/error :debugger}))
            thrown (throw thrown)
            :else value))
        (finally (swap! handed dissoc n))))))

;;; What a client sees

(defn- the-pause
  "The pause of the thread a client named, which has to be stopped."
  [id]
  (let [pause (get @pauses id)]
    (when-not (and pause (:stopped pause))
      (refuse :not-paused "Thread " (pr-str id) " is not stopped"))
    pause))

(defn- thread-of [msg]
  (let [id (:thread msg)]
    (if (integer? id)
      (long id)
      (refuse :invalid-message "A thread is named by its id, got: " (pr-str id)))))

(defn- frame-of [msg]
  (let [frame (or (:frame msg) 0)]
    (if (nat-int? frame)
      frame
      (refuse :invalid-message "A frame is named by its index, got: " (pr-str frame)))))

(defn frame-value
  "What a view of the frame INDEX of the thread ID shows: the locals of that
  frame while the thread is stopped, and :running otherwise."
  [id index]
  (if-let [pause (let [p (get @pauses id)] (when (:stopped p) p))]
    (some-> (if (and (zero? index) (:locals pause))
              (:locals pause)
              (or (get-in pause [:frames index])
                  (do (ask! {:op :locals :thread id :frame index})
                      (get-in @pauses [id :frames index]))))
            ;; in the order they were bound in, rather than sorted
            (vary-meta assoc :replique.inspector/in-its-order true))
    :running))

(defmethod protocol/handle :debug-paused [_ _]
  {:available (available?)
   :paused (vec (for [[id pause] (sort-by key @pauses) :when (:stopped pause)]
                  (described id pause)))})

(defmethod protocol/handle :debug-frames [_ msg]
  (let [id (thread-of msg)
        {:keys [where]} (the-pause id)
        frames (:frames (ask! {:op :frames :thread id}))]
    {:frames (vec (map-indexed
                   (fn [i {:keys [source] :as frame}]
                     (merge frame
                            (sym/source-of source)
                            ;; the frame that asked to stop knows its column
                            (when (and (zero? i) (:column where))
                              {:column (:column where)})))
                   frames))}))

(defmethod protocol/handle :debug-continue [_ msg]
  (let [id (thread-of msg)]
    (the-pause id)
    (swap! stepping dissoc id)
    (swap! pauses assoc-in [id :abort] (boolean (:abort msg)))
    (ask! {:op :resume :thread id})
    (resumed! id)
    {:resumed true}))

(def ^:private step-modes
  {"next" :next "in" :in "out" :out})

;; Let the thread go on to the next form of the call it stopped in - or the
;; one it returns to - :in to the next form wherever it is, a call it makes
;; included, and :out to the next form after the call it is in returned
(defmethod protocol/handle :debug-step [_ msg]
  (let [id (thread-of msg)
        mode (or (step-modes (:mode msg))
                 (refuse :invalid-message "A step is \"next\", \"in\" or \"out\", got: "
                         (pr-str (:mode msg))))
        pause (the-pause id)]
    (swap! stepping assoc id {:mode mode :depth (:depth pause) :floor (:floor pause)})
    (ask! {:op :resume :thread id})
    (resumed! id)
    {:resumed true}))

;; Stop stepping through the unit :unit - or every one, without it. Its code
;; stays what it is, compiled to be stepped through, until it is evaluated
;; again: what stops is the stopping
(defmethod protocol/handle :debug-uninstrument [_ msg]
  (let [unit (:unit msg)]
    (when-not (or (nil? unit) (integer? unit))
      (refuse :invalid-message "A unit is named by its number, got: " (pr-str unit)))
    {:uninstrumented (vec (filter uninstrument! (if unit [(long unit)] (keys @units))))}))

;; Stop a thread once the form of the unit :unit written at :line and
;; :column - as the unit was read, from where it was written - has its value,
;; or on entering the fn there: a breakpoint, where :condition, code run in
;; the frame, holds. A form that is not evaluated - a literal, a name, a
;; vector - has no breakpoint, and is refused
(defmethod protocol/handle :debug-break [_ msg]
  (let [{:keys [unit line column condition]} msg]
    (when-not (and (integer? unit) (integer? line) (integer? column))
      (refuse :invalid-message "A breakpoint is at the :line and :column of a :unit, got: "
              (pr-str (select-keys msg [:unit :line :column]))))
    (when-not (or (nil? condition) (string? condition))
      (refuse :invalid-message "A condition is code, got: " (pr-str condition)))
    (when-not (contains? @units unit)
      (refuse :unknown-unit "Unit " unit " is not stepped through"))
    (let [here (for [[n site] (sort-by key @sites)
                     :when (and (= unit (:unit site)) (= line (:line site))
                                (= column (:column site)))]
                 [n site])
          [n site] (or (first (filter #(= :value (:kind (second %))) here))
                       (first (filter #(:outermost (second %)) here))
                       (refuse :no-form "No form evaluated at line " line ", column " column))]
      (swap! breaks assoc n {:condition (not-empty condition)})
      (merge {:site n} (select-keys site [:kind :line :column :end-line :end-column])))))

(defmethod protocol/handle :debug-unbreak [_ msg]
  (let [site (:site msg)]
    (when-not (integer? site)
      (refuse :invalid-message "A breakpoint is named by its :site, got: " (pr-str site)))
    (let [[before _] (swap-vals! breaks dissoc site)]
      {:unbroken (contains? before site)})))

;; What the unit :unit recorded: the calls of it kept, and what each form
;; evaluated to in the call :call - the last call, without it - printed to be
;; shown after the form: its last value in the call, and how many times it was
;; evaluated in it
(defmethod protocol/handle :debug-recorded [_ msg]
  (let [{:keys [unit call]} msg]
    (when-not (integer? unit)
      (refuse :invalid-message "A unit is named by its number, got: " (pr-str unit)))
    (when-not (or (nil? call) (integer? call))
      (refuse :invalid-message "A call is named by its number, got: " (pr-str call)))
    (when-not (:record (get @units unit))
      (refuse :unknown-unit "Unit " unit " does not record"))
    (if (= :cljs (:dialect (get @units unit)))
      (cljs-recorded unit (some-> call long))
     (let [kept (get @recorded unit [])
          all @calls
          c (if call (long call) (peek kept))]
      (when (and call (not (some #{c} kept)))
        (refuse :unknown-call "Call " call " is not kept any more"))
      (cond-> {:calls (mapv (fn [k] (let [{:keys [thread at]} (get all k)]
                                      {:call k :thread thread :at at}))
                            kept)
               :values (vec (for [[site {:keys [times values]}] (sort-by key (:values (get all c)))]
                              (merge {:site site :times times
                                      :value (printed-briefly (peek values))}
                                     (select-keys (get @sites site)
                                                  [:line :column :end-line :end-column]))))}
        c (assoc :call c))))))

(defmethod protocol/handle :debug-restart [_ msg]
  (let [id (thread-of msg)
        frame (frame-of msg)
        pause (the-pause id)]
    ;; Taken away before the call is made again, since the call is what may
    ;; stop the thread again - and put back where it could not be
    (swap! pauses dissoc id)
    (swap! stepping dissoc id)
    (try (ask! {:op :restart :thread id :frame frame})
         (catch Throwable t
           (swap! pauses assoc id pause)
           (throw t)))
    (resumed! id)
    {:restarted true}))

(defonce ^:private evaluations (atom 0))

(defn- evaluated
  "What came of running CODE in the frame FRAME of the stopped thread ID, as
  the `debug-evaluated' event says it - without the keys that say which."
  [id frame code]
  (try (edn/read-string (:printed (ask! {:op :eval :thread id :frame frame :code code
                                         ;; the locals of the frame that stopped are
                                         ;; the ones break! handed over, where it did
                                         :read (not (and (zero? frame)
                                                         (:locals (get @pauses id))))}
                                        Long/MAX_VALUE)))
       (catch Throwable t
         (if-let [kind (:replique/error (ex-data t))]
           {:error kind :message (.getMessage t)}
           {:exception (protocol/exception->data t)}))))

;; Answered at once, with the number of the evaluation, and what came of it
;; said later by a `debug-evaluated' event carrying that number. Code run in a
;; frame runs for as long as it runs - a loop that never ends included - and
;; the connection that asked has its other questions to answer meanwhile,
;; in the order they were asked: an answer held back until the code was done
;; would be one answered out of turn.
;;
;; The reply is written here rather than returned, so that it is written
;; before the code starts: the event about an evaluation must not reach the
;; client before the reply that numbered it.
(defmethod protocol/handle :debug-eval [conn msg]
  (let [id (thread-of msg)
        frame (frame-of msg)
        code (:code msg)]
    (when-not (string? code)
      (refuse :invalid-message "The :debug-eval op needs the :code to run, got: " (pr-str code)))
    (the-pause id)
    (let [evaluation (swap! evaluations inc)]
      (protocol/write-frame! conn (protocol/reply msg {:evaluation evaluation}))
      ;; A virtual thread, which does nothing but wait - for the debugger, and
      ;; then for the client to take the event - for as long as the code runs
      (-> (Thread/ofVirtual)
          (.name (str "replique-debug-eval-" evaluation))
          (.start
           ^Runnable
           (fn []
             (let [said (evaluated id frame code)]
               ;; Written rather than emitted: this is the answer somebody is
               ;; waiting for, and an event is dropped where the client is behind
               (try (protocol/write-frame!
                     conn (protocol/event "debug-evaluated"
                                          (merge said {:thread id :evaluation evaluation})))
                    (catch Throwable _ nil))))))
      protocol/no-reply)))

;;; Keeping locals

(defn locals-cleared?
  "Whether the code compiled from now on clears its locals."
  []
  (not (:disable-locals-clearing *compiler-options*)))

;; Whether the code compiled from now on clears its locals, and :clear to say
;; whether it should. The root of *compiler-options*, which is what every
;; thread compiles with where it binds none: a repl and a load included, and
;; the files loaded again after this - nothing already compiled changes, which
;; is why a client offers to load what is being debugged again.
(defmethod protocol/handle :locals-clearing [_ msg]
  (when (contains? msg :clear)
    (let [clear (:clear msg)]
      (when-not (boolean? clear)
        (refuse :invalid-message "The :locals-clearing op takes :clear true or false, got: "
                (pr-str clear)))
      (alter-var-root #'*compiler-options*
                      #(if clear
                         (dissoc % :disable-locals-clearing)
                         (assoc % :disable-locals-clearing true)))))
  {:clear (locals-cleared?)})

;; A thread is never left stopped with nobody to continue it: when the last
;; editor goes, so does every pause
(state/on-close!
 ::pauses
 (fn [_]
   (when (empty? (output/control-connections))
     (doseq [[id pause] @pauses :when (:stopped pause)]
       (try (ask! {:op :resume :thread id}) (catch Throwable _ nil))))))
