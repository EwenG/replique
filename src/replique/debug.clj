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
  printing shows, and one more for it to say there are more.

  Printing a value is not what the code it came from did with it: a seq
  realized here runs code - side effects and all - on another thread than
  the one it was written for, before the code got to it, or where it never
  would have."
  [x level]
  (letfn [(some-of [coll] (take (inc print-length) coll))]
    (cond
      (unrealized-seq? x) (Unrealized. (.getSimpleName (class x)))
      (zero? level) x
      (seq? x) (loop [s x, n 0, acc []]
                 (cond
                   (unrealized-seq? s) (apply list (conj acc (Unrealized. nil)))
                   (> n print-length) (apply list acc)
                   ;; realized, which `seq' and `rest' leave as it is
                   :else (if-let [s (seq s)]
                           (recur (rest s) (inc n) (conj acc (unforced (first s) (dec level))))
                           (apply list acc))))
      (map-entry? x) (clojure.lang.MapEntry/create (unforced (key x) (dec level))
                                                   (unforced (val x) (dec level)))
      ;; a record keeps its type, and its fields
      (record? x) (reduce-kv (fn [m k v] (assoc m k (unforced v (dec level)))) x x)
      (map? x) (into (empty x) (map (fn [[k v]] [k (unforced v (dec level))])) (some-of x))
      (or (vector? x) (set? x)) (into (empty x) (map #(unforced % (dec level))) (some-of x))
      :else x)))

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
;; a form a breakpoint is on, where its condition holds - see `breaks'.
;;
;; What was evaluated is a unit, and its sites go with it: code of a unit that
;; is gone may still run - it is still compiled to call `hook' - and nothing
;; stops it. A definition is a unit for as long as it is what its var holds,
;; and anything else for as long as it is being evaluated.

(defonce ^:private units
  ;; number -> {:ns :file :line :column :end-line :end-column :var :shown
  ;;            :entry}: what was evaluated to be stepped through, :shown once
  ;; clients were told, and whether :entry to it stops a thread
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
        where (if (:line site) site evaluated)]
    (swap! sites assoc n (merge {:ns ns :file file :unit unit}
                                (select-keys site [:kind :outermost])
                                (select-keys where [:line :column :end-line :end-column])))
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

(defn hook
  "What code compiled to be stepped through calls: (hook n) on entering a fn,
  (hook n value) once a form has its value. As cheap as can be where no thread
  steps, since it is called for every form."
  ([n]
   (when-not *in-frame*
     (reached! n nil))
   nil)
  ([n value]
   ;; count rather than empty?, which would make a seq of a map that is not
   (when (and (or (pos? (count @stepping)) (contains? @breaks n))
              (not *in-frame*))
     (reached! n value))
   nil))

(defrecord Instrumented [form entry])

(defn instrumenting?
  "Whether this process can compile code to be stepped through - which it can
  with the compiler of replique-clj only."
  []
  (boolean (compiler-var "INSTRUMENT")))

(defn read-instrumented
  "Read a form from READER, as if it started at LINE and COLUMN, recording
  where every form inside it starts and ends - which is where a step shows it
  is - and answer it, to be evaluated with `eval-instrumented': to stop on
  ENTRY, or at its breakpoints only.

  Read twice, as `clojure.main/renumbering-read' does: once to have its text,
  and again from the text, from where it was written."
  [reader line column entry]
  (let [[_ text] (read+string {:read-cond :allow} reader)
        again (doto (LineNumberingPushbackReader.
                     (StringReader. (str (apply str (repeat (dec column) \space)) text)))
                (.setLineNumber line))
        read #(read {:read-cond :allow} again)]
    (->Instrumented (if-let [^clojure.lang.Var positions (compiler-var "RECORD_POSITIONS")]
                      (with-bindings {positions true} (read))
                      (read))
                    entry)))

(defn- unit-described
  "The unit numbered N as a client is told about it."
  [n {:keys [ns file line column end-line end-column entry]}]
  (merge {:unit n :ns ns}
         (sym/source-of file)
         (cond-> {}
           line (assoc :line line)
           column (assoc :column column)
           end-line (assoc :end-line end-line)
           end-column (assoc :end-column end-column)
           ;; :entry is a file's, inside a jar
           (false? entry) (assoc :breaks-only true))))

(defn- uninstrument!
  "Stop stepping through the unit numbered N, saying so to the clients that
  were told about it. Whether there was one to stop."
  [n]
  (let [[before _] (swap-vals! units dissoc n)]
    (when-let [{:keys [var shown]} (get before n)]
      (let [[sites-before _] (swap-vals! sites #(into {} (remove (fn [[_ site]] (= n (:unit site))))
                                                      %))]
        (swap! breaks #(apply dissoc % (for [[k site] sites-before :when (= n (:unit site))] k))))
      (when var (remove-watch var [::unit n]))
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
  not to stop on :entry, but at its breakpoints only. Compiled to keep its
  locals, which a step shows and a restart needs.

  A definition stays stepped through until it is uninstrumented, or what its
  var holds is no longer what was evaluated here, and clients are told so -
  by `debug-instrumented' and `debug-uninstrumented' events."
  [{:keys [form entry]}]
  (let [^clojure.lang.Var instrument (compiler-var "INSTRUMENT")]
    (cond
      (nil? instrument)
      (refuse :unavailable "Stepping through code needs the compiler of replique-clj,"
              " which this process does not run")
      (not (available?))
      (refuse :unavailable "Stepping through code needs the process to be started with the"
              " debugger - see replique.debug")
      :else
      (let [n (swap! unit-numbers inc)
            where (meta form)
            _ (swap! units assoc n {:entry entry})
            v (try (with-bindings {instrument {:site (partial site! (str *ns*) *file* n where)
                                               :hook #'hook}
                                   #'*compiler-options* (assoc *compiler-options*
                                                               :disable-locals-clearing true)}
                     (eval form))
                   (catch Throwable t
                     (uninstrument! n)
                     (throw t)))]
        (if-not (definition? form)
          (uninstrument! n)
          (let [unit (merge {:ns (str *ns*) :file *file* :shown true :entry entry}
                            (select-keys where [:line :column :end-line :end-column])
                            (when (var? v) {:var v}))]
            (swap! units assoc n unit)
            ;; defined again, the ordinary way or not: the code it holds is
            ;; not this unit's any more
            (when (var? v)
              (add-watch v [::unit n] (fn [_ _ _ _] (uninstrument! n))))
            (output/broadcast-event! (protocol/event "debug-instrumented"
                                                     (unit-described n unit)))))
        v))))

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

(defn eval-in!
  "Evaluate CODE on the stopped thread ID, which is the thread calling this,
  with the locals of a frame bound: the ones `break!' was given where PAIRS is
  nil, and PAIRS - read off the frame by the debugger - otherwise. In the
  namespace NS, or the one the thread stopped in for a frame that has none.

  Answers what came of it as EDN, {:value printed} or {:exception data}: what
  the debugger can hand back is a string."
  [id code ns pairs]
  (pr-str
   (try
     (let [{:keys [where locals]} (get @pauses id)
           ;; a frame of Java code has no namespace, and locals of its own
           locals (if pairs (locals-of pairs) locals)
           ns (or (find-ns (symbol (or ns (:ns where)))) *ns*)
           form (binding [*ns* ns] (read-string code))
           bound (vec (mapcat (fn [s] [s `(get *bound* '~s)]) (keys locals)))]
       (binding [*ns* ns
                 *in-frame* true
                 *bound* locals]
         {:value (printed (eval `(let ~bound ~form)))}))
     (catch Throwable t
       {:exception (protocol/exception->data t)}))))

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
