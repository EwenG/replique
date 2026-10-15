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
            [clojure.string :as str]
            [clojure.walk :as walk]
            [replique.cljs :as cljs]
            [replique.json :as json]
            [replique.iterations :as iterations]
            [replique.output :as output]
            [replique.protocol :as protocol]
            [replique.scope :as scope]
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

(defn printed-briefly
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
;; once it has it, and `enter' with the number of every fn it enters and what
;; it was called with - see clojure.lang.Compiler/INSTRUMENT. Which form a
;; number is, is what `site!' was told while the code was compiled. A thread stops at the entry of the
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
  ;;            :entry :record :definition :form}: what was evaluated to be
  ;; stepped through, :shown once clients were told, whether :entry to it
  ;; stops a thread, and whether it records - the :form, read with its
  ;; spans, being what says which locals a recorded call had where
  (atom {}))

(defonce ^:private unit-numbers (atom 0))

(defonce ^:private sites
  ;; number -> {:unit, :kind :value, :enter or :local, :outermost, :ns,
  ;;            :file, :line, :column, :end-line, :end-column, and for an
  ;;            :enter the :params of the fn, as written, and the :marker
  ;;            that says which arity of the source it is - see
  ;;            `scope/marker'. A :local is a name a let or a loop binds, in a
  ;;            unit that records: what it was bound to}
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

(defonce ^:private wrapping
  ;; unit number -> {:var :original :wrapper :sites}: the vars whose calls only
  ;; are recorded, what each held and holds now - see "Recording calls only"
  (atom {}))

(defn- compiler-var
  "The Var of Compiler named NAME, which only the compiler of replique-clj has
  - or nil."
  [name]
  (try (.get (.getField clojure.lang.Compiler name) nil)
       (catch Exception _ nil)))

(defn- local-site
  "The number of the site of the unit UNIT that a :local SITE is already -
  bound by the same name, where it was written: a macro binding a name of the
  source twice, as doseq does, one for a chunk and one for a seq, binds one
  local of the source - or nil."
  [unit site]
  (when (= :local (:kind site))
    (let [where (select-keys site [:line :column :end-line :end-column])]
      (some (fn [[n s]]
              (when (and (= unit (:unit s)) (= :local (:kind s))
                         (= where (select-keys s [:line :column :end-line :end-column])))
                n))
            @sites))))

(defn- written-at
  "The span of the form written in FORM at the place [LINE COLUMN], as a site
  says it - or nil."
  [form line column]
  (some (fn [x]
          (when-let [[l c el ec] (scope/span x)]
            (when (= [line column] [l c])
              {:line l :column c :end-line el :end-column ec})))
        (tree-seq coll? seq form)))

(defn- reader-fn?
  "Whether the arity A of a fn written nowhere - see `scope/arities' - is what
  #(...) reads to, its params named by the reader, p1__42# and rest__43#."
  [a]
  (let [params (remove #{'&} (:params a))]
    (and (seq params)
         (every? #(and (symbol? %) (re-matches #"(p\d+|rest)__\d+#" (name %))) params))))

(defn- entry-place
  "Where the fn whose arity ARITY - (params body...), as the compiler sees it -
  is about to be compiled is written in FORM, what was evaluated, as a site
  says it: a fn and defn write an arity the source did not, but its body the
  source did, the same form, which says which fn it is. #(...) is where the
  body read in it is, from the # before it. A fn a macro made, written nowhere,
  is where the form written around it is - the one the compiler is in - and
  :made."
  [form arity]
  (let [b (second arity)
        a (when (instance? clojure.lang.IObj b)
            (some (fn [x] (some #(when (identical? b (first (:body %))) %) (scope/arities x)))
                  (tree-seq coll? seq form)))
        [l c el ec] (:span a)]
    (cond
      l {:line l :column c :end-line el :end-column ec}
      (and a (reader-fn? a) (scope/span b))
      (let [[l c el ec] (scope/span b)]
        {:line l :column (dec c) :end-line el :end-column ec})
      :else
      (let [line (some-> ^clojure.lang.Var (compiler-var "LINE") deref)
            column (some-> ^clojure.lang.Var (compiler-var "COLUMN") deref)]
        (assoc (or (when (and line column) (written-at form line column))
                   (select-keys (meta form) [:line :column :end-line :end-column]))
               :made true)))))

(defn- new-site!
  "A number for the site the compiler is about to compile, of the unit UNIT
  evaluated from FILE in the namespace NS - the form EVALUATED - which no
  other site has."
  [ns file unit evaluated site]
  (let [n (swap! site-numbers inc)
        ;; the entry of an arity the source did not write, as a single arity
        ;; fn's is, is where its fn is
        where (cond (:line site) site
                    (= :enter (:kind site)) (entry-place evaluated (:form site))
                    :else (meta evaluated))
        {:keys [record definition dialect]} (get @units unit)]
    (swap! sites assoc n (merge {:ns ns :file file :unit unit}
                                (select-keys site [:kind :outermost])
                                (select-keys where [:made])
                                (when-let [params (:params site)]
                                  {:params (mapv str params)})
                                ;; an arity is where its fn is, the one fn made
                                ;; from it being written nowhere
                                (when (= :enter (:kind site))
                                  {:marker (scope/marker (:form site))})
                                (select-keys where [:line :column :end-line :end-column])))
    ;; a ClojureScript site is recorded where its code runs - see "Recording
    ;; ClojureScript"
    (when (and record (not= :cljs dialect))
      (swap! recording assoc n {:unit unit
                                :starts (boolean (and definition (:outermost site)
                                                      (= :enter (:kind site))))}))
    n))

(defn- site!
  "Number the site the compiler is about to compile, of the unit UNIT
  evaluated from FILE in the namespace NS - the form EVALUATED - or answer
  the number it already has. See Compiler/INSTRUMENT."
  [ns file unit evaluated site]
  (or (local-site unit site)
      (new-site! ns file unit evaluated site)))

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
  going there anyway stops whatever the condition says.

  A local bound is nowhere to stop: it is recorded, and that is all."
  [n value]
  (when-let [site (let [site (get @sites n)]
                    (when-not (= :local (:kind site)) site))]
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
;; to is kept instead, call by call - the first `values-kept' values of each
;; form and the last ones, a form in a loop being evaluated many times in one
;; call. Each numbered in the call, which is what
;; says which iteration of a loop a value is of - see replique.iterations. Which is what makes the data that went through a
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
;; What a fn was called with is what its entry evaluated to: a vector of the
;; value of each param - see Compiler/INSTRUMENT's :enter.
;;
;; Of the calls of a unit made in no other call kept, the last `calls-kept'
;; are kept. A call made in another is kept as long as that one is - and as
;; long as the value it was made for is: the first one that call had once it
;; was over, which says what it was made for, kept of it or not as any value
;; is. Of the calls made for the same value - in a loop that records nothing
;; else, (mapv f xs) - the first `values-kept' and the last ones. So what was
;; called to make a value kept is kept, and gone with it. Of all the calls,
;; `calls-kept-in-all' at most, the oldest let go of first.
;;
;; The values are kept as they are, and printed when a client asks: what
;; changed since of a mutable one is shown changed, and what was realized
;; since of a lazy one, realized.

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
  "How often clients are told that a unit recorded, at most, in ms."
  200)

(defonce ^:private calls
  ;; call number -> {:unit :thread :at :count :values {site {:times :values
  ;; :numbers :head}} :parent :children :until}: what the thread named :thread did
  ;; from :at, in ms - :count values, each site evaluated :times times, its
  ;; last :values, which are the values numbered :numbers in the call, and the
  ;; first ones, :head, [number value]s. Made in another call recorded, once
  ;; that one had :count values, its :parent, [call count] - and of that one,
  ;; one of the :children, [count call]s - and once it was over and that one
  ;; had :until values, made for the next one. See "Going through what was
  ;; recorded". Sorted, the oldest first
  (atom (sorted-map)))

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

(defn- forget-calls
  "Of M, the calls by number, without the calls GONE - which the calls that
  made them have no more among their :children."
  [m gone]
  (reduce (fn [m g]
            (let [[parent] (:parent (get m g))]
              (cond-> (dissoc m g)
                (contains? m parent)
                (update-in [parent :children] (fn [cs] (filterv #(not= g (second %)) cs))))))
          m gone))

(defn- with-made
  "The calls CS, the calls they made and so on - of M, the calls by number."
  [m cs]
  (loop [acc #{}, todo (vec cs)]
    (if-let [c (peek todo)]
      (if (contains? acc c)
        (recur acc (pop todo))
        (recur (conj acc c) (into (pop todo) (map second) (:children (get m c)))))
      acc)))

(defn- forget!
  "Let go of the calls CS, and of the calls they made and so on."
  [cs]
  (when (seq cs)
    (let [m @calls
          gone (with-made m cs)
          by-unit (group-by #(:unit (get m %)) gone)]
      (swap! calls forget-calls gone)
      (swap! recorded (fn [r]
                        (reduce-kv (fn [r unit gs]
                                     (let [gs (set gs)]
                                       (cond-> r
                                         (contains? r unit) (update unit #(filterv (complement gs) %)))))
                                   r by-unit))))))

(defn- forget-oldest!
  "Let go of the oldest calls not running, where more than
  `calls-kept-in-all' are kept."
  []
  (let [m @calls
        over (- (count m) calls-kept-in-all)]
    (when (pos? over)
      (let [running (into #{} (comp (mapcat val) (map :call)) @in-calls)]
        (forget! (take over (remove running (keys m))))))))

(defn- call-started!
  "Start a call of the unit UNIT on this thread, over once the site RETURN
  has its value - or never, without one: see `call-ended!'. Made in the
  innermost call the thread is in, where it is in one. Returns its number."
  [unit return]
  (let [thread (Thread/currentThread)
        id (.threadId thread)
        frames (frames-here)
        here (count frames)
        c (swap! call-numbers inc)
        in (still-in-calls (get @in-calls id) frames here)
        parent (:call (peek in))]
    (swap! calls (fn [m]
                   (let [made-in (get m parent)
                         at (:count made-in 0)]
                     (cond-> (assoc m c (cond-> {:unit unit :thread (.getName thread)
                                                 :at (System/currentTimeMillis) :values {}}
                                          made-in (assoc :parent [parent at])))
                       made-in (update-in [parent :children] (fnil conj []) [at c])))))
    (swap! in-calls update id #(conj (still-in-calls % frames here)
                                     {:call c :unit unit :depth here :class (first frames)
                                      :return return}))
    ;; of the calls made in none kept, the last ones
    (let [r (swap! recorded update unit (fnil conj []) c)
          m @calls]
      (forget! (drop-last calls-kept (remove #(contains? m (first (:parent (get m %))))
                                             (get r unit)))))
    (forget-oldest!)
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
  ;; made for the next value of the call that made it
  (let [m (swap! calls (fn [m]
                         (let [call (get m c)
                               p (get m (first (:parent call)))]
                           (if (and p (not (:until call)))
                             (assoc-in m [c :until] (:count p 0))
                             m))))
        until (:until (get m c))]
    (when until
      (let [same (filter #(= until (:until (get m %)))
                         (map second (:children (get m (first (:parent (get m c)))))))]
        ;; the first ones and the last ones
        (forget! (take (- (count same) (* 2 values-kept)) (drop values-kept same))))))
  (changed! unit))

(defn- with-value
  "M, the calls by number, with VALUE kept as what the site N evaluated to in
  the call C - numbered in it, see replique.iterations."
  [m c n value]
  (if-let [call (get m c)]
    (let [i (inc (:count call 0))]
      (assoc m c (-> call
                     (assoc :count i)
                     (update-in [:values n]
                                (fn [{:keys [times values numbers head]}]
                                  (let [values (conj (or values []) value)
                                        numbers (conj (or numbers []) i)
                                        over (> (count values) values-kept)]
                                    {:times (inc (or times 0))
                                     ;; vectors of their own rather than
                                     ;; subvecs, which hold on to all of it
                                     :values (if over (vec (rest values)) values)
                                     :numbers (if over (vec (rest numbers)) numbers)
                                     :head (if (< (count head) values-kept)
                                             (conj (or head []) [i value])
                                             head)}))))))
    m))

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
      (let [[before _] (swap-vals! calls with-value c n value)
            {:keys [numbers head]} (get-in before [c :values n])
            out (first numbers)]
        ;; the value that is not kept any more, and the calls made for it
        (when (and (= values-kept (count numbers)) (not-any? #(= out (first %)) head))
          (forget! (keep (fn [[_ made]] (when (= (dec out) (:until (get before made))) made))
                         (:children (get before c))))))
      (when (and call (= n (:return call)))
        (call-ended! unit c)))))

(defn- kept-in
  "What is kept of each site of the call CALL, as replique.iterations takes
  it: a fn of the site, nil where nothing is."
  [call]
  (fn [site]
    (when-let [{:keys [times values numbers head]} (get-in call [:values site])]
      {:times times :head head :tail (mapv vector numbers values)})))

(defn- span-of [site]
  (mapv site [:line :column :end-line :end-column]))

(defn- loops-of
  "The loops of the code of the unit N - see `scope/loops' - whose first name
  bound each time round is a site, outermost first: each {:site :span
  :spans}, and :sites, the sites of all its names. An arity of a fn is one
  too, with :fn and its :args, its first site being its entry - which records
  what each call of it was called with, the names of its params among it -
  each with the :names it binds. Not a fn of a definition in no other, a call
  of which is a call of the unit."
  [n]
  (let [{:keys [form definition]} (get @units n)
        unit-sites (filter #(= n (:unit (val %))) @sites)
        local-sites (into {} (for [[k s] unit-sites :when (= :local (:kind s))]
                               [(span-of s) k]))
        arities (scope/all-arities form)
        entry-of (fn [l]
                   (some (fn [[k s]]
                           (when (and (= :enter (:kind s))
                                      (= (:span l) (:span (scope/entered-arity arities (:marker s)))))
                             k))
                         unit-sites))]
    (vec (for [{:keys [names outermost fn] :as l} (scope/loops form)
               :when (not (and definition outermost))
               :let [sites (vec (keep identity (cons (when fn (entry-of l))
                                                     (map #(get local-sites (scope/span %)) names))))]
               :when (seq sites)]
           (-> l (dissoc :outermost) (assoc :site (first sites) :sites sites))))))

(defn- loops-said
  "The loops of the code of the unit N that went round in a call, as
  `:debug-recorded' answers them - TIMES-OF answering how many times the site
  of a name was bound in it, or nil."
  [n times-of]
  (vec (for [{:keys [site span fn]} (loops-of n)
             :let [times (times-of site)]
             :when times]
         (merge {:site site :iterations times
                 :kept (iterations/kept-iterations times values-kept)}
                (when fn {:fn true})
                (zipmap [:line :column :end-line :end-column] span)))))

(defn- loop-has?
  "Whether the site K, which is S, is evaluated again each time round of the
  loop L, of `loops-of' - a fn's entry being where the fn is, and of each call
  of it."
  [l k s]
  (boolean (or (some #{k} (:sites l))
               (and (:line s) (some #(scope/in-span? % [(:line s) (:column s)]) (:spans l))))))

(defn- iterating
  "What says which values of a call of the unit N are of the iteration INDEX
  of the loop whose name is bound at the site SITE - {:loop :index}, the
  sites of the names of the loops it is in, :enclosing, :owners, of each
  site of the unit evaluated again each time round of one of these loops, the
  innermost one's site, and :starts, the sites of all the names of each of
  these loops - see `iterations/ranges'."
  [n {:keys [site index]}]
  (let [loops (loops-of n)
        it (or (some #(when (= site (:site %)) %) loops)
               (refuse :unknown-iteration "Site " site " starts no iteration of a loop of unit " n))
        in? loop-has?
        chain (filter #(in? % site (get @sites site)) loops)]
    {:loop site
     :index index
     :enclosing (mapv :site (remove #{it} chain))
     :starts (into {} (map (juxt :site :sites)) (cons it chain))
     :owners (into {} (for [[k s] @sites
                            :when (= n (:unit s))
                            :let [owner (last (filter #(in? % k s) chain))]
                            :when owner]
                        [k (:site owner)]))}))

(defn- in-iteration
  "A fn answering, of a site, what it evaluated to in the iteration ITERATING
  says - see `iterating' - of a call of which KEPT-OF says what is kept:
  {:times :value}, or nil where it was not evaluated then, or
  ::iterations/not-kept - or DEFAULT where it is not evaluated again each time
  round. Refuses an iteration not kept."
  [iterating kept-of]
  (or (iterations/picker kept-of iterating)
      (refuse :unknown-iteration "Iteration " (:index iterating) " is not kept")))

(defn- iteration-of
  "What a site of the unit N evaluated to in a call: the value it is, of the
  call C, where it was evaluated in it - of the iteration ITERATION,
  {:site :index}, of a loop of it, where there is one. Or :not-recorded."
  [n c site iteration]
  (let [call (get @calls c)
        values (get-in call [:values site :values])
        v (if iteration
            ((in-iteration (iterating n iteration) (kept-in call))
             site (when values {:value (peek values)}))
            (when values {:value (peek values)}))]
    (if (map? v) (:value v) :not-recorded)))

(defn recorded-value
  "What a view of what the site SITE evaluated to in the call CALL shows: the
  last value, where it was evaluated more than once - or, without CALL, the
  last value of each call kept it was evaluated in, by the number of the call
  among the ones kept, from 1, as a loop's are by iteration: an outline, see
  `iterations/by-call'. And :not-recorded where nothing is kept.

  With ITERATION, {:site :index}, what it evaluated to last in that iteration
  of a loop of the call CALL, or of the last call - see replique.iterations."
  ([site call] (recorded-value site call nil))
  ([site call iteration]
   (let [all @calls
         unit (:unit (get @sites site))]
     (cond
       iteration (iteration-of unit (or call (peek (get @recorded unit))) site iteration)
       call (if-let [values (get-in all [call :values site :values])]
              (peek values)
              :not-recorded)
       :else (let [values (into []
                                (keep-indexed (fn [i c]
                                                (when-let [vs (get-in all [c :values site :values])]
                                                  [(inc i) (peek vs)])))
                                (get @recorded unit))]
               (if (seq values) (iterations/by-call values) :not-recorded))))))

(defn- columns-of
  "The names of the code of the unit N bound each time round of the loop
  ITERATING says - see `iterating' - and in it, in the order written: [name
  site] pairs - or [name site i], of a param of a fn, the Ith value of what
  its entry, SITE, recorded."
  [n {:keys [loop owners]}]
  (let [names (scope/names-by-span (:form (get @units n)))
        l (some #(when (= loop (:site %)) %) (loops-of n))
        args (when (:fn l)
               (for [[name i] (:args l)
                     :let [[line column] (scope/span name)]
                     :when line]
                 [[line column] [(with-meta name nil) loop i]]))]
    (->> owners
         (keep (fn [[k owner]]
                 (let [site (get @sites k)]
                   (when (and (= loop owner) (= :local (:kind site)))
                     (when-let [name (get names (span-of site))]
                       [[(:line site) (:column site)] [name k]])))))
         (concat args)
         (sort-by first)
         (mapv second))))

(defn- in-loop?
  "Whether the site K of the unit N is evaluated again each time round of the
  loop whose name is bound at the site SITE."
  [n site k]
  (let [{:keys [line column]} (get @sites k)
        l (some #(when (= site (:site %)) %) (loops-of n))]
    (boolean (and l line (some #(scope/in-span? % [line column]) (:spans l))))))

(defn- loop-plan
  "What `table' says the loop of the unit N whose name is bound at the site
  SITE with: the loop, as `iterating' says it, and its :columns - and of the
  site FORM, where it is evaluated again each time round and is not a name of
  them, a column too, after them, named the code written there: what it
  evaluated to last in each iteration. With ONLY, the column of FORM alone,
  and :only - where there is one: FORM's, or one of the names, where it is
  one."
  [n site form only]
  (let [it (iterating n {:site site :index 1})
        columns (columns-of n it)
        named (when form (some #(when (= form (second %)) %) columns))
        code (when (and form (in-loop? n site form) (not named))
               (scope/form-at (:form (get @units n)) (span-of (get @sites form))))
        alone (or named (when code [code form]))]
    (cond-> {:iteration (cond-> (dissoc it :index) code (assoc-in [:owners form] site))
             :columns (cond-> columns code (conj [code form]))}
      (and alone only) (assoc :columns [alone] :only true))))

(defn loop-table
  "What each iteration of the loop whose name is bound at the site SITE
  evaluated to in the call CALL - the last one, without - by iteration: see
  `iterations/table'. The iterations kept only. With FORM, a site evaluated
  in the loop, what it evaluated to too: see `loop-plan' - and with ONLY,
  that alone, by iteration, of the iterations it was evaluated in."
  ([site call] (loop-table site call nil nil))
  ([site call form only]
   (let [unit (or (:unit (get @sites site))
                  (refuse :unknown-iteration "No site " site))
         plan (loop-plan unit site form only)
         c (or call (peek (get @recorded unit)))
         rows (iterations/table (kept-in (get @calls c)) (:iteration plan) (:columns plan)
                                values-kept)]
     (if (:only plan) (iterations/by-iteration rows) rows))))

(defn- calls-plan
  "The columns of the table of the calls of the unit N, as
  `iterations/calls-table' takes them: the params of each of its outermost
  fns, by the name written, or what destructures, the arg of the entry they
  are - and of the site FORM, where there is one and it is not an entry, a
  column too, after them, named the code written there."
  [n form]
  (let [code (:form (get @units n))
        arities (scope/all-arities code)
        params (for [[k s] (sort-by key @sites)
                     :when (and (= n (:unit s)) (= :enter (:kind s)) (:outermost s))
                     :let [arity (scope/entered-arity arities (:marker s))]
                     :when arity
                     {:keys [pattern arg]} (scope/params arity)]
                 [(if (symbol? pattern) (with-meta pattern nil) pattern) k arg])
        form-site (get @sites form)]
    (cond-> (vec params)
      (and form-site (not= :enter (:kind form-site)))
      (conj [(scope/form-at code (span-of form-site)) form]))))

(defn calls-table
  "What each call kept of the unit N evaluated to, by call - see
  `calls-plan' and `iterations/calls-table'. With FORM, a site, what it
  evaluated to last in each call too."
  [n form]
  (when-not (get @units n)
    (refuse :unknown-unit "No unit " n))
  (let [all @calls]
    (iterations/calls-table (mapv #(kept-in (get all %)) (get @recorded n []))
                            (calls-plan n form))))

(defn- runtime-columns
  "COLUMNS, [name site ...]s, as the runtime takes them: each name a string."
  [columns]
  (mapv (fn [[name & more]]
          (into [(if (symbol? name) (str name) (pr-str name))] more))
        columns))

(defn- for-site
  "The iteration ITERATING says - see `iterating' - as the runtime takes it
  for the site SITE alone."
  [iterating site]
  (-> iterating
      (update :owners #(vec (select-keys % [site])))
      (update :starts vec)))

(declare cursor-plan)

(defn for-runtime
  "The :debug source of a view of a ClojureScript value recorded, SOURCE, as
  the runtime takes it: an :iteration, a :loop, the table of the :calls of a
  unit, or what is at a :cursor - the :plan of it - said with what the
  runtime does not know - see `iterating', `calls-plan' and `cursor-plan'."
  [{:keys [site iteration loop only calls cursor] :as source}]
  (let [unit (:unit (get @sites (or site loop)))]
    (cond
      cursor
      {:plan (cursor-plan cursor)}

      calls
      (assoc source :columns (runtime-columns (calls-plan calls site)))

      (and site iteration)
      (assoc source :iteration (for-site (iterating unit iteration) site))

      loop
      (-> (loop-plan unit loop site only)
          (update-in [:iteration :owners] vec)
          (update-in [:iteration :starts] vec)
          (update :columns runtime-columns)
          (merge (select-keys source [:call]))
          (assoc :loop loop :kept values-kept))

      :else source)))

(defn- entered!
  "The thread entered the fn of the site N, called with ARGS - or with what
  it does not say, ARGS nil."
  [n args]
  (when-not *in-frame*
    (when-let [{:keys [starts unit]} (get @recording n)]
      (when starts (call-started! unit (return-of n)))
      ;; a fn of no param was called with nothing to show
      (when (seq args) (record! unit n args)))
    (reached! n nil)))

(defn enter
  "What code compiled to be stepped through calls on entering a fn, with what
  it was called with: (enter n args) - see Compiler/INSTRUMENT's :enter."
  [n args]
  (entered! n args)
  nil)

(defn hook
  "What code compiled to be stepped through calls: (hook n) on entering a fn,
  where the compiler has no :enter, (hook n value) once a form has its value.
  As cheap as can be where no thread steps and nothing records, since it is
  called for every form."
  ([n]
   (entered! n nil)
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
  [n {:keys [ns file line column end-line end-column entry record dialect target calls-only]}]
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
           record (assoc :record true)
           ;; what it is written is not recorded, nor marked
           calls-only (assoc :calls-only true))))

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
        (forget! (get before n)))
      (swap! in-calls (fn [m]
                        (into {} (keep (fn [[id in]]
                                         (let [in (filterv #(not= n (:unit %)) in)]
                                           (when (seq in) [id in]))))
                              m)))
      (when (var? var) (remove-watch var [::unit n]))
      (let [[before _] (swap-vals! wrapping dissoc n)]
        (when-let [{:keys [^clojure.lang.Var var original wrapper]} (get before n)]
          (remove-watch var [::calls n])
          (when (identical? wrapper (.getRawRoot var))
            (alter-var-root var (constantly original)))))
      (when (= :cljs dialect) (cljs-forget! n unit))
      (when shown
        (output/broadcast-event! (protocol/event "debug-uninstrumented" {:unit n})))
      ;; the calls only of a function are recorded for the calls that code
      ;; recorded makes: with none of it left in that runtime, they go too
      (when (and (:record unit) (not (:calls-only unit)))
        (let [same? #(and (= dialect (:dialect %)) (= (:target unit) (:target %)))
              left @units]
          (when-not (some #(and (:record %) (not (:calls-only %)) (same? %)) (vals left))
            (doseq [[k u] left :when (and (:calls-only u) (same? u))]
              (uninstrument! k)))))
      true)))

(defn- underlying
  "The function F wraps, where something holding a var for a while puts a
  function of its own in it - see \"Recording calls only\" - and F otherwise."
  [f]
  (if-let [wrapped (::wraps (meta f))] (recur wrapped) f))

(defn- definition?
  "Whether FORM defines something, which is what the compiler tells by the
  name of what it calls: the fns it makes are called later, by others."
  [form]
  (and (seq? form) (symbol? (first form)) (.startsWith (name (first form)) "def")))

(defn- returning-literals
  "FORM, with the literal map, vector or set each fn written in it returns -
  the last form of an arity - written (do literal), where the literal is: a
  literal is not a form the compiler records the value of, and what a call
  returned is what it is looked at for. The rest of FORM is FORM's own."
  [form]
  (let [returned (java.util.IdentityHashMap.)]
    (doseq [a (scope/all-arities form)
            :let [x (last (:body a))]
            :when (and (or (map? x) (vector? x) (set? x)) (scope/span x))]
      (.put returned x (with-meta (list 'do x) (meta x))))
    (if (.isEmpty returned)
      form
      (letfn [(walk [x]
                (if (.containsKey returned x)
                  (.get returned x)
                  (let [same (fn [xs ys] (every? true? (map identical? xs ys)))]
                    (cond
                      (and (seq? x) (= 'quote (first x))) x
                      (seq? x) (let [ys (doall (map walk x))]
                                 (if (same x ys) x (with-meta (apply list ys) (meta x))))
                      (map? x) (let [ys (mapv (fn [[k v]] [(walk k) (walk v)]) x)]
                                 (if (same (mapcat identity x) (mapcat identity ys))
                                   x
                                   (with-meta (into {} ys) (meta x))))
                      (or (vector? x) (set? x))
                      (let [ys (mapv walk x)]
                        (if (same x ys) x (into (empty x) ys)))
                      :else x))))]
        (walk form)))))

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
            evaluated (cond-> form record returning-literals)
            v (try (with-bindings {instrument {:site (partial site! (str *ns*) *file* n evaluated)
                                               :hook #'hook
                                               :enter #'enter
                                               ;; what each local was bound to,
                                               ;; which is what is recorded
                                               :locals (boolean record)}
                                   #'*compiler-options* (assoc *compiler-options*
                                                               :disable-locals-clearing true)}
                     (eval evaluated))
                   (catch Throwable t
                     (uninstrument! n)
                     (throw t))
                   (finally
                     (when evaluation (call-ended! n evaluation))))]
        (if-not (or definition record)
          (uninstrument! n)
          (let [unit (merge {:ns (str *ns*) :file *file* :shown true :entry entry
                             :record record :definition definition :form form}
                            (select-keys where [:line :column :end-line :end-column])
                            (when (var? v) {:var v}))]
            ;; the evaluation of a function is not one of its calls, which are
            ;; what is looked at
            (when (and evaluation (var? v) (fn? @v))
              (forget! [evaluation]))
            (swap! units assoc n unit)
            ;; defined again, the ordinary way or not: the code it holds is
            ;; not this unit's any more. Wrapped or let go of to record
            ;; its calls only, it is still the same code
            (when (var? v)
              (add-watch v [::unit n] (fn [_ _ old now]
                                        (when-not (identical? (underlying old) (underlying now))
                                          (uninstrument! n)))))
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

(defn- shown-said
  "Of SAID, the values of the unit N recorded as `:debug-recorded' answers
  them - each with its :site - the ones shown, each with the :kind of its site
  said where it is a local: a local bound to a form whose value is shown is
  not, being that form's - see `scope/plain-locals'."
  [n said]
  (let [plain (scope/plain-locals (:form (get @units n)))]
    (into [] (keep (fn [v]
                     (let [site (get @sites (:site v))]
                       (cond
                         (not= :local (:kind site)) v
                         (contains? plain (mapv site [:line :column :end-line :end-column])) nil
                         :else (assoc v :kind "local")))))
          said)))

(defn- cljs-recorded
  "What the ClojureScript unit N recorded, as `:debug-recorded' answers it -
  asked of the runtime, which keeps it, and placed here, where its sites are -
  of the iteration ITERATING says, where it says one, see `iterating'."
  [n call iterating]
  (let [unit (get @units n)]
    (when (cljs-unit-gone? unit)
      (uninstrument! n)
      (refuse :unknown-unit "Unit " n " was recorded in a page that is gone"))
    (let [answer (binding [cljs/*target* (:target unit)]
                   (cljs-op (cond-> {:op "recorded" :unit n}
                              call (assoc :call call)
                              ;; pairs, a JSON object's keys being strings
                              iterating (assoc :iteration
                                               (update iterating :owners #(vec %))))))
          thread (name (:target unit))
          times (:times answer)]
      (when-let [why (:error answer)]
        (refuse (case (:kind answer)
                  "unknown-call" :unknown-call
                  "unknown-unit" :unknown-unit
                  "unknown-iteration" :unknown-iteration
                  :runtime)
                why))
      (-> answer
          (dissoc :times)
          (assoc :loops (loops-said n #(get times %)))
          (cond-> iterating (assoc :iteration {:site (:loop iterating)
                                               :index (:index iterating)}))
          (update :calls (fn [cs] (mapv #(assoc % :thread thread) cs)))
          (update :values (fn [vs]
                            (shown-said
                             n (mapv (fn [v]
                                       (let [site (get @sites (:site v))]
                                         (cond-> (merge v (select-keys site [:line :column
                                                                             :end-line :end-column]))
                                           (:args v) (assoc :kind "enter"
                                                            :params (:params site [])))))
                                     vs))))))))

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
        evaluated (returning-literals form)
        r (try (cljs-runtime-half!)
               (cljs/with-instrument* {:site (partial site! ns file n evaluated)
                                       :hook (cljs-hook n definition)
                                       :locals true}
                                      #(cljs/eval-form evaluated opts))
               (catch Throwable t
                 (uninstrument! n)
                 (throw t)))]
    (if (= :error (:status r))
      (do (uninstrument! n) r)
      (let [unit (merge {:dialect :cljs :target cljs/*target* :page (cljs/page)
                         :ns ns :file file :shown true :entry false :record true
                         :definition definition :form form}
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
                           ;; a function recorded by its calls is wrapped
                           ;; again, defined again - see debug.cljs
                           (and (not (:calls-only unit))
                                (or (contains? vars (some-> (:var unit) str))
                                    (contains? nss (:ns unit))))))]
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

;;; Recording calls only

;; A function whose code is not recorded - a library's, in a jar - can be
;; recorded by its calls: what each was called with, and what it returned or
;; threw, a call among the others, in the call that made it. Its var is made
;; to hold a function that records the call and calls the one it held - and
;; defined again, it holds that again, wrapped again. Nothing is compiled
;; again. What cannot be seen is a call that does not go through the var: a
;; direct-linked one, or one of a function captured as a value before.
;;
;; WHAT IS RECORDED IS THE CALLS MADE IN A CALL RECORDED: a library function
;; is looked at as what the code recorded handed it and got back, and is
;; called everywhere else too, where a call is not worth a stack walk, nor a
;; place among the calls kept. So it is recorded for as long as code is
;; recorded in its runtime, and let go of with the last of it - see
;; `uninstrument!'.
;;
;; A unit of its own, as a definition recorded is, whose sites are made up:
;; its entry, which has what it was called with, and what it returned, and
;; what it threw - written nowhere, and said by their :label.

(defn- calls-only-sites!
  "Number the sites of the unit N, recorded by its calls only, of the
  namespace NS: [entry returned threw]. The params, of a function of one
  arity - ARGLISTS - are what its entry is shown under."
  [n ns arglists]
  (let [site! (fn [site]
                (let [k (swap! site-numbers inc)]
                  (swap! sites assoc k (assoc site :unit n :ns ns))
                  k))
        params (when (= 1 (count arglists)) (first arglists))
        entry (site! {:kind :enter :outermost true
                      :label (if (vector? params) (pr-str params) "args")})
        returned (site! {:kind :value :label "returned"})
        threw (site! {:kind :value :label "threw"})]
    ;; a call is over once it returned, see `record!'
    (swap! returns assoc entry returned)
    [entry returned threw]))

(defn- calls-recorder
  "What the var of the unit N holds while its calls are recorded, F being
  what it held: SITES are the unit's, see `calls-only-sites!'. A call made in
  no call recorded is only made."
  [n [entry returned threw] f]
  (with-meta
   (fn recorded [& args]
     (if (or *in-frame* (empty? (get @in-calls (.threadId (Thread/currentThread)))))
       (apply f args)
       (let [c (call-started! n returned)]
         (when (seq args) (record! n entry (vec args)))
         (try
           (let [r (apply f args)]
             (record! n returned r)
             r)
           (catch Throwable t
             (record! n threw t)
             (call-ended! n c)
             (throw t))))))
   {::wraps f}))

(defn- unrecordable
  "Why the calls of the function F, the root of the var V, cannot be recorded
  - or nil."
  [^clojure.lang.Var v f]
  (let [m (meta v)]
    (cond
      (:macro m) "a macro"
      (.isDynamic v) "dynamic: what is bound is not the var's"
      (:protocol m) "a protocol's method"
      (not (fn? f)) "not a function"
      (some #(.startsWith (.getName ^Class %) "clojure.lang.IFn$") (supers (class f)))
      "it takes or returns primitives, which a caller calls it with directly")))

(defn- rewrapped!
  "The var of the unit N, whose calls are recorded, was defined again to NOW:
  wrapped again - or, defined to what cannot be, let go of."
  [n now]
  (when-let [{:keys [var wrapper sites]} (get @wrapping n)]
    (when-not (identical? wrapper now)
      (if (unrecordable var now)
        (uninstrument! n)
        (let [w (calls-recorder n sites now)]
          (swap! wrapping update n assoc :original now :wrapper w)
          (alter-var-root var (constantly w)))))))

(defn record-calls!
  "Record the calls of the function the var V holds - see \"Recording calls
  only\". Answers why it cannot be, or nil - where it is, already."
  [^clojure.lang.Var v]
  (let [f (.getRawRoot v)]
    (or (unrecordable v f)
        (when-not (some #(= v (:var %)) (vals @wrapping))
          (let [n (swap! unit-numbers inc)
                m (meta v)
                ns (str (.ns v))
                ss (calls-only-sites! n ns (:arglists m))
                unit (merge {:ns ns :file (:file m) :var v :shown true :entry false
                             :record true :definition true :calls-only true
                             :returns (subvec ss 1)}
                            (select-keys m [:line :column]))
                w (calls-recorder n ss f)]
            (swap! units assoc n unit)
            (swap! wrapping assoc n {:var v :original f :wrapper w :sites ss})
            (alter-var-root v (constantly w))
            (add-watch v [::calls n] (fn [_ _ _ now] (rewrapped! n now)))
            (output/broadcast-event! (protocol/event "debug-instrumented" (unit-described n unit)))
            nil)))))

(defn cljs-record-calls!
  "Record the calls of the functions of the vars NAMED, qualified symbols, in
  the runtime of `cljs/*target*' - see \"Recording calls only\", and
  replique/debug.cljs, which wraps them. Answers [name why] of each that
  cannot be, where it is already being none."
  [named]
  (cljs-runtime-half!)
  (let [page (cljs/page)
        already (set (for [[_ u] @units
                           :when (and (:calls-only u) (= cljs/*target* (:target u))
                                      (= page (:page u)))]
                       (:var u)))
        made (vec (for [q named :when (not (contains? already q))]
                    (let [n (swap! unit-numbers inc)
                          ss (calls-only-sites! n (namespace q) nil)]
                      (swap! units assoc n {:dialect :cljs :target cljs/*target* :page page
                                            :ns (namespace q) :var q :entry false :record true
                                            :definition true :calls-only true
                                            :returns (subvec ss 1)})
                      {:name (str q) :unit n :enter (ss 0) :return (ss 1) :threw (ss 2)})))
        answer (cljs-op {:op "wrap" :wraps made})
        _ (when-let [why (:error answer)]
            (doseq [{:keys [unit]} made] (uninstrument! unit))
            (refuse (keyword (:kind answer "runtime")) why))
        skipped (into {} (map (juxt :name :why)) (:skipped answer))]
    (doseq [{:keys [name unit]} made]
      (if (contains? skipped name)
        (uninstrument! unit)
        (let [u (get (swap! units update unit assoc :shown true) unit)]
          (output/broadcast-event! (protocol/event "debug-instrumented" (unit-described unit u))))))
    (vec (for [q named :let [why (get skipped (str q))] :when why] [q why]))))

(defn stop-value
  "What a view of the value the thread ID stopped after shows: the value,
  while the thread is stopped, and :running otherwise."
  [id]
  (if-let [pause (let [p (get @pauses id)] (when (:stopped p) p))]
    (:value pause)
    :running))

;;; Going through what was recorded

;; What was recorded is gone through a value at a time, the way a thread is
;; stepped through - back as well as forward, over the values kept. Where it
;; is at is a cursor: a call, and the number of one of its values, which says
;; the rest - the form that had that value, the iteration of each loop around
;; it, the locals in scope there and what they were, and the calls it is in.
;; A call of a unit that records, started on a thread while a call of another
;; one was running there, is that one's, made once it had as many values as
;; its :parent says. The calls the cursor is in, and the loops around it in
;; each, are its frames, innermost first - each at a form: the form of the
;; cursor, and in a call that made the next one in, the form whose value that
;; call was made for, the first one it had after.
;;
;; A client keeps the cursor, asks where a move takes it - `:debug-cursor' -
;; and browses what is there, a view of {:debug {:cursor ...}}. A move is made
;; from one of the frames: to the value after or before, into a call made
;; there, out of the call, to the same form in another iteration of a loop or
;; in another call of the same function. Where the frame is not the
;; innermost, the cursor goes on being in a call of the same function made
;; there, at the same form - as far down as there is one.
;;
;; Of ClojureScript, what is kept is asked of the runtime, which says the
;; numbers of the values - see replique/debug.cljs - and keeps the values,
;; which a view of the cursor is browsed there.

(defn- call-data
  "What is known of the call numbered C of this process: {:call :unit :thread
  :at :parent :children :until :calls :kept-of :events} - :calls the calls of its
  unit kept, :kept-of what is kept of each site, as replique.iterations takes
  it, and :events the values kept, [number site]s in order. Or nil where it
  is not kept."
  [c]
  (when-let [call (get @calls c)]
    (let [kept-of (kept-in call)]
      {:call c :unit (:unit call) :thread (:thread call) :at (:at call)
       :parent (:parent call) :children (:children call []) :until (:until call)
       :calls (get @recorded (:unit call) [])
       :kept-of kept-of
       :events (iterations/events kept-of (keys (:values call)))})))

(defn- cljs-call-data
  "What `call-data' says of the call C of the runtime of `cljs/*target*' - the
  last call of the unit UNIT, C nil - asked of it: the numbers of the values
  kept, without the values, which stay there."
  [unit c]
  (let [answer (cljs-op (cond-> {:op "call" :unit unit} c (assoc :call c)))]
    (when-not (:error answer)
      (let [sites (:sites answer)
            kept-of (fn [site]
                      (when-let [{:keys [times head tail]} (get sites site)]
                        {:times times
                         :head (mapv #(vector % nil) head)
                         :tail (mapv #(vector % nil) tail)}))]
        (assoc (select-keys answer [:call :unit :at :parent :children :until :calls])
               :thread (name cljs/*target*)
               :kept-of kept-of
               :events (iterations/events kept-of (keys sites)))))))

(defn- calls-of
  "How the calls of the runtime the unit UNIT records in are known: {:call-of
  :latest}, a fn answering `call-data' of a call, and one answering it of the
  last call of UNIT - each call asked of a ClojureScript runtime once."
  [unit]
  (let [u (get @units unit)]
    (when-not (:record u)
      (refuse :unknown-unit "Unit " unit " does not record"))
    (if (= :cljs (:dialect u))
      (do (when (cljs-unit-gone? u)
            (uninstrument! unit)
            (refuse :unknown-unit "Unit " unit " was recorded in a page that is gone"))
          (let [asked (atom {})
                ask (fn [c] (binding [cljs/*target* (:target u)] (cljs-call-data unit c)))]
            {:call-of (fn [c]
                        (if (contains? @asked c)
                          (get @asked c)
                          (let [d (ask c)] (swap! asked assoc c d) d)))
             :latest #(ask nil)}))
      {:call-of call-data
       :latest #(some-> (peek (get @recorded unit)) call-data)})))

(defn- site-at [d n] (some (fn [[k site]] (when (= k n) site)) (:events d)))
(defn- event-after [d n] (first (filter #(> (first %) n) (:events d))))
(defn- event-before [d n] (last (filter #(< (first %) n) (:events d))))
(defn- event-upto [d n] (last (filter #(<= (first %) n) (:events d))))

(defn- site-events
  "The numbers of the values of the site K kept of the call D, from FROM on
  and before TO - nil, for no end."
  [d k from to]
  (for [[n site] (:events d)
        :when (and (= k site) (<= from n) (or (nil? to) (< n to)))]
    n))

(defn- same-place
  "Of the values of the call D numbered FROM on and before TO, the one of the
  site K that is the ORDINALth of them, from 1, or the last - or nil where K
  has none there."
  [d k from to ordinal]
  (let [ns (site-events d k from to)]
    (when (seq ns) (nth ns (dec (min (max ordinal 1) (count ns)))))))

(defn- levels
  "The calls the cursor at the value numbered AT of the call D is in,
  innermost first: each {:data :at}, the call and the value it is at - and
  last, where the call that made the outermost one kept is not kept any
  more, {:gone c}."
  [call-of d at]
  (loop [d d, at at, acc []]
    (let [acc (conj acc {:data d :at at})
          [parent pos] (:parent d)
          made (or (:until d) pos)
          p (when (and parent (< (count acc) 1000)) (call-of parent))
          e (when p (or (event-after p made) (event-upto p made)))]
      (cond
        e (recur p (first e) acc)
        parent (conj acc {:gone parent})
        :else acc))))

(defn- arg-written
  "The name #(...) was written with for the param the reader named S - %,
  %2, %& - or nil, for another name."
  [s]
  (when-let [[_ i] (re-matches #"(?:p(\d+)|rest)__\d+#" s)]
    (cond (nil? i) "%&" (= "1" i) "%" :else (str "%" i))))

(defn- as-written
  "FORM with what #(...) read to written back so: (fn* [p1__42#] (f p1__42#))
  as #(f %) - a symbol printing so - and its params, where they are, as %."
  [form]
  (walk/prewalk
   (fn [x]
     (cond
       (and (symbol? x) (arg-written (name x))) (symbol (arg-written (name x)))
       (and (seq? x) (= 'fn* (first x)) (= 3 (count x)) (vector? (second x))
            (seq (remove #{'&} (second x)))
            (every? #(and (symbol? %) (arg-written (name %))) (remove #{'&} (second x))))
       (symbol (str "#" (pr-str (as-written (nth x 2)))))
       :else x))
   form))

(defn- code-text
  "The code FORM on one line, as long as a label is - on a line of its own."
  [form]
  (let [s (str/replace (binding [*print-length* 5 *print-level* 3] (pr-str (as-written form)))
                       #"\s+" " ")]
    (if (> (count s) 70) (str (subs s 0 69) "…") s)))

(defn- site-label
  "What the value of the site K of the unit UNIT is shown under: the code
  written there - the params, of a fn's entry."
  [unit k]
  (let [site (get @sites k)]
    (cond
      ;; made up, of a function recorded by its calls only
      (:label site) (:label site)
      (= :enter (:kind site))
      (str "[" (str/join " " (map #(or (arg-written %) %) (:params site))) "]")
      :else
      (or (some-> (scope/form-at (:form (get @units unit)) (span-of site)) code-text)
          (str "line " (:line site))))))

(defn- unit-label
  "What the unit UNIT defines, or where it is."
  [unit]
  (let [{:keys [var line calls-only]} (get @units unit)]
    (cond
      ;; a library's, which its namespace says
      (and calls-only var) (str (if (var? var) (symbol var) var))
      (var? var) (name (.sym ^clojure.lang.Var var))
      (symbol? var) (name var)
      line (str "line " line)
      :else "evaluation")))

(defn- loop-label
  "What the loop L of the code of the unit UNIT is called: its head and the
  names it binds each time round - fn and its params, of a fn."
  [unit l]
  (if (:fn l)
    (str "fn [" (str/join " " (:names l)) "]")
    (let [form (scope/form-at (:form (get @units unit)) (:span l))]
      (str/join " " (cons (if (seq? form) (first form) "loop") (:names l))))))

(defn- place
  "Where the site K is written, as a frame says it, and what it is."
  [k]
  (let [site (get @sites k)]
    (cond-> (select-keys site [:line :column :end-line :end-column])
      ;; the entry of a fn a macro made, at the form around it: not its params
      (:kind site) (assoc :site-kind (if (:made site) "made" (name (:kind site)))))))

(defn- loops-around
  "The loops of the code of the call D around its value numbered AT, that
  went round by then, innermost first: each of `loops-of', with the :index
  of the iteration AT is of - nil where that is not kept - and how many
  times it went round in the call, :of."
  [d at]
  (let [k (site-at d at)
        site (get @sites k)
        kept-of (:kept-of d)]
    (->> (loops-of (:unit d))
         (filter #(loop-has? % k site))
         reverse
         (keep (fn [l]
                 (when-let [of (iterations/times-round kept-of (:sites l))]
                   (let [index (iterations/index-at kept-of (:sites l) at)]
                     (when-not (= 0 index)
                       (assoc l :index index :of of)))))))))

(defn- frames-of
  "The frames of the cursor in the calls LEVELS - see `levels' - innermost
  first: in each call, the loops around where it is, then the call itself.
  The :level of each is the call it is of, and its :form the site the cursor
  is at in that call, with the :code written there; a call frame says which
  iteration of the innermost loop around it is, where that is kept."
  [levels]
  (vec
   (mapcat
    (fn [j {:keys [data at gone]}]
      (if gone
        [{:kind "gone" :level j :call gone}]
        (let [unit (:unit data)
              loops (loops-around data at)
              calls (:calls data)
              index (.indexOf ^java.util.List calls (:call data))
              k (site-at data at)
              form {:form k :code (site-label unit k)}]
          (concat
           (for [l loops]
             (merge {:kind "loop" :level j :unit unit :call (:call data) :loop (:site l)
                     :label (loop-label unit l) :of (:of l)}
                    form
                    (when (:fn l) {:fn true})
                    (when (:index l) {:index (:index l)})
                    ;; the form the cursor is at, in the loop as in its call
                    (place k)))
           [(merge {:kind "call" :level j :unit unit :call (:call data)
                    :label (unit-label unit) :of (count calls)
                    :thread (:thread data) :started (:at data)}
                   form
                   (when-not (neg? index) {:index (inc index)})
                   (when-let [l (first (filter :index loops))]
                     {:iteration {:site (:site l) :index (:index l)}})
                   (place k))]))))
    (range) levels)))

(defn- made-until
  "The number of the last value the call that made the call MADE, made once
  it had POS values, had before it was over: the next one is the one it was
  made for."
  [made pos]
  (or (:until made) pos))

(defn- descend
  "Where the cursor goes, the value numbered AT of the call D taking the place
  of the one the calls BELOW were made for - innermost first, each {:data
  :at}: in each, the call of the same function made there, at the same form,
  as many values of it in as it was - as far down as there is one. [call at
  note]."
  [call-of d at below]
  (loop [d d, at at, [old & more] (reverse below)]
    (if-not old
      [d at nil]
      (let [unit (:unit (:data old))
            k (site-at (:data old) (:at old))
            ordinal (count (site-events (:data old) k 0 (inc (:at old))))
            from (or (first (event-before d at)) 0)
            child (->> (:children d)
                       (filter (fn [[pos _]] (< pos at)))
                       (keep (fn [[pos c]] (some-> (call-of c) (vector pos))))
                       (filter (fn [[made pos]] (<= from (made-until made pos) (dec at))))
                       (map first)
                       (filter #(and (= unit (:unit %)) (seq (:events %))))
                       last)]
        (cond
          (nil? child) [d at (str (unit-label unit) " was not called there")]
          (same-place child k 0 nil ordinal) (recur child (same-place child k 0 nil ordinal) more)
          :else [child (ffirst (:events child)) nil])))))

(defn- unit-called
  "The unit recording the function the form at the value numbered AT of the
  call D calls - or nil, where it calls none recorded."
  [d at]
  (let [unit (get @units (:unit d))
        form (scope/form-at (:form unit) (span-of (get @sites (site-at d at))))
        head (when (seq? form) (first form))
        called (when (symbol? head)
                 (if (= :cljs (:dialect unit))
                   (symbol (or (namespace head) (:ns unit)) (name head))
                   (some-> (find-ns (symbol (:ns unit))) (ns-resolve head))))]
    (when called
      (some (fn [[n u]]
              (when (and (:record u) (= called (:var u)))
                n))
            @units))))

(defn- into-call
  "Where a move into a call made at the value numbered AT of the call D goes:
  the first value of the call it was made for, over since the value before -
  or of the call made after it, before the value after. What it was called
  with, of a fn's: that call is gone through from its start."
  [call-of d at]
  (let [before (or (first (event-before d at)) 0)
        after (first (event-after d at))
        made (keep (fn [[pos c]]
                     (when-let [made (call-of c)]
                       (when (seq (:events made)) [pos made])))
                   (:children d))]
    (if-let [[_ made] (or (last (filter (fn [[pos made]] (<= before (made-until made pos) (dec at)))
                                        made))
                          (first (filter (fn [[pos]] (and (<= at pos) (or (nil? after) (< pos after))))
                                         made)))]
      [made (ffirst (:events made)) nil]
      (if-let [unit (unit-called d at)]
        (refuse :no-move (unit-label unit) " was called here, but that call is not"
                " recorded: it was not being recorded then, or it is not kept"
                " any more")
        (refuse :no-move "No call recorded was made here")))))

(defn- iteration-move
  "Where a move to another iteration of a loop goes, from the frame F of the
  cursor of FRAMES, whose calls are LEVELS: the same form in the next one
  kept, MOVE \"next-iteration\", the one before, or the iteration INDEX,
  \"iteration\". Of the loop of F, or of the innermost one of its call."
  [call-of levels frames f move index]
  (let [level (:level f)
        l (if (= "loop" (:kind f))
            f
            (or (some #(when (and (= level (:level %)) (= "loop" (:kind %))) %) frames)
                (refuse :no-move "No loop went round around here")))
        {d :data at :at} (get levels level)
        unit (:unit d)
        kept-of (:kept-of d)
        current (:index l)
        _ (when (and (nil? current) (not= "iteration" move))
            (refuse :no-move "Which iteration this is is not kept"))
        kept (iterations/kept-indices kept-of (:loop l) values-kept)
        target (case move
                 "next-iteration" (first (filter #(> % current) kept))
                 "previous-iteration" (last (filter #(< % current) kept))
                 "iteration" (some #{index} kept))
        _ (when-not target
            (refuse :no-move (case move
                               "next-iteration" "This is the last iteration kept"
                               "previous-iteration" "This is the first iteration kept"
                               (str "Iteration " index " is not kept"))))
        range-of (fn [i] (get (iterations/ranges kept-of (iterating unit {:site (:loop l) :index i}))
                              (:loop l)))
        [from to] (or (range-of target) (refuse :no-move "Iteration " target " is not kept"))
        k (site-at d at)
        ordinal (if-let [[from-now] (when current (range-of current))]
                  (count (site-events d k from-now (inc at)))
                  1)
        skipped (when (and current (not= "iteration" move) (> (abs (- target current)) 1))
                  (str "Iterations " (inc (min target current)) " to " (dec (max target current))
                       " are not kept"))]
    (if-let [n (same-place d k from to ordinal)]
      (let [[made at note] (descend call-of d n (subvec levels 0 level))]
        [made at (some->> (remove nil? [skipped note]) seq (str/join " - ")) (:call d)])
      [d (or (first (event-after d (dec from))) at)
       (str/join " - " (remove nil? [skipped (str (site-label unit k) " was not evaluated in"
                                                  " iteration " target)]))
       (:call d)])))

(defn- call-move
  "Where a move to another call of the same function goes, from the frame F
  of the cursor whose calls are LEVELS: the same form in the call after,
  MOVE \"next-call\", the one before, or the call INDEX among the ones kept,
  from 1, \"call\"."
  [call-of levels f move index]
  (let [level (:level f)
        {d :data at :at} (get levels level)
        calls (:calls d)
        i (.indexOf ^java.util.List calls (:call d))
        c (cond
            (neg? i) (refuse :no-move "This call is not kept any more")
            (= "next-call" move) (get calls (inc i))
            (= "call" move) (when (and index (pos? index)) (get calls (dec index)))
            :else (when (pos? i) (get calls (dec i))))
        _ (when-not c
            (refuse :no-move (case move
                               "next-call" "This is the last call kept"
                               "previous-call" "This is the first call kept"
                               (str "Call " index " is not kept"))))
        made (or (call-of c) (refuse :no-move "That call is not kept any more"))
        k (site-at d at)]
    (when (empty? (:events made))
      (refuse :no-move "That call recorded nothing kept"))
    (if-let [n (same-place made k 0 nil (count (site-events d k 0 (inc at))))]
      (conj (descend call-of made n (subvec levels 0 level)) c)
      [made (ffirst (:events made)) (str (site-label (:unit d) k) " was not evaluated in that call")
       c])))

(defn- made-event?
  "Whether the value [n site] of a call is the entry of a fn a macro made -
  written nowhere, gone past."
  [[_ site]]
  (:made (get @sites site)))

(defn- not-kept-between
  "What says how many values of the call D between the numbers FROM and TO
  were not kept, or nil."
  [d from to]
  (let [kept (count (filter #(< from (first %) to) (:events d)))
        gone (- to from 1 kept)]
    (when (pos? gone) (str gone " values in between are not kept"))))

(def ^:private cursor-moves
  #{"next" "previous" "in" "out" "next-iteration" "previous-iteration" "iteration"
    "next-call" "previous-call" "call"})

(defn- move-cursor
  "Where the move MOVE takes the cursor whose calls are LEVELS and frames
  FRAMES, from the frame numbered FRAME - INDEX the iteration of a move
  \"iteration\", the call of a move \"call\": [call at note moved], MOVED the call the frame moved from
  is in after a move to another iteration or call - or refused, where it goes
  nowhere."
  [call-of levels frames frame move index]
  (let [f (get frames frame)
        {d :data at :at up :gone} (get levels (:level f))
        outer (get levels (inc (:level f)))]
    (when up (refuse :no-move "That call is not kept any more"))
    (case move
      "next" (if-let [[n] (first (remove made-event? (filter #(> (first %) at) (:events d))))]
               [d n (not-kept-between d at n)]
               (if (:data outer)
                 [(:data outer) (:at outer) nil]
                 (refuse :no-move "This is the last value kept of the outermost call")))
      "previous" (if-let [[n] (last (remove made-event? (filter #(< (first %) at) (:events d))))]
                   [d n (not-kept-between d n at)]
                   (if-let [[n] (when (:data outer) (event-upto (:data outer) (second (:parent d))))]
                     [(:data outer) n nil]
                     (refuse :no-move "This is the first value kept of the outermost call")))
      "out" (if (:data outer)
              [(:data outer) (:at outer) nil]
              (refuse :no-move "This is the outermost call kept"))
      "in" (into-call call-of d at)
      ("next-iteration" "previous-iteration" "iteration")
      (iteration-move call-of levels frames f move index)
      ("next-call" "previous-call" "call") (call-move call-of levels f move index))))

(defn- at-plan
  "What a view of the cursor at the value numbered AT of the call D shows,
  as `iterations/at-outline' takes it: the value, under the code written
  there, and the locals in scope at its form, as the code wrote them - each
  the last value it was bound to by then. A param, what its fn was called
  with; a name a let or a loop binds, what was recorded where it is written,
  or, where nothing was, what the form a let binds it to evaluated to. The
  innermost of the same name only."
  [d at]
  (let [unit (:unit d)
        form (:form (get @units unit))
        k (site-at d at)
        site (get @sites k)
        kept-of (:kept-of d)
        mine (filter #(= unit (:unit (val %))) @sites)
        by-span (fn [kind] (into {} (for [[n s] mine :when (= kind (:kind s))] [(span-of s) n])))
        local-sites (by-span :local)
        value-sites (by-span :value)
        arities (scope/all-arities form)
        entries (into {} (for [[n s] mine
                               :when (and (= :enter (:kind s)) (:marker s))
                               :let [a (scope/entered-arity arities (:marker s))]
                               :when a]
                           [(:span a) n]))
        bound (fn [label k arg]
                (when k
                  (let [l (iterations/latest (kept-of k) at)]
                    (cond
                      (= ::iterations/not-kept l) [label k nil arg]
                      l [label k (first l) arg]))))
        locals (when (:line site)
                 (for [{:keys [pattern init arity arg]} (scope/locals-at form [(:line site) (:column site)])
                       :let [names (scope/pattern-names pattern)
                             named (map #(get local-sites (scope/span %)) names)]
                       local (cond
                               (and (seq names) (every? some? named))
                               (map (fn [nm k] (bound (str nm) k nil)) names named)
                               arity [(bound (pr-str pattern) (get entries (:span arity)) arg)]
                               init [(bound (pr-str pattern) (get value-sites (scope/span init)) nil)]
                               :else [])
                       :when local]
                   local))]
    {:value [(site-label unit k) k at nil]
     :locals (->> (reverse locals)
                  (reduce (fn [[seen acc] l]
                            (if (seen (first l)) [seen acc] [(conj seen (first l)) (conj acc l)]))
                          [#{} []])
                  second
                  reverse
                  vec)}))

(defn- cursor-at
  "The call and the value the frame :frame of the cursor CURSOR - {:unit :call
  :at :frame} - is at, and the frames: [call at frames]."
  [{:keys [unit call at frame]}]
  (let [{:keys [call-of]} (calls-of unit)
        d (or (call-of call) (refuse :unknown-call "Call " call " is not kept any more"))
        ls (levels call-of d (or (first (event-upto d at)) (ffirst (:events d))))
        fs (frames-of ls)
        {:keys [data at]} (get ls (:level (get fs (or frame 0) (first fs))))]
    (when-not data
      (refuse :unknown-call "That call is not kept any more"))
    [data at fs]))

(defn cursor-value
  "What a view of the cursor CURSOR shows - see `at-plan'."
  [cursor]
  (let [[d at] (cursor-at cursor)
        kept-of (:kept-of d)]
    (iterations/at-outline (fn [k n]
                             (if-let [v (iterations/numbered (kept-of k) n)]
                               (:value v)
                               ::iterations/not-kept))
                           (at-plan d at))))

(defn- cursor-plan
  "What a view of the cursor CURSOR shows, as the runtime keeping its values
  takes it: `at-plan', and the :call."
  [cursor]
  (let [[d at] (cursor-at cursor)]
    (assoc (at-plan d at) :call (:call d))))

;; Where the move :move takes the cursor at the value :at of the call :call
;; of the unit :unit - or at the value of the site :site nearest it, before
;; it where there is one, or its last - from its frame :frame, the innermost
;; without. Without :call, a call of the unit is the last one, and so is a
;; call not kept any more, which is said - and not moved from. Answers the
;; cursor, its :frames, the frame moved from, and the :note of what was
;; passed by or not found
(defmethod protocol/handle :debug-cursor [_ msg]
  (let [{:keys [unit call at site frame move index]} msg]
    (when-not (and (integer? unit) (or (nil? call) (integer? call)) (or (nil? at) (integer? at))
                   (or (nil? site) (integer? site)) (or (nil? frame) (nat-int? frame))
                   (or (nil? move) (contains? cursor-moves move))
                   (or (nil? index) (integer? index)))
      (refuse :invalid-message "A cursor is at the value :at of the :call of a :unit, or at"
              " its :site, and moves by :move, one of " (str/join ", " (sort cursor-moves))
              " - from its :frame - got: "
              (pr-str (select-keys msg [:unit :call :at :site :frame :move :index]))))
    (let [{:keys [call-of latest]} (calls-of unit)
          [d note] (if-let [d (when call (call-of call))]
                     [d nil]
                     [(or (latest) (refuse :unknown-call "Unit " unit " was not called yet"))
                      (when call "That call is not kept any more - this is the last one")])
          _ (when (empty? (:events d))
              (refuse :unknown-call "That call recorded nothing kept"))
          at (if-let [ns (when site (seq (site-events d site 0 nil)))]
               (or (when (and at (not note)) (or (last (filter #(<= % at) ns)) (first ns)))
                   (last ns))
               (or (when (and at (not note)) (first (event-upto d at)))
                   (ffirst (:events d))))
          ls (levels call-of d at)
          fs (frames-of ls)
          frame (min (or frame 0) (dec (count fs)))
          f (get fs frame)
          [d at said moved] (if (and move (not note))
                              (move-cursor call-of ls fs frame move index)
                              [d at nil nil])
          fs (frames-of (levels call-of d at))
          ;; the frame moved from stays the one looked at, where it is still
          ;; there - or its call
          in-moved (fn [kind loop]
                     (first (keep-indexed (fn [i g]
                                            (when (and (= moved (:call g)) (= kind (:kind g))
                                                       (= loop (:loop g)))
                                              i))
                                          fs)))
          frame (if moved
                  (or (in-moved (:kind f) (:loop f)) (in-moved "call" nil) 0)
                  0)]
      (cond-> {:unit (:unit d) :call (:call d) :at at :frame frame :frames fs}
        (or note said) (assoc :note (or note said))))))

;;; The calls recorded, as a tree

;; The calls a client is shown, to go to one: the calls kept made in none, or
;; in one not kept any more - of every unit that records, on the JVM and in
;; each ClojureScript runtime - and the calls each made, each with what it was
;; called with and what it returned, where that is kept.

(defn- call-ends
  "The sites a call of the unit UNIT starts and ends at: [entries returns],
  the entries of its outermost fns - which record what it was called with -
  and the last form of each, whose value it returns."
  [unit]
  (let [entries (vec (for [[k s] (sort-by key @sites)
                           :when (and (= unit (:unit s)) (= :enter (:kind s)) (:outermost s))]
                       k))]
    [entries (or (:returns (get @units unit)) (vec (keep return-of entries)))]))

(defn- call-told
  "What a client is told of the call C, CALL, as the runtime keeping it says
  it: {:call :unit :calls :at :thread :parent :made :last}, :calls the calls
  of its unit kept, and :last the last value of each of the sites KS it
  has a value of - {:number :value}, and the :args, of an entry - printed."
  [c call ks]
  {:call c :unit (:unit call) :calls (get @recorded (:unit call) [])
   :at (:at call) :thread (:thread call)
   :parent (first (:parent call)) :made (count (:children call))
   :last (into {} (for [k ks
                        :let [{:keys [values numbers]} (get-in call [:values k])]
                        :when (seq values)]
                    [k (cond-> {:number (peek numbers)
                                :value (let [v (peek values)]
                                         ;; what a call recorded by its calls
                                         ;; only threw, which a line says
                                         (if (instance? Throwable v)
                                           (str (.getName (class v))
                                                (some->> (ex-message v) (str ": ")))
                                           (printed-briefly v)))}
                         (and (= :enter (:kind (get @sites k))) (vector? (peek values)))
                         (assoc :args (mapv printed-briefly (peek values))))]))})

(defn- call-summary
  "A call as a client is shown it, from what its runtime told - see
  `call-told': its :label, which of the calls kept of its unit it is,
  :index, of :of, what it was called with, :args, and returned, :value -
  where they are kept - how many calls it made that are kept, :made, and,
  among calls made in none kept, ROOT, whether it was made in one not kept
  any more, :made-in-gone - and, of a call recorded by its calls only that
  threw, :threw, its :value being what it threw. ENDS is `call-ends'."
  [ends root {:keys [call unit calls at thread parent made] :as told}]
  (let [[entries returns] (ends unit)
        latest (fn [ks] (->> (keep #(some-> (get (:last told) %) (assoc :site %)) ks)
                             (sort-by :number)
                             last))
        args (latest entries)
        value (latest returns)
        index (.indexOf ^java.util.List calls call)]
    (cond-> (merge {:unit unit :call call :label (unit-label unit) :thread thread :started at
                    :made (or made 0)}
                   ;; which runtime it is in, where it is not this one
                   (let [{:keys [dialect target]} (get @units unit)]
                     (when (= :cljs dialect) {:dialect "cljs" :target (name target)})))
      (not (neg? index)) (assoc :index (inc index) :of (count calls))
      args (assoc :args (:args args))
      value (assoc :value (:value value))
      (and value (= (:site value) (second (:returns (get @units unit))))) (assoc :threw true)
      (and root parent) (assoc :made-in-gone true))))

(defn- cljs-calls-told
  "What the runtime of `cljs/*target*' tells of its calls: the ones made in
  none, or in one it does not keep - or, with PARENT, the ones that call
  made - see `call-told'."
  [parent]
  (let [mine (for [[n u] @units :when (and (:record u) (= :cljs (:dialect u))
                                           (= cljs/*target* (:target u)))]
               n)
        sites (vec (mapcat #(apply concat (call-ends %)) mine))
        answer (cljs-op (cond-> {:op "calls" :sites sites} parent (assoc :parent parent)))]
    (when (:error answer)
      (refuse (keyword (:kind answer "runtime")) (:error answer)))
    (mapv #(assoc % :thread (name cljs/*target*)) (:calls answer))))

(defn- calls-told
  "The calls of this process made in none kept - or, with PARENT, the ones
  the call PARENT of the unit UNIT made - as their runtimes tell them, oldest
  first."
  [unit parent]
  (let [all @calls
        jvm (fn []
              (let [sites (vec (mapcat #(apply concat (call-ends %))
                                       (for [[n u] @units :when (and (:record u) (not= :cljs (:dialect u)))]
                                         n)))]
                (if parent
                  (for [[_ c] (get-in all [parent :children]) :let [call (get all c)] :when call]
                    (call-told c call sites))
                  (for [[c call] all
                        :when (not (contains? all (first (:parent call))))]
                    (call-told c call sites)))))
        targets (distinct (for [[_ u] @units
                                :when (and (:record u) (= :cljs (:dialect u))
                                           (not (cljs-unit-gone? u)))]
                            (:target u)))]
    (if parent
      (let [u (or (get @units unit) (refuse :unknown-unit "No unit " unit))]
        (if (= :cljs (:dialect u))
          (binding [cljs/*target* (:target u)]
            (cljs-calls-told parent))
          (vec (jvm))))
      (let [cljs (mapcat (fn [t] (binding [cljs/*target* t]
                                   (when (cljs/runtime-connected?) (cljs-calls-told nil))))
                         targets)]
        (->> (concat (jvm) cljs) (sort-by (juxt :at :call)) vec)))))

;; The calls kept made in none kept - or, with :unit and :call, the calls that
;; call made - each as `call-summary' says it
(defmethod protocol/handle :debug-calls [_ msg]
  (let [{:keys [unit call]} msg]
    (when-not (or (and (nil? unit) (nil? call)) (and (integer? unit) (integer? call)))
      (refuse :invalid-message "The calls a call made are asked by its :unit and its :call,"
              " got: " (pr-str (select-keys msg [:unit :call]))))
    {:calls (mapv (partial call-summary (memoize call-ends) (nil? call)) (calls-told unit call))}))

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

(defn site-ns
  "The namespace the code of the site N was read in - or nil, for a site
  nobody numbered, or a namespace removed since."
  [n]
  (some-> (get @sites n) :ns symbol find-ns))

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

;;; Code evaluated against a recorded call

;; A call recorded is what a function had, form by form, and what somebody
;; wants to try things against, the way a stopped thread's frame is - without
;; anything stopped. Code is evaluated against a call of a recorded unit at a
;; place of its code, with the locals in scope there bound to what they were
;; in one iteration of a loop of that call - which is how the first iteration
;; where something holds is found, see "The first iteration of a loop of a
;; recorded call" below.
;;
;; A local is what the code bound it to: a param, what it was called with,
;; and a name a let or a loop binds, what it was bound to - which the code
;; records itself, see Compiler/INSTRUMENT's :locals, and the :locals of
;; clojure.cljs.emitter/*instrument* for ClojureScript: the element of a doseq
;; too, and each name a destructuring binds. Where that was not recorded, a
;; local of a let is what the form it was bound to evaluated to, which was -
;; not evaluated again. One whose value is not known - a form that was not
;; evaluated in that iteration, a loop that did not go round - is not bound. A
;; value is the last in the iteration, or in the whole call for code the loop
;; does not go round, a form of a fn called many times having had many: what
;; the code shows.
;;
;; A ClojureScript call is evaluated against in the runtime that keeps its
;; values, the locals bound to what replique.debug/recorded-in answers there.

(defn recorded-in
  "What the site SITE evaluated to last in the iteration ITERATION,
  {:site :index}, of a loop of the call C - which code evaluated against a
  call binds its locals to, see \"Code evaluated against a recorded call\":
  :replique/not-recorded where it was not evaluated in it."
  [c site iteration]
  (let [call (or (get @calls c)
                 (throw (ex-info (str "Call " c " is not kept any more")
                                 {:replique/error :unknown-call})))
        v (iteration-of (:unit call) c site iteration)]
    (if (= :not-recorded v) :replique/not-recorded v)))

(defn- call-bindings
  "The bindings of a let binding the locals of the code of the unit UNIT in
  scope at POS, [line column], to what they were in a call - and the names of
  the ones whose value is not known, which are not bound.

  Of the call, HAD? says whether a site has a value in it, and VALUE answers
  the code that has that value, of a site; KNOWN? says whether a name the code
  has a local bound to is one there is outside of it."
  [unit pos had? value known?]
  (let [{:keys [form]} (get @units unit)
        had (fn [[n _]] (had? n))
        mine (filter (fn [[_ site]] (= unit (:unit site))) @sites)
        ;; the span of a form -> the site that has its value
        value-sites (into {} (for [[n site] mine
                                   :when (and (= :value (:kind site)) (had [n site]))]
                               [(mapv site [:line :column :end-line :end-column]) n]))
        ;; the span of an arity of the source -> the site of its entry
        arities (scope/all-arities form)
        entries (into {} (for [[n site] mine
                               :when (and (= :enter (:kind site)) (:marker site) (had [n site]))
                               :let [a (scope/entered-arity arities (:marker site))]
                               :when a]
                           [(:span a) n]))
        ;; the span of a name a let or a loop binds -> the site of what it
        ;; was bound to
        local-sites (into {} (for [[n site] mine
                                   :when (and (= :local (:kind site)) (had [n site]))]
                               [(mapv site [:line :column :end-line :end-column]) n]))
        value-of (fn [span] (some-> (get value-sites span) value))]
    ((juxt first peek)
     (reduce (fn [[bindings names unknown] {:keys [pattern init arity arg unbound]}]
               (let [these (scope/pattern-names pattern)
                     ;; what each name was bound to, where each was recorded:
                     ;; the element of a doseq too, and what destructures
                     bound (mapv #(get local-sites (scope/span %)) these)
                     bound-to (cond
                                (and (seq these) (every? some? bound)) ::bound
                                unbound ::scope/unknown
                                arity (if-let [n (get entries (:span arity))]
                                        (list 'nth (value n) arg)
                                        ::scope/unknown)
                                :else (scope/replayed init value-of
                                                      #(or (contains? names %) (known? %))))]
                 (cond
                   (= ::bound bound-to)
                   [(into bindings (mapcat (fn [s n] [(with-meta s nil) (value n)]) these bound))
                    (into names these)
                    (apply disj unknown these)]

                   (= ::scope/unknown bound-to)
                   ;; not the value of a local of the same name further out
                   (let [shadowed (filter names these)]
                     [(into bindings (mapcat (fn [s] [s :replique/not-recorded]) shadowed))
                      names
                      (into unknown these)])

                   :else
                   [(conj bindings pattern bound-to) (into names these) (apply disj unknown these)])))
             [[] #{} #{}]
             (scope/locals-at form pos)))))

(defn unit-ns
  "The namespace the code of the unit N was read in - or nil, for a unit gone,
  or a namespace removed since."
  [n]
  (some-> (get @units n) :ns symbol find-ns))

(defn unit-ns-name
  "The name of the namespace the code of the unit N was read in, of either
  dialect - or nil, for a unit gone."
  [n]
  (some-> (get @units n) :ns symbol))

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
    (if (and (zero? index) (:locals pause))
      (:locals pause)
      (or (get-in pause [:frames index])
          (do (ask! {:op :locals :thread id :frame index})
              (get-in @pauses [id :frames index]))))
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
;; evaluated in it. A fn entered is said with its :params and the :args it was
;; last called with, each printed, and a name a let or a loop bound, with the
;; :kind "local" - one bound to a form whose value is shown being left out
(defn- entered-said
  "SAID of the site SITE, which evaluated to V - with, where it is a fn's
  entry, its :params and the :args V is, each printed."
  [site said v]
  (if (and (= :enter (:kind site)) (vector? v))
    (assoc said :kind "enter" :params (:params site []) :args (mapv printed-briefly v))
    said))

(defmethod protocol/handle :debug-recorded [_ msg]
  (let [{:keys [unit call iteration]} msg]
    (when-not (integer? unit)
      (refuse :invalid-message "A unit is named by its number, got: " (pr-str unit)))
    (when-not (or (nil? call) (integer? call))
      (refuse :invalid-message "A call is named by its number, got: " (pr-str call)))
    (when-not (:record (get @units unit))
      (refuse :unknown-unit "Unit " unit " does not record"))
    (when-not (or (nil? iteration)
                  (and (map? iteration) (integer? (:site iteration))
                       (integer? (:index iteration))))
      (refuse :invalid-message "An iteration is {:site :index}, got: " (pr-str iteration)))
    (let [iteration (when iteration
                      (iterating unit {:site (long (:site iteration))
                                       :index (long (:index iteration))}))]
      (if (= :cljs (:dialect (get @units unit)))
        (cljs-recorded unit (some-> call long) iteration)
        (let [kept (get @recorded unit [])
              all @calls
              c (if call (long call) (peek kept))
              _ (when (and call (not (some #{c} kept)))
                  (refuse :unknown-call "Call " call " is not kept any more"))
              recorded (:values (get all c))
              pick (if iteration
                     (in-iteration iteration (kept-in (get all c)))
                     (fn [_ default] default))]
          (cond-> {:calls (mapv (fn [k] (let [{:keys [thread at]} (get all k)]
                                          {:call k :thread thread :at at}))
                                kept)
                   :loops (loops-said unit #(get-in recorded [% :times]))
                   :values (shown-said
                            unit
                            (for [[site {:keys [times values]}] (sort-by key recorded)
                                  :let [picked (pick site {:times times :value (peek values)})
                                        where (select-keys (get @sites site)
                                                           [:line :column :end-line :end-column])]
                                  ;; not code written: no params to show its args on
                                  :when (and picked (not (:made (get @sites site))))]
                              (if (= ::iterations/not-kept picked)
                                (merge {:site site :not-kept true} where)
                                (entered-said (get @sites site)
                                              (merge {:site site :times (:times picked)
                                                      :value (printed-briefly (:value picked))}
                                                     where)
                                              (:value picked)))))}
            c (assoc :call c)
            iteration (assoc :iteration {:site (:loop iteration)
                                         :index (:index iteration)})))))))

;; The first iteration of a loop of a recorded call where something holds:
;; CODE, evaluated with the locals of the code in scope at :line and :column
;; bound to what they were in each iteration kept - after the iteration
;; :from, or before it, :backward, where there is one - as "Code evaluated
;; against a recorded call" says.

(defn- searched
  "The indices of the iterations KEPT - [first last] ranges - to look at
  after the iteration FROM, or before it, BACKWARD, in that order."
  [kept from backward]
  (let [all (for [[a b] kept i (range a (inc b))] i)]
    (cond
      (nil? from) (if backward (reverse all) all)
      backward (reverse (filter #(< % from) all))
      :else (filter #(> % from) all))))

(defn- find-clj-iteration
  [unit c loop [line column] code indices]
  (let [ns (or (unit-ns unit) (refuse :unknown-unit "The namespace of unit " unit " is gone"))
        values (:values (get @calls c))
        k (gensym "iteration")
        [bindings _] (call-bindings unit [line column] #(contains? values %)
                                    (fn [n] `(recorded-in ~c ~n {:site ~loop :index ~k}))
                                    #(binding [*ns* ns] (resolve %)))
        f (binding [*ns* ns]
            (eval `(fn [~k] (let ~bindings ~(read-string code)))))]
    (iterations/first-where f indices)))

(defn- find-cljs-iteration
  [unit {c :call values :values} loop [line column] code indices]
  (let [{:keys [target]} (get @units unit)
        ns (unit-ns-name unit)
        it (iterating unit {:site loop :index 1})
        had (set (keep #(when-not (:not-kept %) (:site %)) values))
        k (gensym "iteration")]
    (binding [cljs/*target* target]
      (cljs/with-target-lock*
       #(cljs/with-ns* ns
          (fn []
            (let [[form] (cljs/read-form (cljs/reader code) ::eof)
                  [bindings unknown] (call-bindings
                                      unit [line column] had
                                      (fn [n] (list 'replique.debug/recorded-in unit c n
                                                    (assoc (for-site it n) :index k)))
                                      (fn [s] (some? (cljs/resolve-var ns s))))
                  {:keys [status value]}
                  (cljs/eval-form
                   (list 'replique.debug/first-where
                         (list 'fn [k] (list 'let (into bindings
                                                        (mapcat (fn [s] [s :replique/not-recorded]))
                                                        (sort unknown))
                                             form))
                         (vec indices)))]
              (if (= :success status)
                (edn/read-string value)
                (refuse :runtime value)))))))))

(defmethod protocol/handle :debug-find-iteration [_ msg]
  (let [{:keys [unit call site from backward line column code]} msg]
    (when-not (and (integer? unit) (integer? site) (or (nil? call) (integer? call))
                   (or (nil? from) (integer? from)) (pos-int? line) (pos-int? column)
                   (string? code))
      (refuse :invalid-message "An iteration is found in a :unit, a :call or the last,"
              " of the loop whose name is bound at :site, after the iteration :from, by"
              " the :code evaluated at :line and :column, got: "
              (pr-str (select-keys msg [:unit :call :site :from :line :column :code]))))
    (when-not (:record (get @units unit))
      (refuse :unknown-unit "Unit " unit " does not record"))
    (let [unit (long unit), site (long site), call (some-> call long)
          pos [line column]
          answer (if (= :cljs (:dialect (get @units unit)))
                   (let [said (cljs-recorded unit call nil)
                         {:keys [kept]} (or (some #(when (= site (:site %)) %) (:loops said))
                                            (refuse :unknown-iteration "Site " site
                                                    " starts no iteration of a loop of unit " unit))]
                     (find-cljs-iteration unit said site pos code (searched kept from backward)))
                   (let [c (or call (peek (get @recorded unit))
                               (refuse :unknown-call "Unit " unit " was not called yet"))
                         _ (when-not (contains? @calls c)
                             (refuse :unknown-call "Call " c " is not kept any more"))
                         {:keys [kept]} (or (some #(when (= site (:site %)) %)
                                                  (loops-said unit #(get-in @calls [c :values % :times])))
                                            (refuse :unknown-iteration "Site " site
                                                    " starts no iteration of a loop of unit " unit))]
                     (find-clj-iteration unit c site pos code (searched kept from backward))))]
      (cond-> {:index (:index answer)}
        (:error answer) (assoc :error (:error answer))))))

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
