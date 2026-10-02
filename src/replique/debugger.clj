(ns replique.debugger
  "The other half of `replique.debug': a jvm of its own, attached to the
  process it debugs through JDWP, which is what stops a thread there and what
  works on its frames while it is stopped.

  A JVM CANNOT DEBUG ITSELF. What suspends a thread is the JDWP agent of the
  process, and what tells the agent to is a debugger on the other end of a
  socket - one that has to keep running while the threads it stopped do not.
  So this runs in a jvm started by the process, on the classpath the process
  was started with, and is spoken to over its standard input and output:

    process -> here   {:id 3 :op :frames :thread 41}
    here -> process   {:id 3 :result {...}}  or  {:id 3 :error \"...\"}
                      {:event :paused :thread 41}

  one EDN map a line, each way. The first line written is {:ready true}, once
  the breakpoint is set, or {:failed \"why\"} where it could not be.

  ONE BREAKPOINT, AND IT NEVER MOVES. It is on `replique.debug/pause-here', the
  trampoline: code that wants to stop calls it, and the breakpoint is what turns
  the call into a stop. Where to stop and whether to is decided in the process,
  by Clojure code that knows the forms - what this adds is what only a debugger
  can do to a stopped thread: list its frames and read their locals, run code on
  it with everything it has bound, and pop frames off it so that a call starts
  over.

  Only the thread that called stops - SUSPEND_EVENT_THREAD. The process has to
  keep answering the editor while one of its threads is stopped, and the
  threads that answer are threads of the same jvm.

  Nothing of replique is loaded here, and nothing that prints: standard output
  is the channel, and a line on it that is not a message is a line the process
  cannot read."
  (:require [clojure.edn :as edn])
  (:import [com.sun.jdi AbsentInformationException ArrayReference ArrayType
            BooleanValue ByteValue CharValue ClassType DoubleValue FloatValue
            IntegerValue LocalVariable Location LongValue Method ObjectReference
            PrimitiveValue ReferenceType ShortValue StackFrame StringReference
            ThreadReference Value VirtualMachine]
           [com.sun.jdi.connect AttachingConnector Connector$Argument]
           [com.sun.jdi.event BreakpointEvent EventSet VMDeathEvent VMDisconnectEvent]
           [com.sun.jdi.request EventRequest]
           [java.io BufferedReader InputStreamReader PrintStream]
           [java.nio.charset StandardCharsets]))

(def ^:private ^PrintStream channel
  ;; Taken before anything could replace it, and *out* sent to stderr: what
  ;; this namespace prints by mistake must not reach the process as a message
  (PrintStream. (java.io.FileOutputStream. java.io.FileDescriptor/out) true "UTF-8"))

(defn- send! [m]
  (let [line (binding [*print-length* nil *print-level* nil *print-meta* false]
               (pr-str m))]
    (locking channel
      (.println channel line))))

(def ^:private breakpoint-at
  "The class and method the one breakpoint is on - see `replique.debug'."
  ["replique.debug$pause_here" "invokeStatic"])

(def ^:private own-frames
  "The prefix of the classes of the frames above the one that asked to stop:
  the trampoline, and `replique.debug/break*' that called it. Nobody asked to
  see those."
  "replique.debug$")

;;; Attaching

(defn- attach
  "Attach to the jvm PID through its JDWP agent.

  By pid rather than by port: the process is started with the agent listening
  on a port of its own choosing - address=127.0.0.1:0 - and the attach
  mechanism is what can ask a jvm which one that was."
  ^VirtualMachine [pid]
  (let [manager (com.sun.jdi.Bootstrap/virtualMachineManager)
        ^AttachingConnector connector
        (or (first (filter #(= "com.sun.jdi.ProcessAttach" (.name ^AttachingConnector %))
                           (.attachingConnectors manager)))
            (throw (ex-info "This jvm has no process attaching connector" {})))
        args (.defaultArguments connector)]
    (.setValue ^Connector$Argument (get args "pid") (str pid))
    (.attach connector args)))

(defn- trampoline-method ^Method [^VirtualMachine vm]
  (let [[class-name method-name] breakpoint-at
        ^ReferenceType type (or (first (.classesByName vm class-name))
                                (throw (ex-info (str class-name " is not loaded in the process") {})))]
    (or (first (.methodsByName type method-name))
        (first (.methodsByName type "invoke"))
        (throw (ex-info (str class-name " has no " method-name) {})))))

;;; What is stopped

(defonce ^:private stopped
  ;; the java thread id the process knows a thread by -> its ThreadReference
  (atom {}))

(defn- unboxed
  "The number a Long in the debugged jvm holds."
  [^Value v]
  (cond
    (instance? LongValue v) (.value ^LongValue v)
    (instance? ObjectReference v)
    (let [^ObjectReference o v]
      (.value ^LongValue (.getValue o (.fieldByName (.referenceType o) "value"))))
    :else nil))

(defn- the-thread ^ThreadReference [id]
  (or (get @stopped id)
      (throw (ex-info (str "Thread " id " is not stopped") {}))))

;;; Frames

(defn- bridge?
  "Whether FRAME is the method of a function that only passes the call on to
  the frame inside it, which is the same function: a defn is compiled to an
  invoke that calls invokeStatic, and a variadic one to a doInvoke. Shown once,
  as the frame inside - which is also the one a restart pops."
  [^StackFrame frame ^StackFrame inner]
  (and inner
       (= (.declaringType (.location frame)) (.declaringType (.location inner)))
       (contains? #{"invoke" "doInvoke" "applyTo"} (.name (.method (.location frame))))))

(defn- shown-frames
  "The frames of THREAD that are shown, innermost first, each with its index
  among the frames of the thread - which is what a frame is popped by."
  [^ThreadReference thread]
  (let [frames (vec (.frames thread))
        start (or (first (keep-indexed
                          (fn [i ^StackFrame f]
                            (when-not (.startsWith (.name (.declaringType (.location f)))
                                                   ^String own-frames)
                              i))
                          frames))
                  0)]
    (loop [i start inner nil shown []]
      (if (< i (count frames))
        (let [^StackFrame frame (nth frames i)]
          (recur (inc i) frame (if (bridge? frame inner) shown (conj shown [i frame]))))
        shown))))

(defn- source-path [^Location location]
  (try (.sourcePath location) (catch AbsentInformationException _ nil)))

(defn- line-of
  "The line LOCATION is on, as the class says - its Java stratum. The
  Clojure one is what the compiler maps the lines of a file to, and a form
  evaluated at a repl is read from no file: it has lines, and no map of them."
  [^Location location]
  (.lineNumber location "Java"))

(defn- clojure-fn
  "What a frame of a Clojure function is called, ns/name - or nil where the
  class is not one. A function's class is named after it, munged, with a $
  between the namespace and the name; and the compiler says the class is its
  own by mapping its lines to a Clojure stratum, whether or not they were read
  from a file."
  [^ReferenceType type]
  (let [class-name (.name type)]
    (when (and (pos? (.indexOf class-name "$"))
               (some #{"Clojure"} (.availableStrata type)))
      (clojure.lang.Compiler/demunge class-name))))

(defn- describe-frame [index [_ ^StackFrame frame]]
  (let [location (.location frame)
        class-name (.name (.declaringType location))
        source (source-path location)
        line (line-of location)
        fn-name (clojure-fn (.declaringType location))]
    (cond-> {:index index
             :class class-name
             :method (.name (.method location))}
      source (assoc :source source)
      (pos? line) (assoc :line line)
      fn-name (assoc :fn fn-name))))

(defn- the-frame [^ThreadReference thread index]
  (or (nth (shown-frames thread) index nil)
      (throw (ex-info (str "There is no frame " index) {}))))

;;; Running code on a stopped thread

(defn- method-of ^Method [^ReferenceType type name arity]
  (or (first (filter #(and (= arity (count (.argumentTypeNames ^Method %)))
                           (not (.isVarArgs ^Method %)))
                     (.methodsByName type name)))
      (throw (ex-info (str (.name type) " has no " name " of " arity) {}))))

(defn- class-of ^ClassType [^VirtualMachine vm name]
  (or (first (.classesByName vm name))
      (throw (ex-info (str name " is not loaded in the process") {}))))

(defn- invoke-static [^ThreadReference thread class-name method-name & args]
  (let [type (class-of (.virtualMachine thread) class-name)]
    (.invokeMethod type thread (method-of type method-name (count args)) (vec args)
                   ObjectReference/INVOKE_SINGLE_THREADED)))

(defn- call
  "Call the var NS/NAME of the process with ARGS, on THREAD - which runs it,
  with whatever that thread has bound."
  [^ThreadReference thread ns name & args]
  (let [vm (.virtualMachine thread)
        ^ObjectReference var (invoke-static thread "clojure.lang.RT" "var"
                                            (.mirrorOf vm ^String ns) (.mirrorOf vm ^String name))]
    (.invokeMethod var thread (method-of (.referenceType var) "invoke" (count args)) (vec args)
                   ObjectReference/INVOKE_SINGLE_THREADED)))

(def ^:private boxes
  {BooleanValue ["java.lang.Boolean" "(Z)Ljava/lang/Boolean;"]
   ByteValue ["java.lang.Byte" "(B)Ljava/lang/Byte;"]
   CharValue ["java.lang.Character" "(C)Ljava/lang/Character;"]
   ShortValue ["java.lang.Short" "(S)Ljava/lang/Short;"]
   IntegerValue ["java.lang.Integer" "(I)Ljava/lang/Integer;"]
   LongValue ["java.lang.Long" "(J)Ljava/lang/Long;"]
   FloatValue ["java.lang.Float" "(F)Ljava/lang/Float;"]
   DoubleValue ["java.lang.Double" "(D)Ljava/lang/Double;"]})

(defn- boxed
  "V as an object of the debugged jvm: a primitive local is boxed there, by
  its valueOf, so that it can travel in an Object[]."
  [^ThreadReference thread ^Value v]
  (if (instance? PrimitiveValue v)
    (let [[class-name signature] (some (fn [[k box]] (when (instance? k v) box)) boxes)
          type (class-of (.virtualMachine thread) class-name)
          method (first (filter #(= signature (.signature ^Method %))
                                (.methodsByName type "valueOf")))]
      (.invokeMethod type thread method [v] ObjectReference/INVOKE_SINGLE_THREADED))
    v))

(defn- mirrored
  "X as a value of the debugged jvm: a string or a long is made there, nil is
  null, and a value that is already one is itself. What is made is passed to
  KEEP!, since nothing there references it until an invocation does - and an
  object nothing references is the collector's."
  [^ThreadReference thread keep! x]
  (let [vm (.virtualMachine thread)]
    (cond
      (nil? x) nil
      (instance? Value x) x
      (string? x) (keep! (.mirrorOf vm ^String x))
      (integer? x) (keep! (boxed thread (.mirrorOf vm (long x))))
      :else (throw (ex-info (str "Cannot make " (pr-str x) " in the process") {})))))

(defn- calling
  "Call the var NS/NAME of the process on THREAD with ARGS, made there - see
  `mirrored' - and let go of what was made once it returned. MADE is what was
  made beforehand for the same call, let go of with the rest."
  [^ThreadReference thread made ns name & args]
  (let [kept (atom (vec made))
        keep! (fn [^ObjectReference o] (.disableCollection o) (swap! kept conj o) o)]
    (try
      (apply call thread ns name (mapv #(mirrored thread keep! %) args))
      (finally (doseq [^ObjectReference o @kept]
                 (try (.enableCollection o) (catch Exception _ nil)))))))

(def ^:private slot-of
  "The slot of a local in its frame - or nil, where the debugger's jvm keeps
  it to itself.

  Which is the order the compiler bound the locals in, and the order they
  read in: it numbers a local as it binds it. What JDI says the locals of a
  frame are is in no order, and the local variable table lists the locals
  of an inner let before the ones of the let around it."
  (let [method (try (doto (.getDeclaredMethod (Class/forName "com.sun.tools.jdi.LocalVariableImpl")
                                              "slot" (make-array Class 0))
                      (.setAccessible true))
                    (catch Throwable _ nil))]
    (fn [v]
      (when method
        (try (.invoke ^java.lang.reflect.Method method v (object-array 0))
             (catch Throwable _ nil))))))

(defn- locals-array
  "The locals of FRAME as an Object[] of the debugged jvm - name, value, name,
  value - together with everything made there to build it.

  Read from the local variable table, which names a local the way the compiler
  munged it. Read before anything is invoked: an invocation resumes the thread
  for its duration, and the frames read before it are no longer valid after."
  [^ThreadReference thread index]
  (let [vm (.virtualMachine thread)
        [_ ^StackFrame frame] (the-frame thread index)
        pairs (try (let [vars (sort-by (juxt #(or (slot-of %) 0) #(.name ^LocalVariable %))
                                       (.visibleVariables frame))
                         values (.getValues frame vars)]
                     (mapv (fn [^LocalVariable v] [(.name v) (.get values v)]) vars))
                   (catch AbsentInformationException _ []))
        made (atom [])
        keep! (fn [^ObjectReference o] (when o (.disableCollection o) (swap! made conj o)) o)
        ^ArrayType type (class-of vm "java.lang.Object[]")
        ^ArrayReference array (keep! (.newInstance type (* 2 (count pairs))))]
    (doseq [[i [name value]] (map-indexed vector pairs)]
      (.setValue array (int (* 2 i)) ^Value (keep! (.mirrorOf vm ^String name)))
      (let [^Value v (boxed thread value)]
        (when (instance? ObjectReference v) (keep! v))
        (.setValue array (int (inc (* 2 i))) v)))
    [array @made]))

(defn- string-of [^Value v]
  (when (instance? StringReference v) (.value ^StringReference v)))

;;; The ops

(defmulti ^:private handle (fn [_ msg] (:op msg)))

(defmethod handle :frames [_ {:keys [thread]}]
  {:frames (vec (map-indexed describe-frame (shown-frames (the-thread thread))))})

(defn- frame-ns
  "The namespace the code of a frame was compiled in, as its class says."
  [^ThreadReference thread index]
  (let [[_ ^StackFrame frame] (the-frame thread index)
        class-name (.name (.declaringType (.location frame)))
        i (.indexOf class-name "$")]
    (when (pos? i) (clojure.lang.Compiler/demunge (subs class-name 0 i)))))

(defmethod handle :locals [_ {:keys [thread frame]}]
  (let [t (the-thread thread)
        [array made] (locals-array t frame)]
    (calling t made "replique.debug" "keep-locals!" thread frame array)
    {:kept true}))

(defmethod handle :eval [_ {:keys [thread frame code]}]
  (let [t (the-thread thread)
        ;; Frame 0 is the one that asked to stop, and asked with its locals
        ;; already in hand - written there by the compiler, unmunged and with
        ;; what it closed over - so only another frame has its read here
        [array made] (if (zero? frame) [nil []] (locals-array t frame))
        ns (when-not (zero? frame) (frame-ns t frame))]
    {:printed (string-of (calling t made "replique.debug" "eval-in!" thread code ns array))}))

(defmethod handle :resume [_ {:keys [thread]}]
  (let [t (the-thread thread)]
    (swap! stopped dissoc thread)
    (.resume t)
    {:resumed true}))

(defmethod handle :restart [_ {:keys [thread frame]}]
  (let [t (the-thread thread)
        [_ ^StackFrame f] (the-frame t frame)]
    ;; Every frame above it goes with it, and the call that made it is made
    ;; again: the instruction that called is executed once more, with the
    ;; arguments the popped frame had - which is why they must not have been
    ;; cleared, see `replique.debug'
    (.popFrames t f)
    (swap! stopped dissoc thread)
    (.resume t)
    {:restarted true}))

(defmethod handle :default [_ msg]
  (throw (ex-info (str "Unknown op " (pr-str (:op msg))) {})))

;;; Running

(defn- watch-events!
  "Tell the process about every thread that stops at the trampoline, until the
  jvm goes."
  [^VirtualMachine vm]
  (let [queue (.eventQueue vm)]
    (loop []
      (let [^EventSet events (.remove queue)
            gone? (atom false)]
        (doseq [e events]
          (cond
            (instance? BreakpointEvent e)
            (let [^ThreadReference thread (.thread ^BreakpointEvent e)
                  id (unboxed (first (.getArgumentValues (.frame thread 0))))]
              (swap! stopped assoc id thread)
              (send! {:event :paused :thread id}))
            (or (instance? VMDisconnectEvent e) (instance? VMDeathEvent e))
            (reset! gone? true)))
        (when-not @gone? (recur))))))

(defn- serve! [vm]
  (let [in (BufferedReader. (InputStreamReader. System/in StandardCharsets/UTF_8))]
    (loop []
      (when-let [line (.readLine in)]
        (let [msg (try (edn/read-string line) (catch Exception _ nil))]
          (when (map? msg)
            ;; Each on a thread of its own: code run on a stopped thread runs
            ;; for as long as it runs, and another thread's continue must not
            ;; wait for it. A virtual one, since all it does is wait on the
            ;; jdwp connection - and with *out* conveyed: the root binding is
            ;; the channel
            (.start (Thread/ofVirtual)
                    ^Runnable
                    (bound-fn []
                      (send! (try {:id (:id msg) :result (handle vm msg)}
                                  (catch Throwable t
                                    {:id (:id msg)
                                     :error (or (.getMessage t) (.getName (class t)))})))))))
        (recur)))))

(defn- attached
  "Attach to the jvm PID and set the one breakpoint. Returns the jvm, or says
  why it could not and exits."
  ^VirtualMachine [pid]
  (try
    (let [vm (attach pid)
          request (.createBreakpointRequest (.eventRequestManager vm)
                                            (.location (trampoline-method vm)))]
      (.setSuspendPolicy request EventRequest/SUSPEND_EVENT_THREAD)
      (.enable request)
      vm)
    (catch Throwable t
      (send! {:failed (str (or (.getMessage t) (.getName (class t)))
                           (when-let [c (.getCause t)] (str ": " (.getMessage c))))})
      (System/exit 1))))

(defn -main [pid]
  (binding [*out* *err*]
    (let [vm (attached pid)]
      (send! {:ready true})
      (doto (Thread. #(do (watch-events! vm) (System/exit 0)) "replique-debugger-events")
        (.setDaemon true)
        (.start))
      ;; The process closing its end is the process going, or letting go of
      ;; this one: whatever is still stopped is let go of with it, which is
      ;; what detaching does
      (serve! vm)
      (try (.dispose vm) (catch Throwable _ nil))
      (System/exit 0))))
