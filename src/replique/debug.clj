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
  (:import [java.io BufferedReader InputStreamReader Writer OutputStreamWriter]
           [java.lang.management ManagementFactory]
           [java.nio.charset StandardCharsets]
           [java.util.concurrent TimeUnit]
           [java.util.concurrent.atomic AtomicLong]))

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
           :ids (AtomicLong. 0)
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
   (let [{:keys [^Writer in pending ^AtomicLong ids]}
         (or @debugger (refuse :no-debugger "The debugger is not running"))
         id (.incrementAndGet ids)
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

(defn- described
  "A pause as a client is told about it."
  [id {:keys [^Thread thread where]}]
  (let [{:keys [ns file line column cleared]} where]
    (merge {:thread id
            :name (.getName thread)}
           (sym/source-of file)
           (cond-> {:ns ns}
             line (assoc :line line)
             column (assoc :column column)
             ;; whether the function that stopped clears its locals, which a
             ;; client asked to start its call over has to warn about
             cleared (assoc :locals-cleared true)
             (repl-of thread) (assoc :connection (repl-of thread))))))

(defn- stopped!
  "Say that the thread ID stopped, as the debugger says it did."
  [id]
  (when-let [pause (get (swap! pauses #(cond-> % (get % id) (assoc-in [id :stopped] true))) id)]
    (output/broadcast-event! (protocol/event "debug-paused" (described id pause)))))

(defn- resumed! [id]
  (output/broadcast-event! (protocol/event "debug-resumed" {:thread id})))

(defn break*
  "Stop the calling thread, which has LOCALS in scope, at WHERE - see
  `break!'. Returns nil once it is told to continue, and throws where it is
  told to abort."
  [locals where]
  (when-not *in-frame*
    (cond
      (not (available?))
      (tell-once! :agent "(break!) did not stop: the process was not started with the JDWP"
                  " agent, which is what a debugger attaches to - see replique.debug")
      :else
      (when (try (the-debugger) true
                 (catch Throwable t
                   (binding [*out* *err*]
                     (println (str "replique: (break!) did not stop: " (.getMessage t))))
                   false))
        (let [thread (Thread/currentThread)
              id (.threadId thread)]
          (swap! pauses assoc id {:thread thread :where where :locals locals :frames {}})
          (pause-here id)
          ;; Told to continue. Not reached where the call was started over
          ;; instead: the frames this would run in are gone, and whoever
          ;; restarted it took the pause away
          (let [{:keys [abort]} (get @pauses id)]
            (swap! pauses dissoc id)
            (when abort
              (throw (ex-info "Aborted from the debugger" {:replique/aborted true})))
            nil))))))

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

(def ^:private printed-up-to
  "How much of a value run in a frame is printed, in chars."
  100000)

(defn- printed [x]
  (let [s (binding [*print-length* 100 *print-level* 10] (pr-str x))]
    (if (> (count s) printed-up-to) (str (subs s 0 printed-up-to) "...") s)))

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
    (some-> (if (zero? index)
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
    (swap! pauses assoc-in [id :abort] (boolean (:abort msg)))
    (ask! {:op :resume :thread id})
    (resumed! id)
    {:resumed true}))

(defmethod protocol/handle :debug-restart [_ msg]
  (let [id (thread-of msg)
        frame (frame-of msg)
        pause (the-pause id)]
    ;; Taken away before the call is made again, since the call is what may
    ;; stop the thread again - and put back where it could not be
    (swap! pauses dissoc id)
    (try (ask! {:op :restart :thread id :frame frame})
         (catch Throwable t
           (swap! pauses assoc id pause)
           (throw t)))
    (resumed! id)
    {:restarted true}))

(defonce ^:private evaluations (AtomicLong. 0))

(defn- evaluated
  "What came of running CODE in the frame FRAME of the stopped thread ID, as
  the `debug-evaluated' event says it - without the keys that say which."
  [id frame code]
  (try (edn/read-string (:printed (ask! {:op :eval :thread id :frame frame :code code}
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
    (let [evaluation (.incrementAndGet ^AtomicLong evaluations)]
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
