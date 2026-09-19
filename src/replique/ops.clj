(ns replique.ops
  "The ops handled by the control connection."
  (:require [clojure.java.basis :as basis]
            [clojure.repl.deps :as deps]
            [replique.analysis :as analysis]
            [replique.classpath :as classpath]
            [replique.completion :as completion]
            [replique.names :as names]
            [replique.protocol :as protocol]
            [replique.server :as server]
            [replique.symbol :as sym]
            [replique.state :as state]))

(defmethod protocol/handle :process-info [conn _]
  (let [info (state/info)]
    (assoc info
           :connection (:id conn)
           :uptime (when-let [started-at (:started-at info)]
                     (- (System/currentTimeMillis) started-at))
           ;; Whether the compiler of this process writes down what it
           ;; resolved, which is what :usages is answered out of. Here rather
           ;; than in the handshake reply, which is what `replique.state/info'
           ;; is: it is a fact about what this process can be asked, and the
           ;; handshake is about how to talk to it at all. A client needs no
           ;; such flag to ask - an op that cannot be answered says so, and
           ;; says what to start the process on instead - so this is for
           ;; somebody looking at the process rather than for the code path.
           :analysis (analysis/available?))))

;; Protocol smoke test. :value comes back both as JSON - which is lossy, EDN
;; keywords and symbols become strings - and as the EDN the process read, which
;; is not.
(defmethod protocol/handle :echo [_ msg]
  (protocol/frame {:value (:value msg)
                   :printed (pr-str (:value msg))}))

;; The namespaces the process has, for a client to offer a choice of. What
;; has been loaded rather than what is on the classpath: a repl can only be
;; moved into a namespace that exists, and one that exists only as a file is
;; one nothing can be evaluated in yet.
;;
;; Sorted here rather than by the client. It is the same order for every
;; client, it is the order somebody reading a list expects, and the client
;; that asked is about to show it to somebody.
(defmethod protocol/handle :namespaces [_ _]
  {:namespaces (vec (sort (map (comp str ns-name) (all-ns))))})

;; The names that could be written where a name is being written. What is
;; asked depends on the slot of the form point is in - a namespace, a var of
;; one, a class of a package - and reading that out of the text is the
;; client's half: it has the buffer, and it is the half that knows whether
;; what is being edited is Clojure or ClojureScript. What travels is the slot
;; it read and the text typed there, and what comes back is what could replace
;; that text.
(defmethod protocol/handle :completions [_ msg]
  (completion/completions (assoc msg :position (protocol/as-keyword (:position msg)))))

;; What the one name written there is, which is the other half of the same
;; question. An editor shows an arglist and a docstring while somebody writes
;; a call, and opens a file at a line when they ask where a name came from -
;; and both are what that name resolves to in that namespace, so both are
;; answered here and in one message.
;;
;; Asked with the request a completion is asked with: the same position, the
;; same text, the same namespace and locals and tag around it. Reading a name
;; out of a buffer is one job on the client's side, and this is the same
;; reading with point moved to the end of what it read.
(defmethod protocol/handle :symbol [_ msg]
  (sym/named (assoc msg :position (protocol/as-keyword (:position msg)))))

;; What a namespace calls the vars a client names.
;;
;; A tool that reads a form to work out what it means has to know which
;; symbol means which var, and inside a namespace that is not a settled
;; question. clojure.core/let is written let where core is referred, c/let
;; where core is aliased, and something else again where it was referred under
;; another name - while a namespace that excluded it and defined a let of its
;; own writes let for a var that is not this one at all. Only the process can
;; say, because only the process has the namespace.
;;
;; The client names the vars it cares about and reads the answer against its
;; own table, rather than this saying what each of them does. What a form does
;; with what it binds is the client's half - it is the half with the parse -
;; and a list of forms kept in both places is a list that has to agree in both
;; places. It also leaves this op with no opinion about binding at all: it
;; answers how a namespace writes a var, which is as true of a require as of a
;; let.

(defn- var-named
  "The var SYMBOL names, or nil when it names none."
  [symbol]
  (when-let [found (try (resolve symbol) (catch Exception _ nil))]
    (when (var? found) found)))

(defn- vars-asked
  "The vars a client asked about, by the name it wrote each of them as.

  A qualified name is required, because what is being asked is what a
  namespace calls clojure.core/let - let is the answer rather than the
  question, and a name with no namespace on it names nothing to ask about.

  One that resolves to nothing is left out rather than refused. That is a fact
  about the process, which may not have loaded the namespace it names, where a
  name that is not a name at all is a message written wrongly."
  [vars]
  (when-not (and (sequential? vars) (seq vars))
    (throw (ex-info (str "The :spellings op needs the :vars to look for, as a list of "
                         "qualified names, got: " (pr-str vars))
                    {:replique/error :invalid-message})))
  (reduce (fn [acc asked]
            (let [written (or (protocol/as-name asked)
                              (throw (ex-info (str "A var must be named by a name, got: "
                                                   (pr-str asked))
                                              {:replique/error :invalid-message})))
                  named (symbol written)]
              (when-not (namespace named)
                (throw (ex-info (str "A var must be named by a qualified name, got: "
                                     (pr-str asked))
                                {:replique/error :invalid-message})))
              (if-let [found (var-named named)]
                (assoc acc written found)
                acc)))
          {} vars))

(defn- written-as
  "Every symbol the namespace NS can write each of VARS as, by var.

  What the namespace maps, which covers a refer, a refer under another name,
  and a var of its own that shadows one of these; and what an alias makes
  writable, which is a way to write a var whatever the namespace maps."
  [ns vars]
  (let [wanted (set vars)]
    (as-> {} found
      (reduce (fn [found [written mapped]]
                (if (contains? wanted mapped)
                  (update found mapped (fnil conj #{}) (str written))
                  found))
              found (ns-map ns))
      (reduce (fn [found [alias aliased]]
                (reduce (fn [found ^clojure.lang.Var var]
                          (if (= aliased (.ns var))
                            (update found var (fnil conj #{})
                                    (str alias "/" (.sym var)))
                            found))
                        found wanted))
              found (ns-aliases ns)))))

(defn- qualified-name [^clojure.lang.Var var]
  (str (ns-name (.ns var)) "/" (.sym var)))

(defmethod protocol/handle :spellings [_ msg]
  ;; The namespace is read the way a completion reads it, which includes what
  ;; a namespace the process does not have is answered as - see
  ;; `names/namespace-named'.
  (let [asked (vars-asked (:vars msg))
        found (written-as (names/namespace-named msg) (vals asked))]
    ;; Sorted, so that the same namespace answers the same way twice - what
    ;; the mappings of a namespace are read in is whatever order a map has.
    ;;
    ;; The qualified name is in every answer without being looked for: it is
    ;; how the var can be written in any namespace at all, this one included,
    ;; and a namespace that shadows the short name has it as the only way
    ;; left to write it.
    {:spellings (reduce-kv (fn [acc written var]
                             (assoc acc written
                                    (vec (sort (conj (get found var #{})
                                                     (qualified-name var))))))
                           {} asked)}))

;; Where a name is used, which is the question a rename starts with.
;;
;; The client sends what it sends for `:symbol' - the slot of the form point
;; is in, the text written there, the namespace and the locals around it - and
;; that is not a convenience, it is the whole resolution. What is being asked
;; about is the var, and the name at point is one of the many ways a namespace
;; can write one; `:spellings' is the same fact read the other way round.
;;
;; A var, a keyword and a class are all answered, because all three are things
;; somebody renames and none of the three can be found by reading the text: a
;; keyword written ::thing is of whatever namespace the file is, and
;; ::other/thing of whatever that alias stands for.
;;
;; Answered out of what the compiler resolved rather than out of a search, so
;; a usage written by a macro is a usage and a name that merely looks the same
;; is not - see `replique.analysis'.
(defmethod protocol/handle :usages [_ msg]
  (analysis/usages (assoc msg :position (protocol/as-keyword (:position msg)))))

;; The vars a namespace has, for a client to offer a choice of.
;;
;; Which is how a definition is taken away, because the var to remove is
;; nearly never the one at point: renaming a definition and evaluating the
;; file again leaves the process holding both, and the one to be rid of is the
;; old name - which is by then written nowhere in the buffer. What a client
;; can offer is the namespace's own vars, and this is where it reads them.
;;
;; `ns-interns' rather than `ns-publics': a defn- renamed is a defn- left
;; behind like any other. Which is the rule `:remove-var' finds a var by, so
;; what can be chosen here is exactly what can be removed there.
;;
;; Sorted where they were written, which is what makes the list read like the
;; file - the definition somebody just renamed is where they would look for
;; it. Only the process can sort them that way, since what says where a var
;; was written is the var, so it is sorted here rather than by the client, as
;; the namespaces are. One the process cannot place - a var interned rather
;; than written, which carries no file - goes last.
;;
;; What each one is travels beside it, from the list a completion candidate
;; and a `:symbol' answer use. What it was written in does not: a client that
;; wants to open a definition asks `:symbol', which answers the file and the
;; jar entry properly resolved, and a half resolved file here would be a
;; second and worse spelling of the same thing.

(defn- written-at
  "Where VAR was written, as what to sort it among its namespace by.

  The file first and the line inside it, which is what puts the vars of one
  namespace in the order that namespace's file has them - the order that
  makes the list read like the file.

  One the process cannot place goes last: a var interned rather than written
  carries no file at all, and there is nowhere among the ones that were
  written that it belongs. Sorting it by the empty string would put it
  first, which is the one place it certainly does not go."
  [^clojure.lang.Var var]
  (let [{:keys [file line column]} (meta var)]
    [(if file 0 1) (str file) (long (or line 0)) (long (or column 0))]))

(defmethod protocol/handle :vars [_ msg]
  (let [written (or (protocol/as-name (:ns msg))
                    (throw (ex-info (str "The :vars op needs the :ns to look in, as a name, "
                                         "got: " (pr-str (:ns msg)))
                                    {:replique/error :invalid-message})))
        found (find-ns (symbol written))]
    ;; A namespace the process does not have is answered with no vars rather
    ;; than refused. Every file is one until it has been loaded, and holding
    ;; none is a fact about the process - the same thing `:spellings' means by
    ;; leaving a name out. A client showing a list of nothing says so better
    ;; than an error would: there is nothing there to remove.
    {:vars (if (nil? found)
             []
             (->> (ns-interns found)
                  (sort-by (comp written-at val))
                  (mapv (fn [[sym var]]
                          (cond-> {:name (str sym)
                                   :type (names/var-kind var)}
                            (:private (meta var)) (assoc :private true))))))}))

;; Taking a definition away.
;;
;; Unmapping a var is not `ns-unmap', because a var is rarely in one place. One
;; that was referred is in every namespace that referred it, under whatever
;; name that namespace referred it as - so unmapping it where it was defined
;; leaves every caller still calling it. That is the long repl's oldest
;; disease: a definition is renamed, the old name goes on resolving to the var
;; that is still there, the repl agrees with itself all afternoon, and the
;; build is the first thing to disagree.
;;
;; An op rather than something to evaluate. It produces no output, it is
;; asked by the editor rather than typed by somebody, and what it did - which
;; namespaces held the var, and under what names - is an answer rather than a
;; printed value. It also stays out of the repl's *1, which a form that
;; removed a var would not.
;;
;; Removing a var of clojure.core is not refused. It unmaps it from every
;; namespace that refers it, which is nearly all of them, and there is no
;; undoing it short of restarting - but the client is the developer, the name
;; had to be written out in full to get here, and a process somebody can break
;; is the price of a process somebody can change.

(defn- var-to-remove
  "The var a :remove-var names.

  Named where it lives rather than where it is written, which is why the name
  must be qualified. The name at point in a buffer means whatever the
  namespace around it maps, and for `map' or `str' that is a var of
  clojure.core - so \"remove the definition I am pointing at\" must not be a
  way to unmap clojure.core from the process. Turning what is at point into
  the name of the var it came from is the `:symbol' op's job, which is the one
  that reads a buffer.

  Looked up in the interns of the namespace the name is qualified by, rather
  than resolved. Both refuse a name that a namespace only refers - a referred
  var is not interned there - but `resolve' asks the namespace the calling
  thread happens to be in what the qualifier means, and answers through an
  alias of it. The thread answering an op is in whatever namespace it was left
  in, which has nothing to do with the request, and a message must mean the
  same thing whichever thread reads it.

  A name that is interned nowhere is refused rather than answered with
  nothing done. `:spellings' leaves such a name out, because it asks about
  several and one the process has not loaded is a fact about the process;
  this asks for one thing to be done to one var, and a request that found
  nothing to do has not been carried out."
  ^clojure.lang.Var [msg]
  (let [written (or (protocol/as-name (:var msg))
                    (throw (ex-info (str "The :remove-var op needs the :var to remove, as a "
                                         "qualified name, got: " (pr-str (:var msg)))
                                    {:replique/error :invalid-message})))
        named (symbol written)]
    (when-not (namespace named)
      (throw (ex-info (str "A var must be named by a qualified name, got: " (pr-str (:var msg)))
                      {:replique/error :invalid-message})))
    (or (when-let [home (find-ns (symbol (namespace named)))]
          (get (ns-interns home) (symbol (name named))))
        (throw (ex-info (str "No var is interned as " written)
                        {:replique/error :unknown-var})))))

(defn- unmap-everywhere!
  "Unmap VAR wherever it is mapped, and say where that was.

  One sweep over what every namespace maps, rather than the namespace it was
  interned in plus the `ns-refers' of the others: the interned name is in the
  `ns-map' of its own namespace, a refer is in the `ns-map' of the namespace
  that referred it, and a refer under another name is in there under that
  name. One rule finds all three, and finds each of them by the name it is
  actually written as - which is also what the answer has to say, because the
  namespace that referred it as something else is the one whose code will not
  compile until it is edited."
  [^clojure.lang.Var the-var]
  (reduce (fn [acc ns]
            (let [written (->> (ns-map ns)
                               (keep (fn [[sym mapped]] (when (identical? mapped the-var) sym)))
                               sort
                               vec)]
              (if (seq written)
                (do (run! #(ns-unmap ns %) written)
                    (assoc acc (str (ns-name ns)) (mapv str written)))
                acc)))
          {} (all-ns)))

(defmethod protocol/handle :remove-var [_ msg]
  ;; Under the require lock, which is the one `replique.repl/load!' holds and
  ;; clojure's own serialized-require takes. Without it a load running on
  ;; another connection can intern the var again halfway through the sweep,
  ;; and leave it mapped in every namespace this had not reached yet - which
  ;; is the state this op exists to get out of, arrived at by asking for it.
  (locking clojure.lang.RT/REQUIRE_LOCK
    (let [the-var (var-to-remove msg)]
      {:removed (qualified-name the-var)
       ;; Always at least the namespace it was interned in, which is where it
       ;; was found; the rest are the namespaces that referred it, and they
       ;; are the ones whose code will not compile until somebody edits it.
       :unmapped (unmap-everywhere! the-var)})))

;; Reading the classpath again. It is read when the process starts and kept,
;; since walking every jar and every directory of it behind a keystroke is not
;; work worth doing - so a file written after that is not found until this is
;; sent. What knows when to send it is the client: it is the half of this that
;; watches the files of a project.
(defmethod protocol/handle :update-classpath [_ _]
  (let [{:keys [namespaces classes]} (classpath/rescan!)]
    ;; how many of each, which is what says the reading found the entry that
    ;; was added rather than only that it happened
    {:namespaces (count namespaces) :classes (count classes)}))

;; Adding libraries to a running process, which is what clojure.repl.deps
;; does. It asks two things of the thread it runs on: a DynamicClassLoader to
;; add to, which every connection has because every connection loads through
;; the one the process shares - see replique.state - and *repl* bound, which
;; it reads as somebody having asked for this rather than as anything about
;; where the asking came from. It is bound here for that reason and no other.
;;
;; The classpath is read again afterwards rather than left to a second
;; message: what was added is on it now, and this is the op that knows.
;;
;; It takes about a second whenever there is something to resolve, because
;; resolving runs the deps tool, and a control connection answers in request
;; order - so this is the op that holds the channel. A client with something
;; else to ask meanwhile opens a second control connection, which is what the
;; protocol says to do about exactly this.

(defn- with-basis
  "Run f where there is a basis to resolve libraries against.

  There is one when the process was started by the clojure cli, which is what
  wrote the file it is read from. Started any other way there is nothing to
  resolve against and nothing that says what is resolved already, and saying
  so plainly beats what tools.deps says about a nil."
  [f]
  (when (nil? (basis/initial-basis))
    (throw (ex-info (str "This process was not started by the clojure cli, so there is "
                         "no basis to resolve libraries against")
                    {:replique/error :no-basis})))
  (binding [*repl* true
            ;; Bound because adding a library ends by setting them, and
            ;; setting a var needs it bound - a repl has them bound and a
            ;; connection answering an op does not. What lands there is then
            ;; given to the process: the library went onto the classpath every
            ;; connection loads through, so the readers it brought are the
            ;; process's and not this thread's.
            *data-readers* *data-readers*]
    (let [result (f)]
      (alter-var-root #'*data-readers* merge *data-readers*)
      result)))

(defn- added
  "What was added, and what the classpath holds now that it is on it."
  [libs]
  (let [{:keys [namespaces classes]} (classpath/rescan!)]
    ;; a vector however few: nothing added is an empty one rather than an
    ;; absent key, which is what says the request was answered and found
    ;; nothing to do
    {:added (mapv str libs)
     :namespaces (count namespaces)
     :classes (count classes)}))

(defn- library-name [lib]
  (cond
    (symbol? lib) lib
    (string? lib) (symbol lib)
    :else (throw (ex-info (str "A library must be named by a symbol, got: " (pr-str lib))
                          {:replique/error :invalid-message}))))

(defn- libraries
  "The libraries a client asked for, by the name and the coordinates the deps
  reader knows them under."
  [libs]
  (when-not (and (map? libs) (seq libs))
    (throw (ex-info (str "The :add-libs op needs the :libs to add, as a map of a library "
                         "to where it is to be found, got: " (pr-str libs))
                    {:replique/error :invalid-message})))
  (reduce-kv (fn [acc lib coordinates]
               (when-not (map? coordinates)
                 (throw (ex-info (str "The coordinates of " (pr-str lib) " must be a map, got: "
                                      (pr-str coordinates))
                                 {:replique/error :invalid-message})))
               (assoc acc (library-name lib) coordinates))
             {} libs))

(defmethod protocol/handle :add-libs [_ msg]
  (added (with-basis #(deps/add-libs (libraries (:libs msg))))))

(defn- aliases
  "The aliases of a deps.edn a sync is to be done under, or nil for none."
  [msg]
  (let [value (:aliases msg)]
    (cond
      (nil? value) nil
      (sequential? value)
      (mapv (fn [alias]
              (or (protocol/as-keyword alias)
                  (throw (ex-info (str "An alias must be a name, got: " (pr-str alias))
                                  {:replique/error :invalid-message}))))
            value)
      :else (throw (ex-info (str "The :aliases of a :sync-deps must be a list of names, got: "
                                 (pr-str value))
                            {:replique/error :invalid-message})))))

;; What deps.edn says the process should have and does not. The message an
;; editor sends after somebody edited that file, which is the way a library
;; gets added and stays added: what :add-libs adds is gone when the process is.
(defmethod protocol/handle :sync-deps [_ msg]
  (let [under (aliases msg)]
    (added (with-basis #(if (seq under) (deps/sync-deps :aliases under) (deps/sync-deps))))))

;; Stopping an evaluation that went wrong. The client names the repl
;; connection it wants interrupted - it knows the id, the handshake reply of
;; every connection it opened carries it.
;;
;; This interrupts the thread. It stops code that blocks or that checks the
;; interrupt flag, and nothing else: Thread.stop is gone since jdk 20 and the
;; jvm offers no other way. An infinite loop that computes has to be waited
;; out, or the process restarted.
(defmethod protocol/handle :interrupt [_ msg]
  (let [id (:connection msg)
        target (get (state/connections) id)]
    (cond
      (not (string? id))
      (throw (ex-info (str "The :interrupt op needs the :connection to interrupt, got: "
                           (pr-str id))
                      {:replique/error :invalid-message}))

      (nil? target)
      (throw (ex-info (str "Unknown connection: " (pr-str id))
                      {:replique/error :unknown-connection}))

      (not (identical? :repl @(:role target)))
      (throw (ex-info (str "Connection " id " is not a repl")
                      {:replique/error :not-a-repl}))

      :else {:connection id :interrupted (server/interrupt! target)})))

;; Stopping the process. A client that started one can signal it; a client
;; that connected to one cannot - it is not a child of that editor, and after
;; the editor restarts none of them are. Asking is the way that works for
;; both, and it is also the graceful one: the process exits through its
;; shutdown hook, which is what deletes the port file.
(defn exit!
  "End the process. A var of its own so that a test can run the op without
  taking the test runner with it - the tests run inside the process they
  test."
  []
  (System/exit 0))

(def exit-delay-ms
  "How long the reply has before the process goes. Long enough for a write to
  a connection on this machine, short enough to be under the time a client
  waits for the process to be gone."
  300)

(defmethod protocol/handle :shutdown [_ _]
  ;; The payload is returned to be framed and written like any other op's,
  ;; and all that is arranged here is that the process does not go first.
  ;;
  ;; The exit is what is delayed, rather than the reply waited for, because a
  ;; write to a client that stopped reading never returns - and a client that
  ;; asked the process to stop is exactly a client about to stop reading. An
  ;; exit that waited for the write would be an exit that never happened, so
  ;; the two are not connected at all: the connection thread writes the reply,
  ;; and this ends the process whether that write got anywhere or not.
  (doto (Thread. (fn [] (Thread/sleep (long exit-delay-ms)) (exit!)) "replique-exit")
    (.setDaemon true)
    (.start))
  {:stopping true})
