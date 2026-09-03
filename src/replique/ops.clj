(ns replique.ops
  "The ops handled by the control connection."
  (:require [clojure.java.basis :as basis]
            [clojure.repl.deps :as deps]
            [replique.classpath :as classpath]
            [replique.completion :as completion]
            [replique.protocol :as protocol]
            [replique.server :as server]
            [replique.state :as state]))

(defmethod protocol/handle :process-info [conn _]
  (let [info (state/info)]
    (assoc info
           :connection (:id conn)
           :uptime (when-let [started-at (:started-at info)]
                     (- (System/currentTimeMillis) started-at)))))

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
