(ns replique.inspect
  "The :inspect ops: a value, browsed from the editor a piece at a time, and
  watched where it is a reference. See replique.inspector for what a view is
  and doc/protocol.md for the ops.

  A VIEW IS OPENED ON A SOURCE, which says where its value comes from:

    {:var \"ns/name\"}   the var, and the references under it - a var holding
                       an atom is a view of what the atom holds, and is watched
                       through both, since `def'ing the var again is a change
                       as much as swapping the atom is
    {:results id}      the last three results of the repl connection ID, as
                       *1, *2 and *3
    {:taps true}       the last values `tap>' was given, numbered

  A source that is a reference is WATCHED: the client is told that the view
  changed, by an `inspect-changed' event, and asks for it again. The event
  says nothing else, and it is said once: after it, nothing more is said of
  that view until the client has asked for it again. What the client was told
  is that what it shows is out of date, and a thousand swaps make it no more
  out of date than one did - an atom swapped in a loop must cost the editor
  nothing it did not ask for.

  A view belongs to the control connection that opened it, and goes with it:
  it holds the value it shows, and nothing else would let go of it.

  CLOJURESCRIPT VIEWS ARE IN THE RUNTIME, where the value is - see
  replique/inspect.cljs, which is compiled into the program the first time a
  view is opened on it. What is kept here is who to tell and which page the
  view is in: a page that reloads is a new program, with none of the views
  the last one had, and an op on one of them is answered with a
  `view-gone' error, which a client answers by opening it again."
  (:require [clojure.edn :as edn]
            [replique.cljs :as cljs]
            [replique.inspector :as inspector]
            [replique.json :as json]
            [replique.names :as names]
            [replique.protocol :as protocol]
            [replique.state :as state])
  (:import [java.util.concurrent.atomic AtomicBoolean AtomicLong]))

(defonce ^:private views
  ;; id -> {:id :conn :dialect, and :session for a Clojure view, :target :page
  ;; for a ClojureScript one}
  (atom {}))

(defonce ^:private ids (AtomicLong. 0))

(defn- refuse [kind & message]
  (throw (ex-info (apply str message) {:replique/error kind})))

;;; Telling the client

(defn- notify!
  "Tell the client of VIEW that it changed, unless it was told since it last
  asked for the view.

  On the thread that made the change, which is the application's: once per
  refresh is what makes that affordable, and `emit-event!' never waits for a
  client that is not reading."
  [{:keys [id conn ^AtomicBoolean told]}]
  (when (and (.compareAndSet told false true)
             (contains? @views id))
    (try (protocol/emit-event! conn (protocol/event "inspect-changed" {:view id}))
         (catch Throwable _ nil))))

(defn- asked!
  "Say that the client of VIEW is asking for it again, so that the next change
  is one to tell it about. Before the value is read: a change between the two
  is then told rather than lost."
  [{:keys [^AtomicBoolean told]}]
  (.set told false))

;;; Where a Clojure view's value comes from

(defonce ^:private results
  ;; repl connection id -> an atom of {*1 v, *2 v, *3 v}
  (atom {}))

(defn record-result!
  "Keep VALUE as the last result of the repl connection ID - which is what
  *1 is to that repl, and what a view of its results shows."
  [id value]
  (let [a (or (get @results id)
              (get (swap! results update id #(or % (atom {}))) id))]
    (swap! a (fn [m]
               (let [xs (take 3 (cons value (vals m)))]
                 (apply array-map (interleave '[*1 *2 *3] xs)))))))

(def ^:private taps-kept 100)

(defonce ^:private taps (atom (sorted-map)))

(defonce ^:private tap-count (AtomicLong. 0))

(defonce ^:private tapping
  ;; From the moment the process is up: the value somebody tapped before
  ;; opening the view is the one they open it for
  (let [f (fn [x]
            (let [n (.incrementAndGet tap-count)]
              (swap! taps (fn [m]
                            (let [m (assoc m n x)]
                              (if (> (count m) taps-kept)
                                (dissoc m (first (keys m)))
                                m))))))]
    (add-tap f)
    f))

(defn- top-of
  "What a Clojure view on SOURCE takes its value from."
  [{:keys [var results taps] :as source}]
  (cond
    var (let [sym (symbol (protocol/as-name var))
              found (when (namespace sym)
                      (try (find-var sym) (catch Exception _ nil)))]
          (when-not found
            (refuse :unknown-var "No var " var
                    " - a var is named in full, ns/name, and its namespace has to be loaded"))
          (constantly found))
    results (let [id (str results)
                  conn (get (state/connections) id)]
              (when-not (and conn (= :repl @(:role conn)))
                (refuse :unknown-connection "No repl connection " (pr-str results)))
              (constantly (or (get @replique.inspect/results id)
                              (get (swap! replique.inspect/results update id
                                          #(or % (atom {})))
                                   id))))
    taps (constantly replique.inspect/taps)
    :else (refuse :invalid-message
                  "A view is opened on a :var, on the :results of a repl"
                  " connection or on the :taps, got: " (pr-str source))))

;;; The ops

(defn- opts-of [msg]
  (select-keys msg [:offset :limit :width :meta]))

(defn- the-view [msg]
  (let [id (:view msg)]
    (or (get @views id)
        (refuse :unknown-view "No view " (pr-str id)
                " - it was closed, or this process is not the one that opened it"))))

(defn- watching! [{:keys [id session] :as view}]
  (inspector/watch! session [::view id]
                    (fn []
                      (inspector/record! session)
                      ;; the reference the root comes from may be a new one
                      (watching! view)
                      (notify! view))))

(defn- close! [id]
  (when-let [{:keys [dialect session] :as view} (get @views id)]
    (swap! views dissoc id)
    (if (= :cljs dialect)
      (try (binding [cljs/*target* (:target view)]
             (when (= (:page view) (cljs/page))
               (cljs/eval-js-here (str "$CLJS.namespaces.get(\"replique.inspect\")"
                                       "?.op(" (json/write-str (json/write-str {:op "close" :view id}))
                                       ")")
                                  1000)))
           (catch Throwable _ nil))
      (inspector/unwatch! session [::view id]))))

(state/on-close!
 ::views
 (fn [conn-id]
   (doseq [[id view] @views :when (= conn-id (:id (:conn view)))]
     (close! id))
   (swap! results dissoc conn-id)))

;;; ClojureScript, in the runtime

(defonce ^:private compiled
  ;; the output directories replique.inspect has been compiled into
  (atom #{}))

(def ^:private runtime-within
  "How long a runtime is given to answer about a view, in ms."
  5000)

(defn- compile-runtime-half!
  "Compile replique.inspect into `*target*''s program, once - and the views
  it reads them with, which it requires."
  []
  (let [{:keys [out-dir]} (cljs/environment)
        dir (str out-dir)]
    (when-not (contains? @compiled dir)
      (cljs/with-target-lock* #(cljs/compile-namespace! 'replique.inspect))
      (swap! compiled conj dir))))

(defn- in-runtime!
  "Ask the runtime of `*target*' MSG, and answer what it said.

  The page the view is in where it is one, and refused where that is not the
  page there is now."
  [msg view]
  (when-not (cljs/runtime-connected?)
    (refuse :no-runtime "No " (name cljs/*target*) " runtime is running"
            (when (= :browser cljs/*target*) ", and no page is open")
            " - start a ClojureScript repl"))
  (when (and view (not= (:page view) (cljs/page)))
    (refuse :view-gone "The page view " (:id view) " was in is gone"))
  (compile-runtime-half!)
  (let [js (str "(async () => { await $CLJS.require(\"replique.inspect\");"
                " return $CLJS.namespaces.get(\"replique.inspect\").op("
                (json/write-str (json/write-str msg)) "); })()")
        {:keys [status value]} (cljs/eval-js-here js runtime-within)]
    (when-not (= :success status)
      (refuse :runtime value))
    (let [answer (edn/read-string (edn/read-string value))]
      (when-let [why (:error answer)]
        (refuse (case (:kind answer)
                  "unknown-view" :view-gone
                  ("unknown-var" "invalid-message") (keyword (:kind answer))
                  :runtime)
                why))
      answer)))

(defonce ^:private prepared
  ;; target -> the page replique.inspect was last loaded into
  (atom {}))

(defn prepare-page!
  "Load replique.inspect into the page `*target*' evaluates in, where it is
  not yet - before a form is evaluated there, so that what the page taps is
  kept from the first form on, and not from the first view.

  Once per page: a page that reloads is a program without it. Quietly: a
  page that cannot load it is a page whose repl must still work, and what
  went wrong is said again by the first view opened on it."
  []
  (try
    (let [page (cljs/page)]
      (when (and page (not= page (get @prepared cljs/*target*)))
        (compile-runtime-half!)
        (let [{:keys [status]} (cljs/eval-js-here
                                "(async () => { await $CLJS.require(\"replique.inspect\"); return true; })()"
                                runtime-within)]
          (when (= :success status)
            (swap! prepared assoc cljs/*target* page)))))
    ;; an :interrupt of the repl waiting for the target's lock is the repl's
    (catch InterruptedException e (throw e))
    (catch Throwable _ nil)))

(defn- cljs-view [id]
  (let [view (get @views id)]
    (when (= :cljs (:dialect view)) view)))

(cljs/on-notify!
 (fn [target content]
   (doseq [id (:changed (try (edn/read-string content) (catch Exception _ nil)))
           :let [view (cljs-view id)]
           :when (and view (= target (:target view)))]
     (notify! view))))

(defn evaluated!
  "Say that a form was evaluated in `*target*''s runtime, which changes what
  *1 is there - and the runtime does not say so itself, its *1 being a var
  that is set rather than a reference that is swapped."
  []
  (doseq [[_ view] @views
          :when (and (= :cljs (:dialect view))
                     (= cljs/*target* (:target view))
                     (:results (:source view)))]
    (notify! view)))

;;; The ops

(defn- base-of
  "The code a Clojure view's root is reached by."
  [{:keys [source session]}]
  (when-let [var (:var source)]
    ;; the var is one of the references, and a var is written as itself
    (inspector/base-form var (dec (inspector/derefs session)))))

(defn- with-view
  "Call F with the view MSG names, in the dialect it was opened in."
  [msg f]
  (let [view (the-view msg)]
    (if (= :cljs (:dialect view))
      (binding [cljs/*target* (:target view)] (f view))
      (f view))))

(defmethod protocol/handle :inspect [conn msg]
  (names/with-dialect msg
    (let [id (.incrementAndGet ids)
          source (:source msg)
          history (when (nat-int? (:history msg)) (:history msg))
          base {:id id :conn conn :source source :told (AtomicBoolean. false)}]
      (when-not (map? source)
        (refuse :invalid-message "The :inspect op needs the :source of the view, got: "
                (pr-str source)))
      (if (names/cljs?)
        (let [shown (in-runtime! (merge (opts-of msg)
                                        {:op "open" :view id :history history
                                         :source (cond-> source
                                                   (:var source) (update :var protocol/as-name))})
                                 nil)]
          (swap! views assoc id (assoc base :dialect :cljs :target cljs/*target*
                                       :page (cljs/page)))
          (assoc shown :view id))
        (let [session (inspector/session (top-of source) history)
              view (assoc base :dialect :clj :session session)]
          (swap! views assoc id view)
          (watching! view)
          (assoc (inspector/shown session (opts-of msg)) :view id))))))

(defmethod protocol/handle :inspect-refresh [_ msg]
  (with-view msg
    (fn [{:keys [id dialect session] :as view}]
      (let [at (when (nat-int? (:at msg)) (:at msg))]
        (asked! view)
        (if (= :cljs dialect)
          (in-runtime! (merge (opts-of msg) {:op "refresh" :view id :at at}) view)
          (do (watching! view)
              (inspector/snapshot! session at)
              (inspector/shown session (opts-of msg))))))))

(defn- node-of [msg]
  (let [node (:node msg)]
    (if (nat-int? node)
      node
      (refuse :invalid-message "A node is the number the view gave it, got: " (pr-str node)))))

(defmethod protocol/handle :inspect-children [_ msg]
  (with-view msg
    (fn [{:keys [id dialect session] :as view}]
      (if (= :cljs dialect)
        (in-runtime! (merge (opts-of msg) {:op "children" :view id :node (node-of msg)}) view)
        (inspector/children (:model session) (node-of msg) (opts-of msg))))))

(def ^:private printed-up-to
  "How much of a value printed whole is sent, in chars."
  1000000)

(defmethod protocol/handle :inspect-print [_ msg]
  (with-view msg
    (fn [{:keys [id dialect session] :as view}]
      (if (= :cljs dialect)
        (in-runtime! {:op "print" :view id :node (node-of msg)
                      :limit printed-up-to :meta (boolean (:meta msg))}
                     view)
        (inspector/printed (:model session) (node-of msg) printed-up-to
                           (boolean (:meta msg)))))))

(defmethod protocol/handle :inspect-path [_ msg]
  (with-view msg
    (fn [{:keys [id dialect session source] :as view}]
      (let [node (node-of msg)
            value (str "(replique.inspect/value " id " " node ")")]
        (if (= :cljs dialect)
          (assoc (in-runtime! {:op "path" :view id :node node
                               :var (some-> (:var source) protocol/as-name)}
                              view)
                 :value value)
          (let [form (inspector/path-form (:model session) node (base-of view))]
            (cond-> {:value value}
              form (assoc :code (binding [*print-length* nil *print-level* nil]
                                  (pr-str form))))))))))

(defmethod protocol/handle :inspect-close [_ msg]
  (close! (:view msg))
  {:closed true})

;;; At the repl

(defn value
  "What the node NODE of the Clojure view VIEW holds - which is how a value an
  editor is showing is brought to the repl. The :inspect-path op writes the
  call."
  [view node]
  (let [{:keys [session]} (or (get @views view)
                              (throw (ex-info (str "No view " view) {:view view})))
        v (when session (inspector/value (:model session) node))]
    (if (or (nil? session) (= inspector/gone v))
      (throw (ex-info (str "The node " node " of the view " view " is no longer there")
                      {:view view :node node}))
      v)))
