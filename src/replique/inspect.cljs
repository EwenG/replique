(ns replique.inspect
  "The runtime's half of inspecting a ClojureScript value - see
  replique/inspect.clj, which is the half the editor talks to.

  HERE BECAUSE THE VALUE IS HERE. An atom in a page is an object in that
  page, and nothing on the JVM could hold it, walk it or watch it: what
  crosses the wire is what an editor is shown of it, a page at a time, and
  never the value.

  Asked through `op', with what the JVM wants as JSON and what it gets back as
  EDN - the two formats each side reads natively - and the views are kept here
  by the ids the JVM gave them. A page that reloads has none, which is how the
  JVM finds out that a view has to be opened again.

  What changed is said with the runtime's notify, once per view until the
  view is asked for again: a watch on an atom that is swapped at every frame
  says so once, and not sixty times a second."
  (:require [replique.inspector :as inspector]))

(defonce ^:private views (atom {}))

;;; Saying what changed

(defonce ^:private told
  ;; the views said to have changed since they were last asked for
  (atom #{}))

(defn- notify! [id]
  (when-not (contains? @told id)
    (swap! told conj id)
    (when-let [notify (.-notify (.-$CLJS js/globalThis))]
      (notify (pr-str {:changed [id]})))))

;;; What was tapped

(def ^:private taps-kept 100)

(defonce ^:private taps (atom (sorted-map)))

(defonce ^:private tap-count (atom 0))

(defonce ^:private tapping
  ;; From the moment this is loaded: the value somebody tapped before opening
  ;; the view is the one they open it for
  (add-tap (fn [x]
             (let [n (swap! tap-count inc)]
               (swap! taps (fn [m]
                             (let [m (assoc m n x)]
                               (if (> (count m) taps-kept)
                                 (dissoc m (first (keys m)))
                                 m))))))))

;;; Where a view's value comes from

(defn- var-value
  "What the var NAME, written ns/name, holds - read off its namespace object,
  which is what a var is in this runtime."
  [name]
  (let [i (.indexOf name "/")
        ns (when (pos? i) (subs name 0 i))
        n (subs name (inc i))
        o (when ns (.get (.-namespaces (.-$CLJS js/globalThis)) ns))
        prop ((.-munge (.-$CLJS js/globalThis)) n)]
    (if (and o (.call (.-hasOwnProperty (.-prototype js/Object)) o prop))
      (unchecked-get o prop)
      (throw (ex-info (str "This runtime has no var " name)
                      {:kind "unknown-var"})))))

(defn- top-of [source]
  (cond
    (:var source) (let [var (:var source)] (fn [] (var-value var)))
    (:results source) (fn [] (array-map '*1 *1 '*2 *2 '*3 *3))
    (:taps source) (constantly taps)
    :else (throw (ex-info "A view is of a :var, the :results or the :taps"
                          {:kind "invalid-message"}))))

;;; The ops

(defn- the-view [id]
  (or (get @views id)
      (throw (ex-info (str "This runtime has no view " id) {:kind "unknown-view"}))))

(defn- opts-of [msg]
  (select-keys msg [:offset :limit :width :meta]))

(defn- handle [{:keys [op view node at limit meta] :as msg}]
  (case op
    "open"
    (let [s (inspector/session (top-of (:source msg)) (:history msg))]
      (inspector/watch! s [::view view] #(do (inspector/record! s) (notify! view)))
      (swap! views assoc view s)
      (inspector/shown s (opts-of msg)))

    "refresh"
    (let [s (the-view view)]
      (swap! told disj view)
      (inspector/watch! s [::view view] #(do (inspector/record! s) (notify! view)))
      (inspector/snapshot! s at)
      (inspector/shown s (opts-of msg)))

    "children"
    (inspector/children (:model (the-view view)) node (opts-of msg))

    "print"
    (inspector/printed (:model (the-view view)) node limit meta)

    "path"
    (let [s (the-view view)
          base (when-let [var (:var msg)]
                 ;; a var here is its value, so every reference under it is
                 ;; one deref
                 (inspector/base-form var (inspector/derefs s)))
          form (inspector/path-form (:model s) node base)]
      (cond-> {}
        form (assoc :code (pr-str form))))

    "close"
    (do (when-let [s (get @views view)]
          (inspector/unwatch! s [::view view]))
        (swap! views dissoc view)
        {})))

(defn op
  "Answer MSG, a JSON object as a string, with EDN as a string. What goes wrong
  is answered too, as {:error :kind}, rather than thrown: the JVM is reading an
  answer, and a throw would reach it as a failed evaluation saying nothing
  about which view or why."
  [msg]
  (pr-str
   (try
     (handle (js->clj (js/JSON.parse msg) :keywordize-keys true))
     (catch :default e
       {:error (or (ex-message e) (str e))
        :kind (or (:kind (ex-data e)) "exception")}))))

(defn value
  "What the node NODE of the view VIEW holds - which is how a value an editor
  is showing is brought to the repl."
  [view node]
  (let [v (inspector/value (:model (the-view view)) node)]
    (if (= inspector/gone v)
      (throw (ex-info (str "The node " node " of the view " view " is no longer there")
                      {:view view :node node}))
      v)))
