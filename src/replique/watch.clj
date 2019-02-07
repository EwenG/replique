(ns replique.watch
  (:require [replique.tooling-msg :as tooling-msg]
            [replique.utils :as utils]
            [replique.completion :as completion]
            [replique.elisp-printer :as elisp]
            [replique.watch-protocols :as protocols]
            [clojure.core.server :as server])
  (:import [clojure.lang IRef IDeref IPending]
           [java.util Map Collection UUID]
           [replique.watch_protocols WatchedRef RecordedWatchedRef]))

(def ^:private cljs-repl-env (utils/dynaload 'replique.repl-cljs/repl-env))
(def ^:private cljs-repl-env-nashorn (utils/dynaload 'replique.nashorn/repl-env))
(def ^:private cljs-evaluate-form (utils/dynaload 'replique.repl-cljs/-evaluate-form))
(def ^:private cljs-munged (utils/dynaload 'cljs.compiler/munge))

(defonce watched-refs (atom {}))

(defn pr-str-for-eval [& xs]
  (binding [*print-length* nil
            *print-level* nil]
    (apply pr-str xs)))

(defn notification-watcher [buffer-id k ref old-value value]
  (binding [*out* tooling-msg/tooling-out]
    (utils/with-lock tooling-msg/tooling-out-lock
      (tooling-msg/tooling-prn {:type :watch-update
                                :process-id tooling-msg/process-id
                                :buffer-id buffer-id}))))

(declare recorder-watcher)

(defn recorded-watched-ref-with-record-size [ref values index record-size meta]
  (if (> (count values) record-size)
    (let [drop-n (- (count values) record-size)
          new-values (if (> drop-n index)
                       (into [(get values index)] (drop (inc drop-n)) values)
                       (into [] (drop drop-n) values))]
      (protocols/->RecordedWatchedRef ref new-values (max 0 (- index drop-n)) record-size meta))
    (protocols/->RecordedWatchedRef ref values index record-size meta)))

(extend-type WatchedRef
  protocols/IGetRef
  (protocols/get-ref [this] (.-ref this))
  protocols/IWatchHandler
  (protocols/add-watch-handler [this buffer-id]
    (add-watch (.-ref this) (keyword "replique.watch" (str buffer-id))
               (partial notification-watcher buffer-id)))
  protocols/IRecordable
  (prtocols/start-recording [this buffer-id record-size]
    (remove-watch (.-ref this) (keyword "replique.watch" (str buffer-id)))
    (let [watched-ref (protocols/->RecordedWatchedRef (.-ref this) (.-values this)
                                                      (.-index this) record-size
                                                      (.-meta this))]
      (protocols/add-watch-handler watched-ref buffer-id)
      watched-ref))
  (protocols/stop-recording [this buffer-id] this)
  (protocols/record-position [this]
    {:index (inc (.-index this)) :count (count (.-values this))})
  (protocols/most-recent-value [this]
    (protocols/->WatchedRef (.-ref this) [@(.-ref this)] 0 (.-meta this)))
  (protocols/value-at-index [this index]
    (protocols/->WatchedRef (.-ref this) (.-values this)
                            (max (min index (dec (count (.-values this)))) 0)
                            (.-meta this)))
  protocols/IRecordSize
  (protocols/record-size [this] nil))

(extend-type RecordedWatchedRef
  protocols/IGetRef
  (protocols/get-ref [this] (.-ref this))
  protocols/IWatchHandler
  (protocols/add-watch-handler [this buffer-id]
    (add-watch (.-ref this) (keyword "replique.watch" (str buffer-id))
               (juxt
                (partial recorder-watcher buffer-id)
                (partial notification-watcher buffer-id))))
  protocols/IRecordable
  (protocols/start-recording [this buffer-id record-size]
    (recorded-watched-ref-with-record-size (.-ref this) (.-values this)
                                           (.-index this) record-size
                                           (.-meta this)))
  (protocols/stop-recording [this buffer-id]
    (remove-watch ref (keyword "replique.watch" (str buffer-id)))
    (let [watched-ref (protocols/->WatchedRef (.-ref this) (.-values this) (.-index this)
                                              (.-meta this))]
      (protocols/add-watch-handler watched-ref buffer-id)
      watched-ref))
  (protocols/record-position [this]
    {:index (inc (.-index this)) :count (count (.-values this))})
  (protocols/most-recent-value [this]
    (protocols/->RecordedWatchedRef (.-ref this) (.-values this) (dec (count (.-values this)))
                                    (.-record-size this)
                                    (.-meta this)))
  (protocols/value-at-index [this index]
    (protocols/->RecordedWatchedRef (.-ref this) (.-values this)
                                    (max (min index (dec (count (.-values this)))) 0)
                                    (.-record-size this)
                                    (.-meta this)))
  protocols/IRecordSize
  (protocols/record-size [this] (.-record-size this)))

(defn add-recorded-watch-value [^RecordedWatchedRef recorded-watch new-value]
  (recorded-watched-ref-with-record-size (.-ref recorded-watch)
                                         (conj (.-values recorded-watch) new-value)
                                         (.-index recorded-watch)
                                         (.-record-size recorded-watch)
                                         (.-meta recorded-watch)))

(defn recorder-watcher [buffer-id k ref old-value value]
  (swap! watched-refs update buffer-id add-recorded-watch-value value))

(defn maybe-nested-iref [x]
  (loop [iref x
         candidate x]
    (cond (instance? IRef candidate)
          (recur candidate @candidate)
          (instance? IDeref candidate)
          (if (instance? IPending candidate)
            (if (realized? candidate)
              (recur iref @candidate)
              iref)
            (recur iref @candidate)) 
          :else iref)))

(defn add-replique-watch [var-sym buffer-id]
  (let [var (resolve var-sym)
        ref (maybe-nested-iref var)
        watched-ref (protocols/->WatchedRef ref [@ref] 0 nil)]
    (swap! watched-refs assoc buffer-id watched-ref)
    (protocols/add-watch-handler watched-ref buffer-id)
    {}))

(defn add-watch-and-retry [{:keys [buffer-id var-sym] :as msg} retry-fn]
  (if var-sym
    (let [var (resolve var-sym)
          ref (maybe-nested-iref var)]
      (if ref
        (do
          (add-replique-watch var-sym buffer-id)
          (retry-fn msg))
        {:error (IllegalStateException. (str var-sym " is not defined"))
         :undefined true}))
    {:error (IllegalStateException. (str "Nothing to watch for buffer-id: " buffer-id))
     :undefined true}))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :add-watch]
  [{:keys [var-sym buffer-id] :as msg}]
  (tooling-msg/with-tooling-response msg
    (add-replique-watch var-sym buffer-id)))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :remove-watch]
  [{:keys [buffer-id] :as msg}]
  (tooling-msg/with-tooling-response msg
    ;; The ref may not be found if the process has been restarted
    (when-let [watched-ref (get @watched-refs buffer-id)]
      (let [ref (protocols/get-ref watched-ref)]
        (remove-watch ref (keyword "replique.watch" (str buffer-id)))
        (alter-meta! ref dissoc :replique.watch/value))
      (swap! watched-refs dissoc buffer-id)
      nil)))

;; Like clojure.core/get for maps. Get the nth element for collections
(defn browse-get [o k]
  (cond (or (map? o) (instance? Map o))
        (get o k)
        (and (or (coll? o) (instance? Collection  o))
             (number? k))
        (nth (seq o) k nil)
        :else nil))

;; like clojure.core/get-in but uses browse-get instead of get
(defn browse-get-in [m ks]
  (reduce browse-get m ks))

(defn parse-browse-path [browse-path]
  (into '() (map read-string) browse-path))

(defn refresh-watch [{:keys [buffer-id update? index
                             print-length print-level print-meta
                             browse-path]
                      :as msg}]
  (if-let [watched-ref (get @watched-refs buffer-id)]
    (let [browse-path (parse-browse-path browse-path)
          watched-ref (cond update?
                            (protocols/most-recent-value watched-ref)
                            index
                            (protocols/value-at-index watched-ref index)
                            :else watched-ref)]
      (swap! watched-refs assoc buffer-id watched-ref)
      (let [ref-value @watched-ref
            ref (protocols/get-ref watched-ref)
            ref-value-at-browse-path (browse-get-in ref-value browse-path)]
        (alter-meta! ref assoc :replique.watch/value ref-value-at-browse-path)
        {:refresh-watch {:var-value (binding [*print-length* print-length
                                              *print-level* print-level
                                              *print-meta* print-meta]
                                      (pr-str ref-value-at-browse-path))
                         :record-size (protocols/record-size watched-ref)}}))
    (add-watch-and-retry msg refresh-watch)))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :refresh-watch]
  [msg]
  (tooling-msg/with-tooling-response msg
    (refresh-watch msg)))

#_(declare serializable?)

#_(defn serializable-map-entry? [e]
  (and (serializable? (key e)) (serializable? (val e))))

#_(def ^:const symbols-separators-re #"[\]\[\s,\(\)\{\}\"\n\t~@^`;]")

;; Not all symbols can be read but the Clojure LispReader
#_(defn serializable-symbol? [sym]
  (let [sym-str (name sym)]
    (not (or (.startsWith sym-str ":")
             (.endsWith sym-str ":")
             (.contains sym-str "::")
             (.startsWith sym-str "#")
             (.startsWith sym-str "/")
             (.endsWith sym-str "/")
             ;; The symbol does not start with a decimal
             (<= 48 (.codePointAt sym-str 0) 57)
             (re-find symbols-separators-re sym-str)))))

#_(defn serializable-keyword? [kw]
  (let [keyword-name (name kw)]
    (not (or (.endsWith keyword-name ":")
             (.contains keyword-name "::")
             (.startsWith keyword-name "/")
             (.endsWith keyword-name "/")
             (re-find symbols-separators-re keyword-name)))))

#_(defn serializable? [x]
  (cond (nil? x) true
        (boolean? x) true
        (number? x) true
        (string? x) true
        (and (symbol? x) (serializable-symbol? x)) true
        (and (keyword? x) (serializable-keyword? x)) true
        (or (map? x) (instance? Map x)) (every? serializable-map-entry? x)
        (or (coll? x) (instance? Collection x)) (every? serializable? x)
        :else false))

(declare browsable-key?)

(defn browsable-map-entry? [e]
  (and (browsable-key? (key e)) (browsable-key? (val e))))

(defn browsable-key? [x]
  (cond (nil? x) true
        (instance? Boolean x) true
        (number? x) true
        (string? x) true
        (symbol? x) true
        (keyword? x) true
        (or (map? x) (instance? Map x)) (every? browsable-map-entry? x)
        (or (coll? x) (instance? Collection x)) (every? browsable-key? x)
        (instance? UUID x) true
        :else false))

;; We can only browse keys that can be read by the clojure/clojurescript reader and that implement
;; equality by value, because of hashmaps lookups.
;; This allows keeping the browse path on the client side and thus makes serveral things
;; easier to implement, like handling a browser refresh for example
(defn browsable-serialized-key? [x-str]
  (try
    (let [x (read-string x-str)]
      ;; check things like (read-string "ee~rr")
      (if (or (and (symbol? x) (not= x-str (pr-str x)))
              (and (keyword? x) (not= x-str (pr-str x))))
        false
        (browsable-key? x)))
    (catch Exception e false)))

(defn browse-candidates [{:keys [buffer-id prefix print-meta browse-path] :as msg}]
  (if-let [watched-ref (get @watched-refs buffer-id)]
    (let [browse-path (parse-browse-path browse-path)
          ref-value (browse-get-in @watched-ref browse-path)
          prefix-tokens (completion/tokenize-prefix (str prefix))
          candidates (cond (or (map? ref-value) (instance? Map ref-value))
                           (doall
                            (for [k (keys ref-value)
                                  :when (and (completion/matches? (str k) prefix-tokens))]
                              (binding [*print-length* nil
                                        *print-level* nil
                                        *print-meta* print-meta]
                                (pr-str k))))
                           (or (coll? ref-value) (instance? Collection ref-value)
                               (and (class ref-value) (.isArray (class ref-value))))
                           (doall
                            (for [i (range (count ref-value))
                                  :when (completion/matches? (str i) prefix-tokens)]
                              (binding [*print-length* nil
                                        *print-level* nil
                                        *print-meta* print-meta]
                                (pr-str i)))))]
      {:candidates (if (empty? (str prefix))
                     (cons "" candidates)
                     candidates)})
    (add-watch-and-retry msg browse-candidates)))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :browse-candidates] [msg]
  (tooling-msg/with-tooling-response msg
    (browse-candidates msg)))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :can-browse?]
  [{:keys [candidate] :as msg}]
  (tooling-msg/with-tooling-response msg
    {:can-browse? (browsable-serialized-key? candidate)}))

(defn add-replique-watch-cljs [repl-env {:keys [var-sym buffer-id] :as msg}]
  (let [{:keys [status value]}
        (@cljs-evaluate-form
         repl-env
         (format "replique.cljs_env.watch.add_replique_watch(%s, %s);"
                 (pr-str (@cljs-munged var-sym)) (pr-str buffer-id))
         :timeout-before-submitted 100)]
    (if-not (= status :success)
      {:error value}        
      {})))

(defmethod tooling-msg/tooling-msg-handle [:replique/browser :add-watch] [msg]
  (tooling-msg/with-tooling-response msg
    (add-replique-watch-cljs @@cljs-repl-env msg)))

(defn remove-watch-cljs [repl-env {:keys [buffer-id] :as msg}]
  (let [{:keys [status value]}
        (tooling-msg/with-tooling-response msg
          (@cljs-evaluate-form
           repl-env
           (format "replique.cljs_env.watch.remove_replique_watch(%s);" (pr-str buffer-id))
           :timeout-before-submitted 100))]
    (if-not (= status :success)
      {:error value}
      {})))

(defmethod tooling-msg/tooling-msg-handle [:replique/browser :remove-watch] [msg]
  (tooling-msg/with-tooling-response msg
    (remove-watch-cljs @@cljs-repl-env msg)))

(defmethod tooling-msg/tooling-msg-handle [:replique/nashorn :remove-watch] [msg]
  (tooling-msg/with-tooling-response msg
    (remove-watch-cljs @@cljs-repl-env-nashorn msg)))

(defn browse-path->js-array [browse-path]
  `[~@(interpose (symbol ",") browse-path)])

(defn refresh-watch-cljs
  [repl-env {:keys [update? var-sym buffer-id
                    print-length print-level print-meta
                    browse-path]
             :as msg}]
  (let [{:keys [status value stacktrace] :as ret}
        (@cljs-evaluate-form
         repl-env
         (format "replique.cljs_env.watch.refresh_watch(%s, %s, [%s, %s, %s, %s, %s]);"
                 (pr-str buffer-id) (pr-str (@cljs-munged var-sym))
                 (pr-str (boolean update?))
                 (if (nil? print-length)
                   "null"
                   (pr-str print-length))
                 (if (nil? print-level)
                   "null"
                   (pr-str print-level))
                 (if (nil? print-meta)
                   "null"
                   (pr-str print-meta))
                 (if (nil? browse-path)
                   "null"
                   (pr-str-for-eval (browse-path->js-array browse-path))))
         :timeout-before-submitted 100)]
    (if-not (= status :success)
      {:error (or stacktrace value)
       :undefined (.contains ^String value (str :replique-watch/undefined))}
      {:refresh-watch (elisp/->ElispString value)})))

(defmethod tooling-msg/tooling-msg-handle [:replique/browser :refresh-watch]
  [msg]
  (tooling-msg/with-tooling-response msg
    (refresh-watch-cljs @@cljs-repl-env msg)))

(defmethod tooling-msg/tooling-msg-handle [:replique/nashorn :refresh-watch]
  [msg]
  (tooling-msg/with-tooling-response msg
    (refresh-watch-cljs @@cljs-repl-env-nashorn msg)))

(defn browse-candidates-cljs
  [repl-env {:keys [var-sym buffer-id prefix print-meta browse-path] :as msg}]
  (let [{:keys [status value stacktrace] :as ret}
        (@cljs-evaluate-form
         repl-env
         (format "replique.cljs_env.watch.browse_candidates(%s, %s, [%s, %s, %s]);"
                 (pr-str buffer-id) (pr-str  (@cljs-munged var-sym))
                 (pr-str prefix)
                 (if (nil? print-meta)
                   "null"
                   (pr-str print-meta))
                 (if (nil? browse-path)
                   "null"
                   (pr-str-for-eval (browse-path->js-array browse-path))))
         :timeout-before-submitted 100)]
    (if-not (= status :success)
      {:error (or stacktrace value)
       :undefined (.contains ^String value (str :replique-watch/undefined))}
      {:candidates (elisp/->ElispString value)})))

(defmethod tooling-msg/tooling-msg-handle [:replique/browser :browse-candidates]
  [msg]
  (tooling-msg/with-tooling-response msg
    (browse-candidates-cljs @@cljs-repl-env msg)))

(defmethod tooling-msg/tooling-msg-handle [:replique/nashorn :browse-candidates]
  [msg]
  (tooling-msg/with-tooling-response msg
    (browse-candidates-cljs @@cljs-repl-env-nashorn msg)))

(defn can-browse-cljs
  [repl-env {:keys [candidate] :as msg}]
  (let [{:keys [status value stacktrace] :as ret}
        (@cljs-evaluate-form
         repl-env
         (format "replique.cljs_env.watch.can_browse(%s);" (pr-str candidate))
         :timeout-before-submitted 100)]
    (if-not (= status :success)
      {:error (or stacktrace value)}
      {:can-browse? (elisp/->ElispString value)})))

(defmethod tooling-msg/tooling-msg-handle [:replique/browser :can-browse?]
  [msg]
  (tooling-msg/with-tooling-response msg
    (can-browse-cljs @@cljs-repl-env msg)))

(defmethod tooling-msg/tooling-msg-handle [:replique/nashorn :can-browse?]
  [msg]
  (tooling-msg/with-tooling-response msg
    (can-browse-cljs @@cljs-repl-env-nashorn msg)))

(defn do-start-recording [{:keys [buffer-id record-size] :as msg}]
  (if-let [watched-ref (get @watched-refs buffer-id)]
    (swap! watched-refs update buffer-id #(protocols/start-recording % buffer-id record-size))
    (add-watch-and-retry msg do-start-recording)))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :start-recording]
  [msg]
  (tooling-msg/with-tooling-response msg
    (do-start-recording msg)))

(defn do-stop-recording [{:keys [buffer-id] :as msg}]
  (if-let [watched-ref (get @watched-refs buffer-id)]
    (swap! watched-refs update buffer-id #(protocols/stop-recording % buffer-id))
    (add-watch-and-retry msg do-stop-recording)))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :stop-recording]
  [msg]
  (tooling-msg/with-tooling-response msg
    (do-stop-recording msg)
    {}))

(defn start-recording-cljs
  [repl-env {:keys [buffer-id var-sym record-size] :as msg}]
  (let [{:keys [status value stacktrace] :as ret}
        (@cljs-evaluate-form
         repl-env
         (format "replique.cljs_env.watch.do_start_recording(%s, %s, [%s]);"
                 (pr-str buffer-id) (pr-str (@cljs-munged var-sym))
                 (pr-str record-size))
         :timeout-before-submitted 100)]
    (if-not (= status :success)
      {:error (or stacktrace value)}
      {})))

(defmethod tooling-msg/tooling-msg-handle [:replique/browser :start-recording]
  [msg]
  (tooling-msg/with-tooling-response msg
    (start-recording-cljs @@cljs-repl-env msg)))

(defmethod tooling-msg/tooling-msg-handle [:replique/nashorn :start-recording]
  [msg]
  (tooling-msg/with-tooling-response msg
    (start-recording-cljs @@cljs-repl-env-nashorn msg)))

(defn stop-recording-cljs
  [repl-env {:keys [buffer-id var-sym] :as msg}]
  (let [{:keys [status value stacktrace] :as ret}
        (@cljs-evaluate-form
         repl-env
         (format "replique.cljs_env.watch.do_stop_recording(%s, %s);"
                 (pr-str buffer-id) (pr-str (@cljs-munged var-sym)))
         :timeout-before-submitted 100)]
    (if-not (= status :success)
      {:error (or stacktrace value)}
      {})))

(defmethod tooling-msg/tooling-msg-handle [:replique/browser :stop-recording]
  [msg]
  (tooling-msg/with-tooling-response msg
    (stop-recording-cljs @@cljs-repl-env msg)))

(defmethod tooling-msg/tooling-msg-handle [:replique/nashorn :stop-recording]
  [msg]
  (tooling-msg/with-tooling-response msg
    (stop-recording-cljs @@cljs-repl-env-nashorn msg)))

(defn get-record-position [{:keys [buffer-id] :as msg}]
  (if-let [watched-ref (get @watched-refs buffer-id)]
    {:record-position (protocols/record-position watched-ref)}
    (add-watch-and-retry msg get-record-position)))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :record-position]
  [msg]
  (tooling-msg/with-tooling-response msg
    (get-record-position msg)))

(defn get-record-position-cljs
  [repl-env {:keys [buffer-id var-sym] :as msg}]
  (let [{:keys [status value stacktrace] :as ret}
        (@cljs-evaluate-form
         repl-env
         (format "replique.cljs_env.watch.get_record_position(%s, %s);"
                 (pr-str buffer-id) (pr-str (@cljs-munged var-sym)))
         :timeout-before-submitted 100)]
    (if-not (= status :success)
      {:error (or stacktrace value)}
      {:record-position (elisp/->ElispString value)})))

(defmethod tooling-msg/tooling-msg-handle [:replique/browser :record-position]
  [msg]
  (tooling-msg/with-tooling-response msg
    (get-record-position-cljs @@cljs-repl-env msg)))

(defmethod tooling-msg/tooling-msg-handle [:replique/nashorn :record-position]
  [msg]
  (tooling-msg/with-tooling-response msg
    (get-record-position-cljs @@cljs-repl-env-nashorn msg)))

(defn set-record-position [{:keys [buffer-id index] :as msg}]
  (if-let [watched-ref (get @watched-refs buffer-id)]
    (let [watched-ref (protocols/value-at-index watched-ref index)]
      (swap! watched-refs assoc buffer-id watched-ref)
      {:record-position (protocols/record-position watched-ref)})
    (add-watch-and-retry msg set-record-position)))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :set-record-position]
  [msg]
  (tooling-msg/with-tooling-response msg
    (set-record-position msg)))

(defn set-record-position-cljs
  [repl-env {:keys [buffer-id var-sym index] :as msg}]
  (let [{:keys [status value stacktrace] :as ret}
        (@cljs-evaluate-form
         repl-env
         (format "replique.cljs_env.watch.set_record_position(%s, %s, [%s]);"
                 (pr-str buffer-id) (pr-str (@cljs-munged var-sym))
                 (pr-str index))
         :timeout-before-submitted 100)]
    (if-not (= status :success)
      {:error (or stacktrace value)}
      {:record-position (elisp/->ElispString value)})))

(defmethod tooling-msg/tooling-msg-handle [:replique/browser :set-record-position]
  [msg]
  (tooling-msg/with-tooling-response msg
    (set-record-position-cljs @@cljs-repl-env msg)))

(defmethod tooling-msg/tooling-msg-handle [:replique/nashorn :set-record-position]
  [msg]
  (tooling-msg/with-tooling-response msg
    (set-record-position-cljs @@cljs-repl-env-nashorn msg)))

(def ^:dynamic *printed* nil)
(def ^:dynamic *results* nil)

(defn make-watched-pr [out]
  (fn watched-pr
    ([]
     (replique.repl/core-pr))
    ([x]
     (when (and (identical? *out* out) (= replique.repl/*repl-context* :eval))
       (reset! *printed* x))
     (when (= replique.repl/*repl-context* :print)
       (reset! *results* x))
     (replique.repl/core-pr x))
    ([x & more]
     ;; No need to set! *results* since print is only called with one argument by the REPL loop
     (when (and (identical? *out* out) (= replique.repl/*repl-context* :eval))
       (reset! *printed* x)
       (doseq [x more]
         (reset! *printed* x)))
     (apply replique.repl/core-pr x more))))

(defn repl []
  (binding [*printed* (or *printed* (atom nil))
            *results* (or *results* (atom nil))]
    (let [printed-buffer-id (str "printed-" (:client server/*session*))
          printed-watched-ref (protocols/->RecordedWatchedRef *printed* [@*printed*] 0 3 nil)
          results-buffer-id (str "results-" (:client server/*session*))
          results-watched-ref (protocols/->RecordedWatchedRef *results* [@*results*] 0 3 nil)]
      (swap! watched-refs assoc printed-buffer-id printed-watched-ref)
      (protocols/add-watch-handler printed-watched-ref printed-buffer-id)
      (swap! watched-refs assoc results-buffer-id results-watched-ref)
      (protocols/add-watch-handler results-watched-ref results-buffer-id)
      (try
        (binding [pr (make-watched-pr *out*)]
          (replique.repl/repl :init (fn [] (in-ns 'user))))
        (finally
          (remove-watch (protocols/get-ref printed-watched-ref)
                        (keyword "replique.watch" (str printed-buffer-id)))
          (swap! watched-refs dissoc printed-buffer-id)
          (remove-watch (protocols/get-ref results-watched-ref)
                        (keyword "replique.watch" (str results-buffer-id)))
          (swap! watched-refs dissoc results-buffer-id))))))

(comment
  (def tt (atom {{:e 33} "e" :f "f"}))

  (reset! tt '["eee" rrrr123])
  
  (get-in @tt [(symbol ":eee")])

  (doto (java.util.HashMap.) (.put "e" 33))

  (:replique.watch/value (meta tt))

  )

;; Not all non-readable symbols/keywords are handled - For example, symbols with spaces / symbols
;; that start with a "#" ...
