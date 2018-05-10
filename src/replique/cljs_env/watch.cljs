(ns replique.cljs-env.watch
  (:require [replique.cljs-env.repl :as repl]
            [replique.cljs-env.elisp-printer :as elisp]
            [replique.cljs-env.completion :as completion]
            [cljs.reader :as reader]
            [goog.object :as o]))

(defonce watched-refs (atom {}))

(defn notification-watcher [buffer-id k ref old-value value]
  (repl/send-print-tooling
   (binding [*print-level* nil
             *print-length* nil]
     (elisp/pr-str {:type :watch-update
                    :process-id repl/*process-id*
                    :buffer-id buffer-id}))))

(declare add-recorded-watch-value)

(defn recorder-watcher [buffer-id k ref old-value value]
  (swap! watched-refs update buffer-id add-recorded-watch-value value))

(defn maybe-deref [x]
  (if (satisfies? IDeref x) @x x))

(defprotocol IWatchHandler
  (add-watch-handler [this buffer-id]))

(defprotocol IMostRecentValue
  (most-recent-value [this]))

(defprotocol IRecordable
  (start-recording [this buffer-id record-size])
  (stop-recording [this buffer-id])
  (record-position [this])
  (value-at-index [this index]))

(defprotocol IGetWatchable
  (get-watchable [this]))

(declare ->Watched)
(declare ->RecordedWatched)

(defn recorded-watched-with-record-size [watchable values index record-size]
  (if (> (count values) record-size)
    (let [drop-n (- (count values) record-size)
          new-values (if (> drop-n index)
                       (into [(get values index)] (drop (inc drop-n)) values)
                       (into [] (drop drop-n) values))]
      (->RecordedWatched watchable new-values (max 0 (- index drop-n)) record-size))
    (->RecordedWatched watchable values index record-size)))

(deftype Watched [watchable values index]
  IGetWatchable
  (get-watchable [this] watchable)
  IDeref
  (-deref [this] (get values index))
  IWatchHandler
  (add-watch-handler [this buffer-id]
    (add-watch watchable (keyword "replique.watch" (str buffer-id))
               (partial notification-watcher buffer-id)))
  IMostRecentValue
  (most-recent-value [this] (->Watched watchable [(maybe-deref watchable)] 0))
  IRecordable
  (start-recording [this buffer-id record-size]
    (remove-watch watchable (keyword "replique.watch" (str buffer-id)))
    (let [watched (->RecordedWatched watchable values index record-size)]
      (add-watch-handler watched buffer-id)
      watched))
  (stop-recording [this buffer-id] this)
  (record-position [this]
    {:index (inc index) :count (count values)})
  (value-at-index [this index]
    (->Watched watchable values (max (min index (dec (count values))) 0))))

(deftype RecordedWatched [watchable values index record-size]
  IGetWatchable
  (get-watchable [this] watchable)
  IDeref
  (-deref [this] (get values index))
  IWatchHandler
  (add-watch-handler [this buffer-id]
    (add-watch watchable (keyword "replique.watch" (str buffer-id))
               (juxt
                (partial recorder-watcher buffer-id)
                (partial notification-watcher buffer-id))))
  IMostRecentValue
  (most-recent-value [this] (->RecordedWatched watchable values
                                               (dec (count values))
                                               record-size))
  IRecordable
  (start-recording [this buffer-id record-size]
    (recorded-watched-with-record-size watchable values index record-size))
  (stop-recording [this buffer-id]
    (remove-watch watchable (keyword "replique.watch" (str buffer-id)))
    (let [watched (->Watched watchable values index)]
      (add-watch-handler watched buffer-id)
      watched))
  (record-position [this]
    {:index (inc index) :count (count values)})
  (value-at-index [this index]
    (->RecordedWatched watchable values (max (min index (dec (count values))) 0) record-size)))

(defn add-recorded-watch-value [recorded-watch new-value]
  (recorded-watched-with-record-size (.-watchable recorded-watch)
                                     (conj (.-values recorded-watch) new-value)
                                     (.-index recorded-watch)
                                     (.-record-size recorded-watch)))

(defn maybe-nested-iref [x]
  (loop [iref x
         candidate x]
    (cond (and (satisfies? IWatchable candidate)
               (not (satisfies? IWatchable iref)))
          (recur candidate candidate)
          (satisfies? IDeref candidate)
          (if (satisfies? IPending candidate)
            (if (realized? candidate)
              (recur iref @candidate)
              iref)
            (recur iref @candidate)) 
          :else iref)))

(defn add-replique-watch [var-sym buffer-id]
  (let [watchable (maybe-nested-iref var-sym)
        watched (->Watched watchable [(maybe-deref watchable)] 0)]
    (swap! watched-refs assoc buffer-id watched)
    (add-watch-handler watched buffer-id)))

(defn add-watch-and-retry
  ([buffer-id var-sym retry-fn]  
   (add-watch-and-retry buffer-id var-sym retry-fn nil))
  ([buffer-id var-sym retry-fn params]
   (let [watchable (maybe-nested-iref var-sym)]
     (if watchable
       (do
         (add-replique-watch var-sym buffer-id)
         (if params
           (retry-fn buffer-id var-sym params)
           (retry-fn buffer-id var-sym)))
       (throw (js/Error. :replique-watch/undefined))))))

(defn remove-replique-watch [buffer-id]
  ;; If the js runtime is restarted between add-watch and remove-watch, the watchable object
  ;; may not be found
  (when-let [watched (get @watched-refs buffer-id)]
    (remove-watch (get-watchable watched) (keyword "replique.watch" (str buffer-id)))
    (swap! watched-refs dissoc buffer-id)))

(defn browse-get [o k]
  (cond (map? o)
        (get o k)
        (object? o) (o/get o k)
        (array? o) (aget o k)
        (and (coll? o) (seqable? o)) (nth (seq o) k)
        :else nil))

(defn browse-get-in [m ks]
  (reduce browse-get m ks))

(defn parse-browse-path [browse-path]
  (into '() (map reader/read-string) browse-path))

(defn refresh-watch [buffer-id var-sym
                     [update? print-length print-level print-meta
                      browse-path :as params]]
  (if-let [watched (get @watched-refs buffer-id)]
    (let [browse-path (parse-browse-path browse-path)
          watched (if update? (most-recent-value watched) watched)]
      (swap! watched-refs assoc buffer-id watched)
      (let [watchable-value @watched]
        (binding [*print-length* print-length
                  *print-level* print-level
                  *print-meta* print-meta]
          (pr-str (browse-get-in watchable-value browse-path)))))
    (add-watch-and-retry buffer-id var-sym refresh-watch params)))

#_(declare serializable?)

#_(def ^:const symbols-separators-re #"[\]\[\s,\(\)\{\}\"\n\t~@^`;]")

#_(defn serializable-symbol? [sym]
  (let [sym-str (name sym)]
    (not (or (s/startsWith sym-str ":")
             (s/endsWith sym-str ":")
             (s/contains sym-str "::")
             (s/startsWith sym-str "#")
             (s/startsWith sym-str "/")
             (s/endsWith sym-str "/")
             ;; The symbol does not start with a decimal
             (<= 48 (.charCodeAt sym-str 0) 57)
             (re-find symbols-separators-re sym-str)))))

#_(defn serializable-keyword? [kw]
  (let [keyword-name (name kw)]
    (not (or (s/endsWith keyword-name ":")
             (s/contains keyword-name "::")
             (s/startsWith keyword-name "/")
             (s/endsWith keyword-name "/")
             (re-find symbols-separators-re keyword-name)))))

#_(defn serializable-map-entry? [e]
  (and (serializable? (key e)) (serializable? (val e))))

#_(defn serializable-object-entry? [o k]
  (and (serializable? k) (serializable? (o/get o k))))

#_(defn serializable? [x]
  (cond (nil? x) true
        (boolean? x) true
        (number? x) true
        (string? x) true
        (and (symbol? x) (serializable-symbol? x)) true
        (and (keyword? x) (serializable-keyword? x)) true
        (map? x) (every? serializable-map-entry? x)
        (object? x) (and o/getAllPropertyNames
                         (every? (partial serializable-object-entry? x)
                                 (o/getAllPropertyNames x)))
        (or (coll? x) (array? x)) (every? serializable? x)
        :else false))

(defn browse-candidates [buffer-id var-sym
                         [prefix print-meta browse-path :as params]]
  (if-let [watched (get @watched-refs buffer-id)]
    (let [browse-path (parse-browse-path browse-path)
          watchable-value (browse-get-in @watched browse-path)
          prefix-tokens (completion/tokenize-prefix (str prefix))
          candidates (cond (or (map? watchable-value) (object? watchable-value))
                           (doall
                            (let [m-keys (if (object? watchable-value)
                                           (o/getKeys watchable-value)
                                           (keys watchable-value))]
                              (for [k m-keys
                                    :when (and (completion/matches? (str k) prefix-tokens))]
                                (binding [*print-length* nil
                                          *print-level* nil
                                          *print-meta* print-meta]
                                  (pr-str k)))))
                           (or (coll? watchable-value) (array? watchable-value))
                           (doall
                            (for [i (range (count watchable-value))
                                  :when (completion/matches? (str i) prefix-tokens)]
                              (binding [*print-length* nil
                                        *print-level* nil
                                        *print-meta* print-meta]
                                (pr-str i)))))]
      (binding [*print-level* nil
                *print-length* nil]
        (if (empty? (str prefix))
          (elisp/pr-str (cons "" candidates))
          (elisp/pr-str candidates))))
    (add-watch-and-retry buffer-id var-sym browse-candidates params)))

(declare browsable-key?)

(defn browsable-map-entry? [e]
  (and (browsable-key? (key e)) (browsable-key? (val e))))

;; objects / arrays are not vaid browsable keys since they do not implement cljs equality.
;; Iterating over the keys of a map would not be the right thing to do since there could
;; be multiple objects/arrays with the same value (according to cljs equality)
(defn browsable-key? [x]
  (cond (nil? x) true
        (boolean? x) true
        (number? x) true
        (string? x) true
        (symbol? x) true
        (keyword? x) true
        (map? x) (every? browsable-map-entry? x)
        (coll? x) (every? browsable-key? x)
        :else false))

(defn browsable-serialized-key? [x-str]
  (try
    (let [x (reader/read-string x-str)]
      ;; check things like (read-string "ee~rr")
      (if (or (and (symbol? x) (not= x-str (pr-str x)))
              (and (keyword? x) (not= x-str (pr-str x))))
        false
        (browsable-key? x)))
    (catch js/Error e false)))

(defn can-browse [candidate]
  (binding [*print-level* nil
            *print-length* nil]
    (elisp/pr-str (browsable-serialized-key? candidate))))

(defn do-start-recording [buffer-id var-sym [record-size :as params]]
  (if-let [watched (get @watched-refs buffer-id)]
    (swap! watched-refs update buffer-id #(start-recording % buffer-id record-size))
    (add-watch-and-retry buffer-id var-sym do-start-recording params)))

(defn do-stop-recording [buffer-id var-sym]
  (if-let [watched (get @watched-refs buffer-id)]
    (swap! watched-refs update buffer-id #(stop-recording % buffer-id))
    (add-watch-and-retry buffer-id var-sym do-stop-recording)))

(defn get-record-position [buffer-id var-sym]
  (if-let [watched (get @watched-refs buffer-id)]
    (binding [*print-level* nil
              *print-length* nil]
      (elisp/pr-str (record-position watched)))
    (add-watch-and-retry buffer-id var-sym get-record-position)))

(defn set-record-position [buffer-id var-sym [index :as params]]
  (if-let [watched (get @watched-refs buffer-id)]
    (let [watched (value-at-index watched index)]
      (swap! watched-refs assoc buffer-id watched)
      (elisp/pr-str (record-position watched)))
    (add-watch-and-retry buffer-id var-sym set-record-position params)))

(defonce printed (atom nil))
(defonce core-pr-with-opts cljs.core/pr-with-opts)
(defonce results (atom nil))

(defn watched-pr-with-opts
  ([objs opts]
   (doseq [obj objs]
     (reset! printed obj))
   (core-pr-with-opts objs opts)))

(o/set js/cljs.core "pr_with_opts" watched-pr-with-opts)

(let [buffer-id "var-replique.cljs-env.watch/printed"
      watched-ref (->RecordedWatched printed [@printed] 0 3)]
  (when (not (contains? watched-refs buffer-id))
    (swap! watched-refs assoc buffer-id watched-ref)
    (add-watch-handler watched-ref buffer-id)))

(let [buffer-id "var-replique.cljs-env.watch/results"
      watched-ref (->RecordedWatched results [@results] 0 3)]
  (when (not (contains? watched-refs buffer-id))
    (swap! watched-refs assoc buffer-id watched-ref)
    (add-watch-handler watched-ref buffer-id)))

(comment
  (def tt (atom {:e "e"}))
  (reset! tt {(keyword "ee~rr") #js [1 2 3 4]
              #js {:f 2} #js [1 2 3 4]
              #js {:f 2} #js [1 2 3 4]
              \r 33
              {'eeee "eeee3"} true})

  (reset! tt (with-meta {(with-meta [33] {33 44 "ee" 44}) "e"
                         #js {:f 2} #js [1 2 3 4]}
               {:m "m"}))

  (remove-watch tt (keyword "replique.watch" "1"))
  (.-watches tt)

  (add-replique-watch "/home/ewen/clojure/replique/"
                      replique.cljs-env.watch/tt
                      1)
  (remove-replique-watch 1)

  (maybe-nested-iref replique.cljs-env.watch/tt)
  (maybe-deref tt)
  )
