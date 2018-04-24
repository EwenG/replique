(ns replique.cljs-env.watch
  (:require [replique.cljs-env.repl :as repl]
            [replique.cljs-env.elisp-printer :as elisp]
            [replique.cljs-env.completion :as completion]
            [cljs.reader :as reader]
            [goog.object :as o]
            [goog.string :as s]))

(defonce watched-refs (atom {}))

(defn notification-watcher [process-id buffer-id k ref old-value value]
  (repl/send-print-tooling
   (binding [*print-level* nil
             *print-length* nil]
     (elisp/pr-str {:type :watch-update
                    :process-id process-id
                    :buffer-id buffer-id}))))

(declare add-recorded-watch-value)

(defn recorder-watcher [buffer-id k ref old-value value]
  (swap! watched-refs update buffer-id add-recorded-watch-value value))

(defn maybe-deref [x]
  (if (satisfies? IDeref x) @x x))

(defprotocol IWatchHandler
  (add-watch-handler [this process-id buffer-id]))

(defprotocol IMostRecentValue
  (most-recent-value [this]))

(defprotocol IRecordable
  (record [this])
  (stop-recording [this]))

(defprotocol IGetWatchable
  (get-watchable [this]))

(deftype Watched [watchable values index]
  IGetWatchable
  (get-watchable [this] watchable)
  IDeref
  (-deref [this] (get values index))
  IWatchHandler
  (add-watch-handler [this process-id buffer-id]
    (add-watch watchable (keyword "replique.watch" (str buffer-id))
               (partial notification-watcher process-id buffer-id)))
  IMostRecentValue
  (most-recent-value [this] (Watched. watchable [(maybe-deref watchable)] 0)))

(deftype RecordedWatched [watchable values index]
  IGetWatchable
  (get-watchable [this] watchable)
  IDeref
  (-deref [this] (get values index))
  IWatchHandler
  (add-watch-handler [this process-id buffer-id]
    (add-watch watchable (keyword "replique.watch" (str buffer-id))
               (juxt
                (partial recorder-watcher buffer-id)
                (partial notification-watcher buffer-id))))
  IMostRecentValue
  (most-recent-value [this] (RecordedWatched. watchable values
                                              (dec (count values)))))

(defn add-recorded-watch-value [recorded-watch new-value]
  (->RecordedWatched (.-ref recorded-watch)
                     (conj (.-values recorded-watch) new-value)
                     (.-index recorded-watch)))

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

(defn add-replique-watch [process-id var-sym buffer-id]
  (let [watchable (maybe-nested-iref var-sym)
        watched (->Watched watchable [(maybe-deref watchable)] 0)]
    (swap! watched-refs assoc buffer-id watched)
    (add-watch-handler watched process-id buffer-id)))

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

(defn refresh-watch [process-id update? var-sym buffer-id
                     print-length print-level print-meta
                     browse-path]
  (let [watched (get @watched-refs buffer-id)]
    (if (some? watched)
      (let [browse-path (parse-browse-path browse-path)
            watched (if update? (most-recent-value watched) watched)]
        (swap! watched-refs assoc buffer-id watched)
        (let [watchable-value @watched]
          (binding [*print-length* print-length
                    *print-level* print-level
                    *print-meta* print-meta]
            (pr-str (browse-get-in watchable-value browse-path)))))
      (let [watchable (maybe-nested-iref var-sym)]
        (if watchable
          (do
            (add-replique-watch process-id var-sym buffer-id)
            (recur process-id update? var-sym buffer-id
                   print-length print-level print-meta
                   browse-path))
          (throw (js/Error. :replique-watch/undefined)))))))

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

(defn browse-candidates [process-id var-sym buffer-id prefix print-meta browse-path]
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
    (let [watchable (maybe-nested-iref var-sym)]
      (if watchable
        (do
          (add-replique-watch process-id var-sym buffer-id)
          (recur process-id var-sym buffer-id prefix print-meta browse-path))
        (throw (js/Error. :replique-watch/undefined))))))

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

(defn can-browse [process-id candidate]
  (binding [*print-level* nil
            *print-length* nil]
    (elisp/pr-str (browsable-serialized-key? candidate))))

(comment
  (def tt (atom {:e "e"}))
  (reset! tt {(keyword "ee~rr") #js [1 2 3 4]
              #js {:f 2} #js [1 2 3 4]
              #js {:f 2} #js [1 2 3 4]
              \r 33
              {'eeee "eeee"} true})

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
