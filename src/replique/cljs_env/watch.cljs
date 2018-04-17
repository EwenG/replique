(ns replique.cljs-env.watch
  (:require [replique.cljs-env.repl :as repl]
            [replique.cljs-env.elisp-printer :as elisp]
            [replique.cljs-env.completion :as completion]
            [cljs.reader :as reader]
            [goog.object :as o]))

(defonce watched-refs (atom {}))
(defonce watched-refs-values (atom {}))

(defn update-watch [process-id buffer-id ref value]
  (repl/send-print-tooling
   (binding [*print-level* nil
             *print-length* nil]
     (elisp/pr-str {:type :watch-update
                    :process-id process-id
                    :buffer-id buffer-id}))))

(defn ref-watcher [process-id buffer-id]
  (fn ref-watcher* [k r o n]
    (update-watch process-id buffer-id r n)))

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

(defn maybe-deref [x]
  (if (satisfies? IDeref x) @x x))

(defn add-replique-watch [process-id var-sym buffer-id]
  (let [watchable (maybe-nested-iref var-sym)]
    (add-watch watchable
               (keyword "replique.watch" (str buffer-id))
               (ref-watcher process-id buffer-id))
    (swap! watched-refs assoc buffer-id watchable)))

(defn remove-replique-watch [buffer-id]
  ;; If the js runtime is restarted between add-watch and remove-watch, the watchable object
  ;; may not be found
  (when-let [watchable (get @watched-refs buffer-id)]
    (remove-watch watchable (keyword "replique.watch" (str buffer-id)))
    (swap! watched-refs dissoc buffer-id)
    (swap! watched-refs-values dissoc buffer-id)))

(declare js-equal)

(defn js-equal-objects [x1 x2]
  (let [ks (o/getKeys x1)
        ks-length (.-length ks)]
    (loop [i 0]
      (if (>= i ks-length)
        (let [ks (o/getKeys x2)
              ks-length (.-length ks)]
          (loop [i 0]
            (if (>= i ks-length)
              true
              (let [k (aget ks i)]
                (if (not (o/containsKey x1 k))
                  false
                  (recur (inc i)))))))
        (let [k (aget ks i)]
          (if (or (not (o/containsKey x2 k))
                  (not (js-equal (o/get x1 k) (o/get x2 k))))
            false
            (recur (inc i))))))))

(defn js-equal-arrays [x1 x2]
  (let [x1-length (.-length x1)
        x2-length (.-length x2)]
    (when (= x1-length x2-length)
      (loop [i 0]
        (if (>= i x1-length)
          true
          (if (js-equal (aget x1 i) (aget x2 i))
            (recur (inc i))
            false))))))

(defn js-equal [x1 x2]
  (cond (and (object? x1) (object? x2))
        (js-equal-objects x1 x2)
        (and (array? x1) (array? x2))
        (js-equal-arrays x1 x2)
        :else (= x1 x2)))

;; js objects/array do not implement cljs equality
(defn browse-get-js [o k]
  (when (map? o)
    (when-let [entry (some #(when (js-equal k (key %)) %) o)]
      (val entry))))

(defn browse-get [o k]
  (cond (implements? ILookup o)
        (if (or (object? k) (array? k))
          (browse-get-js o k)
          (get o k))
        (object? o) (o/get o k)
        (array? o) (aget o k)
        (and (coll? o) (seqable? o)) (nth (seq o) k)
        :else nil))

(defn browse-get-in [m ks]
  (reduce browse-get m ks))

(defn parse-browse-path [browse-path]
  (into '() (map reader/read-string) browse-path))

(defn refresh-watch [process-id update? var-sym buffer-id
                     print-length print-level browse-path]
  (let [watchable (get @watched-refs buffer-id)]
    (if (some? watchable)
      (let [browse-path (parse-browse-path browse-path)]
        (when update?
          (swap! watched-refs-values assoc buffer-id (maybe-deref watchable)))
        (let [watchable-value (get @watched-refs-values buffer-id)]
          (binding [*print-length* print-length
                    *print-level* print-level]
            (pr-str (browse-get-in watchable-value browse-path)))))
      (let [watchable (maybe-nested-iref var-sym)]
        (if watchable
          (do
            (add-replique-watch process-id var-sym buffer-id)
            (recur process-id update? var-sym buffer-id
                   print-length print-level browse-path))
          (throw (js/Error. :replique-watch/undefined)))))))

(declare serializable?)

(defn serializable-map-entry? [e]
  (and (serializable? (key e)) (serializable? (val e))))

(defn serializable-object-entry? [o k]
  (and (serializable? k) (serializable? (o/get o k))))

(defn serializable? [x]
  (cond (nil? x) true
        (boolean? x) true
        (number? x) true
        (string? x) true
        (symbol? x) true
        (keyword? x) true
        (map? x) (every? serializable-map-entry? x)
        (object? x) (and o/getAllPropertyNames
                         (every? (partial serializable-object-entry? x)
                                 (o/getAllPropertyNames x)))
        (or (coll? x) (array? x)) (every? serializable? x)
        :else false))

(defn browse-candidates [process-id var-sym buffer-id prefix browse-path]
  (let [watchable (get @watched-refs buffer-id)]
    (if (some? watchable)
      (let [watchable-value (maybe-deref watchable)
            browse-path (parse-browse-path browse-path)
            watchable-value (browse-get-in watchable-value browse-path)
            prefix-tokens (completion/tokenize-prefix (str prefix))]
        (-> (cond (or (map? watchable-value) (object? watchable-value))
                 (doall
                  (for [k (if (object? watchable-value)
                            (o/getKeys watchable-value)
                            (keys watchable-value))
                        :when (and (completion/matches? (str k) prefix-tokens)
                                   (serializable? k))]
                    (binding [*print-length* nil
                              *print-level* nil
                              *print-meta* nil]
                      (pr-str k))))
                 (or (coll? watchable-value) (array? watchable-value))
                 (doall
                  (for [i (range (count watchable-value))
                        :when (completion/matches? (str i) prefix-tokens)]
                    (binding [*print-length* nil
                              *print-level* nil
                              *print-meta* nil]
                      (pr-str i))))
                 :else nil)
            elisp/pr-str))
      (let [watchable (maybe-nested-iref var-sym)]
        (if watchable
          (do
            (add-replique-watch process-id var-sym buffer-id)
            (recur process-id var-sym buffer-id prefix browse-path))
          (throw (js/Error. :replique-watch/undefined)))))))

(comment
  (def tt (atom {:e "e"}))
  (reset! tt {#js {:e 2} #js [1 2 3 4]
              #js {:f 2} #js [1 2 3 4]
              [1 2 3] nil})

  (remove-watch tt (keyword "replique.watch" "1"))
  (.-watches tt)

  (add-replique-watch "/home/ewen/clojure/replique/"
                      replique.cljs-env.watch/tt
                      1)
  (remove-replique-watch 1)

  (maybe-nested-iref replique.cljs-env.watch/tt)
  (maybe-deref tt)
  )
