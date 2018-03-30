(ns replique.cljs-env.watch
  (:require [replique.cljs-env.repl :as repl]
            [replique.cljs-env.elisp-printer :as elisp]))

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
    (swap! watched-refs dissoc buffer-id)))

(defn update-replique-watch [process-id var-sym buffer-id]
  (let [watchable (get @watched-refs buffer-id)]
    (if (some? watchable)
      (let [watchable-value (maybe-deref watchable)]
        (swap! watched-refs-values assoc buffer-id watchable-value)
        watchable-value)
      (let [watchable (maybe-nested-iref var-sym)]
        (if watchable
          (do
            (add-replique-watch process-id var-sym buffer-id)
            (recur process-id var-sym buffer-id))
          (throw (js/Error. :replique-watch/undefined)))))))

(comment
  (def tt (atom 33))
  (reset! tt 5)

  (remove-watch tt (keyword "replique.watch" "1"))
  (.-watches tt)

  (add-replique-watch "/home/ewen/clojure/replique/"
                      replique.cljs-env.watch/tt
                      1)
  (remove-replique-watch 1)

  (maybe-nested-iref replique.cljs-env.watch/tt)
  (maybe-deref tt)
  )
