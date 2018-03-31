(ns replique.watch
  (:require [replique.tooling-msg :as tooling-msg]
            [replique.utils :as utils])
  (:import [clojure.lang IRef IDeref IPending]
           [java.util Map]))

(def ^:private cljs-repl-env (utils/dynaload 'replique.repl-cljs/repl-env))
(def ^:private cljs-evaluate-form (utils/dynaload 'replique.repl-cljs/evaluate-form))
(def ^:private cljs-munged (utils/dynaload 'cljs.compiler/munge))

(defonce watched-refs (atom {}))
(defonce watched-refs-values (atom {}))

(defn update-watch [buffer-id ref value]
  (binding [*out* tooling-msg/tooling-out]
    (utils/with-lock tooling-msg/tooling-out-lock
      (tooling-msg/tooling-prn {:type :watch-update
                                :process-id tooling-msg/process-id
                                :buffer-id buffer-id}))))

(defn ref-watcher [buffer-id]
  (fn ref-watcher* [k r o n]
    (update-watch buffer-id r n)))

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

(comment
  (type (maybe-nested-iref #'replique.repl-cljs/compiler-env))
  (.getWatches (maybe-nested-iref #'replique.repl-cljs/compiler-env))
  (instance? clojure.lang.IDeref @#'replique.repl-cljs/compiler-env)
  (delay? @#'replique.repl-cljs/compiler-env)

  (remove-watch (maybe-nested-iref #'replique.repl-cljs/compiler-env) ::2)
  )

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :add-watch]
  [{:keys [var-sym buffer-id] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [var (resolve var-sym)
          ref (maybe-nested-iref var)]
      (add-watch ref (keyword "replique.watch" (str buffer-id)) (ref-watcher buffer-id))
      (swap! watched-refs assoc buffer-id ref)
      {})))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :remove-watch]
  [{:keys [buffer-id] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [ref (get @watched-refs buffer-id)]
      (remove-watch ref (keyword "replique.watch" (str buffer-id)))
      (swap! watched-refs dissoc buffer-id)
      (swap! watched-refs-values dissoc buffer-id)
      nil)))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :update-watch]
  [{:keys [buffer-id print-length print-level] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [ref (get @watched-refs buffer-id)
          ref-value @ref]
      (swap! watched-refs-values assoc buffer-id ref-value)
      {:var-value (binding [*print-length* print-length
                            *print-level* print-level]
                    (pr-str ref-value))})))

(defmethod tooling-msg/tooling-msg-handle [:replique/cljs :add-watch]
  [{:keys [process-id var-sym buffer-id] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [{:keys [status value]}
          (@cljs-evaluate-form
           @@cljs-repl-env
           (format "replique.cljs_env.watch.add_replique_watch(%s, %s, %s);"
                   (pr-str process-id) (pr-str (@cljs-munged var-sym)) (pr-str buffer-id))
           :timeout-before-submitted 100)]
      (if-not (= status :success)
        {:error value}        
        {}))))

(defmethod tooling-msg/tooling-msg-handle [:replique/cljs :remove-watch]
  [{:keys [process-id buffer-id] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [{:keys [status value]}
          (tooling-msg/with-tooling-response msg
            (@cljs-evaluate-form
             @@cljs-repl-env
             (format "replique.cljs_env.watch.remove_replique_watch(%s);" (pr-str buffer-id))
             :timeout-before-submitted 100))]
      (if-not (= status :success)
        {:error value}
        {}))))

(defmethod tooling-msg/tooling-msg-handle [:replique/cljs :update-watch]
  [{:keys [process-id var-sym buffer-id print-length print-level] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [{:keys [status value stacktrace] :as ret}
          (@cljs-evaluate-form
           @@cljs-repl-env
           (format "replique.cljs_env.watch.update_replique_watch(%s, %s, %s, %s, %s);"
                   (pr-str process-id) (pr-str (@cljs-munged var-sym)) (pr-str buffer-id)
                   (if (nil? print-length) "null" (pr-str print-length))
                   (if (nil? print-level) "null" (pr-str print-level)))
           :timeout-before-submitted 100)]
      (if-not (= status :success)
        {:error (or stacktrace value)
         :undefined (.contains ^String value (str :replique-watch/undefined))}
        {:var-value value}))))

(comment
  (def tt (atom 2))
  (reset! tt 5)
  
  '{:type :add-watch, :repl-env :replique/browser, :var-sym replique.cljs-env.watch/tt, :buffer-id 1, :process-id "/home/ewen/clojure/replique/", :correlation-id 3512}

  )
