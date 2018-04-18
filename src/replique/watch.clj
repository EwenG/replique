(ns replique.watch
  (:require [replique.tooling-msg :as tooling-msg]
            [replique.utils :as utils]
            [replique.completion :as completion]
            [replique.elisp-printer :as elisp])
  (:import [clojure.lang IRef IDeref IPending ILookup]
           [java.util Map Collection]))

(def ^:private cljs-repl-env (utils/dynaload 'replique.repl-cljs/repl-env))
(def ^:private cljs-repl-env-nashorn (utils/dynaload 'replique.nashorn/repl-env))
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

(defn add-replique-watch [var-sym buffer-id]
  (let [var (resolve var-sym)
        ref (maybe-nested-iref var)]
    (add-watch ref (keyword "replique.watch" (str buffer-id)) (ref-watcher buffer-id))
    (swap! watched-refs assoc buffer-id ref)
    {}))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :add-watch]
  [{:keys [var-sym buffer-id] :as msg}]
  (tooling-msg/with-tooling-response msg
    (add-replique-watch var-sym buffer-id)))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :remove-watch]
  [{:keys [buffer-id] :as msg}]
  (tooling-msg/with-tooling-response msg
    ;; The ref may not be found if the process has been restarted
    (when-let [ref (get @watched-refs buffer-id)]
      (remove-watch ref (keyword "replique.watch" (str buffer-id)))
      (swap! watched-refs dissoc buffer-id)
      (swap! watched-refs-values dissoc buffer-id)
      nil)))

;; Like clojure.core/get for ILookup and maps. Get the nth element for collections
(defn browse-get [o k]
  (cond (or (instance? ILookup o) (instance? Map o))
        (get o k)
        (and (or (coll? o) (instance? Collection  o)))
        (nth (seq o) k)
        :else nil))

;; like clojure.core/get-in but uses browse-get instead of get
(defn browse-get-in [m ks]
  (reduce browse-get m ks))

(defn parse-browse-path [browse-path]
  (into '() (map read-string) browse-path))

(defn refresh-watch [{:keys [buffer-id update? var-sym print-length print-level browse-path]
                      :as msg}]
  (let [ref (get @watched-refs buffer-id)]
    (if (some? ref)
      (let [browse-path (parse-browse-path browse-path)]
        (when update?
          (swap! watched-refs-values assoc buffer-id @ref))
        (let [ref-value (get @watched-refs-values buffer-id)]
          {:var-value (binding [*print-length* print-length
                                *print-level* print-level]
                        (pr-str (browse-get-in ref-value browse-path)))}))
      (let [var (resolve var-sym)
            ref (maybe-nested-iref var)]
        (if ref
          (do
            (add-replique-watch var-sym buffer-id)
            (recur msg))
          {:error (IllegalStateException. (str var-sym " is not defined"))
           :undefined true})))))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :refresh-watch]
  [msg]
  (tooling-msg/with-tooling-response msg
    (refresh-watch msg)))

(declare serializable?)

(defn serializable-map-entry? [e]
  (and (serializable? (key e)) (serializable? (val e))))

(def ^:const symbols-separators-re #"[\]\[\s,\(\)\{\}\"\n\t~@^`;]")

;; Not all symbols can be read byt the Clojure LispReader
(defn serializable-symbol? [sym]
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

(defn serializable-keyword? [kw]
  (let [keyword-name (name kw)]
    (not (or (.endsWith keyword-name ":")
             (.contains keyword-name "::")
             (.startsWith keyword-name "/")
             (.endsWith keyword-name "/")
             (re-find symbols-separators-re keyword-name)))))

(defn serializable? [x]
  (cond (nil? x) true
        (instance? Boolean x) true
        (number? x) true
        (string? x) true
        (and (symbol? x) (serializable-symbol? x)) true
        (and (keyword? x) (serializable-keyword? x)) true
        (or (map? x) (instance? Map x)) (every? serializable-map-entry? x)
        (or (coll? x) (instance? Collection x)) (every? serializable? x)
        :else false))

(defn browse-candidates [{:keys [buffer-id var-sym prefix browse-path] :as msg}]
  (if-let [ref (get @watched-refs buffer-id)]
    (let [browse-path (parse-browse-path browse-path)
          ref-value (browse-get-in @ref browse-path)
          prefix-tokens (completion/tokenize-prefix (str prefix))]
      ;; Only return candidates that can be read by the clojure/clojurescript reader.
      ;; This allows keeping the browse path on the client side and thus makes serveral things
      ;; easier to implement, like handling a browser refresh for example
      {:candidates
       (cond (or (map? ref-value) (instance? Map ref-value))
             (doall
              (for [k (keys ref-value)
                    :when (and (completion/matches? (str k) prefix-tokens)
                               (serializable? k))]
                (binding [*print-length* nil
                          *print-level* nil
                          *print-meta* nil]
                  (pr-str k))))
             (or (coll? ref-value) (instance? Collection ref-value)
                 (and (class ref-value) (.isArray (class ref-value))))
             (doall
              (for [i (range (count ref-value))
                    :when (completion/matches? (str i) prefix-tokens)]
                (binding [*print-length* nil
                          *print-level* nil
                          *print-meta* nil]
                  (pr-str i))))
             :else nil)})
    (let [var (resolve var-sym)
          ref (maybe-nested-iref var)]
      (if ref
        (do
          (add-replique-watch var-sym buffer-id)
          (recur msg))
        {:error (IllegalStateException. (str var-sym " is not defined"))
         :undefined true}))))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :browse-candidates] [msg]
  (tooling-msg/with-tooling-response msg
    (browse-candidates msg)))

(comment
  (.isArray (class (int-array 2)))
  (.isArray (class nil))
  (get (int-array 2) 1)

  (instance? java.util.Map (doto (java.util.HashMap.)
                             (.put "e" 3)))
  (map key (doto (java.util.HashMap.)
                     (.put "e" 3)))
  (coll? "ee")
  (instance? java.util.Collection (doto (java.util.ArrayList.)
                                    (.add 3)))
  (nth (doto (java.util.ArrayList.)
         (.add 3)) 0)
  (get (doto (java.util.ArrayList.)
         (.add 3)) 1)
  
  (= (doto (java.util.ArrayList.)
       (.add 3)) [3])

  (coll? #{"e"})
  
  )

(defn add-replique-watch-cljs [repl-env {:keys [process-id var-sym buffer-id] :as msg}]
  (let [{:keys [status value]}
        (@cljs-evaluate-form
         repl-env
         (format "replique.cljs_env.watch.add_replique_watch(%s, %s, %s);"
                 (pr-str process-id) (pr-str (@cljs-munged var-sym)) (pr-str buffer-id))
         :timeout-before-submitted 100)]
    (if-not (= status :success)
      {:error value}        
      {})))

(defmethod tooling-msg/tooling-msg-handle [:replique/browser :add-watch] [msg]
  (tooling-msg/with-tooling-response msg
    (add-replique-watch-cljs @@cljs-repl-env msg)))

(defmethod tooling-msg/tooling-msg-handle [:replique/nashorn :add-watch] [msg]
  (tooling-msg/with-tooling-response msg
    (add-replique-watch-cljs @@cljs-repl-env-nashorn msg)))

(defn remove-watch-cljs [repl-env {:keys [process-id buffer-id] :as msg}]
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
  [repl-env {:keys [process-id update? var-sym buffer-id print-length print-level browse-path]
             :as msg}]
  (let [{:keys [status value stacktrace] :as ret}
        (@cljs-evaluate-form
         repl-env
         (format "replique.cljs_env.watch.refresh_watch(%s, %s, %s, %s, %s, %s, %s);"
                 (pr-str process-id) (pr-str (boolean update?)) 
                 (pr-str (@cljs-munged var-sym)) (pr-str buffer-id)
                 (if (nil? print-length) "null" (pr-str print-length))
                 (if (nil? print-level) "null" (pr-str print-level))
                 (if (nil? browse-path) "null" (pr-str (browse-path->js-array browse-path))))
         :timeout-before-submitted 100)]
    (if-not (= status :success)
      {:error (or stacktrace value)
       :undefined (.contains ^String value (str :replique-watch/undefined))}
      {:var-value value})))

(defmethod tooling-msg/tooling-msg-handle [:replique/browser :refresh-watch]
  [msg]
  (tooling-msg/with-tooling-response msg
    (refresh-watch-cljs @@cljs-repl-env msg)))

(defmethod tooling-msg/tooling-msg-handle [:replique/nashorn :refresh-watch]
  [msg]
  (tooling-msg/with-tooling-response msg
    (refresh-watch-cljs @@cljs-repl-env-nashorn msg)))

(defn browse-candidates-cljs
  [repl-env {:keys [process-id var-sym buffer-id prefix browse-path] :as msg}]
  (let [{:keys [status value stacktrace] :as ret}
        (@cljs-evaluate-form
         repl-env
         (format "replique.cljs_env.watch.browse_candidates(%s, %s, %s, %s, %s);"
                 (pr-str process-id) (pr-str (@cljs-munged var-sym)) (pr-str buffer-id)
                 (pr-str prefix) (if (nil? browse-path)
                                   "null"
                                   (pr-str (browse-path->js-array browse-path))))
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

(comment
  (def tt (atom {:e "e" :f "f"}))
  
  (reset! tt {(symbol "\"ee\"") 33
              (keyword "e~ee") 33
              "eee  " 44
              \e 33
              Collection 33})

  (get-in @tt [(symbol ":eee")])
  
  '{:type :add-watch, :repl-env :replique/browser, :var-sym replique.cljs-env.watch/tt, :buffer-id 1, :process-id "/home/ewen/clojure/replique/", :correlation-id 3512}

  )
