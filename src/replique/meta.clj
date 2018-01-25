(ns replique.meta
  (:require [clojure.java.io :as io]
            [replique.environment :as env]
            [clojure.string :as string]
            [clojure.repl]))

(defn safe-ns-resolve [comp-env ns sym]
  (try (env/ns-resolve comp-env ns sym)
       (catch ClassNotFoundException e nil)))

(defn ns-resolve-munged [comp-env ns sym]
  (let [splitted (string/split (str sym) #"\$")]
    (when (> (count splitted) 1)
      (->> (subvec splitted 0 2)
           (string/join "/")
           clojure.repl/demunge
           symbol
           (safe-ns-resolve comp-env ns)))))

;; The "jar:" protocol is not always added by the clojurescript compiler
(defn url-with-protocol-prefix [^String url-str]
  (cond
    (and (.contains url-str ".jar!") (not (.startsWith url-str "jar:")))
    (format "jar:%s" url-str)
    (and (.contains url-str ".zip!") (not (.startsWith url-str "zip:")))
    (format "zip:%s" url-str)
    :else url-str))

(defn ns-files [comp-env the-ns]
  (let [interns (env/ns-interns comp-env the-ns)]
    (distinct (keep (fn [[_ v]] (:file (env/meta comp-env v))) interns))))

(defn resource-str [^String f]
  (when f
    (try
      (url-with-protocol-prefix (str (io/as-url f)))
      (catch Exception _
        (try
          (let [file (java.io.File. f)]
            (if (.exists file)
              (.getAbsolutePath file)
              (when-let [res (io/resource f)]
                (str res))))
          (catch Exception _ nil))))))

(defn handle-meta-str [sym-at-point]
  (when-let [f (resource-str sym-at-point)]
    {:file f}))

#_(defn read-keyword [ns keyword-string]
  (let [kw (try (read-string keyword-string) (catch Exception e nil))]
    (when (keyword? kw)
      (if (= keyword-string (str kw))
        kw
        (keyword (str (env/ns-name ns)) (name kw))))))

(defn handle-meta [comp-env ns {:keys [in-comment? in-string? locals] :as context} ^String prefix]
  (let [ns (or (and ns (env/find-ns comp-env (symbol ns)))
               (env/find-ns comp-env (env/default-ns comp-env)))]
    (when (and prefix (not in-comment?))
      (if in-string?
        (handle-meta-str prefix)
        (let [prefix (.replaceFirst prefix "^#_" "")]
          (cond (contains? locals prefix) (let [[point-start] (get locals prefix)]
                                            {:local? true
                                             :point-start point-start})
                #_(.startsWith prefix ":")
                #_(let [double-colon? (.startsWith prefix "::")
                      prefix (.replaceFirst prefix "(^::?)" "")
                      scope-split-index (.lastIndexOf prefix "/")
                      scope-name (when (> scope-split-index -1) (subs prefix 0 scope-split-index))
                      prefix (if (> scope-split-index -1)
                               (subs prefix (inc scope-split-index))
                               prefix)]
                  (cond (and double-colon? scope-name)
                        (let [scope (env/resolve-namespace comp-env (symbol scope-name) ns)]
                          (when scope
                            {:keyword? true
                             :namespace (str scope)
                             :name (str prefix)
                             :files (->> (ns-files comp-env scope)
                                         (map resource-str))}))
                        double-colon?
                        {:keyword? true
                         :namespace (str (env/ns-name ns))
                         :name (str prefix)
                         :files (->> (ns-files comp-env ns)
                                     (map resource-str))}
                        scope-name
                        (let [scope (env/resolve-namespace comp-env (symbol scope-name) ns)]
                          (when scope
                            {:keyword? true
                             :namespace (str scope)
                             :name (str prefix)
                             :files (->> (ns-files comp-env scope)
                                         (map resource-str))}))))
                :else
                (let [prefix-sym (symbol prefix)
                      resolved (safe-ns-resolve comp-env ns prefix-sym)
                      resolved (when (and resolved (not (class? resolved))) resolved)
                      resolved-ns (when (nil? resolved) (env/find-ns comp-env prefix-sym))
                      ;; example: clojure.main$repl
                      resolved-munged (when (and (nil? resolved) (nil? resolved-ns))
                                        (ns-resolve-munged comp-env ns prefix-sym))
                      m (env/meta comp-env (or resolved resolved-ns resolved-munged))]
                  (cond resolved (let [protocol (:protocol m)
                                       m (if (and protocol (not (contains? m :file)))
                                           (env/meta comp-env protocol)
                                           m)]
                                   (update m :file resource-str))
                        resolved-munged (update m :file resource-str)
                        resolved-ns (let [files (->> (ns-files comp-env resolved-ns)
                                                     (map resource-str))]
                                      (if (> (count files) 1)
                                        (assoc m :files files)
                                        (assoc m :file (first files))))))))))))

