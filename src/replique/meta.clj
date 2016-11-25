(ns replique.meta
  (:refer-clojure :exclude [find-ns meta ns-resolve ns-interns])
  (:require
   [clojure.java.io :as io]
   [replique.compliment.core :as compliment]
   [replique.compliment.context :as context]
   [replique.compliment.sources.local-bindings :refer [bindings-from-context]]
   [replique.environment :refer [find-ns meta ns-resolve ns-interns]]))

;; Whitelist meta to be returned because everything is not elisp printable
(def meta-keys #{:file :arglists :doc :added :static :line :column :name :tag})

(defn safe-ns-resolve [comp-env ns symbol]
  (try (ns-resolve comp-env ns symbol)
       (catch Exception _ nil)))

(defn safe-find-ns [comp-env symbol]
  (try (find-ns comp-env symbol)
       (catch Exception _ nil)))

(defn ns-file [comp-env the-ns]
  (let [interns (ns-interns comp-env the-ns)]
    (some (fn [[_ v]] (:file (meta comp-env v))) interns)))

(defn resource-str [^String f]
  (when f
    (try
      (let [file (java.io.File. f)]
        (if (.exists file)
          (.getAbsolutePath file)
          (when-let [res (io/resource f)]
            (str res))))
      (catch Exception _ nil))))

(defn handle-meta [comp-env ns context sym-at-point]
  (let [ns (compliment/ensure-ns comp-env (when ns (symbol ns)))
        sym-at-point (and sym-at-point (symbol sym-at-point))
        bindings (set (bindings-from-context context))]
    (when (and
           ;; Exclude keywords, ...
           (symbol? sym-at-point)
           ;; If sym-at-point is a binding symbol or a local, there is no meta
           (not (some #{"__prefix__" (str sym-at-point)} bindings)))
      (let [resolved-sym (safe-ns-resolve comp-env ns sym-at-point)
            resolved-ns (when (nil? resolved-sym) (safe-find-ns comp-env sym-at-point))
            m (-> (meta comp-env (or resolved-sym resolved-ns))
                  (select-keys meta-keys)
                  (assoc :ns (str ns)))]
        (cond resolved-sym (update m :file resource-str)
              resolved-ns (->> (ns-file comp-env resolved-ns)
                               resource-str
                               (assoc m :file))
              :else nil)))))

(comment
  (binding-symbol? '(let [[e __prefix__] f] nil) 1)
  )
