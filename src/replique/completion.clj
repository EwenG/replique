(ns replique.completion
  (:require [replique.environment :as env]
            [replique.classpath :as classpath]
            [replique.files :as files]
            [clojure.string :as string])
  (:import [java.io File]
           [java.util Comparator]
           [java.applet Applet]))

(defonce classpath-data (atom nil))

;; Split on dashes, dots or upper case letters
(defn tokenize-prefix [prefix]
  (->> (string/split prefix #"-|\.|\/|(?=\p{Upper})")
       (filter #(not= "" %))))

(defn class-file? [path-str]
  (and (.endsWith path-str ".class")
       (not (.contains path-str "__"))
       (not (.contains path-str "$"))))

(defn class-file->class-name [path-str]
  (.. (if (.startsWith path-str File/separator)
        (.substring path-str 1) path-str)
      (replace ".class" "") (replace File/separator ".")))

(defn file-name->namespace [extension path-str]
  (when (and extension
             (not (.startsWith path-str "META-INF")))
    (let [[_ ^String nsname] (->
                              "[^\\w]?(.+)(\\.%s)"
                              (format extension)
                              re-pattern 
                              (re-matches path-str))]
      (when nsname
        (.. nsname (replace File/separator ".") (replace "_" "-"))))))

(defn matches? [candidate prefix-tokens]
  (loop [prefix-tokens (seq prefix-tokens)
         match-index nil]
    (if-let [token (first prefix-tokens)]
      (let [maybe-match-index (.indexOf candidate token)]
        (when (> maybe-match-index -1)
          (if (or (nil? match-index) (> maybe-match-index match-index))
            (recur (rest prefix-tokens) (+ (count token) maybe-match-index))
            (recur (rest prefix-tokens) match-index))))
      match-index)))

(def ^:dynamic *classes-on-classpath* nil)
(def ^:dynamic *file-resources* nil)
(def ^:dynamic *namespaces-on-classpath* nil)

(defn handle-classpath-file [path attrs]
  (let [path-str (str path)]
    (when (class-file? path-str)
      (let [class-name (class-file->class-name)]
        (set! *classes-on-classpath* (conj! *classes-on-classpath* class-name))))
    (when-let [source-file-extension (env/source-file-extension path-str)]
      (when-let [namespace (file-name->namespace source-file-extension path-str)]
        (let [namespaces-on-classpath (get *namespaces-on-classpath*
                                           source-file-extension
                                           (transient []))]
          (.put *namespaces-on-classpath* source-file-extension
                (conj! namespaces-on-classpath namespace)))))))

(defn handle-classpath-jar [jar-entry]
  (let [path-str (.getName jar-entry)]
    (when (class-file? path-str)
      (let [class-name (class-file->class-name)]
        (set! *classes-on-classpath* (conj! *classes-on-classpath* class-name))))
    (when-let [source-file-extension (env/source-file-extension path-str)]
      (when-let [namespace (file-name->namespace source-file-extension path-str)]
        (let [namespaces-on-classpath (get *namespaces-on-classpath*
                                           source-file-extension
                                           (transient []))]
          (.put *namespaces-on-classpath* source-file-extension
                (conj! namespaces-on-classpath namespace)))))))

(defn handle-classpath-file-boot [path attrs]
  (let [path-str (str path)]
    (when (class-file? path-str)
      (let [class-name (class-file->class-name path-str)]
        (set! *classes-on-classpath* (conj! *classes-on-classpath* class-name))))))

(defn handle-classpath-jar-boot [jar-entry]
  (let [path-str (.getName jar-entry)]
    (when (class-file? path-str)
      (let [class-name (class-file->class-name path-str)]
        (set! *classes-on-classpath* (conj! *classes-on-classpath* class-name))))))

(defn compute-classpath-data []
  (binding [*classes-on-classpath* (transient [])
            *file-resources* (transient [])
            *namespaces-on-classpath* (java.util.HashMap.)]
    (files/visit-files (classpath/paths)
                       handle-classpath-file handle-classpath-jar)
    (files/visit-files (classpath/boot-class-paths)
                       handle-classpath-file-boot handle-classpath-jar-boot)
    (doseq [k (keys *namespaces-on-classpath*)]
      (.put *namespaces-on-classpath* k (persistent! (get *namespaces-on-classpath* k))))
    (reset! classpath-data {:classes-on-classpath (persistent! *classes-on-classpath*)
                            :file-resources (persistent! *file-resources*)
                            :namespaces-on-classpath *namespaces-on-classpath*})))

(compute-classpath-data)

(defn locals-candidates [{:keys [locals in-string?]} prefix-tokens]
  (when-not in-string?
    (for [[local-name _] locals
          :let [match-index (matches? local-name prefix-tokens)]
          :when match-index]
      {:candidate local-name :type :local :match-index match-index})))

(defn all-ns-candidates [prefix-tokens]
  (for [ns-str (map (comp name ns-name) (all-ns))
        :let [match-index (matches? ns-str prefix-tokens)]
        :when match-index]
    {:candidate ns-str :type :namespace :match-index match-index}))

(defn classpath-namespaces-candidates [namespaces-on-classpath prefix-tokens]
  (for [ns-str (concat (get namespaces-on-classpath "cljc")
                       (get namespaces-on-classpath "clj"))
        :let [match-index (matches? ns-str prefix-tokens)]
        :when match-index]
    {:candidate ns-str :type :namespace :match-index match-index}))

(defn classpath-namespaces-with-prefix [ns-prefix prefix-tokens]
  (let [namespaces-on-classpath (:namespaces-on-classpath @classpath-data)]
    (if (= 0 (count ns-prefix))
      (classpath-namespaces-candidates namespaces-on-classpath prefix-tokens)
      (for [ns-str (concat (get namespaces-on-classpath "cljc")
                           (get namespaces-on-classpath "clj"))
            :when (.startsWith ns-str ns-prefix)
            :let [ns-suffix (subs ns-str (count ns-prefix))
                  ns-suffix (if (.startsWith ns-suffix ".") (subs ns-suffix 1) ns-suffix)]
            :when (not (.contains ns-suffix "."))
            :let [match-index (matches? ns-suffix prefix-tokens)]
            :when match-index]
        {:candidate ns-suffix
         :ns ns-str
         :type :namespace
         :match-index match-index}))))

(defn ns-aliases-candidates [ns prefix-tokens]
  (for [alias-str (->> ns ns-aliases keys (map name))
        :let  [match-index (matches? alias-str prefix-tokens)]
        :when match-index]
    {:candidate alias-str :type :namespace :match-index match-index}))

(defn ns-imports-candidates [ns prefix-tokens]
  (for [[simple-name ^Class qualified-name] (ns-imports ns)
        :let [simple-name-match-index (matches? (name simple-name) prefix-tokens)]
        :when simple-name-match-index]
    {:candidate (name simple-name) :type :class
     :package (.getName (.getPackage qualified-name))
     :match-index simple-name-match-index}))

;; When a class is imported through its prefixed name, we cannot distinguish between imported classes
;; and all other classes thus we don't always fuzzy match on classes on classpath
(defn classpath-classes-candidates [ns prefix prefix-tokens]
  (if (.contains prefix ".")
    (for [class-str (:classes-on-classpath @classpath-data)
          :let [match-index (matches? class-str prefix-tokens)]
          :when match-index]
      {:candidate class-str :type :class :match-index match-index})
    (for [class-str (:classes-on-classpath @classpath-data)
          :let [match-index (when (.startsWith class-str prefix) (count prefix))]
          :when match-index]
      {:candidate class-str :type :class :match-index match-index})))

(defn dependencies-candidates [prefix-tokens {:keys [position prefix] :as dependency-context}]
  (cond (= position :namespace)
        (classpath-namespaces-with-prefix prefix prefix-tokens)))

(def by-length-comparator
  (reify Comparator
    (compare [_ s1 s2]
      (let [res (compare (count s1) (count s2))]
        (if (zero? res)
          (compare s1 s2)
          res)))))

(defn candidates
  [comp-env ns {:keys [in-string? in-ns-form? dependency-context] :as context} prefix]
  (let [ns (or (env/find-ns comp-env ns)
               (env/find-ns comp-env (env/default-ns comp-env)))]
    (when (and (not in-string?) ns)
      (let [prefix-tokens (tokenize-prefix prefix)
            candidates (if in-ns-form?
                         (dependencies-candidates prefix-tokens dependency-context)
                         (concat
                          (locals-candidates context prefix-tokens)
                          (all-ns-candidates prefix-tokens)
                          (ns-aliases-candidates ns prefix-tokens)
                          (ns-imports-candidates ns prefix-tokens)
                          (classpath-classes-candidates ns prefix prefix-tokens)))]
        (sort-by :candidate by-length-comparator candidates)))))

(comment
  (ns rrr
    (:require [eee :refer]))

  (require )

  (let [eeee nil]
    
    )

  )
