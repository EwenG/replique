(ns replique.completion
  (:require [replique.environment :as env]
            [replique.classpath :as classpath]
            [replique.files :as files]
            [clojure.string :as string])
  (:import [java.io File]
           [java.util Comparator]
           [java.applet Applet]
           [javax.annotation.concurrent]))

(def ^:const max-candidates-number 200)

(defonce classpath-data (atom nil))
(defonce all-ns-data (atom nil))

;; Split on dashes, dots or upper case letters
(defn tokenize-prefix [prefix]
  (->> (string/split prefix #"-|\.|\/|(?=\p{Upper})")
       (filter #(not= "" %))
       distinct))

(def by-length-comparator
  (reify Comparator
    (compare [_ s1 s2]
      (let [res (compare (count s1) (count s2))]
        (if (zero? res)
          (compare s1 s2)
          res)))))

(defn class-file? [^String path-str]
  (and (.endsWith path-str ".class")
       (not (.contains path-str "__"))
       (not (.contains path-str "$"))))

(defn class-file->class-name [^String path-str]
  (.. (if (.startsWith path-str File/separator)
        (.substring path-str 1) path-str)
      (replace ".class" "") (replace File/separator ".")))

(defn file-name->namespace [extension ^String path-str]
  (when (and extension
             (not (.startsWith path-str "META-INF")))
    (let [[_ ^String nsname] (->
                              "[^\\w]?(.+)(\\.%s)"
                              (format extension)
                              re-pattern 
                              (re-matches path-str))]
      (when nsname
        (.. nsname (replace File/separator ".") (replace "_" "-"))))))

(defn matches? [^String candidate prefix-tokens]
  (loop [prefix-tokens (seq prefix-tokens)
         match-index nil]
    (if-let [^String token (first prefix-tokens)]
      (let [maybe-match-index (if (Character/isUpperCase (.charAt token 0))
                                (.lastIndexOf candidate token)
                                (let [maybe-match-index
                                      (or (.lastIndexOf candidate (str "." token))
                                          (.lastIndexOf candidate (str "-" token)))]
                                  (if (> maybe-match-index -1)
                                    (inc maybe-match-index)
                                    (if (.startsWith candidate token) 0 -1))))]
        (when (> maybe-match-index -1)
          (let [maybe-match-index (+ (count token) maybe-match-index)]
            (if (or (nil? match-index) (> maybe-match-index match-index))
              (recur (rest prefix-tokens) maybe-match-index)
              (recur (rest prefix-tokens) match-index)))))
      match-index)))

(def ^:dynamic *classes-on-classpath* nil)
(def ^:dynamic *file-resources* nil)
(def ^{:dynamic true :tag 'java.util.Map} *namespaces-on-classpath* nil)

(defn handle-classpath-file [path attrs]
  (let [path-str (str path)]
    (when (class-file? path-str)
      (let [class-name (class-file->class-name path-str)]
        (set! *classes-on-classpath* (conj! *classes-on-classpath* class-name))))
    (when-let [source-file-extension (env/source-file-extension path-str)]
      (when-let [namespace (file-name->namespace source-file-extension path-str)]
        (let [namespaces-on-classpath (get *namespaces-on-classpath*
                                           source-file-extension
                                           (transient []))]
          (.put *namespaces-on-classpath* source-file-extension
                (conj! namespaces-on-classpath namespace)))))))

(defn handle-classpath-jar [jar-entry]
  (let [path-str (.getName ^java.util.jar.JarEntry jar-entry)]
    (when (class-file? path-str)
      (let [class-name (class-file->class-name path-str)]
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

(defn handle-classpath-jar-boot [^java.util.jar.JarEntry jar-entry]
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
    (let [cljc-namespaces (.remove *namespaces-on-classpath* "cljc")
          cljc-namespaces (when cljc-namespaces (persistent! cljc-namespaces))]
      (doseq [k (keys *namespaces-on-classpath*)]
        (.put *namespaces-on-classpath* k (->> (get *namespaces-on-classpath* k)
                                               persistent!
                                               (concat cljc-namespaces)
                                               (sort by-length-comparator)))))
    ;; Sort classpath-data in order for max-candidates-number (lazyness) to be more useful
    (let [classes-on-classpath (->>
                                (persistent! *classes-on-classpath*)
                                (sort by-length-comparator))
          file-resources (->> (persistent! *file-resources*)
                              (sort by-length-comparator))
          namespaces-on-classpath *namespaces-on-classpath*]
      (reset! classpath-data {:classes-on-classpath classes-on-classpath
                              :file-resources file-resources
                              :namespaces-on-classpath namespaces-on-classpath}))))

(defn compute-all-ns-data [comp-env all-ns*]
  (reset! all-ns-data (->> all-ns*
                           (map (comp name env/ns-name))
                           (sort by-length-comparator))))

(compute-classpath-data)

(defn locals-candidates [{:keys [locals in-string?]} prefix-tokens]
  (when-not in-string?
    (->> (for [[local-name _] locals
               :let [match-index (matches? local-name prefix-tokens)]
               :when match-index]
           {:candidate local-name :type :local :match-index match-index})
         (sort-by :candidate by-length-comparator)
         (take max-candidates-number))))

;; We assume that the number of namespaces only grows, this is often true, but not always (when
;; remove-ns is used)
(defn all-ns-candidates [comp-env prefix-tokens]
  (let [all-ns* (env/all-ns comp-env)]
    (when (not= (count @all-ns-data) (count all-ns*))
      (compute-all-ns-data comp-env all-ns*))
    (->> (for [ns-str @all-ns-data
               :let [match-index (matches? ns-str prefix-tokens)]
               :when match-index]
           {:candidate ns-str :type :namespace :match-index match-index})
         (take max-candidates-number))))

(defn classpath-namespaces-candidates [comp-env namespaces-on-classpath prefix-tokens]
  (->> (for [ns-str (get namespaces-on-classpath (env/file-extension comp-env))
             :let [match-index (matches? ns-str prefix-tokens)]
             :when match-index]
         {:candidate ns-str :type :namespace :match-index match-index})
       (take max-candidates-number)))

(defn classpath-namespaces-candidates-no-suffix [comp-env namespaces-on-classpath prefix-tokens]
  (->> (for [ns-str (get namespaces-on-classpath (env/file-extension comp-env))
             :let [dot-last-index (.lastIndexOf ns-str ".")]
             :when (> dot-last-index -1)
             :let [ns-str-without-suffix (subs ns-str 0 dot-last-index)
                   match-index (matches? ns-str-without-suffix prefix-tokens)]
             :when match-index]
         {:candidate ns-str-without-suffix :type :namespace :match-index match-index})
       (take max-candidates-number)))

(defn classpath-namespaces-with-prefix [comp-env ns-prefix prefix-tokens]
  (let [namespaces-on-classpath (:namespaces-on-classpath @classpath-data)]
    (if (= 0 (count ns-prefix))
      (let [ns-candidates (classpath-namespaces-candidates
                           comp-env namespaces-on-classpath prefix-tokens)
            ns-candidates-no-suffix (classpath-namespaces-candidates-no-suffix
                                     comp-env namespaces-on-classpath prefix-tokens)]
        (distinct (concat ns-candidates-no-suffix ns-candidates)))
      (->> (for [^String ns-str (get namespaces-on-classpath (env/file-extension comp-env))
                 :when (.startsWith ns-str ns-prefix)
                 :let [ns-suffix (subs ns-str (count ns-prefix))
                       ns-suffix (if (.startsWith ns-suffix ".") (subs ns-suffix 1) ns-suffix)]
                 :when (not (.contains ns-suffix "."))
                 :let [match-index (matches? ns-suffix prefix-tokens)]
                 :when match-index]
             {:candidate ns-suffix
              :ns ns-str
              :type :namespace
              :match-index match-index})
           (take max-candidates-number)))))

(defn ns-aliases-candidates [comp-env ns prefix-tokens]
  (->> (for [alias-str (->> (env/ns-aliases comp-env ns) keys (map name))
            :let  [match-index (matches? alias-str prefix-tokens)]
            :when match-index]
         {:candidate alias-str :type :namespace :match-index match-index})
       (sort-by :candidate by-length-comparator)
       (take max-candidates-number)))

(defn ns-imports-candidates [comp-env ns prefix-tokens]
  (when (nil? comp-env)
    (->> (for [[simple-name ^Class qualified-name] (ns-imports ns)
               :let [simple-name-match-index (matches? (name simple-name) prefix-tokens)]
               :when simple-name-match-index]
           {:candidate (name simple-name) :type :class
            :package (.getName (.getPackage qualified-name))
            :match-index simple-name-match-index})
         (sort-by :candidate by-length-comparator)
         (take max-candidates-number))))

;; When a class is imported through its prefixed name, we cannot distinguish between imported classes
;; and all other classes thus we don't always fuzzy match on classes on classpath
(defn classpath-classes-candidates [comp-env prefix prefix-tokens]
  (when (nil? comp-env)
    (if (.contains ^String prefix ".")
      (->> (for [class-str (:classes-on-classpath @classpath-data)
                 :let [match-index (matches? class-str prefix-tokens)]
                 :when match-index]
             {:candidate class-str :type :class :match-index match-index})
           (take max-candidates-number))
      (->> (for [^String class-str (:classes-on-classpath @classpath-data)
                 :let [match-index (when (.startsWith class-str prefix) (count prefix))]
                 :when match-index]
             {:candidate class-str :type :class :match-index match-index})
           (take max-candidates-number)))))

(defn classpath-classes-for-import-no-suffix [comp-env prefix-tokens]
  (->> (for [class-str (:classes-on-classpath @classpath-data)
             :let [dot-last-index (.lastIndexOf class-str ".")]
             :when (> dot-last-index -1)
             :let [class-str-without-suffix (subs class-str 0 dot-last-index)
                   match-index (matches? class-str-without-suffix prefix-tokens)]
             :when match-index]
         {:candidate class-str-without-suffix :type :class :match-index match-index})
       (take max-candidates-number)))

(defn classpath-classes-for-import [comp-env prefix-tokens]
  (when (nil? comp-env)
    (let [classes-candidates (->> (for [class-str (:classes-on-classpath @classpath-data)
                                       :let [match-index (matches? class-str prefix-tokens)]
                                       :when match-index]
                                    {:candidate class-str :type :class :match-index match-index})
                                  (take max-candidates-number))
          classes-candidates-no-suffix (classpath-classes-for-import-no-suffix
                                        comp-env prefix-tokens)]
      (distinct (concat classes-candidates-no-suffix classes-candidates)))))

(defn simple-class-with-package [comp-env package prefix]
  (when (nil? comp-env)
    (->> (for [^String class-str (:classes-on-classpath @classpath-data)
               :when (.startsWith class-str (str package "." prefix))
               :let [dot-last-index (.lastIndexOf class-str ".")]
               :when (< dot-last-index (count class-str))]
           {:candidate (subs class-str (inc (.lastIndexOf class-str ".")))
            :type :class
            :match-index (count prefix)})
         (take max-candidates-number))))

(defn vars-from-namespace-candidates [comp-env ns-str prefix-tokens]
  (when-let [ns (env/find-ns comp-env (symbol ns-str))]
    (->> (for [[var-sym var] (env/ns-publics comp-env ns)
               :let [var-name (name var-sym)
                     match-index (matches? var-name prefix-tokens)]
               :when match-index]
           (let [{:keys [arglists macro]} (env/meta comp-env var)]
             {:candidate var-name
              :type (cond macro :macro
                          arglists :function
                          :else :var)
              :ns ns-str
              :match-index match-index}))
         (sort-by :candidate by-length-comparator candidates)
         (take max-candidates-number))))

(defn dependencies-candidates [comp-env prefix prefix-tokens
                               {:keys [position] :as dependency-context}]
  (cond (= position :namespace)
        (classpath-namespaces-with-prefix comp-env (:prefix dependency-context) prefix-tokens)
        (= position :var)
        (vars-from-namespace-candidates comp-env (:namespace dependency-context) prefix-tokens)
        (= position :package-or-class)
        (classpath-classes-for-import comp-env prefix-tokens)
        (= position :class)
        (simple-class-with-package comp-env (:package dependency-context) prefix)))

(defn candidates
  [comp-env ns {:keys [in-string? in-ns-form? dependency-context] :as context} prefix]
  (let [ns (or (env/find-ns comp-env ns)
               (env/find-ns comp-env (env/default-ns comp-env)))]
    (when (and (not in-string?) ns)
      (let [prefix-tokens (tokenize-prefix prefix)
            candidates (if in-ns-form?
                         (dependencies-candidates comp-env prefix prefix-tokens dependency-context)
                         (concat
                          (locals-candidates context prefix-tokens)
                          (all-ns-candidates comp-env prefix-tokens)
                          (ns-aliases-candidates comp-env ns prefix-tokens)
                          (ns-imports-candidates comp-env ns prefix-tokens)
                          (classpath-classes-candidates comp-env prefix prefix-tokens)))]
        (->>
         (sort-by :candidate by-length-comparator candidates)
         (take max-candidates-number))))))

(comment
  (ns rrr
    (:require [eee :refer]))

  (require )

  (let [eeee nil]
    
    )

  )
