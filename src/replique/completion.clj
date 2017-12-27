(ns replique.completion
  (:require [replique.environment :as env]
            [replique.classpath :as classpath]
            [replique.files :as files]
            [clojure.string :as string])
  (:import [java.io File]
           [java.util Comparator]
           [replique.environment CljsCompilerEnv]
           [java.lang.reflect Modifier Method Field]))

(def ^:const max-candidates-number 200)

(defonce classpath-data (atom nil))
(defonce all-ns-data (atom nil))

;; Split on dashes, dots or upper case letters
(defn tokenize-prefix [prefix]
  (->> (string/split prefix #"-|\.|(?=\p{Upper})")
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
         match-index 0]
    (if-let [^String token (first prefix-tokens)]
      (let [^long maybe-match-index
            (if (Character/isUpperCase (.charAt token 0))
              (.lastIndexOf candidate token)
              (let [maybe-match-index (.lastIndexOf candidate (str "." token))
                    maybe-match-index (if (= -1 maybe-match-index)
                                        (.lastIndexOf candidate (str "-" token))
                                        maybe-match-index)]
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

(defonce js-dependency-index-data (atom nil))

(defn provides-from-js-dependency-index [comp-env]
  (if (nil? @js-dependency-index-data)
    (reset! js-dependency-index-data (->> comp-env (env/get-js-index) vals (mapcat :provides)
                                          distinct
                                          (sort by-length-comparator)))
    @js-dependency-index-data))

(defn locals-candidates [locals prefix-tokens]
  (->> (for [[local-name _] locals
             :let [match-index (matches? local-name prefix-tokens)]
             :when match-index]
         {:candidate local-name :type :local :match-index match-index})
       (sort-by :candidate by-length-comparator)
       (take max-candidates-number)))

(defn all-ns-candidates* [all-ns-data prefix-tokens]
  (->> (for [ns-str all-ns-data
             :let [match-index (matches? ns-str prefix-tokens)]
             :when match-index]
         {:candidate ns-str :type :namespace :match-index match-index})
       (take max-candidates-number)))

;; We assume that the number of namespaces only grows, this is often true, but not always (when
;; remove-ns is used)
(defn all-ns-candidates [comp-env prefix-tokens]
  (let [all-ns* (env/all-ns comp-env)]
    (when (not= (count @all-ns-data) (count all-ns*))
      (compute-all-ns-data comp-env all-ns*))
    (->> (all-ns-candidates* @all-ns-data prefix-tokens)
         (take max-candidates-number))))

(defn classpath-namespaces-candidates [comp-env namespaces-on-classpath prefix-tokens]
  (->> (for [ns-str (get namespaces-on-classpath (env/file-extension comp-env))
             :let [match-index (matches? ns-str prefix-tokens)]
             :when match-index]
         {:candidate ns-str :type :namespace :match-index match-index})
       (take max-candidates-number)))

(defn classpath-namespaces-candidates-no-suffix [comp-env namespaces-on-classpath prefix-tokens]
  (->> (for [^String ns-str (get namespaces-on-classpath (env/file-extension comp-env))
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

(defn all-ns-candidates-no-suffix [all-ns-data prefix-tokens]
  (->> (for [^String ns-str all-ns-data
             :let [dot-last-index (.lastIndexOf ns-str ".")]
             :when (> dot-last-index -1)
             :let [ns-str-without-suffix (subs ns-str 0 dot-last-index)
                   match-index (matches? ns-str-without-suffix prefix-tokens)]
             :when match-index]
         {:candidate ns-str-without-suffix :type :namespace :match-index match-index})
       (take max-candidates-number)))

(defn all-ns-with-prefix [comp-env ns-prefix prefix-tokens]
  (let [all-ns* (env/all-ns comp-env)]
    (when (not= (count @all-ns-data) (count all-ns*))
      (compute-all-ns-data comp-env all-ns*))
    (if (= 0 (count ns-prefix))
      (let [ns-candidates (all-ns-candidates* @all-ns-data prefix-tokens)
            ns-candidates-no-suffix (all-ns-candidates-no-suffix @all-ns-data prefix-tokens)]
        (distinct (concat ns-candidates-no-suffix ns-candidates)))
      (->> (for [^String ns-str @all-ns-data
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

(defn dependency-index-candidates [comp-env prefix-tokens]
  (when (instance? CljsCompilerEnv comp-env)
    (->> (for [class-str (provides-from-js-dependency-index comp-env)
               :let [match-index (matches? class-str prefix-tokens)]
               :when match-index]
           {:candidate class-str :type :namespace :match-index match-index})
         (take max-candidates-number))))

(defn classpath-classes-for-import-no-suffix [comp-env prefix-tokens]
  (when (not (instance? CljsCompilerEnv comp-env))
    (->> (for [^String class-str (:classes-on-classpath @classpath-data)
               :let [dot-last-index (.lastIndexOf class-str ".")]
               :when (> dot-last-index -1)
               :let [class-str-without-suffix (subs class-str 0 dot-last-index)
                     match-index (matches? class-str-without-suffix prefix-tokens)]
               :when match-index]
           {:candidate class-str-without-suffix :type :class :match-index match-index})
         (take max-candidates-number))))

(defn classpath-classes-for-import [comp-env prefix-tokens]
  (when (not (instance? CljsCompilerEnv comp-env))
    (let [classes-candidates (->> (for [class-str (:classes-on-classpath @classpath-data)
                                        :let [match-index (matches? class-str prefix-tokens)]
                                        :when match-index]
                                    {:candidate class-str :type :class :match-index match-index})
                                  (take max-candidates-number))
          classes-candidates-no-suffix (classpath-classes-for-import-no-suffix
                                        comp-env prefix-tokens)]
      (distinct (concat classes-candidates-no-suffix classes-candidates)))))

(defn generated-classes [comp-env ns package]
  (when-let [the-ns (env/find-ns comp-env (symbol package))]
    (for [[sym var] (env/ns-map comp-env the-ns)
          :when (if (instance? CljsCompilerEnv comp-env)
                  (:type var)
                  (class? var))]
      (str package "." sym))))

(defn simple-class-with-package [comp-env ns package prefix]
  (->> (for [^String class-str (concat
                                (if (instance? CljsCompilerEnv comp-env)
                                  (provides-from-js-dependency-index comp-env)
                                  (:classes-on-classpath @classpath-data))
                                (generated-classes comp-env ns package))
             :when (.startsWith class-str (str package "." prefix))
             :let [dot-last-index (.lastIndexOf class-str ".")]
             :when (< dot-last-index (count class-str))]
         {:candidate (subs class-str (inc (.lastIndexOf class-str ".")))
          :type :class
          :match-index (count prefix)})
       (take max-candidates-number)))

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
         (sort-by :candidate by-length-comparator)
         (take max-candidates-number))))

(defn dependency-type-candidates [prefix]
  (->> (for [dependency-type ["require" "require-macros" "use" "import" "refer-clojure" "load"]
             :let [dependency-type (str dependency-type)]
             :when (.startsWith dependency-type prefix)]
         {:candidate dependency-type :match-index (count prefix) :type :keywoprd})
       (sort-by :candidate by-length-comparator)))

(defn libspec-option-candidates [prefix]
  (->> (for [^String option ["as" "refer" "rename"]
             :when (.startsWith option prefix)]
         {:candidate (str ":" option) :match-index (count prefix) :type :keyword})
       (sort-by :candidate by-length-comparator)))

(defn libspec-option-refer-candidates [prefix]
  (->> (for [^String option ["exclude" "only" "rename"]
             :when (.startsWith option prefix)]
         {:candidate (str ":" option) :match-index (count prefix) :type :keyword})
       (sort-by :candidate by-length-comparator)))

(defn flag-candidates [prefix]
  (->> (for [^String flag ["reload" "reload-all" "verbose"]
             :when (.startsWith flag prefix)]
         {:candidate (str ":" flag) :match-index (count prefix) :type :keyword})
       (sort-by :candidate by-length-comparator)))

(defn dependencies-candidates [comp-env ns prefix prefix-tokens
                               {{position :position :as dependency-context} :dependency-context
                                locals :locals in-ns-form? :in-ns-form?
                                :as context}]
  (cond (= position :namespace)
        (concat
         (classpath-namespaces-with-prefix comp-env (:prefix dependency-context) prefix-tokens)
         (dependency-index-candidates comp-env prefix-tokens)
         (all-ns-with-prefix comp-env (:prefix dependency-context) prefix-tokens)
         ;; require is a macro in clojurescript
         (when-not (and in-ns-form?
                        (not (instance? CljsCompilerEnv comp-env)))
           (locals-candidates locals prefix-tokens)))
        (= position :namespace-macros)
        (concat
         (classpath-namespaces-with-prefix nil (:prefix dependency-context) prefix-tokens)
         (all-ns-with-prefix nil (:prefix dependency-context) prefix-tokens)
         (when (and in-ns-form?
                    (not (instance? CljsCompilerEnv comp-env)))
           (locals-candidates locals prefix-tokens)))
        (= position :var)
        (let [namespace (if (= :refer-clojure (:namespace dependency-context))
                          (name (env/core-namespace comp-env))
                          (:namespace dependency-context))]
          (if (and in-ns-form? (nil? comp-env))
            (vars-from-namespace-candidates comp-env namespace prefix-tokens)
            (concat
             (locals-candidates locals prefix-tokens)
             (vars-from-namespace-candidates comp-env namespace prefix-tokens))))
        (= position :package-or-class)
        (concat (classpath-classes-for-import comp-env prefix-tokens)
                (dependency-index-candidates comp-env prefix-tokens)
                (all-ns-candidates comp-env prefix-tokens))
        (= position :class)
        ;; package can be a namespace for deftype / defrecord generated classes
        (simple-class-with-package comp-env ns (:package dependency-context) prefix)
        (= position :dependency-type)
        (dependency-type-candidates prefix)
        (= position :libspec-option)
        (libspec-option-candidates prefix)
        (= position :libspec-option-refer)
        (libspec-option-refer-candidates prefix)
        (= position :flag)
        (flag-candidates prefix)))

(defn scoped-candidates [comp-env ns scope-name prefix-tokens]
  (let [scope (env/resolve-namespace comp-env (symbol scope-name) ns)
        var-candidates (when scope (env/ns-publics comp-env scope))]
    (for [[var-sym var] var-candidates
          :let [var-name (name var-sym)
                {:keys [arglists macro] :as var-meta} (env/meta comp-env var)
                match-index (matches? var-name prefix-tokens)]
          :when match-index]
      {:candidate (str scope-name "/" var-name)
       :type (cond macro :macro
                   arglists :function
                   :else :var)
       :ns (str (or (:ns var-meta) ns))
       :match-index (+ 1 (count scope-name) match-index)})))

(defn mapping-candidates [comp-env ns prefix-tokens]
  (->> (let [var-candidates (env/ns-map comp-env ns)]
        (for [[var-sym var] var-candidates
              :let [var-name (name var-sym)
                    {:keys [arglists macro] :as var-meta} (env/meta comp-env var)
                    match-index (matches? var-name prefix-tokens)]
              :when match-index]
          (if (= (type var) Class)
            {:candidate var-name
             :type :class
             :package (when-let [pkg (.getPackage ^Class var)]
                        ;; Some classes don't have a package
                        (.getName ^Package pkg))
             :match-index match-index}
            {:candidate var-name
             :type (cond macro :macro
                         arglists :function
                         :else :var)
             :ns (str (or (:ns var-meta) ns))
             :match-index match-index})))
       (sort-by :candidate by-length-comparator)
       (take max-candidates-number)))

(defn alias-candidate->keyword [candidate]
  (assoc candidate
         :candidate (str "::" (:candidate candidate))
         :match-index (+ 2 (:match-index candidate))))

(defn keyword-candidates [comp-env ns prefix-tokens double-colon? scope-name]
  (cond (and double-colon? scope-name)
        (let [scope (env/resolve-namespace comp-env (symbol scope-name) ns)
              namespace-name (str scope)]
          (for [kw (env/keywords comp-env)
                :when (= (namespace kw) namespace-name)
                :let [match-index (matches? (name kw) prefix-tokens)]
                :when match-index]
            {:candidate (str "::" scope-name "/" (name kw))
             :type :keyword
             :match-index (+ 3 (count scope-name) match-index)}))
        double-colon?
        (concat (for [kw (env/keywords comp-env)
                      :when (= (namespace kw) (str ns))
                      :let [match-index (matches? (name kw) prefix-tokens)]
                      :when match-index]
                  {:candidate (str "::" (name kw))
                   :type :keyword
                   :match-index (+ 2 match-index)})
                (for [alias-str (->> (env/ns-aliases comp-env ns) keys (map name))
                      :let  [match-index (matches? alias-str prefix-tokens)]
                      :when match-index]
                  {:candidate (str "::" alias-str)
                   :type :keyword
                   :match-index (+ 2 match-index)}))
        :else (for [kw (env/keywords comp-env)
                    :let [match-index (matches? (str kw) prefix-tokens)]
                    :when match-index]
                {:candidate (str ":" kw)
                 :type :keyword
                 :match-index (+ 1 match-index)})))

(defn resolve-class [ns class]
  (let [maybe-class (try (ns-resolve ns class)
                         (catch ClassNotFoundException ex nil))]
    (when (class? maybe-class) maybe-class)))

(defn static-methods-candidates [comp-env ns scope-name prefix prefix-tokens]
  (when (nil? comp-env)
    (when-let [^Class class (resolve-class ns (symbol scope-name))]
      (for [^Method method (.getMethods class)
            :when (Modifier/isStatic (.getModifiers method))
            :let [match-index (matches? (.getName method) prefix-tokens)]
            :when match-index]
        {:candidate (str scope-name "/" (.getName method))
         :type :static-method
         :match-index (+ 1 (count scope-name) match-index)}))))

(defn static-fields-candidates [comp-env ns scope-name prefix prefix-tokens]
  (when (nil? comp-env)
    (when-let [^Class class (resolve-class ns (symbol scope-name))]
      (for [^Field field (.getFields class)
            :when (Modifier/isStatic (.getModifiers field))
            :let [match-index (matches? (.getName field) prefix-tokens)]
            :when match-index]
        {:candidate (str scope-name "/" (.getName field))
         :type :static-field
         :match-index (+ 1 (count scope-name) match-index)}))))

(defn method-candidates [comp-env ns locals param param-meta prefix prefix-tokens]
  (when (and (nil? comp-env) param)
    (let [[_ _ local-meta :as local-param] (get locals param)
          param-meta (or param-meta local-meta)]
      (cond param-meta
            (let [param-meta (try (read-string param-meta)
                                  (catch Exception e nil))]
              (cond (symbol? param-meta)
                    (when-let [^Class class (resolve-class ns param-meta)]
                      (for [^Method method (.getMethods class)
                            :when (not (Modifier/isStatic (.getModifiers method)))
                            :let [match-index (matches? (.getName method) prefix-tokens)]
                            :when match-index]
                        {:candidate (str "." (.getName method))
                         :type :method
                         :match-index (+ 1 match-index)}))
                    (map? param-meta)
                    (when-let [tag (get param-meta :tag)]
                      (when-let [^Class class (resolve-class ns tag)]
                        (for [^Method method (.getMethods class)
                              :when (not (Modifier/isStatic (.getModifiers method)))
                              :let [match-index (matches? (.getName method) prefix-tokens)]
                              :when match-index]
                          {:candidate (str "." (.getName method))
                           :type :method
                           :match-index (+ 1 match-index)})))))
            (and (nil? local-param) param)
            (let [param (try (read-string param)
                             (catch Exception e nil))]
              (when (symbol? param)
                (when-let [value (try (binding [*ns* ns] (eval param))
                                      (catch Exception e _))]
                  (when-let [^Class class (type value)]
                    (for [^Method method (.getMethods class)
                          :when (not (Modifier/isStatic (.getModifiers method)))
                          :let [match-index (matches? (.getName method) prefix-tokens)]
                          :when match-index]
                      {:candidate (str "." (.getName method))
                       :type :method
                       :match-index (+ 1 match-index)})))))))))

(defn field-candidates [comp-env ns locals param param-meta prefix prefix-tokens]
  (when (and (nil? comp-env) param)
    (let [[_ _ local-meta :as local-param] (get locals param)
          param-meta (or param-meta local-meta)]
      (cond param-meta
            (let [param-meta (try (read-string param-meta)
                                  (catch Exception e nil))]
              (cond (symbol? param-meta)
                    (when-let [^Class class (resolve-class ns param-meta)]
                      (for [^Field field (.getFields class)
                            :when (not (Modifier/isStatic (.getModifiers field)))
                            :let [match-index (matches? (.getName field) prefix-tokens)]
                            :when match-index]
                        {:candidate (str ".-" (.getName field))
                         :type :field
                         :match-index (+ 2 match-index)}))
                    (map? param-meta)
                    (when-let [tag (get param-meta :tag)]
                      (when-let [^Class class (resolve-class ns tag)]
                        (for [^Field field (.getFields class)
                              :when (not (Modifier/isStatic (.getModifiers field)))
                              :let [match-index (matches? (.getName field) prefix-tokens)]
                              :when match-index]
                          {:candidate (str ".-" (.getName field))
                           :type :field
                           :match-index (+ 2 match-index)})))))
            (and (nil? local-param) param)
            (let [param (try (read-string param)
                             (catch Exception e nil))]
              (when (symbol? param)
                (when-let [value (try (binding [*ns* ns] (eval param))
                                      (catch Exception e _))]
                  (when-let [^Class class (type value)]
                    (for [^Field field (.getFields class)
                          :when (not (Modifier/isStatic (.getModifiers field)))
                          :let [match-index (matches? (.getName field) prefix-tokens)]
                          :when match-index]
                      {:candidate (str ".-" (.getName field))
                       :type :field
                       :match-index (+ 2 match-index)})))))))))

(defn special-forms-candidates [comp-env ns fn-context-position prefix-tokens]
  (when (= 0 fn-context-position)
    (for [form (env/special-forms comp-env)
          :let [match-index (matches? form prefix-tokens)]
          :when match-index]
      {:candidate form
       :type :special-form
       :match-index match-index})))

(defn literal-candidates [prefix]
  (for [^String literal ["true" "false" "nil"]
        :when (.startsWith literal prefix)]
    {:candidate literal
     :type :special-form
     :match-index (count prefix)}))

(defn candidates
  [comp-env ns {:keys [locals in-string? in-comment? in-ns-form? at-binding-position?
                       fn-context fn-context-position dependency-context]
                :as context}
   ^String prefix]
  (let [ns (or (env/find-ns comp-env ns)
               (env/find-ns comp-env (env/default-ns comp-env)))]
    (when (and (not in-string?) (not in-comment?) (not at-binding-position?) ns)
      (let [prefix (.replaceFirst prefix "^#_" "")
            keyword? (.startsWith prefix ":")
            double-colon? (.startsWith prefix "::")
            field-call? (.startsWith prefix ".-")
            method-call? (and (not field-call?) (.startsWith prefix "."))
            prefix (.replaceFirst prefix "(^::?)|(^\\.-?)" "")
            scope-split-index (.lastIndexOf prefix "/")
            scope-name (when (> scope-split-index -1) (subs prefix 0 scope-split-index))
            prefix (if (> scope-split-index -1) (subs prefix (inc scope-split-index)) prefix)
            prefix-tokens (tokenize-prefix prefix)
            candidates (cond dependency-context
                             (dependencies-candidates comp-env ns prefix prefix-tokens context)
                             keyword? (keyword-candidates comp-env ns prefix-tokens
                                                          double-colon? scope-name)
                             scope-name (concat
                                         (scoped-candidates comp-env ns scope-name prefix-tokens)
                                         (static-methods-candidates comp-env ns scope-name
                                                                    prefix prefix-tokens)
                                         (static-fields-candidates comp-env ns scope-name
                                                                   prefix prefix-tokens))
                             method-call? (method-candidates comp-env ns locals
                                                             (:param context)
                                                             (:param-meta context)
                                                             prefix prefix-tokens)
                             field-call? (field-candidates comp-env ns locals
                                                           (:param context)
                                                           (:param-meta context)
                                                           prefix prefix-tokens)
                             :else
                             (concat
                              (locals-candidates locals prefix-tokens)
                              (all-ns-candidates comp-env prefix-tokens)
                              (ns-aliases-candidates comp-env ns prefix-tokens)
                              (dependency-index-candidates comp-env prefix-tokens)
                              (mapping-candidates comp-env ns prefix-tokens)
                              (special-forms-candidates comp-env ns fn-context-position
                                                        prefix-tokens)
                              (literal-candidates prefix)))]
        (->> candidates
             distinct
             (sort-by :candidate by-length-comparator)
             (take max-candidates-number))))))

;; load-path -> completion for files
;; members -> cljs completion
