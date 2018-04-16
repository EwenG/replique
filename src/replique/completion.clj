(ns replique.completion
  (:require [replique.environment :as env]
            [replique.classpath :as classpath]
            [replique.context :as context]
            [replique.utils :as utils]
            [replique.files :as files]
            [replique.elisp-printer :as elisp]
            [clojure.string :as string])
  (:import [java.io File]
           [java.nio.file.attribute BasicFileAttributes]
           [java.nio.file Paths Path]
           [java.util Comparator]
           [replique.environment CljsCompilerEnv]
           [replique.elisp_printer ElispString]
           [java.lang.reflect Modifier Method Field]
           [java.nio.file FileVisitResult]))

(def ^:private cljs-evaluate-form (utils/dynaload 'replique.repl-cljs/-evaluate-form))
(def ^:private cljs-munged (utils/dynaload 'cljs.compiler/munge))
(def ^:private cljs-tooling-form->js (utils/dynaload 'replique.repl-cljs/tooling-form->js))

(def ^:const max-candidates-number 200)

(defonce classpath-data (atom nil))
(defonce all-ns-data (atom nil))

(defprotocol IAllNsData
  (get-all-ns-data [comp-env])
  (set-all-ns-data [cop-env new-data]))

(extend-protocol IAllNsData
  nil
  (get-all-ns-data [comp-env] (:clj @all-ns-data))
  (set-all-ns-data [cop-env new-data] (swap! all-ns-data assoc :clj new-data))
  CljsCompilerEnv
  (get-all-ns-data [comp-env] (:cljs @all-ns-data))
  (set-all-ns-data [cop-env new-data] (swap! all-ns-data assoc :cljs new-data)))

;; Split on dashes, dots or upper case letters
(defn tokenize-prefix [prefix]
  (->> (string/split prefix #"-|\.|(?=\p{Upper})")
       (filter #(not= "" %))
       distinct))

(defn tokenize-file-prefix [prefix]
  (->> (string/split prefix #"/")
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
       (not (= path-str "module-info.class"))
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
            (if (> maybe-match-index match-index)
              (recur (rest prefix-tokens) maybe-match-index)
              (recur (rest prefix-tokens) match-index)))))
      match-index)))

(defn matches-file? [^String candidate prefix-tokens]
  (loop [prefix-tokens (seq prefix-tokens)
         match-index 0]
    (if-let [^String token (first prefix-tokens)]
      (let [maybe-match-index
            (let [maybe-match-index (.lastIndexOf candidate (str "/" token))]
              (if (> maybe-match-index -1)
                (inc maybe-match-index)
                (if (.startsWith candidate token) 0 -1)))]
        (when (> maybe-match-index -1)
          (let [maybe-match-index (+ (count token) maybe-match-index)]
            (if (> maybe-match-index match-index)
              (recur (rest prefix-tokens) maybe-match-index)
              (recur (rest prefix-tokens) match-index)))))
      match-index)))

(def ^:dynamic *classes-on-classpath* nil)
(def ^:dynamic *file-resources* nil)
(def ^{:dynamic true :tag 'java.util.Map} *namespaces-on-classpath* nil)

(defn handle-classpath-file* [path-str]
  (when (class-file? path-str)
    (let [class-name (class-file->class-name path-str)]
      (set! *classes-on-classpath* (conj! *classes-on-classpath* class-name))))
  (when-let [source-file-extension (env/source-file-extension path-str)]
    (when-let [namespace (file-name->namespace source-file-extension path-str)]
      (let [namespaces-on-classpath (get *namespaces-on-classpath*
                                         source-file-extension
                                         (transient []))]
        (.put *namespaces-on-classpath* source-file-extension
              (conj! namespaces-on-classpath namespace)))))
  (set! *file-resources* (conj! *file-resources* path-str)))

(defn handle-classpath-file [path attrs]
  (let [path-str (str path)]
    (handle-classpath-file* path-str)))

(defn handle-classpath-jar [jar-entry]
  (let [path-str (.getName ^java.util.jar.JarEntry jar-entry)]
    (handle-classpath-file* path-str)))

(defn handle-system-module-resources [r]
  (when (class-file? r)
    (let [class-name (class-file->class-name r)]
      (set! *classes-on-classpath* (conj! *classes-on-classpath* class-name)))))

(defn compute-classpath-data []
  (binding [*classes-on-classpath* (transient [])
            *file-resources* (transient [])
            *namespaces-on-classpath* (java.util.HashMap.)]
    (files/visit-files (classpath/paths) handle-classpath-file handle-classpath-jar)
    (doseq [r classpath/system-module-resources]
      (handle-system-module-resources r))
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
      {:classes-on-classpath classes-on-classpath
       :file-resources file-resources
       :namespaces-on-classpath namespaces-on-classpath})))

(defn compute-all-ns-data [comp-env all-ns*]
  (set-all-ns-data comp-env (->> all-ns*
                                 (map (comp name env/ns-name))
                                 (sort by-length-comparator))))

(reset! classpath-data (compute-classpath-data))

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

(defn all-ns-candidates*
  ([locals all-ns-data prefix-tokens]
   (all-ns-candidates* locals all-ns-data prefix-tokens false))
  ([locals all-ns-data prefix-tokens munge?]
   (->> (for [ns-str all-ns-data
              :let [match-index (and (nil? (get locals ns-str))
                                     (matches? ns-str prefix-tokens))]
              :when match-index]
          (if munge?
            {:candidate (munge ns-str) :type :class :match-index match-index}
            {:candidate ns-str :type :namespace :match-index match-index}))
        (take max-candidates-number))))

;; We assume that the number of namespaces only grows, this is often true, but not always (when
;; remove-ns is used)
(defn all-ns-candidates
  ([comp-env locals prefix-tokens]
   (all-ns-candidates comp-env locals prefix-tokens false))
  ([comp-env locals prefix-tokens munge?]
   (when-not (and (instance? CljsCompilerEnv comp-env) munge?)
     (let [all-ns* (env/all-ns comp-env)]
       (when (not= (count (get-all-ns-data comp-env)) (count all-ns*))
         (compute-all-ns-data comp-env all-ns*))
       (all-ns-candidates* locals (get-all-ns-data comp-env) prefix-tokens munge?)))))

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
    (when (not= (count (get-all-ns-data comp-env)) (count all-ns*))
      (compute-all-ns-data comp-env all-ns*))
    (if (= 0 (count ns-prefix))
      (let [ns-candidates (all-ns-candidates* nil (get-all-ns-data comp-env) prefix-tokens)
            ns-candidates-no-suffix (all-ns-candidates-no-suffix
                                     (get-all-ns-data comp-env) prefix-tokens)]
        (distinct (concat ns-candidates-no-suffix ns-candidates)))
      (->> (for [^String ns-str (get-all-ns-data comp-env)
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

(defn ns-aliases-candidates [comp-env ns locals prefix-tokens]
  (->> (for [alias-str (->> (env/ns-aliases-all comp-env ns) keys (map name))
             :let  [match-index (and (nil? (get locals alias-str))
                                     (matches? alias-str prefix-tokens))]
             :when match-index]
         {:candidate alias-str :type :namespace :match-index match-index})
       (sort-by :candidate by-length-comparator)
       (take max-candidates-number)))

(defn dependency-index-candidates [comp-env locals prefix-tokens]
  (when (instance? CljsCompilerEnv comp-env)
    (->> (for [class-str (provides-from-js-dependency-index comp-env)
               :let [match-index (and (nil? (get locals class-str))
                                      (matches? class-str prefix-tokens))]
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

(defn package-unmunge [package]
  (.replace (str package) \_ \-))

(defn generated-classes [ns package]
  (when-let [the-ns (find-ns (-> package package-unmunge symbol))]
    (for [[sym var] (ns-imports the-ns)
          :when (and (class? var) (.startsWith (.getName ^Class var) package))]
      (str package "." sym))))

(defn simple-class-with-package [comp-env ns package prefix]
  (->> (for [^String class-str (concat
                                (if (instance? CljsCompilerEnv comp-env)
                                  (provides-from-js-dependency-index comp-env)
                                  (concat (:classes-on-classpath @classpath-data)
                                          (generated-classes ns package))))
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
         {:candidate (str ":" dependency-type) :match-index (count prefix) :type :keywoprd})
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
         (dependency-index-candidates comp-env nil prefix-tokens)
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
                (dependency-index-candidates comp-env nil prefix-tokens)
                (all-ns-candidates comp-env nil prefix-tokens true))
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

(defn scoped-candidates* [comp-env ns scope scope-name prefix-tokens]
  (let [var-candidates (when scope (env/ns-publics comp-env scope))]
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

(defn scoped-candidates [comp-env ns scope-name prefix-tokens]
  (let [scope (env/resolve-namespace comp-env (symbol scope-name) ns)]
    (scoped-candidates* comp-env ns scope scope-name prefix-tokens)))

(defn cljs-scoped-candidates [comp-env repl-env ns scope-name prefix prefix-tokens]
  (let [scope (env/resolve-namespace comp-env (symbol scope-name) ns)
        ;; :require of closure goog libs can be in alias but cannot be resolved, thus are not
        ;; found by resolve-namespace
        scope-from-alias (get (:requires ns) (symbol scope-name))]
    (if scope
      (scoped-candidates* comp-env ns scope scope-name prefix-tokens)
      (when (or (and scope-from-alias (symbol? scope-from-alias)) (= "js" scope-name))
        (let [split-index (.lastIndexOf ^String prefix ".")
              js-scope-name (cond (and (> split-index -1) (not= scope-name "js"))
                                  (str scope-from-alias "." (subs prefix 0 split-index))
                                  (and (> split-index -1) (= scope-name "js"))
                                  (subs prefix 0 split-index)
                                  (= scope-name "js")
                                  ""
                                  :else (str scope-from-alias))
              js-prefix (if (> split-index -1)
                          (subs prefix (inc split-index))
                          prefix)
              js-scope-name (@cljs-munged js-scope-name)
              js-prefix (@cljs-munged js-prefix)
              original-scope-name (str scope-name "/" (when (> split-index -1)
                                                        (subs prefix 0 (inc split-index))))]
          (let [{:keys [status value] :as e}
                (@cljs-evaluate-form
                 repl-env
                 (format "replique.cljs_env.completion.js_scoped_candidates(%s, %s, %s, true);"
                         (pr-str original-scope-name) (pr-str js-scope-name) (pr-str js-prefix))
                 :timeout-before-submitted 100)]
            (when (= :success status)
              (elisp/->ElispString value))))))))

(defn mapping-candidates [comp-env ns locals prefix-tokens]
  (->> (let [var-candidates (env/ns-map comp-env ns)]
        (for [[var-sym var] var-candidates
              :let [var-name (name var-sym)
                    {:keys [arglists macro] :as var-meta} (env/meta comp-env var)
                    match-index (and (nil? (get locals var-name))
                                     (matches? var-name prefix-tokens))]
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
        scope-name (for [kw (env/keywords comp-env)
                         :when (= (namespace kw) scope-name)
                         :let [match-index (matches? (name kw) prefix-tokens)]
                         :when match-index]
                     {:candidate (str ":" kw)
                      :type :keyword
                      :match-index (+ 1 (inc (count scope-name)) match-index)})
        :else (let [all-ns* (env/all-ns comp-env)]
                (when (not= (count (get-all-ns-data comp-env)) (count all-ns*))
                  (compute-all-ns-data comp-env all-ns*))
                (for [kw (env/keywords comp-env)
                      :let [match-index (matches? (str kw) prefix-tokens)]
                      :when match-index]
                  {:candidate (str ":" kw)
                   :type :keyword
                   :match-index (+ 1 match-index)}))))

(defn static-methods-candidates [comp-env ns scope-name prefix-tokens]
  (when (nil? comp-env)
    (when-let [^Class class (context/resolve-class ns (symbol scope-name))]
      (for [^Method method (.getMethods class)
            :when (Modifier/isStatic (.getModifiers method))
            :let [match-index (matches? (.getName method) prefix-tokens)]
            :when match-index]
        {:candidate (str scope-name "/" (.getName method))
         :type :static-method
         :match-index (+ 1 (count scope-name) match-index)}))))

(defn static-fields-candidates [comp-env ns scope-name prefix-tokens]
  (when (nil? comp-env)
    (when-let [^Class class (context/resolve-class ns (symbol scope-name))]
      (for [^Field field (.getFields class)
            :when (Modifier/isStatic (.getModifiers field))
            :let [match-index (matches? (.getName field) prefix-tokens)]
            :when match-index]
        {:candidate (str scope-name "/" (.getName field))
         :type :static-field
         :match-index (+ 1 (count scope-name) match-index)}))))

(defn method-candidates [comp-env ns locals param param-meta prefix-tokens]
  (when param
    (let [[_ _ local-meta :as local-param] (get locals param)
          param-meta (or param-meta local-meta)]
      (cond param-meta
            (when-let [^Class param-class (context/try-get-meta-class comp-env ns param-meta)]
              (for [^Method method (.getMethods param-class)
                    :when (not (Modifier/isStatic (.getModifiers method)))
                    :let [match-index (matches? (.getName method) prefix-tokens)]
                    :when match-index]
                {:candidate (str "." (.getName method))
                 :type :method
                 :match-index (+ 1 match-index)}))
            (and (nil? local-param) param)
            (when-let [^Class param-class (context/try-get-object-class comp-env ns param)]
              (for [^Method method (.getMethods param-class)
                    :when (not (Modifier/isStatic (.getModifiers method)))
                    :let [match-index (matches? (.getName method) prefix-tokens)]
                    :when match-index]
                {:candidate (str "." (.getName method))
                 :type :method
                 :match-index (+ 1 match-index)}))))))

(defn field-candidates [comp-env ns locals param param-meta prefix-tokens]
  (when (and (nil? comp-env) param)
    (let [[_ _ local-meta :as local-param] (get locals param)
          param-meta (or param-meta local-meta)]
      (cond param-meta
            (when-let [^Class param-class (context/try-get-meta-class comp-env ns param-meta)]
              (for [^Field field (.getFields param-class)
                    :when (not (Modifier/isStatic (.getModifiers field)))
                    :let [match-index (matches? (.getName field) prefix-tokens)]
                    :when match-index]
                {:candidate (str ".-" (.getName field))
                 :type :field
                 :match-index (+ 2 match-index)}))
            (and (nil? local-param) param)
            (when-let [^Class param-class (context/try-get-object-class comp-env ns param)]
              (for [^Field field (.getFields param-class)
                    :when (not (Modifier/isStatic (.getModifiers field)))
                    :let [match-index (matches? (.getName field) prefix-tokens)]
                    :when match-index]
                {:candidate (str ".-" (.getName field))
                 :type :field
                 :match-index (+ 2 match-index)}))))))

(defn cljs-field-candidates [comp-env repl-env ns locals param prefix method?]
  (when param
    (let [local-param (get locals param)]
      (when (nil? local-param)
        (let [object (try (read-string param)
                          (catch Exception e nil))]
          (when (or (string? object)
                    (symbol? object))
            (let [js-prefix (@cljs-munged prefix)
                  js-param (@cljs-tooling-form->js ns object)
                  {:keys [status value]}
                  (@cljs-evaluate-form
                   repl-env
                   (format "replique.cljs_env.completion.js_fields_candidates(%s, %s, %s);"
                           (pr-str js-prefix) (pr-str js-param) (str method?))
                   :timeout-before-submitted 100)]
              (when (= :success status)
                (elisp/->ElispString value)))))))))

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

(def ^:dynamic *file-candidates* nil)
(def ^:dynamic ^String *file-filter* nil)
(def ^:dynamic *file-directory* nil)

(defn handle-file-candidate [^Path path ^BasicFileAttributes attrs]
  (let [^String file-name (str (.getFileName path))
        match-index (if (= 0 (count *file-filter*))
                      0
                      (.lastIndexOf file-name *file-filter*))]
    (when (> match-index -1)
      (let [candidate (str *file-directory* file-name)
            candidate-map (if (.isDirectory attrs)
                            {:candidate candidate
                             :type :directory
                             :match-index (+ (count *file-directory*)
                                             (count *file-filter*)
                                             match-index)}
                            {:candidate candidate
                             :match-index (+ (count *file-directory*)
                                             (count *file-filter*)
                                             match-index)})]
        (set! *file-candidates* (conj! *file-candidates* candidate-map)))))
  (if (< (count *file-candidates*) max-candidates-number)
    FileVisitResult/CONTINUE
    FileVisitResult/TERMINATE))

(defn file-candidates [comp-env ns context ^String prefix]
  (when (nil? comp-env)
    (when-let [^String fn-context (:fn-context context)]
      (let [fn-context (if (.endsWith fn-context ".")
                         (subs fn-context 0 (dec (count fn-context)))
                         fn-context)
            fn-context-position (:fn-context-position context)
            fn-context-resolved (ns-resolve ns (symbol fn-context))]
        (when (and (or (= File fn-context-resolved)
                       (= #'clojure.java.io/file fn-context-resolved)
                       (= #'clojure.java.io/as-file fn-context-resolved))
                   (= 1 fn-context-position))
          (let [path-separator-last-index (.lastIndexOf prefix "/")
                directory (if (> path-separator-last-index -1)
                            (subs prefix 0 (inc path-separator-last-index))
                            "")
                file-filter (if (> path-separator-last-index -1)
                              (subs prefix (inc path-separator-last-index))
                              prefix)
                user-home (System/getProperty "user.home")
                user-home (if (and user-home (not (.endsWith user-home "/")))
                            (str user-home "/")
                            user-home)
                directory-path (Paths/get (if user-home
                                            (.replaceFirst directory "^~/" user-home)
                                            prefix)
                                          (make-array String 0))]
            (binding [*file-candidates* (transient [])
                      *file-filter* file-filter
                      *file-directory* directory]
              (files/visit-with-limit directory-path 1 false handle-file-candidate)
              (persistent! *file-candidates*))))))))

(def ^:dynamic *load-candidates* nil)
(def ^:dynamic *load-file-extension* nil)
(def ^:dynamic *load-file-tokens* nil)

(defn handle-load-file [path attrs]
  (let [file-str (str path)]
    (cond (.endsWith file-str ".cljc")
          (let [candidate-map {:candidate (subs file-str 0 (- (count file-str) 5))}]
            (set! *load-candidates* (conj! *load-candidates* candidate-map)))
          (.endsWith file-str (str "." *load-file-extension*))
          (let [candidate-map {:candidate (subs file-str 0
                                                (- (count file-str)
                                                   (inc (count *load-file-extension*))))}]
            (set! *load-candidates* (conj! *load-candidates* candidate-map)))))
  (if (< (count *load-candidates*) max-candidates-number)
    FileVisitResult/CONTINUE
    FileVisitResult/TERMINATE))

(defn load-candidates [comp-env ns {{position :position} :dependency-context} ^String prefix]
  (when (and (nil? comp-env) (= position :load-path))
    (if (.startsWith prefix "/")
      (let [path (subs prefix 1)
            path-tokens (tokenize-file-prefix path)]
        (for [^String file-str (:file-resources @classpath-data)
              :when (or (.endsWith file-str "cljc")
                        (.endsWith file-str (env/file-extension comp-env)))
              :let [file-str (cond (.endsWith file-str ".cljc")
                                   (subs file-str 0 (- (count file-str) 5))
                                   (.endsWith
                                    file-str (str "." (env/file-extension comp-env)))
                                   (subs
                                    file-str
                                    0 (- (count file-str)
                                         (inc (count (env/file-extension comp-env))))))]
              :when file-str
              :let [match-index (matches-file? file-str path-tokens)]
              :when match-index]
          {:candidate (str "/" file-str) :match-index (inc match-index)}))
      (let [^String root-dir (#'clojure.core/root-directory (ns-name ns))
            root-dir (if (.startsWith root-dir "/") (subs root-dir 1) root-dir)
            root-dir (str root-dir "/")
            path (str root-dir prefix)
            path (if (.startsWith path "/") (subs path 1) path)
            path-tokens (tokenize-file-prefix prefix)]
        (->> (for [^String file-str (:file-resources @classpath-data)
                   :when (and (.startsWith file-str root-dir)
                              (or (.endsWith file-str "cljc")
                                  (.endsWith file-str (env/file-extension comp-env))))
                   :let [file-str (cond (.endsWith file-str ".cljc")
                                        (subs file-str 0 (- (count file-str) 5))
                                        (.endsWith
                                         file-str (str "." (env/file-extension comp-env)))
                                        (subs
                                         file-str
                                         0 (- (count file-str)
                                              (inc (count (env/file-extension comp-env))))))
                         file-str (when file-str (subs file-str (count root-dir)))]
                   :when file-str
                   :let [match-index (matches-file? file-str path-tokens)]
                   :when match-index]
               {:candidate file-str :match-index match-index})
             distinct
             (sort-by :candidate by-length-comparator)
             (take max-candidates-number))))))

(defn resources-candidates [comp-env ns context ^String prefix]
  (when (nil? comp-env)
    (when-let [^String fn-context (:fn-context context)]
      (let [fn-context-position (:fn-context-position context)
            fn-context-resolved (ns-resolve ns (symbol fn-context))
            prefix-tokens (tokenize-file-prefix prefix)]
        (when (and (= fn-context-resolved #'clojure.java.io/resource)
                   (= 1 fn-context-position))
          (->> (for [^String file-str (:file-resources @classpath-data)
                     :when (not (or (.endsWith file-str ".cljc")
                                    (.endsWith file-str ".clj")
                                    (.endsWith file-str ".cljs")
                                    (.endsWith file-str ".jar")
                                    (.endsWith file-str ".class")))
                     :let [match-index (matches-file? file-str prefix-tokens)]
                     :when match-index]
                 {:candidate file-str :match-index match-index})
               distinct
               (sort-by :candidate by-length-comparator)
               (take max-candidates-number)))))))

(defn in-string-candidates [comp-env ns context prefix]
  (concat (file-candidates comp-env ns context prefix)
          (load-candidates comp-env ns context prefix)
          (resources-candidates comp-env ns context prefix)))

(defn parse-js-dot-access [comp-env ns ^String prefix]
  (when (instance? CljsCompilerEnv comp-env)
    (let [split-index (.indexOf prefix ".")]
      (when (> split-index -1)
        (let [scope-name (subs prefix 0 split-index)
              scope (get (:imports ns) (symbol scope-name))
              original-scope-name (subs prefix 0 (inc (.lastIndexOf prefix ".")))]
          (when scope
            [original-scope-name (str scope) (subs prefix (inc split-index))]))))))

(defn js-dot-candidates [comp-env repl-env ns [original-scope-name scope-name ^String prefix]]
  (let [split-index (.lastIndexOf prefix ".")
        js-scope-name (if (> split-index -1)
                        (str scope-name "." (subs prefix 0 split-index))
                        scope-name)
        js-prefix (if (> split-index -1)
                    (subs prefix (inc split-index))
                    prefix)
        js-scope-name (@cljs-munged js-scope-name)
        js-prefix (@cljs-munged js-prefix)]
    (let [{:keys [status value]}
          (@cljs-evaluate-form
           repl-env
           (format "replique.cljs_env.completion.js_scoped_candidates(%s, %s, %s, false);"
                   (pr-str original-scope-name) (pr-str js-scope-name) (pr-str js-prefix))
           :timeout-before-submitted 100)]
      (when (= :success status)
        (elisp/->ElispString value)))))

(defn candidates
  [comp-env repl-env ns {:keys [locals in-string? in-comment? in-ns-form?
                                at-binding-position? at-local-binding-position?
                                fn-context fn-context-position dependency-context]
                         :as context}
   ^String prefix]
  (let [ns (or (and ns (env/find-ns comp-env (symbol ns)))
               (env/find-ns comp-env (env/default-ns comp-env)))]
    (if in-string?
      (in-string-candidates comp-env ns context prefix)
      (when (and (not in-comment?)
                 (not at-binding-position?) (not at-local-binding-position?)
                 ns)
        (let [prefix (.replaceFirst prefix "^#_" "")
              keyword? (.startsWith prefix ":")
              double-colon? (.startsWith prefix "::")
              field-call? (.startsWith prefix ".-")
              method-call? (and (not field-call?) (.startsWith prefix "."))
              prefix (.replaceFirst prefix "(^::?)|(^\\.-?)" "")
              scope-split-index (.lastIndexOf prefix "/")
              scope-name (when (> scope-split-index -1) (subs prefix 0 scope-split-index))
              prefix (if (> scope-split-index -1)
                       (subs prefix (inc scope-split-index))
                       prefix)
              js-dot-access (and (= -1 scope-split-index)
                                 (not keyword?)
                                 (not field-call?)
                                 (not method-call?)
                                 (parse-js-dot-access comp-env ns prefix))
              prefix-tokens (tokenize-prefix prefix)
              candidates (cond dependency-context
                               (dependencies-candidates comp-env ns
                                                        prefix prefix-tokens context)
                               keyword? (keyword-candidates comp-env ns prefix-tokens
                                                            double-colon? scope-name)
                               ;; No need to check for locals since a scoped symbol cannot
                               ;; be a local
                               (and scope-name (nil? comp-env))
                               (concat
                                (scoped-candidates comp-env ns
                                                   scope-name prefix-tokens)
                                (static-methods-candidates comp-env ns scope-name
                                                           prefix-tokens)
                                (static-fields-candidates comp-env ns scope-name
                                                          prefix-tokens))
                               (and scope-name (instance? CljsCompilerEnv comp-env))
                               (cljs-scoped-candidates comp-env repl-env ns
                                                       scope-name prefix prefix-tokens)
                               (and method-call? (instance? CljsCompilerEnv comp-env))
                               (cljs-field-candidates comp-env repl-env ns locals
                                                      (:fn-param context)
                                                      prefix true)
                               (and method-call? (nil? comp-env))
                               (method-candidates comp-env ns locals
                                                  (:fn-param context)
                                                  (:fn-param-meta context)
                                                  prefix-tokens)
                               (and field-call? (instance? CljsCompilerEnv comp-env))
                               (cljs-field-candidates comp-env repl-env ns locals
                                                      (:fn-param context)
                                                      prefix false)
                               field-call? (field-candidates comp-env ns locals
                                                             (:fn-param context)
                                                             (:fn-param-meta context)
                                                             prefix-tokens)
                               js-dot-access (js-dot-candidates comp-env repl-env ns js-dot-access)
                               :else
                               (concat
                                (locals-candidates locals prefix-tokens)
                                (all-ns-candidates comp-env locals prefix-tokens)
                                (ns-aliases-candidates comp-env ns locals prefix-tokens)
                                (dependency-index-candidates comp-env locals prefix-tokens)
                                (mapping-candidates comp-env ns locals prefix-tokens)
                                (special-forms-candidates comp-env ns fn-context-position
                                                          prefix-tokens)
                                (literal-candidates prefix)))]
          (if (instance? ElispString candidates)
            candidates
            (->> candidates
                 distinct
                 (sort-by :candidate by-length-comparator)
                 (take max-candidates-number))))))))

;; Not all classes are enumerable (deftypes, defrecord generated classes)
