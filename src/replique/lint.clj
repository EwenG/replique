(ns replique.lint
  "What is wrong with a file, as the compiler saw it the last time it compiled it.

  clj-kondo's questions - an unused binding, a require nothing uses, a call with
  the wrong number of arguments, a var that is not there - asked of what the
  compiler resolved rather than of the text. So a macro nobody configured is
  expanded rather than guessed at, a call is counted after `->' has rewritten
  it, and a var is unresolved because the program does not have it.

  ## About what is on disk

  The model is written when a file is compiled, and what it holds is the version
  of the file on disk that was compiled. So is every lint here, and nothing else:

    - a file changed on disk since it was compiled has no lints - they would be
      about a version nobody can see - unless that version failed to compile,
      in which case the failure is the one lint, and it is about the version on
      disk;
    - a lint whose verdict comes from another file - a call of a var whose
      arity changed, a use of a var that was removed - is dropped where that
      other file changed on disk since it was compiled, for the same reason;
    - and so is one about a var redefined since the model recorded it - a
      `defn' typed at a prompt, or sent from a buffer - whose arity is then the
      prompt's and not the disk's, until a load defines it again.

  The answer carries the mtime of the version it is about, which is how a client
  knows whether the text in front of it is that version - see doc/protocol.md.

  ## In clj-kondo's words

  The same linter names and the same messages - `unused binding x', `#'a.b/c is
  referred but never used' - so a lint reads the way somebody who has used
  clj-kondo expects, and a project's `.clj-kondo/config.edn' levels apply:
  `{:linters {:unused-binding {:level :off}}}' turns one off here too.

  ## Out of the model alone

  Nothing here reads the file. Where a lint needs what the source wrote rather
  than what it resolved to - where an ns form wrote an alias, how a use was
  spelled, whether it was a #' - the compiler recorded it while it still had the
  text in hand.

  ## Form and file

  Each lint says its `:scope'. A `form' lint is about one top-level form and
  what that form says - an unused binding, a call - and stays true of the form
  for as long as the form is not edited. A `file' lint is a verdict about the
  whole file - a require nothing in it uses, a private var nothing in it calls -
  which any edit anywhere in the file may have made false."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [replique.analysis :as analysis]
            [replique.cljs :as cljs]
            [replique.names :as names]
            [replique.output :as output]
            [replique.protocol :as protocol]
            [replique.state :as state]
            [replique.symbol :as sym])
  (:import [clojure.lang Namespace Var]
           [java.io File]))

;;; Whether this process can answer

(defn- named-in
  "The functions NAMES of the namespace NS-NAME, or nil where one is missing -
  a model from before lints were written, which answers usages and not this."
  [ns-name names]
  (try
    (into {} (for [n names]
               [(keyword n) (or (requiring-resolve (symbol ns-name (str n)))
                                (throw (ex-info (str "No " ns-name "/" n) {})))]))
    (catch Throwable _ nil)))

(def ^:private clj-model
  (delay (named-in "clojure.analysis"
                   '[file-facts file-failure file-mtime file-namespaces find-usages
                     definition unused-locals unused-aliases unused-refers unused-imports
                     analysed-files])))

(def ^:private cljs-model
  (delay (named-in "clojure.cljs.analysis"
                   '[file-facts file-failure file-mtime file-namespaces find-usages
                     definition unused-locals unused-aliases unused-refers unused-imports
                     unused-macro-aliases unused-macro-refers analysed-files])))

(defn- model
  "The functions of the model this thread's dialect asks, or a refusal."
  []
  (or (if (names/cljs?) @cljs-model @clj-model)
      (throw (ex-info (if (names/cljs?)
                        (str "The ClojureScript compiler of this process does not "
                             "record what lints are made of. Put a build of it that "
                             "does on the classpath - see clojure.cljs.analysis.")
                        (str "This process runs clojure " (clojure-version)
                             ", which does not record what lints are made of. Start "
                             "it on a clojure whose compiler does - see "
                             "clojure.analysis."))
                      {:replique/error :no-analysis}))))

(defn- call [k & args] (apply (get (model) k) args))

;;; Saying that the answer changed

(defonce ^:private generation (atom 0))

(defn changed!
  "Tell every control connection that what the models say may have changed, so a
  client showing lints asks again for the files it is showing.

  An EVENT and not an answer, because what changes the model is somebody else's
  evaluation: a reload typed at a repl, a load from another buffer, a var removed
  by the `:remove-var' op - and the lints of a file can change without that file
  being touched, since a call is wrong the moment its callee's arity is. The
  `:generation' only grows, so a client can tell an event it has already acted on
  from a newer one."
  []
  (output/broadcast-event!
   (protocol/event "analysis" {:generation (swap! generation inc)})))

(defn- version
  "What the two models are now, as tokens compared by identity - nil for a model
  this process has not loaded. The ClojureScript one only where its namespace is
  already loaded: asking would otherwise load the compiler into a process that
  never compiled any ClojureScript."
  []
  [(some-> (resolve 'clojure.analysis/version) (as-> v (v)))
   (when (find-ns 'clojure.cljs.analysis)
     (some-> (resolve 'clojure.cljs.analysis/version) (as-> v (v))))])

(defonce ^:private ^{:doc "The vars defined since the model last recorded them, as
  [dialect qsym] - a `defn' typed at a prompt, or sent from a buffer. See `outside?'."}
  outside
  (atom #{}))

(defn- noted
  "OUTSIDE after an evaluation that defined and removed what EVENTS say - the
  events of clojure.analysis/*defined* - and that LOADED, or not, a file into a
  model.

  A definition the model recorded is the version on disk again; one it did not is
  not, whatever the text it came from. And an evaluation either compiles files
  under the model or does not, which is what LOADED says: an evaluation typed at a
  prompt never moves the model's version, a `#replique/load' does. A ClojureScript
  file says it was compiled by namespace and not by var, since its module is
  rewritten whole - so every var of that namespace is the one on disk again."
  [outside events loaded?]
  (reduce (fn [acc {:keys [dialect op var ns]}]
            (cond
              (= :remove op) (disj acc [dialect var])
              (and (= :define op) (nil? var))
              (into #{} (remove (fn [[d k]] (and (= d dialect)
                                                 (= (str ns) (namespace k)))))
                    acc)
              (not= :define op) acc
              loaded? (disj acc [dialect var])
              :else (conj acc [dialect var])))
          outside events))

(defn watching*
  "Call F, and say the models changed where they did - see `changed!'. Whatever F
  does, including throwing: a file that fails to compile is a change too, since
  the failure is what its lint is.

  AND WHERE WHAT F DEFINED LEFT THE MODEL BEHIND, which moves no model at all: a
  function redefined at a prompt is not the version on disk, and a verdict about
  a call of it - reading its arity off the var - would be about the prompt's. So
  what F defines is collected, through `analysis/defining*', and a var defined
  outside a load is `outside?' until a load defines it again - with the clients
  told when that set changes, since the lints of every file calling it do.

  REPORT, where given, is told every definition too: `analysis/defining*' is a
  binding, and a second one under this would hide what F defines from this one -
  see `replique.hooks/around*'."
  ([f] (watching* nil f))
  ([report f]
   (let [before (version)
         events (volatile! [])]
     (try (analysis/defining* (fn [e] (vswap! events conj e) (when report (report e))) f)
          (finally
            (let [loaded? (not (every? true? (map identical? before (version))))
                  was @outside
                  now (swap! outside noted @events loaded?)]
              (when (or loaded? (not= was now))
                (changed!))))))))

(defn- outside?
  "Whether the var QSYM of DIALECT was defined since the model last recorded it -
  see `watching*'."
  [dialect qsym]
  (contains? @outside [dialect qsym]))

;;; The disk

(defn- disk-file
  "The file SOURCE is on disk, or nil where it is not one - a jar entry, or gone."
  ^File [source]
  (when-let [{:keys [file entry]} (sym/source-of source)]
    (when (and file (nil? entry))
      (let [f (File. ^String file)] (when (.isFile f) f)))))

(defn- disk-mtime [source]
  (some-> (disk-file source) (.lastModified)))

(defn- changed?
  "Whether SOURCE has changed on disk since the model's version of it was compiled.

  Not a file - a jar entry, a namespace nobody compiled from a file - is not
  changed: nothing edits it."
  [source]
  (let [compiled (call :file-mtime source)]
    (boolean (and compiled (disk-file source) (not= compiled (disk-mtime source))))))

;;; The project's levels

(def ^:private kondo-config
  "The project's .clj-kondo/config.edn as last read, and the mtime it was read at."
  (atom nil))

(defn- levels
  "{linter level} out of the project's `.clj-kondo/config.edn', or {} where there is
  none - read again when the file changes. Only the levels are read: the rest of
  clj-kondo's configuration is about how clj-kondo reads code, which is the
  question the compiler answers here."
  []
  (let [dir (:directory (state/info))
        f (when dir (io/file (str dir) ".clj-kondo" "config.edn"))]
    (if-not (and f (.isFile f))
      {}
      (let [mtime (.lastModified f)
            [at read] @kondo-config]
        (if (= at [(.getPath f) mtime])
          read
          (let [config (try (edn/read-string {:default tagged-literal} (slurp f))
                            (catch Throwable _ nil))
                lv (into {} (for [[k v] (:linters config)
                                  :when (and (keyword? k) (map? v) (keyword? (:level v)))]
                              [k (:level v)]))]
            (reset! kondo-config [[(.getPath f) mtime] lv])
            lv))))))

;;; Arity

(defn- arities
  "What the var with metadata M can be called with: [fixed-counts variadic-min], or
  nil where it does not say - a var holding a map, a multimethod.

  :arglists for Clojure, where it is written by `defn' - and quoted, in a
  ClojureScript var; `:top-fn' before it for ClojureScript, which is what the
  compiler dispatches on."
  [m]
  (let [of-lists (fn [a]
                   (when (and (sequential? a) (seq a) (every? vector? a))
                     (reduce (fn [[fixed vmin] args]
                               (let [i (.indexOf ^java.util.List args '&)]
                                 (if (neg? i)
                                   [(conj fixed (count args)) vmin]
                                   [fixed (if vmin (min vmin i) i)])))
                             [#{} nil] a)))
        {:keys [method-params variadic? max-fixed-arity]} (:top-fn m)]
    (if (seq method-params)
      (let [[fixed vmin :as a] (of-lists (vec method-params))]
        ;; a variadic signature whose & the compiler has already taken apart
        (if (and variadic? (nil? vmin) max-fixed-arity)
          [(disj fixed (apply max fixed)) max-fixed-arity]
          a))
      (let [a (:arglists m)]
        (of-lists (if (and (seq? a) (= 'quote (first a))) (second a) a))))))

(defn- expects
  "clj-kondo's way of saying what a function takes: `2', `2 or 3', `1, 2 or 3',
  `3 or more', `1, 2, 3, 4 or more'."
  [fixed vmin]
  (let [fixed (sort (if vmin (remove #(>= % vmin) fixed) fixed))]
    (if vmin
      (string/join ", " (concat (map str fixed) [(str vmin " or more")]))
      (if (next fixed)
        (str (string/join ", " (map str (butlast fixed))) " or " (last fixed))
        (str (first fixed))))))

(defn- accepts? [[fixed vmin] n]
  (or (contains? fixed n) (and vmin (>= n vmin))))

;;; Vars as they are now

(defn- var-now
  "The var QSYM names now, or nil where it is gone - unmapped by a reload, removed
  by hand, or its namespace removed. Of this thread's dialect - a ClojureScript var
  is looked up in the compile environment - unless it is a MACRO, which is a var of
  the JVM's in both."
  (^Var [qsym] (var-now qsym false))
  (^Var [qsym macro?]
   (if (and (names/cljs?) (not macro?))
     (when-let [^Namespace n (cljs/find-namespace (namespace qsym))]
       (.findInternedVar n (symbol (name qsym))))
     (try (find-var qsym) (catch Throwable _ nil)))))

(defn- var-source
  "The source the model says QSYM was defined in, or nil.

  Asked of the model and not of the var's :file, which is where it was last
  defined from: a `defn' sent from a buffer names whatever the editor said, and
  one typed at a prompt names no file at all. Except for a MACRO of a
  ClojureScript file, which is a JVM var the ClojureScript model does not define:
  its :file, then - and a macro redefined at a prompt is `outside?' anyway."
  [qsym macro?]
  (if (and macro? (names/cljs?))
    (some-> (var-now qsym true) meta :file)
    (:source (call :definition qsym))))

;;; The lints

(defn- lint
  [type level message span & {:as more}]
  (merge {:type (name type) :level (name level) :message message}
         (select-keys span [:line :column :end-line :end-column])
         {:scope "form"}
         more))

(defn- form-index
  "A function answering which of FORMS - {:line :column} in file order - a position
  is in: the last one starting at or before it."
  [forms]
  (let [starts (vec (sort (map (juxt :line :column) forms)))]
    (fn [{:keys [line column]}]
      (let [p [line column]]
        (count (take-while #(<= (compare % p) 0) starts))))))

(defn- counts-as-use? [s]
  (and (not= :discard (:dead s)) (nil? (:declaration s))))

(defn- namespace-lints
  "The ns form's verdicts: requires, refers and imports nothing uses - which the
  namespace says, at the places NS-SPECS say the ns form wrote them. The last ns
  form of a namespace, where a file has two: it is the one the namespace is."
  [dialect ns-specs]
  (let [cljs? (= :cljs dialect)
        cenv (when cljs? (:cenv (cljs/environment)))
        unused (fn [k ns] (if cljs? (call k cenv ns) (call k ns)))]
    (for [{ns-sym :ns :keys [requires imports]} (vals (into {} (map (juxt :ns identity))
                                                            ns-specs))
          :let [aliases (set (keys (unused :unused-aliases ns-sym)))
                refers (set (keys (unused :unused-refers ns-sym)))
                macro-aliases (when cljs? (set (keys (unused :unused-macro-aliases ns-sym))))
                macro-refers (when cljs? (set (keys (unused :unused-macro-refers ns-sym))))
                imported (set (keys (unused :unused-imports ns-sym)))]
          l (concat
             (mapcat
              (fn [{:keys [lib at clause as refer renamed refer-all? refer-macros]}]
                (let [macros? (#{:require-macros :use-macros} clause)
                      unused-alias? (if macros? macro-aliases aliases)
                      unused-refer? (if macros? macro-refers refers)
                      local (fn [r] (get renamed r r))
                      refer-lints (for [[r at] refer
                                        :when (and at (unused-refer? (local r)))]
                                    (lint :unused-referred-var :warning
                                          (str "#'" lib "/" r " is referred but never used")
                                          at :scope "file"))
                      macro-refer-lints (for [[r at] refer-macros
                                              :when (and at (macro-refers (local r)))]
                                          (lint :unused-referred-var :warning
                                                (str "#'" lib "/" r " is referred but never used")
                                                at :scope "file"))
                      names (concat (when as [(boolean (unused-alias? as))])
                                    (for [[r _] refer] (boolean (unused-refer? (local r))))
                                    (for [[r _] refer-macros]
                                      (boolean (and macro-refers (macro-refers (local r))))))]
                  (concat
                   (when (and at (seq names) (every? true? names) (not refer-all?))
                     [(lint :unused-namespace :warning
                            (str "namespace " lib " is required but never used")
                            at :scope "file")])
                   refer-lints
                   macro-refer-lints)))
              requires)
             (for [{short :name :keys [at]} imports
                   :when (and at (imported short))]
               (lint :unused-import :warning (str "Unused import " short) at
                     :scope "file")))]
      l)))

(defn- warning-message
  "What a ClojureScript warning says, in ClojureScript's words where it has them."
  [kind info]
  (let [{:keys [prefix suffix ns-sym sym protocol fname name invalid-arity]} info]
    (case kind
      :undeclared-var (str "Use of undeclared Var " prefix "/" suffix)
      :undeclared-ns (str "No such namespace: " ns-sym)
      :js-name-is-a-namespace (str sym " names the namespace " ns-sym
                                   ", not a JavaScript global")
      :protocol-invalid-method
      (if invalid-arity
        (str "Bad method signature in protocol implementation, " protocol
             " does not declare arity " invalid-arity " for " fname)
        (str "Bad method signature in protocol implementation, " protocol
             " does not declare method called " fname))
      :protocol-duped-method (str "Duplicated method " fname " in protocol implementation "
                                  protocol)
      :protocol-multiple-impls (str "Protocol " protocol " implemented multiple times")
      :undeclared-protocol-symbol (str "Can't resolve protocol symbol " protocol)
      :invalid-protocol-symbol (str "Symbol " protocol " is not a protocol")
      :protocol-deprecated (str "Protocol " protocol " is deprecated")
      :protocol-impl-with-variadic-method
      (str "Protocol " protocol " implements method " name " with variadic signature (&)")
      :multiple-variadic-overloads (str name ": Can't have more than 1 variadic overload")
      :variadic-max-arity (str name ": Can't have fixed arity function with more params"
                               " than variadic function")
      :overload-arity (str name ": Can't have 2 overloads with same arity")
      :extending-base-js-type "Extending an existing JavaScript type - use a different symbol name"
      (str (clojure.core/name kind) " " (pr-str info)))))

(defn- warning-lints [dialect warnings]
  (for [{:keys [kind message info] :as w} warnings]
    (if (= :cljs dialect)
      (lint kind :warning (warning-message kind info) w)
      (if (= :earmuffed-var-not-dynamic kind)
        (lint kind :warning
              (str "Var has earmuffed name but is not declared dynamic: " message) w)
        (lint kind :warning message w)))))

(defn- failure-lint
  "The compile failure of the version on disk, as the lint it is - clj-kondo's name
  for it where the compiler's message says which one it is."
  [{:keys [message] :as failure}]
  (let [message (str message)
        at (merge {:column 1} (select-keys failure [:line :column :end-line :end-column]))
        [_ unresolved] (re-find #"^Unable to resolve symbol: (\S+) in this context" message)
        [_ no-var] (re-find #"^No such var: (\S+)" message)
        [_ no-ns] (re-find #"^No such namespace: (\S+)" message)
        [_ private] (re-find #"^var: (#'\S+) is not public" message)]
    (cond
      unresolved (lint :unresolved-symbol :error (str "Unresolved symbol: " unresolved) at
                       :scope "file")
      no-var (lint :unresolved-var :error (str "Unresolved var: " no-var) at :scope "file")
      no-ns (lint :unresolved-namespace :error
                  (str "Unresolved namespace " no-ns ". Are you missing a require?") at
                  :scope "file")
      private (lint :private-call :error (str private " is private") at :scope "file")
      (re-find #"(?i)EOF while reading|Unmatched delimiter|Invalid token|Unsupported" message)
      (lint :syntax :error message at :scope "file")
      :else (lint :compile-error :error message at :scope "file"))))

(defn- file-lints
  "Every lint of SOURCE, whose model version is the version on disk."
  [dialect source]
  (let [facts (call :file-facts source)
        own-form (form-index (:forms facts))
        ;; what another file's changes say about a var: nothing, where that file
        ;; changed on disk since it was compiled
        ;; and nothing, either, about a var defined since at a prompt: what the
        ;; runtime has is then not the version on disk - see `watching*'
        other-changed (memoize (fn [qsym macro?]
                                 (or (outside? (if macro? :clj dialect) qsym)
                                     (let [s (var-source qsym macro?)]
                                       (boolean (and s (not= s source) (changed? s)))))))
        ns-changed (memoize (fn [ns-name]
                              (boolean (some (fn [s] (and (not= s source)
                                                          (contains? (call :file-namespaces s)
                                                                     ns-name)
                                                          (changed? s)))
                                             (call :analysed-files)))))
        usages (filterv #(not= :discard (:dead %)) (:usages facts))]
    (concat
     ;; unused-binding
     (for [{:keys [name] :as l} (call :unused-locals)
           :when (= source (:source l))
           :let [n (str name)]
           :when (not (or (string/starts-with? n "_") (#{"&form" "&env"} n)))]
       (lint :unused-binding :warning (str "unused binding " n) l))

     (namespace-lints dialect (:ns-specs facts))

     ;; unused-private-var: nothing uses it, but its own definition
     (for [{v-sym :var :as d} (:defs facts)
           :let [v (var-now v-sym)]
           :when (and v (:private (meta v)))
           :let [home (own-form d)
                 uses (filter (fn [u] (and (counts-as-use? u)
                                           (not (and (= source (:source u))
                                                     (= home (own-form u))))))
                              (call :find-usages v-sym))]
           :when (empty? uses)]
       (lint :unused-private-var :warning (str "Unused private var " v-sym) d :scope "file"))

     ;; redefined-var: defined again in another top-level form, not by `declare'
     (for [[v-sym defs] (group-by :var (:defs facts))
           :let [real (remove #(= :declare (:declaration %)) defs)
                 by-form (into (sorted-map) (map (juxt own-form identity) real))]
           :when (next by-form)
           d (rest (vals by-form))]
       (lint :redefined-var :warning (str "redefined var #'" v-sym) d :scope "file"))

     ;; uses of what is not there, is private, or is deprecated
     (mapcat
      (fn [{v-sym :var :keys [from-ns macro] :as u}]
        (let [v (var-now v-sym macro)]
          (cond
            (nil? v)
            ;; a `requiring-resolve' loads what it names when it runs, so a name in
            ;; a namespace nothing has loaded yet - an optional dependency - is not
            ;; missing; one in a loaded namespace that does not have it is
            (when-not (or (ns-changed (symbol (namespace v-sym)))
                          (and (= :requiring-resolve (:role u))
                               (nil? (find-ns (symbol (namespace v-sym))))))
              ;; as the source spelled it: the model keeps a spelling that is not
              ;; the var's own name, unqualified
              (let [written (str (or (:written u) (name v-sym)))]
                [(if (string/includes? written "/")
                   (lint :unresolved-var :warning (str "Unresolved var: " written) u)
                   (lint :unresolved-symbol :error (str "Unresolved symbol: " written) u))]))

            (other-changed v-sym macro) nil

            :else
            (concat
             ;; a #' names the var, which is allowed of a private one - and so does
             ;; a `requiring-resolve', which resolves whatever is interned
             (when (and (:private (meta v)) from-ns (not= (str from-ns) (namespace v-sym))
                        (not (:var-form u)) (not= :requiring-resolve (:role u)))
               [(lint :private-call :error (str "#'" v-sym " is private") u)])
             (when-let [d (and (nil? (:declaration u)) (:deprecated (meta v)))]
               [(lint :deprecated-var :warning
                      (str "#'" v-sym " is deprecated"
                           (when (string? d) (str " since " d)))
                      u)])))))
      usages)

     ;; invalid-arity
     (for [{v-sym :var :keys [argc] :as i} (:invokes facts)
           :let [v (var-now v-sym)]
           :when (and v (not (other-changed v-sym false)))
           :let [a (arities (meta v))]
           :when (and a (or (seq (first a)) (second a)) (not (accepts? a argc)))]
       (lint :invalid-arity :error
             (str v-sym " is called with " argc (if (= 1 argc) " arg" " args")
                  " but expects " (apply expects a))
             i))

     (warning-lints dialect (:warnings facts)))))

(defn- source-of-path
  "What the model calls the file at PATH."
  [path]
  (analysis/source-path path))

(defn lints
  "The lints of the file MSG names, as the reply carries them - see the ns doc."
  [{:keys [file]}]
  (let [dialect (if (names/cljs?) :cljs :clj)
        _ (model)
        source (when (string? file) (source-of-path file))
        disk (when source (disk-file source))
        compiled (when source (call :file-mtime source))
        failure (when source (call :file-failure source))
        now (when disk (.lastModified disk))
        config (levels)
        leveled (fn [ls]
                  (vec (keep (fn [l]
                               (let [level (get config (keyword (:type l)))]
                                 (cond (= :off level) nil
                                       level (assoc l :level (name level))
                                       :else l)))
                             ls)))]
    (cond
      (nil? disk)
      {:analysed false :lints []}

      (and failure (= now (:mtime failure)))
      {:analysed true :mtime now :lints (leveled [(failure-lint failure)])}

      (or (nil? compiled) (not= compiled now))
      {:analysed (some? compiled) :changed (some? compiled) :lints []}

      :else
      {:analysed true
       :mtime now
       :lints (->> (file-lints dialect source)
                   (sort-by (juxt :line :column))
                   (distinct)
                   (leveled))})))
