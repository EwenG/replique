(ns replique.environment
  "Unify Clojure platforms (Clojure, Clojurescript, ...) environments"
  (:refer-clojure :exclude [ns-name find-ns ns-publics ns-map
                            ns-aliases ns-resolve all-ns meta])
  (:require [replique.utils :as utils]
            [replique.compliment.utils :refer [defmemoized all-files-on-classpath]]
            [clojure.set])
  (:import [java.io File]
           [java.lang.reflect Field]
           [java.util.concurrent ConcurrentHashMap]))

(defn safe-symbol [x]
  (when x (symbol x)))

(defprotocol ICljsCompilerEnv
  (get-wrapped [compile-env]))
(deftype CljsCompilerEnv [wrapped]
  ICljsCompilerEnv
  (get-wrapped [this] wrapped))

(def ^:private cljs-all-ns
  (utils/dynaload 'cljs.analyzer.api/all-ns))

(def ^:private cljs-find-ns
  (utils/dynaload 'cljs.analyzer.api/find-ns))

;; We don't use the function from the analyzer API because ns-publics can be used with
;; symbols or namespaces
#_(def ^:private cljs-ns-publics
    (utils/dynaload 'cljs.analyzer.api/ns-publics))

(def ^:private cljs-ns-var
  (utils/dynaload 'cljs.analyzer/*cljs-ns*))

(def ^:private cljs-get-js-index
  (utils/dynaload 'cljs.analyzer.api/get-js-index))

(defprotocol NamespaceEnv
  (all-ns [comp-env])
  (find-ns [comp-env sym])
  (ns-publics [comp-env ns])
  (ns-map [comp-env ns])
  (ns-aliases [comp-env ns])
  (ns-resolve [comp-env ns sym])
  (looks-like-var? [comp-env var])
  (meta [comp-env var]))

(defprotocol Namespace
  (ns-name [ns]))

(defrecord CljsNamespace [name doc excludes use-macros require-macros uses
                          requires imports defs]
  Object
  (toString [cljs-ns]
    (str (:name cljs-ns))))

(extend-protocol Namespace
  CljsNamespace
  (ns-name [ns]
    (if (symbol? ns) ns
        (:name ns)))
  clojure.lang.Namespace
  (ns-name [ns]
    (clojure.core/ns-name ns)))

(defn ns-core-refers
  "Returns a list of cljs.core vars visible to the ns."
  [comp-env ns]
  (let [vars (ns-publics comp-env 'cljs.core)
        excludes (:excludes ns)]
    (apply dissoc vars excludes)))

(defn resolve-namespace
  "Tries to resolve a namespace from the given symbol, either from a
  fully qualified name or an alias in the given namespace."
  [comp-env sym ns]
  (or (find-ns comp-env sym) (get (ns-aliases comp-env ns) sym)))

(defn cljs-ns-map-resolve*
  [comp-env [sym ns]]
  (let [ns (get-in @(get-wrapped comp-env) [:cljs.analyzer/namespaces ns])]
    [sym (or (get-in ns [:defs sym]) (get-in ns [:macros sym]))]))

(defn cljs-ns-map-resolve
  "Symbols retrieved from a namespace :uses or :use-macros entries refer to the namespace 
  the symbol is imported from. They must be further resolved to get the var analysis map"
  [comp-env ns-map]
  (->> (map (partial cljs-ns-map-resolve* comp-env) ns-map)
       (into {})))

(extend-protocol NamespaceEnv
  CljsCompilerEnv
  (all-ns [comp-env]
    (@cljs-all-ns (get-wrapped comp-env)))
  (find-ns [comp-env sym]
    (when-let [found-ns (@cljs-find-ns (get-wrapped comp-env) sym)]
      ;; name may be null for clojure namespaces (those defining macros)
      (let [found-ns (if (nil? (:name found-ns)) (assoc found-ns :name sym) found-ns)]
        (map->CljsNamespace found-ns))))
  (ns-publics [comp-env ns]
    (let [ns (if (symbol? ns) (find-ns comp-env ns) ns)]
      (->> (merge (:defs ns) (:macros ns))
           (remove (fn [[k v]] (:private v)))
           (into {}))))
  (ns-map [comp-env ns]
    {:pre [(not (nil? ns))]}
    (let [ns (if (symbol? ns) (find-ns comp-env ns) ns)
          uses (->> (select-keys ns [:uses :use-macros])
                    vals
                    (map (partial cljs-ns-map-resolve comp-env)))
          defs (vals (select-keys ns [:imports :macros :defs]))
          imports (:imports ns)]
      (->> (concat uses defs)
           (concat (list (ns-core-refers comp-env ns) imports))
           (into {}))))
  (ns-aliases [comp-env ns]
    (let [ns (if (symbol? ns) (find-ns comp-env ns) ns)]
      (merge (:requires ns)
             (:require-macros ns))))
  (ns-resolve [comp-env ns sym]
    (let [ns (if (symbol? ns) (find-ns comp-env ns) ns)
          sym-ns (safe-symbol (namespace sym))
          ns (if sym-ns
               (resolve-namespace comp-env sym-ns ns)
               ns)
          sym (symbol (name sym))]
      (when ns
        (get (ns-map comp-env ns) sym))))
  (looks-like-var? [_ var]
    (map? var))
  (meta [comp-env var]
    (let [;; Resolve the namespace
          qualified-name (:name var)
          ;; Arglists might have a spurious (quote ...) wrapping form
          arglists (:arglists var)
          arglists (if (= 'quote (first arglists)) (second arglists) arglists)
          var (if arglists (assoc var :arglists arglists) var)]
      (cond (nil? qualified-name)
            var
            (namespace qualified-name)
            (let [ns-sym (symbol (namespace qualified-name))
                  var-sym (symbol (name qualified-name))
                  ns (find-ns comp-env ns-sym)]
              (assoc var
                     :ns ns
                     :name var-sym))
            :else (assoc var :name qualified-name))))
  nil
  (all-ns [comp-env]
    (clojure.core/all-ns))
  (find-ns [comp-env sym]
    (clojure.core/find-ns sym))
  (ns-publics [_ ns]
    (clojure.core/ns-publics ns))
  (ns-map [_ ns]
    (clojure.core/ns-map ns))
  (ns-aliases [_ ns]
    (clojure.core/ns-aliases ns))
  (ns-resolve [_ ns sym]
    (clojure.core/ns-resolve ns sym))
  (looks-like-var? [_ var]
    (var? var))
  (meta [_ var]
    (clojure.core/meta var)))

(comment

  (require '[replique.repl-cljs :refer [compiler-env]])
  (def comp-env (->CljsCompilerEnv @compiler-env))
  
  (find-ns comp-env 'replique.compliment.ns-mappings-clj-test)

  (:arglists (meta comp-env (ns-resolve comp-env 'replique.compliment.ns-mappings-cljs-test 'my-fn)))

  (ns-resolve comp-env 'replique.compliment.ns-mappings-cljs-test 'file->ns)
  (ns-resolve comp-env 'replique.compliment.ns-mappings-cljs-test 'cljs/file->ns)
  (ns-resolve comp-env 'replique.compliment.ns-mappings-cljs-test 'cljs-ns-m/my-macro)
  (ns-resolve comp-env 'replique.compliment.ns-mappings-cljs-test 'defn)

  (keys (ns-core-refers comp-env 'replique.compliment.ns-mappings-cljs-test))

  (get (:cljs.analyzer/namespaces @@compiler-env )
       'replique.compliment.ns-mappings-cljs-test)
  
  )

(defprotocol EnvDefaults
  (ns-var [comp-env])
  (file-extension [comp-env])
  (default-ns [comp-env]))

(extend-protocol EnvDefaults
  CljsCompilerEnv
  (ns-var [comp-env] (find-ns comp-env @cljs-ns-var))
  (file-extension [_] "cljs")
  (default-ns [_] 'cljs.user)
  nil
  (ns-var [_] *ns*)
  (file-extension [_] "clj")
  (default-ns [_] 'user))

(defmemoized namespaces-on-classpath
  "Returns the list of all Clojure/Clojurescript/... namespaces obtained by classpath scanning."
  [comp-env]
  (set (for [^String file (all-files-on-classpath)
             :when (and (.endsWith file (file-extension comp-env))
                        (not (.startsWith file "META-INF")))
             :let [[_ ^String nsname] (->
                                       "[^\\w]?(.+)(\\.%s|\\.cljc)"
                                       (format (file-extension comp-env))
                                       re-pattern 
                                       (re-matches file))]
             :when nsname]
         (.. nsname (replace File/separator ".") (replace "_" "-")))))

(defmemoized provides-from-js-dependency-index [comp-env]
  (->> comp-env get-wrapped (@cljs-get-js-index) vals (mapcat :provides) set))

(defprotocol Env
  (keywords [comp-env])
  (special-forms [comp-env]))

(defn constant-table->keyword [constant-table]
  (->> (keys constant-table)
       (filter keyword?)
       (map #(symbol (namespace %) (name %)))))

(extend-protocol Env
  CljsCompilerEnv
  (keywords [comp-env]
    (let [global-constant-table (:cljs.analyzer/constant-table @(get-wrapped comp-env))
          constant-tables (->>
                           @(get-wrapped comp-env)
                           :cljs.analyzer/namespaces
                           (map (comp :cljs.analyzer/constant-table second))
                           (cons global-constant-table))]
      (->> (map constant-table->keyword constant-tables)
           (reduce #(into %1 %2) #{}))))
  (special-forms [_]
    (set (map name '[def if do quote recur throw try catch new set!])))
  nil
  (keywords [_]
    (let [^Field field (.getDeclaredField clojure.lang.Keyword "table")]
      (.setAccessible field true)
      (.keySet ^ConcurrentHashMap (.get field nil))))
  (special-forms [_]
    (set (map name '[def if do quote var recur throw try catch
                     monitor-enter monitor-exit new set!]))))

(comment
  (require '[replique.repl-cljs :refer [compiler-env]])
  (def comp-env (->CljsCompilerEnv @compiler-env))

  (file-extension comp-env)
  (namespaces-on-classpath comp-env)
  (filter #(.endsWith % "cljs") (all-files-on-classpath))
  
  (count (provides-from-js-dependency-index comp-env))

  (:cljs.analyzer/constant-table @@compiler-env)
  (:cljs.analyzer/constants (get (:cljs.analyzer/namespaces @@compiler-env) 'replique.compliment.ns-mappings-cljs-test))

  (keywords nil)
  (keywords comp-env)

  (str (find-ns comp-env 'cljs.user))

  (ns-resolve comp-env 'cljs.user 'cljs.user/prn)
  )
