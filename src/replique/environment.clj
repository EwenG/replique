(ns replique.environment
  "Unify Clojure platforms (Clojure, Clojurescript, ...) environments"
  (:refer-clojure :exclude [ns-name find-ns ns-publics ns-map ns-aliases
                            ns-resolve all-ns ns-interns meta remove-ns
                            ns-unmap ns-imports])
  (:require [replique.utils :as utils]
            [clojure.set])
  (:import [java.io File]
           [java.lang.reflect Field]
           [java.util.concurrent ConcurrentHashMap]))

(defn safe-symbol [x]
  (when x (symbol x)))

;; Wrap the clojurescript compiler env because it is just an atom and thus can't participate
;; into polymorphism
(defprotocol ICljsCompilerEnv
  (get-wrapped [compile-env]))

(deftype CljsCompilerEnv [wrapped]
  ICljsCompilerEnv
  (get-wrapped [this] wrapped))

(def ^:private cljs-all-ns
  (utils/dynaload 'cljs.analyzer.api/all-ns))

(def ^:private cljs-find-ns
  (utils/dynaload 'cljs.analyzer.api/find-ns))

(def ^:private cljs-remove-ns
  (utils/dynaload 'cljs.analyzer.api/remove-ns))

;; We don't use the function from the analyzer API because ns-publics can be used with
;; symbols or namespaces
#_(def ^:private cljs-ns-publics
    (utils/dynaload 'cljs.analyzer.api/ns-publics))

(def ^:private cljs-get-js-index
  (utils/dynaload 'cljs.analyzer.api/get-js-index))

(defprotocol NamespaceEnv
  (all-ns [comp-env])
  (find-ns [comp-env sym])
  (ns-publics [comp-env ns])
  (ns-interns [comp-env ns])
  (ns-map [comp-env ns])
  (ns-aliases [comp-env ns])
  (ns-imports [comp-env ns])
  (ns-resolve [comp-env ns sym])
  (looks-like-var? [comp-env var])
  (meta [comp-env var])
  (remove-ns [comp-env the-ns])
  (ns-unmap [comp-env ns sym]))

(defprotocol Namespace
  (ns-name [ns]))

(defrecord CljsNamespace [name doc excludes use-macros require-macros uses
                          requires imports defs renames rename-macros]
  Object
  (toString [cljs-ns]
    (str (:name cljs-ns))))

(extend-protocol Namespace
  clojure.lang.Symbol
  (ns-name [ns] ns)
  CljsNamespace
  (ns-name [ns] (:name ns))
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
  "Symbols retrieved from a namespace :uses, :use-macros, :renames or :rename-macros entries 
  refer to the namespace the symbol is imported from. They must be further resolved to get the 
  var analysis map"
  [comp-env ns-map]
  (->> (map (partial cljs-ns-map-resolve* comp-env) ns-map)
       (into {})))

(defn- unmap-from-ns [{:keys [uses use-macros renames rename-macros] :as ns} sym]
  (-> ns
      (update :uses dissoc sym)
      (update :use-macros dissoc sym)
      
      (update :renames dissoc sym)
      (update :rename-macros dissoc sym)

      (update :defs dissoc sym)
      (update :macros dissoc sym)

      (update :imports dissoc sym)))

(extend-protocol NamespaceEnv
  CljsCompilerEnv
  (all-ns [comp-env]
    (->> (get-wrapped comp-env) (@cljs-all-ns) (remove nil?)
         (map (partial find-ns comp-env))))
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
  (ns-interns [comp-env ns]
    (let [ns (if (symbol? ns) (find-ns comp-env ns) ns)]
      (->> (merge (:defs ns) (:macros ns))
           (into {}))))
  (ns-map [comp-env ns]
    {:pre [(not (nil? ns))]}
    (when-let [ns (if (symbol? ns) (find-ns comp-env ns) ns)]
      (let [uses (->> (select-keys ns [:uses :use-macros :renames :rename-macros])
                      vals
                      (map (partial cljs-ns-map-resolve comp-env)))
            defs (vals (select-keys ns [:macros :defs]))
            imports (:imports ns)]
        (->> (concat uses defs)
             (concat (list (ns-core-refers comp-env ns) imports))
             (into {})))))
  (ns-aliases [comp-env ns]
    (let [ns (if (symbol? ns) (find-ns comp-env ns) ns)]
      (merge (:requires ns)
             (:require-macros ns))))
  (ns-imports [comp-env ns]
    (let [ns (if (symbol? ns) (find-ns comp-env ns) ns)]
      (:imports ns)))
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
    (when (map? var)
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
              :else (assoc var :name qualified-name)))))
  (remove-ns [comp-env ns-sym] (@cljs-remove-ns ns-sym))
  (ns-unmap [comp-env ns sym]
    (let [ns-sym (if (symbol? ns) ns (ns-name ns))]
      (swap! (get-wrapped comp-env) update-in [:cljs.analyzer/namespaces ns-sym]
             unmap-from-ns sym)
      nil))
  nil
  (all-ns [comp-env]
    (clojure.core/all-ns))
  (find-ns [comp-env sym]
    (clojure.core/find-ns sym))
  (ns-publics [_ ns]
    (clojure.core/ns-publics ns))
  (ns-interns [_ ns]
    (clojure.core/ns-interns ns))
  (ns-map [_ ns]
    (clojure.core/ns-map ns))
  (ns-aliases [_ ns]
    (clojure.core/ns-aliases ns))
  (ns-imports [_ ns]
    (clojure.core/ns-imports ns))
  (ns-resolve [_ ns sym]
    (clojure.core/ns-resolve ns sym))
  (looks-like-var? [_ var]
    (var? var))
  (meta [_ var]
    (clojure.core/meta var))
  (remove-ns [_ ns-sym] (clojure.core/remove-ns ns-sym))
  (ns-unmap [comp-env ns sym] (clojure.core/ns-unmap ns sym)))

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
  (default-ns [comp-env])
  (core-namespace [comp-env]))

(extend-protocol EnvDefaults
  CljsCompilerEnv
  (ns-var [comp-env] (find-ns comp-env @(resolve 'cljs.analyzer/*cljs-ns*)))
  (file-extension [_] "cljs")
  (default-ns [_] 'cljs.user)
  (core-namespace [_] 'cljs.core)
  nil
  (ns-var [_] *ns*)
  (file-extension [_] "clj")
  (default-ns [_] 'user)
  (core-namespace [_] 'clojure.core))

(defn source-file-extension [^String file-name]
  (cond (.endsWith file-name "clj") "clj"
        (.endsWith file-name "cljs") "cljs"
        (.endsWith file-name "cljc") "cljc"))

(defn get-js-index [comp-env]
  (->> comp-env get-wrapped (@cljs-get-js-index)))

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
    (let [global-constant-table (:cljs.analyzer/constant-table @(get-wrapped comp-env))]
      (into #{'cljs/quit} (constant-table->keyword global-constant-table))))
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
  
  (:cljs.analyzer/constant-table @@compiler-env)
  (:cljs.analyzer/constants (get (:cljs.analyzer/namespaces @@compiler-env) 'replique.compliment.ns-mappings-cljs-test))

  (keywords nil)
  (keywords comp-env)

  (str (find-ns comp-env 'cljs.user))

  (ns-resolve comp-env 'cljs.user 'cljs.user/prn)
  )
