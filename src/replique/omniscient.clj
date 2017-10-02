(ns replique.omniscient
  (:refer-clojure :exclude [with-redefs])
  (:require [clojure.set]
            [replique.repl] 
            [replique.tooling-msg :as tooling-msg]
            [replique.utils :as utils]
            [replique.omniscient-runtime :refer
             [registry *omniscient-env* append-env last-selected]]
            [replique.environment :as env]
            [clojure.pprint])
  (:import [java.util Date]
           [java.io Writer]
           [java.util.concurrent LinkedTransferQueue]
           [java.util.concurrent.locks ReentrantLock]))

(def ^:private cljs-defn (utils/dynaload 'cljs.core/defn))
(def ^:private cljs-defmethod (utils/dynaload 'cljs.core/defmethod))
(def ^:private cljs-extend-type (utils/dynaload 'cljs.core/extend-type))
(def ^:private cljs-eval-cljs-form (utils/dynaload 'replique.repl-cljs/eval-cljs-form))
(def ^:private cljs-eval-cljs (utils/dynaload 'replique.repl-cljs/eval-cljs))
(def ^:private cljs-repl-env (utils/dynaload 'replique.repl-cljs/repl-env))
(def ^:private cljs-repl-env-nashorn (utils/dynaload 'replique.nashorn/repl-env))
(def ^:private cljs-compiler-env (utils/dynaload 'replique.repl-cljs/compiler-env))
(def ^:private cljs-in-ns* (utils/dynaload 'replique.repl-cljs/in-ns*))
(def ^:private cljs-repl-caught (utils/dynaload 'cljs.repl/repl-caught))
(def ^:private cljs-repl (utils/dynaload 'replique.cljs/repl))
(def ^:private cljs-repl-prompt (utils/dynaload 'cljs.repl/repl-prompt))
(def ^:private cljs-repl-read-with-exit (utils/dynaload 'replique.repl-cljs/repl-read-with-exit))

(comment
  (env/->CljsCompilerEnv @@cljs-compiler-env)
  )

;; whether we are in a debugging REPL
(defonce ^:dynamic *omniscient-repl?* false)
;; the fully qualified symbol currently beeing debugged
(defonce ^:dynamic *omniscient-sym* nil)
;; locals and bindings symbols must be known by the omniscient REPL "eval" function
(defonce ^:dynamic *omniscient-local-syms* nil)
(defonce ^:dynamic *omniscient-binding-syms* nil)

;; protects with-redefs
(defonce ^:private lock (ReentrantLock.))

(defmacro ^:private with-lock
  [lock-expr & body]
  `(let [lockee# ~(with-meta lock-expr {:tag 'java.util.concurrent.locks.ReentrantLock})]
     (.lock lockee#)
     (try
       ~@body
       (finally
         (.unlock lockee#)))))

(defn get-comp-env [&env]
  (if (utils/cljs-env? &env)
    (env/->CljsCompilerEnv @@cljs-compiler-env) nil))

(defn var->sym [comp-env v]
  (let [{:keys [ns name]} (env/meta comp-env v)]
    (when (and ns name)
      (symbol (str (env/ns-name ns)) (str name)))))

(defn sig-symbols [sig]
  (cond
    (and (symbol? sig) (not= '& sig)) #{sig}
    (coll? sig)
    (apply clojure.set/union (map sig-symbols sig))
    :else
    #{}))

(comment
  (sig-symbols [])
  (sig-symbols '[a])
  (sig-symbols '[a b])
  (sig-symbols '[[x & y] c {:keys [d]} {:r {:t e} :as f} & g])
  (sig-symbols '[_])
  )

;; clojure.core/*data-readers* clojure.core/*default-data-reader-fn*

;; Prevent dynamic vars used by the REPL/system from beeing captured
(def excluded-dyn-vars #{'clojure.core/*3 'clojure.core/*print-meta* 'clojure.core/*print-namespace-maps* 'clojure.core/*file* 'clojure.core/*command-line-args* 'clojure.core/*2 'clojure.core/*err* 'clojure.core/*print-length* 'clojure.core/*math-context* 'clojure.core/*e 'clojure.core/*1 'clojure.core/*source-path* 'clojure.core/*unchecked-math* 'clojure.spec/*explain-out* 'clojure.core/*in* 'clojure.core/*print-level* 'replique.server/*session* 'clojure.core/*warn-on-reflection* 'clojure.core/*out* 'clojure.core/*assert* 'clojure.core/*read-eval* 'clojure.core/*ns* 'clojure.core/*compile-path* 'clojure.core.server/*session* 'replique.omniscient/*omniscient-repl?* 'replique.omniscient/*omniscient-sym* 'replique.omniscient/*omniscient-local-syms* 'replique.omniscient/*omniscient-binding-syms* 'replique.omniscient-runtime/*omniscient-env*})

(defn capture-env-clj [qualified-sym method locals]
  ;; exclude dynamic vars that are thread bound when calling "with-redefs". ie those that are
  ;; needed by the REPL/system
  (let [locals (mapcat (fn [x] [`(quote ~x) x]) locals)]
    `(swap! registry update (quote ~qualified-sym)
            append-env {:thread (Thread/currentThread)
                        :time (Date.)
                        :ns (find-ns (quote ~(symbol (namespace qualified-sym))))
                        :var (var ~qualified-sym)
                        ~@(when method [:method `(quote ~method)]) ~@[]
                        :locals {~@locals ~@[]}
                        :bindings (->> (get-thread-bindings)
                                       (map (fn [[k# v#]] (when-let [sym# (var->sym nil k#)]
                                                            (when (not
                                                                   (contains?
                                                                    excluded-dyn-vars sym#))
                                                              [sym# v#]))))
                                       (into {}))})))

(defn ns-map-filter-dynamic [ns-map]
  (filter (fn [[k v]] (:dynamic v)) ns-map))

(defn dyn-vars [comp-env ns]
  (when-let [ns (if (symbol? ns) (env/find-ns comp-env ns) ns)]
    (let [uses (->> (select-keys ns [:uses :renames])
                    vals
                    (map (partial env/cljs-ns-map-resolve comp-env))
                    (map ns-map-filter-dynamic))
          defs (->> (select-keys ns [:defs])
                    vals
                    (map ns-map-filter-dynamic))]
      (->> (concat uses defs)
           (into {})))))

(defn capture-env-cljs [qualified-sym method locals]
  (let [locals (mapcat (fn [x] [`(quote ~x) x]) locals)
        comp-env (env/->CljsCompilerEnv @@cljs-compiler-env)
        dyn-vars (->> (dyn-vars comp-env (symbol (namespace qualified-sym)))
                      (map (comp :name second))
                      (remove (partial contains? excluded-dyn-vars)))]
    `(swap! replique.omniscient-runtime/registry update (quote ~qualified-sym)
            replique.omniscient-runtime/append-env
            {:time (js/Date.)
             :ns (find-ns (quote ~(symbol (namespace qualified-sym))))
             :var (var ~qualified-sym)
             ~@(when method [:method `(quote ~method)]) ~@[]
             :locals {~@locals ~@[]}
             :bindings ~(zipmap (map (fn [x] `(quote ~x)) dyn-vars) dyn-vars)})))

(defn capture-env [&env qualified-sym method locals]
  (if (utils/cljs-env? &env)
    (capture-env-cljs qualified-sym method locals)
    (capture-env-clj qualified-sym method locals)))

(defn is-omniscient-local? [x]
  (::local (meta x)))

(comment
  (capture-env-clj 'replique.omniscient/ee nil #{'a 'b})
  )

(defn fdecl-with-env-capture [[params & body] capture-env-expr]
  `(~params ~@(conj body capture-env-expr)))

(defn- sigs-normalized [sigs]
  (let [name (if (symbol? (first sigs)) (first sigs) nil)
        sigs (if name (next sigs) sigs)
        sigs (if (vector? (first sigs)) 
               (list sigs) 
               (if (seq? (first sigs))
                 sigs
                 ;; Assume single arity syntax
                 (throw (IllegalArgumentException.
                         (if (seq sigs)
                           (str "Parameter declaration " 
                                (first sigs)
                                " should be a vector")
                           (str "Parameter declaration missing"))))))]
    sigs))

(defn- sigs-with-env-capture [sigs sig capture-env-expr]
  ;; Ensure correct type before destructuring sig
  (when (not (seq? sig))
    (throw (IllegalArgumentException.
            (str "Invalid signature " sig
                 " should be a list"))))
  (let [[params & body] sig
        _ (when (not (vector? params))
            (throw (IllegalArgumentException. 
                    (if (seq? (first sigs))
                      (str "Parameter declaration " params
                           " should be a vector")
                      (str "Invalid signature " sig
                           " should be a list")))))
        conds (when (and (next body) (map? (first body))) 
                (first body))
        body (if conds (next body) body)]
    (cond->> body
      true (cons capture-env-expr)
      conds (cons conds)
      true (cons params))))

(defn env-ns [&env]
  (if (utils/cljs-env? &env)
    (env/map->CljsNamespace (:ns &env))
    *ns*))

;; ns-resolve is not sufficient because the var may not be defined yet
(defn compute-qualified-sym [comp-env current-ns sym]
  (if-let [resolved (env/ns-resolve comp-env current-ns sym)]
    (var->sym comp-env resolved)
    (if-let [n (namespace sym)]
      (let [ns-sym (symbol n)
            ns-from-sym (get (env/ns-aliases comp-env current-ns) ns-sym
                             (env/find-ns comp-env ns-sym))]
        (when ns-from-sym
          (symbol (str (env/ns-name ns-from-sym)) (name sym))))
      (symbol (str (env/ns-name current-ns)) (name sym)))))

(comment
  (compute-qualified-sym (env/->CljsCompilerEnv @@cljs-compiler-env) 'replique.omniscient-runtime 'ILookup)
  )

(defn env-locals-clj [&env]
  (->> &env keys (remove is-omniscient-local?)))

(defn env-locals-cljs [&env]
  (->> &env :locals keys))

(defn env-locals [&env]
  (if (utils/cljs-env? &env)
    (env-locals-cljs &env)
    (env-locals-clj &env)))

(defn omniscient-defn [defn-o]
  (fn [&form &env name & fdecl]
    (let [;; fdecl handling is copied from clojure.core/defn
          fdecl (if (string? (first fdecl))
                  (next fdecl)
                  fdecl)
          fdecl (if (map? (first fdecl))
                  (next fdecl)
                  fdecl)
          fdecl (if (vector? (first fdecl))
                  (list fdecl)
                  fdecl)
          fdecl (if (map? (last fdecl))
                  (butlast fdecl)
                  fdecl)
          sigs (sigs-normalized fdecl)
          params (map first sigs)
          locals (map sig-symbols params)
          env-locals (env-locals &env)
          locals (map #(clojure.set/union % env-locals) locals)
          qualified-sym (compute-qualified-sym (get-comp-env &env) (env-ns &env) name)
          capture-env-fn (partial capture-env &env qualified-sym nil)
          capture-env-exprs (map capture-env-fn locals)
          sigs-with-env-capture (partial sigs-with-env-capture sigs)
          sigs (map sigs-with-env-capture sigs capture-env-exprs)]
      (apply defn-o &form &env name sigs))))

(defn omniscient-defmethod [defmethod-o]
  (fn [&form &env multifn dispatch-val & fn-tail]
    (let [sigs (sigs-normalized fn-tail)
          params (map first sigs)
          locals (map sig-symbols params)
          env-locals (env-locals &env)
          locals (map #(clojure.set/union % env-locals) locals)
          qualified-sym (compute-qualified-sym (get-comp-env &env) (env-ns &env) multifn)
          capture-env-fn (partial capture-env &env qualified-sym nil)
          capture-env-exprs (map capture-env-fn locals)
          sigs-with-env-capture (partial sigs-with-env-capture sigs)
          sigs (map sigs-with-env-capture sigs capture-env-exprs)]
      (apply defmethod-o &form &env multifn dispatch-val sigs))))

(defn parse-impl [impl]
  (if (vector? (second impl))
    `(~(first impl) ~(rest impl))
    impl))

(comment
  (parse-impl '(pp ([this] 2) ([this [e f] h] e)))
  (parse-impl '(pp [this] 4 5))
  )

(defn parse-impls [specs]
  (loop [ret '() s specs]
    (if (seq s)
      (recur (conj ret `(~(first s) ~@(->> (next s) (take-while seq?) (map parse-impl))))
             (drop-while seq? (next s)))
      (reverse ret))))

(defn method-expr-with-env-capture [capture-env-exprs [params & body]]
  `(~params ~@(cons capture-env-exprs body)))

(defn method-with-env-capture-extend-type [&env qualified-sym method]
  (let [method-meta (meta method) ;; cljs requires the metadata to be preserved
        [method-name & methods] method
        locals (map (comp sig-symbols first) methods)
        env-locals (env-locals &env)
        locals (map #(clojure.set/union % env-locals) locals)
        capture-env-fn (partial capture-env &env qualified-sym method-name)
        capture-env-exprs (map capture-env-fn locals)
        methods (map method-expr-with-env-capture capture-env-exprs methods)]
    (with-meta `(~method-name ~@methods) method-meta)))

(comment
  (method-with-env-capture-extend-type
   nil 'my-ns/my-protocol '(pp ([this] 2) ([this [e f] h] e)))
  )

(defn impl-with-env-capture-extend-type [&env [protocol-name & methods]]
  (let [qualified-sym (compute-qualified-sym (get-comp-env &env) (env-ns &env) protocol-name)]
    `(~protocol-name ~@(map (partial method-with-env-capture-extend-type &env qualified-sym)
                            methods))))

(comment
  (impl-with-env-capture-extend-type
   nil '(Pp (pp ([this] 2) ([this [e f] h] e)) (pp2 ([this] 3))))
  )

(defn omniscient-extend-type [extend-type-o]
  (fn [&form &env t & specs]
    (let [impls (parse-impls specs)
          impls (mapcat (partial impl-with-env-capture-extend-type &env) impls)]
      (apply extend-type-o &form &env t impls))))

(defn method-with-env-capture-deftype [&env qualified-sym fields [method-name [params & body]]]
  (let [locals (sig-symbols params)
        env-locals (env-locals &env)
        locals (clojure.set/union env-locals fields locals)
        capture-env-exprs (capture-env &env qualified-sym method-name locals)
        method (method-expr-with-env-capture capture-env-exprs `(~params ~@body))]
    `(~method-name ~@method)))

(defn impl-with-env-capture-deftype [&env fields [protocol-name & methods]]
  (let [qualified-sym (compute-qualified-sym (get-comp-env &env) (env-ns &env) protocol-name)]
    `(~protocol-name ~@(map (partial method-with-env-capture-deftype &env qualified-sym fields)
                            methods))))

(comment
  (impl-with-env-capture-deftype
   nil '[a b] '(Pp (pp ([this] 2)) (pp ([this [e f] h] e)) (pp2 ([this] 3))))
  )

(defn omniscient-deftype [deftype-o]
  (fn [&form &env name fields & opts+specs]
    (#'clojure.core/validate-fields fields name)
    (let [qualified-sym (compute-qualified-sym (get-comp-env &env) (env-ns &env) name)
          [opts specs] (#'clojure.core/parse-opts opts+specs)
          impls (parse-impls specs)
          impls (mapcat (partial impl-with-env-capture-deftype &env fields) impls)]
      (when-let [bad-opts (seq (remove #{:no-print :load-ns} (keys opts)))]
        (throw (IllegalArgumentException. (apply print-str "Unsupported option(s) -" bad-opts))))
      (apply deftype-o &form &env name fields (concat (apply concat opts) impls)))))

(defn with-redefs-clj [body]
  (with-lock lock
    (let [defn-tmp @#'defn
          defmethod-tmp @#'defmethod
          extend-type-tmp @#'extend-type
          deftype-tmp @#'deftype
          defrecord-tmp @#'defrecord]
      (alter-var-root #'defn omniscient-defn)
      (alter-var-root #'defmethod omniscient-defmethod)
      (alter-var-root #'extend-type omniscient-extend-type)
      (alter-var-root #'deftype omniscient-deftype)
      (alter-var-root #'defrecord omniscient-deftype)
      (try
        ;; we must print the result to avoid it to be evaluated
        (prn (eval `(do ~@body)))
        (finally
          (alter-var-root #'defn (constantly defn-tmp))
          (alter-var-root #'defmethod (constantly defmethod-tmp))
          (alter-var-root #'extend-type (constantly extend-type-tmp))
          (alter-var-root #'deftype (constantly deftype-tmp))
          (alter-var-root #'defrecord (constantly defrecord-tmp)))))))

;; deftype and defrecord expands to extend-type
(defn with-redefs-cljs [env body]
  (with-lock lock
    (let [defn-tmp @cljs-defn
          defmethod-tmp @cljs-defmethod
          extend-type-tmp @cljs-extend-type]
      ;; do not use the var literal syntax because clojurescript may not be loaded
      (alter-var-root (resolve 'cljs.core/defn) omniscient-defn)
      (alter-var-root (resolve 'cljs.core/defmethod) omniscient-defmethod)
      (alter-var-root (resolve 'cljs.core/extend-type) omniscient-extend-type)
      (try
        ;; bindings potentially set by eval-with-env
        (with-bindings {(resolve 'cljs.analyzer/*recur-frames*) nil
                        (resolve 'cljs.analyzer/*loop-lets*) '()}
          ;; evaluate the body, the result is wrapped in a string.
          ;; the result of the macro is then evaluated by the cljs repl
          (println (@cljs-eval-cljs-form (:repl-env env) `(do ~@body))))
        (finally
          (alter-var-root (resolve 'cljs.core/defn) (constantly defn-tmp))
          (alter-var-root (resolve 'cljs.core/defmethod) (constantly defmethod-tmp))
          (alter-var-root (resolve 'cljs.core/extend-type) (constantly extend-type-tmp)))))))

(defmacro with-redefs [& body]
  (if (utils/cljs-env? &env)
    (with-redefs-cljs &env body)
    (with-redefs-clj body)))

(defn locals-reducer [acc local-sym]
  (conj acc (with-meta local-sym  {::local true})
        `(get-in *omniscient-env* [:locals (quote ~local-sym)])))

(defn form-with-locals [form local-syms]
  `(let ~(reduce locals-reducer [] local-syms)
     ~form))

(comment
  (binding [*omniscient-env* {:locals '{x 3 y 2}}]
    (form-with-locals '(+ 1 2)))
  )

(defn bindings-reducer [acc binding-sym]
  (conj acc binding-sym `(get-in *omniscient-env* [:bindings (quote ~binding-sym)])))

(defn form-with-bindings [form binding-syms]
  `(binding ~(reduce bindings-reducer [] binding-syms)
     ~form))

(comment
  (def ^:dynamic *tt* 33)
  
  (form-with-bindings '(+ 1 2) '(nn/*tt*))
  )

(defn eval-with-env [form]
  (if (= (str *ns*) (namespace *omniscient-sym*))
    (-> form
        (form-with-locals *omniscient-local-syms*)
        (form-with-bindings *omniscient-binding-syms*)
        eval)
    (eval form)))

(defn eval-with-env-cljs [repl-env env form opts]
  (if (= (utils/repl-ns :replique/cljs) (symbol (namespace *omniscient-sym*)))
    (let [form (-> form
                   (form-with-locals *omniscient-local-syms*)
                   (form-with-bindings *omniscient-binding-syms*))]
      (@cljs-eval-cljs repl-env env form opts))
    (@cljs-eval-cljs repl-env env form opts)))

(defn repl-read
  "Enhanced :read hook for repl supporting :omniscient/quit."
  [request-prompt request-exit]
  (or ({:line-start request-prompt :stream-end request-exit}
       (clojure.main/skip-whitespace *in*))
      (let [input (read {:read-cond :allow} *in*)]
        (clojure.main/skip-if-eol *in*)
        (case input
          :omniscient/quit request-exit
          input))))

(defn repl-prompt-clj []
  (print "<omniscient> ")
  (clojure.main/repl-prompt))

(defn repl-prompt-cljs []
  (print "<omniscient> ")
  (@cljs-repl-prompt))

(defn repl-quit-prompt-cljs []
  (println "Type :omniscient/quit to quit"))

(defn- repl-clj* [sym {:keys [locals bindings] :as env}]
  (binding [*omniscient-repl?* true
            *omniscient-sym* sym
            *omniscient-env* env
            *omniscient-local-syms* (keys locals)
            *omniscient-binding-syms* (keys bindings)]
    (apply clojure.main/repl (->> {:init (fn [] (in-ns (-> sym namespace symbol)))
                                   :print prn :caught clojure.main/repl-caught
                                   :read repl-read :eval (partial eval-with-env)
                                   :prompt repl-prompt-clj}
                                  replique.repl/options-with-repl-meta
                                  (apply concat)))))

(defn- repl-cljs* [repl-env sym local-keys binding-keys]
  (binding [*omniscient-repl?* true
            *omniscient-sym* sym
            *omniscient-local-syms* local-keys
            *omniscient-binding-syms* binding-keys]
    (let [compiler-env @@cljs-compiler-env
          repl-opts (replique.repl/options-with-repl-meta
                     {:compiler-env compiler-env
                      :init (fn [] (@cljs-in-ns* repl-env (-> sym namespace symbol)))
                      :print println :caught @cljs-repl-caught
                      :read (@cljs-repl-read-with-exit :omniscient/quit)
                      :eval eval-with-env-cljs
                      :prompt repl-prompt-cljs
                      :quit-prompt repl-quit-prompt-cljs})]
      (apply
       (partial @cljs-repl repl-env)
       (->> (merge (:options @compiler-env) repl-opts)
            (apply concat))))))

(defn repl-clj [qualified-sym index]
  (let [{:keys [locals bindings] :as env} (get-in @registry [qualified-sym index])]
    (if *omniscient-repl?*
      (do (set! *omniscient-env* env)
          (set! *omniscient-local-syms* (keys locals))
          (set! *omniscient-binding-syms* (keys bindings))
          (set! *omniscient-sym* qualified-sym)
          (swap! last-selected assoc qualified-sym index)
          (prn qualified-sym)
          (in-ns (-> qualified-sym namespace symbol)))
      (do (println "Type :omniscient/quit to quit")
          (swap! last-selected assoc qualified-sym index)
          (repl-clj* qualified-sym env)))
    nil))

(defn repl-cljs [repl-env qualified-sym index]
  (let [{:keys [local-keys binding-keys]} (read-string
                                           (@cljs-eval-cljs-form
                                            repl-env
                                            `(replique.omniscient-runtime/locals-bindings-keys
                                              (quote ~qualified-sym) ~index)))]
    (@cljs-eval-cljs-form
     repl-env
     `(do (set! *omniscient-env* (get-in @registry [(quote ~qualified-sym) ~index]))
          (swap! last-selected assoc (quote ~qualified-sym) ~index)))
    (if *omniscient-repl?*
      (do (set! *omniscient-local-syms* local-keys)
          (set! *omniscient-binding-syms* binding-keys)
          (set! *omniscient-sym* qualified-sym)
          (@cljs-in-ns* repl-env (-> qualified-sym namespace symbol)))
      (try
        (repl-cljs* repl-env qualified-sym local-keys binding-keys)
        (finally
          (@cljs-eval-cljs-form repl-env `(set! *omniscient-env* nil)))))
    nil))

(defmacro repl [sym index]
  (let [qualified-sym (compute-qualified-sym (get-comp-env &env) (env-ns &env) sym)]
    (if (utils/cljs-env? &env)
      (repl-cljs (:repl-env &env) qualified-sym index)
      `(repl-clj (quote ~qualified-sym) ~index))))

(defmacro get-env [sym index]
  (let [qualified-sym (compute-qualified-sym (get-comp-env &env) (env-ns &env) sym)]
    `(get-in @registry [(quote ~qualified-sym) ~index])))

(defn symbolize-keys [m]
  (for [[k v] m]
    (if (symbol? k)
      [`(quote ~k) v]
      [k v])))

(defmethod tooling-msg/tooling-msg-handle [:replique/clj :omniscient-filter]
  [{sym :symbol ns :ns is-string? :is-string? filter-term :filter-term
    prev-index :prev-index msg-id :msg-id
    :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [the-ns (and ns (find-ns (symbol ns)))
          sym (and sym (symbol sym))
          qualified-sym (compute-qualified-sym nil the-ns sym)
          filter-form (if (= "" (clojure.string/trim filter-term)) "{}" filter-term)
          filter-form (try
                        (read-string filter-form)
                        (catch Exception e nil))
          filter-form (try (when (map? filter-form)
                             (binding [*ns* the-ns]
                               (eval (->> filter-form symbolize-keys (cons `list)))))
                           (catch Exception e nil))]
      (when (and (not is-string?) qualified-sym)
        (assoc (replique.omniscient-runtime/filter-envs qualified-sym prev-index filter-form)
               :msg-id msg-id)))))

(defn omniscient-filter-cljs [repl-env {sym :symbol ns :ns
                                        is-string? :is-string? filter-term :filter-term
                                        prev-index :prev-index msg-id :msg-id
                                        :as msg}]
  (let [comp-env (env/->CljsCompilerEnv @@cljs-compiler-env)
        the-ns (and ns (env/find-ns comp-env (symbol ns)))
        sym (and sym (symbol sym))
        qualified-sym (compute-qualified-sym comp-env the-ns sym)
        filter-form (if (= "" (clojure.string/trim filter-term)) "{}" filter-term)
        filter-form (try
                      (read-string filter-form)
                      (catch Exception e nil))
        filter-form (when (map? filter-form)
                      (->> filter-form symbolize-keys (cons 'cljs.core/list)))]
    (when (and (not is-string?) qualified-sym)
      (when-let [res (try (@cljs-eval-cljs-form
                           repl-env
                           `(try (replique.omniscient-runtime/filter-envs
                                  (quote ~qualified-sym) ~prev-index ~filter-form)
                                 (catch js/Error ~'_ nil))
                           {:ns (env/ns-name the-ns) :warnings nil})
                          (catch clojure.lang.ExceptionInfo e
                            (if (= (.getData e) {:tag :cljs/analysis-error})
                              nil
                              (throw e))))]
        (assoc (read-string res) :msg-id msg-id)))))

(defmethod tooling-msg/tooling-msg-handle [:replique/browser :omniscient-filter] [msg]
  (tooling-msg/with-tooling-response msg
    (omniscient-filter-cljs @@cljs-repl-env msg)))

(defmethod tooling-msg/tooling-msg-handle [:replique/nashorn :omniscient-filter] [msg]
  (tooling-msg/with-tooling-response msg
    (omniscient-filter-cljs @@cljs-repl-env-nashorn msg)))

;; omniscient only captures locals and (not all) dynamic vars. Omniscient only captures dynamic
;; vars even through in theory, all vars can be redefined

;; cljs dynamic vars are only captured if they had already been required at the moment of the
;; compiliation with "with-redefs"

;; when using with-redefs in an omniscient repl, the eval call in with-redefs ignores
;; the locals/dynamic bindings of eval-with-env which means we can safely use "with-redefs"
;; directly inside an omniscient repl


;; print in a read only buffer, with the page number
;; custom edn printer (with (point) positions) + hide-region-hide
;; print values in the clj process
;; save the folded positions
;; filter expression -> match on all values / provide fn to check the type / save the REPL session ID / or to match on multiple args/vals
;; remove global/session mode
