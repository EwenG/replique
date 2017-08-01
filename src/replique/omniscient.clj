(ns replique.omniscient
  (:refer-clojure :exclude [time with-redefs])
  (:require [clojure.set]
            [clojure.pprint]
            [replique.tooling-msg :as tooling-msg]
            [replique.utils :as utils]
            [replique.repl-cljs :as repl-cljs]
            [replique.omniscient-runtime :refer
             [registry *omniscient-env* append-env time get-env]]
            [replique.environment :as env])
  (:import [java.time LocalDateTime]
           [java.io Writer]
           [java.util.concurrent LinkedTransferQueue]
           [java.util.concurrent.locks ReentrantLock]))

(def ^:private cljs-defn (utils/dynaload 'cljs.core/defn))
(def ^:private cljs-eval-cljs-form (utils/dynaload 'replique.repl-cljs/eval-cljs-form))
(def ^:private cljs-compiler-env (utils/dynaload 'replique.repl-cljs/compiler-env))

(comment
  (env/->CljsCompilerEnv @cljs-compiler-env)
  )

;; whether we are in a debugging REPL
(defonce ^:dynamic *omniscient-repl?* false)
;; the namespace where the var currently beeing debugged is defined 
(defonce ^:dynamic *omniscient-sym* nil)

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

(defn var->sym [v]
  (let [{:keys [ns name]} (meta v)]
    (when (and ns name)
      (symbol (str ns) (str name)))))

;;clojure.core/*data-readers* clojure.core/*default-data-reader-fn*

;; Prevent dynamic vars used in by the REPL/system from beeing captured
(def excluded-dyn-vars #{'clojure.core/*3 'clojure.core/*print-meta* 'clojure.core/*print-namespace-maps* 'clojure.core/*file* 'clojure.core/*command-line-args* 'clojure.core/*2 'clojure.core/*err* 'clojure.core/*print-length* 'clojure.core/*math-context* 'clojure.core/*e 'clojure.core/*1 'clojure.core/*source-path* 'clojure.core/*unchecked-math* 'clojure.spec/*explain-out* 'clojure.core/*in* 'clojure.core/*print-level* 'replique.server/*session* 'clojure.core/*warn-on-reflection* 'clojure.core/*out* 'clojure.core/*assert* 'clojure.core/*read-eval* 'clojure.core/*ns* 'clojure.core/*compile-path* 'clojure.core.server/*session* 'replique.omniscient/*omniscient-repl?* *omniscient-sym* 'replique.omniscient-runtime/*omniscient-env*})

(defn capture-env-clj [qualified-sym method locals]
  ;; exclude dynamic vars that are thread bound when calling "with-redefs". ie those that are
  ;; needed by the REPL/system
  (let [locals (mapcat (fn [x] [`(quote ~x) x]) locals)]
    `(swap! registry update (quote ~qualified-sym)
            append-env {:thread (Thread/currentThread)
                        :time (System/nanoTime)
                        :ns (find-ns (quote ~(symbol (str *ns*))))
                        :var (var ~qualified-sym)
                        ~@(when method [:method `(quote ~method)]) ~@[]
                        :locals {~@locals ~@[]}
                        :bindings (->> (get-thread-bindings)
                                       (map (fn [[k# v#]] (when-let [sym# (var->sym k#)]
                                                            (when (not
                                                                   (contains?
                                                                    excluded-dyn-vars sym#))
                                                              [sym# v#]))))
                                       (into {}))})))

(defn capture-env-cljs [qualified-sym method locals]
  (let [locals (mapcat (fn [x] [`(quote ~x) x]) locals)]
    `(swap! replique.cljs-env.omniscient/registry update (quote ~qualified-sym)
            replique.cljs-env.omniscient/append-env
            {:ns (find-ns (quote ~(symbol (str *ns*))))
             :var (var ~qualified-sym)
             ~@(when method [:method `(quote ~method)]) ~@[]
             :locals {~@locals ~@[]}})))

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

(defn compute-qualified-sym [sym]
  (let [ns-sym (symbol (str (namespace sym)))
        ns (get (ns-aliases *ns*) ns-sym)
        ns (or ns (find-ns ns-sym))]
    (if ns
      (symbol (str ns) (name sym))
      (symbol (str *ns*) (name sym)))))

(defn env-locals-clj [&env]
  (->> &env keys (remove is-omniscient-local?)))

(defn env-locals-cljs [&env]
  #_(doseq [x (->> &env :locals keys)]
    (.println System/out (meta x)))
  (->> &env :locals keys))

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
          env-locals (if (utils/cljs-env? &env)
                       (env-locals-cljs &env)
                       (env-locals-clj &env))
          locals (map #(clojure.set/union % env-locals) locals)
          qualified-sym (compute-qualified-sym name)
          capture-env (if (utils/cljs-env? &env)
                        capture-env-cljs
                        capture-env-clj)
          capture-env-fn (partial capture-env qualified-sym nil)
          capture-env-exprs (map capture-env-fn locals)
          sigs-with-env-capture (partial sigs-with-env-capture sigs)
          sigs (map sigs-with-env-capture sigs capture-env-exprs)]
      (apply defn-o &form &env name sigs))))

(comment
  (let [e 3]
    (defn ff [ee & rr]
      ee))

  (binding [*print-level* 5]
    (ff 7 8))

  (def ^:dynamic *tt* 33)
  (binding [*tt* 5]
    (ff 7 8))

  (let [e 3]
    (defn ff [ee & rr]
      {:pre [(number? ee)]}
      ee))

  (let [e 3]
    (defn ff ^{:pre [(number? ee)]} [ee & rr]
      ee))

  (let [e 3]
    (defn ff
      ([ee] ee)
      ([aa & gg] gg)))

  (do (.start (Thread. (fn [] (with-omniscient (ff 1 2)))))
      (.start (Thread. (fn [] (with-omniscient (ff 3 4))))))
  )

(defn omniscient-defmethod [defmethod-o]
  (fn [&form &env multifn dispatch-val & fn-tail]
    (let [sigs (sigs-normalized fn-tail)
          params (map first sigs)
          locals (map sig-symbols params)
          env-locals (if (utils/cljs-env? &env)
                       (env-locals-cljs &env)
                       (env-locals-clj &env))
          locals (map #(clojure.set/union % env-locals) locals)
          qualified-sym (compute-qualified-sym multifn)
          capture-env (if (utils/cljs-env? &env)
                        capture-env-cljs
                        capture-env-clj)
          capture-env-fn (partial capture-env qualified-sym nil)
          capture-env-exprs (map capture-env-fn locals)
          sigs-with-env-capture (partial sigs-with-env-capture sigs)
          sigs (map sigs-with-env-capture sigs capture-env-exprs)]
      (apply defmethod-o &form &env multifn dispatch-val sigs))))

(comment
  (defmulti rrr (fn [g] g))

  (defmethod rrr :e [x] x)

  (defmethod rrr :f [f] f)
  )

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

(defn method-with-env-capture-extend-type [&env qualified-sym [method-name & methods]]
  (let [locals (map (comp sig-symbols first) methods)
        env-locals (if (utils/cljs-env? &env)
                     (env-locals-cljs &env)
                     (env-locals-clj &env))
        locals (map #(clojure.set/union % env-locals) locals)
        capture-env (if (utils/cljs-env? &env)
                      capture-env-cljs
                      capture-env-clj)
        capture-env-fn (partial capture-env qualified-sym method-name)
        capture-env-exprs (map capture-env-fn locals)
        methods (map method-expr-with-env-capture capture-env-exprs methods)]
    `(~method-name ~@methods)))

(comment
  (method-with-env-capture-extend-type
   nil 'my-ns/my-protocol '(pp ([this] 2) ([this [e f] h] e)))
  )

(defn impl-with-env-capture-extend-type [&env [protocol-name & methods]]
  (let [qualified-sym (compute-qualified-sym protocol-name)]
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

(comment
  (defprotocol Pp
    (pp [this] [this [e f]])
    (pp2 [this]))

  (defprotocol Ppp
    (ppp [this]))

  (deftype Tt [])

  #_(extend-type Tt
      Pp
      (pp ([this] 2) ([this [e f]] e))
      (pp2 [this] 3))

  (extend-type Tt
    Pp
    (pp ([this] 2) ([this [e f]] e))
    (pp2 [this] 3)
    Ppp
    (ppp [this] 4))

  (extend-protocol Pp
    Tt
    (pp ([this] "a") ([this [e f]] e))
    (pp2 [this] "c"))

  (comment
    (pp (Tt.))
    (pp (Tt.) [1 2])
    )
  )

(defn method-with-env-capture-deftype [&env qualified-sym [method-name [params & body]]]
  (let [locals (sig-symbols params)
        env-locals (if (utils/cljs-env? &env)
                     (env-locals-cljs &env)
                     (env-locals-clj &env))
        locals (clojure.set/union env-locals locals)
        capture-env (if (utils/cljs-env? &env)
                      capture-env-cljs
                      capture-env-clj)
        capture-env-exprs (capture-env qualified-sym method-name locals)
        method (method-expr-with-env-capture capture-env-exprs `(~params ~@body))]
    `(~method-name ~@method)))

(defn impl-with-env-capture-deftype [&env [protocol-name & methods]]
  (let [qualified-sym (compute-qualified-sym protocol-name)]
    `(~protocol-name ~@(map (partial method-with-env-capture-deftype &env qualified-sym)
                            methods))))

(comment
  (impl-with-env-capture-deftype
    nil '(Pp (pp ([this] 2)) (pp ([this [e f] h] e)) (pp2 ([this] 3))))
  )

(defn omniscient-deftype [deftype-o]
  (fn [&form &env name fields & opts+specs]
    (#'clojure.core/validate-fields fields name)
    (let [qualified-sym (compute-qualified-sym name)
          [opts specs] (#'clojure.core/parse-opts opts+specs)
          impls (parse-impls specs)
          impls (mapcat (partial impl-with-env-capture-deftype &env) impls)]
      (when-let [bad-opts (seq (remove #{:no-print :load-ns} (keys opts)))]
        (throw (IllegalArgumentException. (apply print-str "Unsupported option(s) -" bad-opts))))
      (apply deftype-o &form &env name fields (concat (apply concat opts) impls)))))

(comment
  (deftype Tt [a b] :load-ns true
    Pp
    (pp [this] a)
    (pp [this [e f]] e)
    (pp2 [this] 3)
    Ppp
    (ppp [this] 4))

  (comment
    (pp (Tt. "a" "b"))
    (pp (Tt. "a" "b") [1 2])
    )
  )

(defn with-redefs-clj [body]
  (with-lock lock
    (let [defn-tmp @#'defn
          defmethod-tmp @#'defmethod
          extend-type-tmp @#'extend-type
          deftype-tmp @#'deftype]
      (alter-var-root #'defn omniscient-defn)
      (alter-var-root #'defmethod omniscient-defmethod)
      (alter-var-root #'extend-type omniscient-extend-type)
      (alter-var-root #'deftype omniscient-deftype)
      (try
        (eval `(do ~@body))
        (finally
          (alter-var-root #'defn (constantly defn-tmp))
          (alter-var-root #'defmethod (constantly defmethod-tmp))
          (alter-var-root #'extend-type (constantly extend-type-tmp))
          (alter-var-root #'deftype (constantly deftype-tmp)))))))

(defn with-redefs-cljs [body]
  (with-lock lock
    (let [defn-tmp @cljs-defn
          defmethod-tmp @#'defmethod
          extend-type-tmp @#'extend-type
          deftype-tmp @#'deftype]
      (alter-var-root (resolve 'cljs.core/defn) omniscient-defn)
      (alter-var-root #'defmethod omniscient-defmethod)
      (alter-var-root #'extend-type omniscient-extend-type)
      (alter-var-root #'deftype omniscient-deftype)
      (try
        ;; evaluate the body, the result is wrapped in a string.
        ;; the result of the macro is then evaluated by the cljs repl
        (println (@cljs-eval-cljs-form `(do ~@body)))
        (finally
          (alter-var-root (resolve 'cljs.core/defn) (constantly defn-tmp))
          (alter-var-root #'defmethod (constantly defmethod-tmp))
          (alter-var-root #'extend-type (constantly extend-type-tmp))
          (alter-var-root #'deftype (constantly deftype-tmp)))))))

(defmacro with-redefs [& body]
  (if (utils/cljs-env? &env)
    (with-redefs-cljs body)
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

#_(defn eval-with-env [bindings form]
    (if (= *ns* (:ns *omniscient-env*))
      (with-bindings bindings
        (eval (form-with-locals form)))
      (eval form)))

(defn eval-with-env [local-syms binding-syms form]
  (if (= (str *ns*) (namespace *omniscient-sym*))
    (-> form (form-with-locals local-syms) (form-with-bindings binding-syms) eval)
    (eval form)))

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

(defn repl-prompt []
  (print "<omniscient> ")
  (clojure.main/repl-prompt))

(defn map-keys [f m]
  (into {} (for [[k v] m] 
             [(f k) v])))

(defn- repl-clj [sym {:keys [locals bindings] :as env}]
  (let [local-syms (keys locals)
        binding-syms (keys bindings)]
    (binding [*omniscient-repl?* true
              *omniscient-sym* sym
              *omniscient-env* env]
      (reset! time (:time env))
      (clojure.main/repl
       :read repl-read
       :eval (partial eval-with-env local-syms binding-syms)
       :init (fn [] (in-ns (symbol (str (:ns env)))))
       :prompt repl-prompt))))

(defn repl [sym index]
  (let [env (get-env sym index)]
    (println (format "Type :omniscient/quit to quit" (:var env)))
    (if *omniscient-repl?*
      (do (set! *omniscient-env* env)
          (reset! time (:time env))
          (in-ns (symbol (str (:ns env)))))
      (do (repl-clj sym env)
          (println "Omniscient REPL exited")))
    nil))

(defn clear []
  (reset! registry {}))

(defn match-val? [filter-v env-v]
  (or (= filter-v env-v)
      (when (ifn? filter-v)
        (try (filter-v env-v) (catch Exception e nil)))))

(defn symbolize-keys [m]
  (for [[k v] m]
    (if (symbol? k)
      [`(quote ~k) v]
      [k v])))

(defn match-env? [filter env]
  (if (= "" (clojure.string/trim filter))
    env
    (let [filter-form (try
                        (read-string filter)
                        (catch Exception e nil))]
      (when (map? filter-form)
        (when-let [filter (try (->> filter-form symbolize-keys (cons `list) eval)
                               (catch Exception e nil))]
          (loop [[[k filter-v] & filter-rest] filter]
            (if k
              (let [placeholder (make-array Integer 0)
                    env-v (get env k (get (:locals env) k (get (:bindings env) k placeholder)))]
                (when (not (identical? env-v placeholder))
                  (let [env-v (if (= :thread k) (.getName env-v) env-v)]
                    (when (match-val? filter-v env-v)
                      (recur filter-rest)))))
              env)))))))

(comment
  (->> "{r \"e\"}" read-string symbolize-keys (cons `list) eval)
  
  (match-env? "{f 44}" '{e "e" f 44})
  (match-env? "{:f #(number? %)}" {:e "e" :f 44})
  )

(defn index-of [xs pred]
  (when (not (empty? xs))
    (loop [a (first xs)
           r (rest xs)
           i 0]
      (cond
        (pred a) i
        (empty? r) nil
        :else (recur (first r) (rest r) (inc i))))))

;; Some dynamic var, eg clojure.core/*data-readers*, may always be bound, only display them after
;; other dynamic var
(def low-priority-namespaces #{"clojure.core" "clojure.spec" "clojure.core.server"
                               "replique.server"
                               "replique.omniscient" "replique.omniscient-runtime"})

(defn print-omniscient-map [m print-one w]
  (let [locals (dissoc (:locals m) '&env '&form)
        locals (take 3 locals)
        user-binding (->> (:bindings m)
                          (filter #(let [ns (namespace (key %))]
                                     (not (contains? low-priority-namespaces ns))))
                          first)
        system-binding (first (:bindings m))
        has-more? (or (> (count locals) 3)
                      (> (count (:bindings m)) 1))]
    (.write w "{")
    (doseq [local locals]
      (print-one (key local) w) (.append w \space)
      (binding [*print-length* 2
                *print-level* 1]
        (print-one (val local) w))
      (.write w ", "))
    (when user-binding
      (print-one (key user-binding) w) (.append w \space)
      (binding [*print-length* 2
                *print-level* 1]
        (print-one (val user-binding) w))
      (.write w ", "))
    (when (and (nil? user-binding) system-binding)
      (print-one (key system-binding) w) (.append w \space)
      (binding [*print-length* 2
                *print-level* 1]
        (print-one (val system-binding) w))
      (.write w ", "))
    (do (print-one :thread w) (.append w \space)
        (print-one (.getName (:thread m)) w))
    (.write w ", ")
    (do (print-one :time w) (.append w \space)
        (print-one (:time m) w))
    (when has-more?
      (.write w ", ..."))
    (.write w "}")))

(defn envs->str [envs]
  (for [{:keys [locals] :as env} envs]
    (-> (print-omniscient-map env #'clojure.core/pr-on *out*)
        with-out-str
        ;; remove line breaks (for example, when printing #error {...})
        (.replace "\n" ""))))

(defn filter-last-index [envs]
  (max (dec (count envs)) 0))

(defn filter-index-time [envs]
  (when @time
    (let [next-index (index-of envs #(>= (:time %) @time))
          next-time (:time (get envs next-index))]
      (when next-time
        (if (= next-time @time)
          next-index
          (let [prev-index (dec next-index)
                prev-time (:time (get envs next-index))]
            (if prev-time
              (let [diff-prev (Math/abs (- @time prev-time))
                    diff-next (Math/abs (- @time next-time))]
                (if (< diff-prev diff-next)
                  prev-index
                  next-index))
              next-index)))))))

(defn filter-index-prev [envs prev-index]
  (index-of envs #(>= (:index %) prev-index)))

(defmethod tooling-msg/tooling-msg-handle :omniscient-filter
  [{sym :symbol ns :ns is-string? :is-string? filter-term :filter-term
    prev-index :prev-index msg-id :msg-id
    :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [ns (and ns (symbol ns))
          sym (and sym (symbol sym))]
      (when (and (not is-string?) ns sym)
        (let [envs (get @registry (symbol (str ns "/" sym)))
              envs (into [] (filter (partial match-env? filter-term)) envs)]
          {:msg-id msg-id
           :locals (envs->str envs)
           :indexes (mapv :index envs)
           :index (-> (if prev-index
                        (filter-index-prev envs prev-index)
                        (filter-index-time envs))
                      (or (filter-last-index envs)))})))))

;; cljs support
;; cljs repl-caught compiler exception printing
;; cljs repl-caught js error printing

;; omniscient only captures locals and (not all) dynamic vars. Omniscient does not capture
;; all vars even though they can all be redefined
