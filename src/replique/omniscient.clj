(ns replique.omniscient
  (:refer-clojure :exclude [time with-redefs])
  (:require [clojure.set]
            [replique.tooling-msg :as tooling-msg])
  (:import [java.time LocalDateTime]
           [java.io Writer]))

(defonce registry (atom {}))
(defonce thread (atom nil))
(defonce time (atom nil))

;; bound when the read-eval-print-loop is started. Its value is merged in the registry
;; at the end of the read-eval-print loop
(defonce ^:dynamic *locals* nil)
;; whether the read-eval-print loop should restore the value of defn after evaluation
(defonce ^:dynamic *restore-redefs?* false)
;; whether we are in a debugging REPL
(defonce ^:dynamic *omniscient-repl?* false)
;; the env (locals) currently beeing debugged.
;; ie the ones that are bound in the omniscient REPL
(defonce ^:dynamic *omniscient-env* nil)
;; temporary stores the value of defn/defmethod while it is redefined by omniscient
(defonce defn-tmp (atom nil))
(defonce defmethod-tmp (atom nil))
(defonce extend-type-tmp (atom nil))
(defonce deftype-tmp (atom nil))

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

(defn append-local [locals new-local]
  (if (nil? locals) [(assoc new-local :index 0)]
      (->> (count locals) (assoc new-local :index) (conj locals))))

(defn capture-locals [qualified-sym method locals]
  (let [locals (mapcat (fn [x] [`(quote ~x) x]) locals)]
    `(when *locals*
       (set! *locals*
             (update-in *locals* [(quote ~qualified-sym) (.getId (Thread/currentThread))]
                        append-local {:thread (Thread/currentThread)
                                      :time (System/nanoTime)
                                      :ns (find-ns (quote ~(symbol (str *ns*))))
                                      :var (var ~qualified-sym)
                                      ~@(when method [:method `(quote ~method)]) ~@[]
                                      ~@locals ~@[]})))))

(defn is-omniscient-local? [x]
  (::local (meta x)))

(comment
  (capture-locals 'replique.omniscient/ee nil #{'a 'b})
  )

(defn fdecl-with-locals-capture [[params & body] capture-locals-expr]
  `(~params ~@(conj body capture-locals-expr)))

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

(defn- sigs-with-locals-capture [sigs sig capture-locals-expr]
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
      true (cons capture-locals-expr)
      conds (cons conds)
      true (cons params))))

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
          env-locals (->> &env keys (remove is-omniscient-local?))
          locals (map #(clojure.set/union % env-locals) locals)
          qualified-sym (symbol (str *ns*) (str name))
          capture-locals-fn (partial capture-locals qualified-sym nil)
          capture-locals-exprs (map capture-locals-fn locals)
          sigs-with-locals-capture (partial sigs-with-locals-capture sigs)
          sigs (map sigs-with-locals-capture sigs capture-locals-exprs)]
      (apply defn-o &form &env name sigs))))

(comment
  (let [e 3]
    (defn ff [ee & rr]
      ee))

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
  )

(defn omniscient-defmethod [defmethod-o]
  (fn [&form &env multifn dispatch-val & fn-tail]
    (let [sigs (sigs-normalized fn-tail)
          params (map first sigs)
          locals (map sig-symbols params)
          env-locals (->> &env keys (remove is-omniscient-local?))
          locals (map #(clojure.set/union % env-locals) locals)
          qualified-sym (symbol (str *ns*) (str multifn))
          capture-locals-fn (partial capture-locals qualified-sym nil)
          capture-locals-exprs (map capture-locals-fn locals)
          sigs-with-locals-capture (partial sigs-with-locals-capture sigs)
          sigs (map sigs-with-locals-capture sigs capture-locals-exprs)]
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

(defn method-expr-with-locals-capture [capture-locals-exprs [params & body]]
  `(~params ~@(cons capture-locals-exprs body)))

(defn method-with-locals-capture-extend-type [&env qualified-sym [method-name & methods]]
  (let [locals (map (comp sig-symbols first) methods)
        env-locals (->> &env keys (remove is-omniscient-local?))
        locals (map #(clojure.set/union % env-locals) locals)
        capture-locals-fn (partial capture-locals qualified-sym method-name)
        capture-locals-exprs (map capture-locals-fn locals)
        methods (map method-expr-with-locals-capture capture-locals-exprs methods)]
    `(~method-name ~@methods)))

(comment
  (method-with-locals-capture-extend-type
   nil 'my-ns/my-protocol '(pp ([this] 2) ([this [e f] h] e)))
  )

(defn impl-with-locals-capture-extend-type [&env [protocol-name & methods]]
  (let [qualified-sym (symbol (str *ns*) (str protocol-name))]
    `(~protocol-name ~@(map (partial method-with-locals-capture-extend-type &env qualified-sym)
                            methods))))

(comment
  (impl-with-locals-capture-extend-type
   nil '(Pp (pp ([this] 2) ([this [e f] h] e)) (pp2 ([this] 3))))
  )

(defn omniscient-extend-type [extend-type-o]
  (fn [&form &env t & specs]
    (let [impls (parse-impls specs)
          impls (mapcat (partial impl-with-locals-capture-extend-type &env) impls)]
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

(defn method-with-locals-capture-deftype [&env qualified-sym [method-name [params & body]]]
  (let [locals (sig-symbols params)
        env-locals (->> &env keys (remove is-omniscient-local?))
        locals (clojure.set/union env-locals locals)
        #__ #_(prn locals)
        _ (prn (keys &env))
        _ (prn (map meta (keys &env)))
        capture-locals-exprs (capture-locals qualified-sym method-name locals)
        method (method-expr-with-locals-capture capture-locals-exprs `(~params ~@body))]
    `(~method-name ~@method)))

(defn impl-with-locals-capture-deftype [&env [protocol-name & methods]]
  (let [qualified-sym (symbol (str *ns*) (str protocol-name))]
    `(~protocol-name ~@(map (partial method-with-locals-capture-deftype &env qualified-sym)
                            methods))))

(comment
  (impl-with-locals-capture-deftype
    nil '(Pp (pp ([this] 2)) (pp ([this [e f] h] e)) (pp2 ([this] 3))))
  )

(defn omniscient-deftype [deftype-o]
  (fn [&form &env name fields & opts+specs]
    (#'clojure.core/validate-fields fields name)
    (let [qualified-sym (symbol (str *ns*) (str name))
          [opts specs] (#'clojure.core/parse-opts opts+specs)
          impls (parse-impls specs)
          impls (mapcat (partial impl-with-locals-capture-deftype &env) impls)]
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

(defmacro with-redefs [& body]
  (assert *locals* "with-redefs must be used in a replique REPL with tooling enabled")
  (reset! defn-tmp @#'defn)
  (reset! defmethod-tmp @#'defmethod)
  (reset! extend-type-tmp @#'extend-type)
  (reset! deftype-tmp @#'deftype)
  (alter-var-root #'defn omniscient-defn)
  (alter-var-root #'defmethod omniscient-defmethod)
  (alter-var-root #'extend-type omniscient-extend-type)
  (alter-var-root #'deftype omniscient-deftype)
  (set! *restore-redefs?* true)
  `(do ~@body))

(defn bindings-reducer [acc [k v]]
  (conj acc (with-meta k  {::local true}) `(get *omniscient-env* (quote ~k))))

(defn form-with-bindings [form]
  `(let ~(reduce bindings-reducer []
                 (dissoc *omniscient-env* :time :thread :ns :var :index :method))
     ~form))

(comment
  (binding [*omniscient-env* '{x 3 y 2}]
    (form-with-bindings '(+ 1 2)))

  (binding [*omniscient-env* (get-env 'replique.omniscient 'Pp 13 0)]
    (form-with-bindings '(+ 1 2)))
  )

(defn eval-with-env [form]
  (if (= *ns* (:ns *omniscient-env*))
    (eval (form-with-bindings form))
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

(defn wrap-repl-read [repl-read]
  (fn omniscient-repl-read [request-prompt request-exit]
    (let [res (repl-read request-prompt request-exit)]
      (set! *locals* {})
      res)))

(defn wrap-print [print]
  (fn omniscient-print [result]
    (swap! registry merge *locals*)
    (set! *locals* nil)
    (when *restore-redefs?*
      (alter-var-root #'defn (constantly @defn-tmp))
      (alter-var-root #'defmethod (constantly @defmethod-tmp))
      (alter-var-root #'extend-type (constantly @extend-type-tmp))
      (alter-var-root #'deftype (constantly @deftype-tmp))
      (reset! defn-tmp nil)
      (reset! defmethod-tmp nil)
      (reset! extend-type-tmp nil)
      (reset! deftype-tmp nil)
      (set! *restore-redefs?* false))
    (print result)))

(defn wrap-caught [repl-caught]
  (fn omniscient-repl-caught [e]
    (swap! registry merge *locals*)
    (set! *locals* nil)
    (when *restore-redefs?*
      (alter-var-root #'defn (constantly @defn-tmp))
      (alter-var-root #'defmethod (constantly @defmethod-tmp))
      (alter-var-root #'extend-type (constantly @extend-type-tmp))
      (alter-var-root #'deftype (constantly @deftype-tmp))
      (reset! defn-tmp nil)
      (reset! defmethod-tmp nil)
      (reset! extend-type-tmp nil)
      (reset! deftype-tmp nil)
      (set! *restore-redefs?* false))
    (repl-caught e)))

(defn repl-prompt []
  (print "<omniscient> ")
  (clojure.main/repl-prompt))

(defn- repl* [env]
  (binding [*omniscient-repl?* true
            *omniscient-env* env]
    (reset! thread (:thread env))
    (reset! time (:time env))
    (clojure.main/repl
     :read (wrap-repl-read repl-read)
     :eval eval-with-env
     :print (wrap-print prn)
     :caught (wrap-caught clojure.main/repl-caught)
     :init (fn [] (in-ns (symbol (str (:ns env)))))
     :prompt repl-prompt)))

(defn repl [env]
  (println (format "Type :omniscient/quit to quit" (:var env)))
  (if *omniscient-repl?*
    (do (set! *omniscient-env* env)
        (reset! thread (:thread env))
        (reset! time (:time env))
        (in-ns (symbol (str (:ns env)))))
    (do (repl* env)
        (println "Omniscient REPL exited")))
  nil)

(defn get-env [ns sym thread index]
  (get-in @registry [(symbol (str ns "/" sym)) thread index]))

(defmacro with-omniscient [& body]
  `(binding [*locals* {}]
     (try 
       ~@body
       (finally
         (swap! registry merge *locals*)))))

(defn match-val? [filter-v env-v]
  (or (= filter-v env-v)
      (when (ifn? filter-v)
        (try (filter-v env-v) (catch Exception e nil)))))

(defn symbolize-keys [m]
  (for [[k v] m]
    (if (symbol? k)
      [`(quote ~k) v]
      [k v])))

(defn parse-date [date-str]
  (LocalDateTime/parse date-str))

(defn match-env? [filter env]
  (if (= "" (clojure.string/trim filter))
    env
    (let [filter-form (try
                        (binding [*data-readers* {'time parse-date}]
                          (read-string filter))
                        (catch Exception e nil))]
      (when (map? filter-form)
        (when-let [filter (try (->> filter-form symbolize-keys (cons `list) eval)
                               (catch Exception e nil))]
          (loop [[[k filter-v] & filter-rest] filter]
            (if k
              (when (contains? env k)
                (let [env-v (get env k)]
                  (when (match-val? filter-v env-v)
                    (recur filter-rest))))
              env)))))))

(comment
  (->> "{r \"e\"}" read-string symbolize-keys (cons `list) eval)
  
  (match-env? "{f 44}" '{e "e" f 44})
  (match-env? "{:f #(number? %)}" {:e "e" :f 44})
  )

(defn index-of [xs pred]
  (when (not (empty xs))
    (loop [a (first xs)
           r (rest xs)
           i 0]
      (cond
        (pred a) i
        (empty? r) nil
        :else (recur (first r) (rest r) (inc i))))))

(defn thread-entry->name-id [[k v]]
  (when (> (count v) 0)
    `(~(.getName (get-in v [0 :thread])) ~k)))

(defmethod tooling-msg/tooling-msg-handle :omniscient-threads
  [{ns :ns sym :symbol is-string? :is-string? :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [ns (and ns (symbol ns))
          sym (and sym (symbol sym))]
      (when (and (not is-string?) ns sym)
        (let [thread-envs (get @registry (symbol (str ns "/" sym)))
              thread-ids (->> thread-envs
                              (map thread-entry->name-id))]
          {:thread-ids thread-ids
           :index (when @thread
                    (index-of
                     thread-ids (fn [[_ id]] (= id (.getId @thread)))))})))))

(defn print-omniscient-map [m print-one w]
  (#'clojure.core/print-sequential
   "{"
   (fn [e ^Writer w]
     (do (print-one (key e) w) (.append w \space)
         (binding [*print-length* 2
                   *print-level* 1]
           (print-one (val e) w))))
   ", "
   "}"
   (seq m) w))

(defn locals->str [locals]
  (let [print-methods (methods clojure.core/print-method)
        print-method-date (get print-methods java.time.LocalDateTime)]
    (defmethod print-method java.time.LocalDateTime [d writer]
      (.write writer (str "#time \"" (str d) "\"")))
    (try (doall
          (for [local locals]
            (with-out-str
              (-> local
                  (dissoc :index :thread :ns :var)
                  (print-omniscient-map #'clojure.core/pr-on *out*)))))
         (finally
           (if print-method-date
             (.addMethod print-method LocalDateTime print-method-date)
             (remove-method print-method LocalDateTime))))))

(defmethod tooling-msg/tooling-msg-handle :omniscient-filter
  [{sym :symbol ns :ns is-string? :is-string?
    thread-id :thread-id filter-term :filter-term
    :as msg}]
  (tooling-msg/with-tooling-response msg
    (with-omniscient
      (let [ns (and ns (symbol ns))
            sym (and sym (symbol sym))]
        (when (and (not is-string?) ns sym)
          (let [locals (get-in @registry [(symbol (str ns "/" sym)) thread-id])
                locals (filter (partial match-env? filter-term) locals)]
            {:locals (locals->str locals)
             :indexes (mapv :index locals)
             :index (when @time (index-of locals #(>= (:time %) @time)))}))))))

(comment
  (get-in @registry ['replique.omniscient/Pp 13])

  ;; [{:ns #object[clojure.lang.Namespace 0x7d894d07 "replique.omniscient"], :time 27384141085334, ee 1, :var #'replique.omniscient/ff, :thread #object[java.lang.Thread 0x42af9014 "Thread[Clojure Connection :replique 2,5,main]"], rr nil, e 3, :index 0}]
  )


;; check if ivy mode is installed
;; remove localDateTime printing / tag reader
;; automatically select the last thread id
