(ns replique.omniscient-runtime
  (:refer-clojure :exclude [time])
  (:require [clojure.string]
            #?(:cljs [cljs.pprint])))

(defonce registry (atom {}))

;; the env currently beeing debugged.
;; ie the one that is bound in the omniscient REPL
(defonce ^:dynamic *omniscient-env* nil)
(defonce last-selected (atom {}))

(defn append-env [envs new-env]
  (if (nil? envs) [(assoc new-env :index 0)]
      (->> (count envs) (assoc new-env :index) (conj envs))))

(defn clear []
  (reset! registry {})
  (reset! last-selected {}))

(defn match-val? [filter-v env-v]
  (or (= filter-v env-v)
      (when (ifn? filter-v)
        (try (filter-v env-v) #?(:clj (catch Exception e nil)
                                 :cljs (catch js/Error e nil))))))

(defn symbolize-keys [m]
  (for [[k v] m]
    (if (symbol? k)
      [`(quote ~k) v]
      [k v])))

(defn var->sym [v]
  (let [{:keys [ns name]} (meta v)]
    (when (and ns name)
      (symbol (str ns) (str name)))))

(defn match-env? [filters env]
  (when (list? filters)
    (loop [[[k filter-v] & filter-rest] filters]
      (let [var-sym (when (var? k) (var->sym k))
            k (or var-sym k)]
        (if k
          (let [placeholder (make-array Integer 0)
                env-v (get env k (get (:locals env) k
                                      (get (:bindings env) k
                                           placeholder)))]
            (when (not (identical? env-v placeholder))
              (let [env-v (if (= :thread k) (.getName env-v) env-v)]
                (when (match-val? filter-v env-v)
                  (recur filter-rest)))))
          env)))))

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

(defn filter-last-index [envs]
  (max (dec (count envs)) 0))

(defn filter-index-prev [envs prev-index]
  (index-of envs #(>= (:index %) prev-index)))

;; Some dynamic var, eg clojure.core/*data-readers*, may always be bound, only display them after
;; other dynamic var
(def low-priority-namespaces #{"clojure.core"})

(defn print-omniscient-map [m print-one]
  (let [locals (dissoc (:locals m) '&env '&form)
        locals (take 3 locals)
        user-binding (->> (:bindings m)
                          (filter #(let [ns (namespace (key %))]
                                     (not (contains? low-priority-namespaces ns))))
                          first)
        system-binding (first (:bindings m))
        has-more? (or (> (count locals) 3)
                      (> (count (:bindings m)) 1))]
    (print "{")
    (doseq [local locals]
      (print-one (key local)) (print " ")
      (binding [*print-length* 2
                *print-level* 1]
        (print-one (val local)))
      (print ", "))
    (when user-binding
      (print "#'")
      (print-one (key user-binding)) (print " ")
      (binding [*print-length* 2
                *print-level* 1]
        (print-one (val user-binding) ))
      (print ", "))
    (when (and (nil? user-binding) system-binding)
      (print-one (key system-binding)) (print " ")
      (binding [*print-length* 2
                *print-level* 1]
        (print-one (val system-binding)))
      (print ", "))
    (when-let [t (:thread m)]
      (print-one :thread) (print " ")
      (print-one (.getName t))
      (print ", "))
    (do (print-one :time) (print " ")
        (print-one (str (:time m))))
    (when has-more?
      (print ", ..."))
    (print "}")))

(defn envs->str [envs]
  (for [{:keys [locals] :as env} envs]
    (-> (print-omniscient-map env pr)
        with-out-str
        ;; remove line breaks (for example, when printing #error {...})
        (clojure.string/replace "\n" ""))))

(comment
  (let [w (StringBufferWriter. (js/goog.string.StringBuffer.))]
    (pr-writer [1 2] w (pr-opts))
    (str w))

  (with-out-str (pr :locals) (pr {'ee 3}))
  
  (filter-envs 'replique.omniscient-runtime/ff nil '())
  )

(defn filter-envs [qualified-sym prev-index filter-term]
  (let [envs (get @registry qualified-sym)
        envs (into [] (filter (partial match-env? filter-term)) envs)]
    {:locals (envs->str envs)
     :indexes (mapv :index envs)
     :index (-> (if prev-index
                  (filter-index-prev envs prev-index)
                  (get @last-selected qualified-sym))
                (or (filter-last-index envs)))}))

(defn locals-bindings-keys [qualified-sym index]
  (let [env (get-in @registry [qualified-sym index])]
    {:local-keys (-> env :locals keys)
     :binding-keys (-> env :bindings keys)}))

;; defn
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

;; defmethod

(comment
  (defmulti rrr (fn [g] g))

  (defmethod rrr :e [x] x)

  (defmethod rrr :f [f] f)
  )

;; extend-type
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

  (pp (Tt.))
  (pp (Tt.) [1 2])
  (pp2 (Tt.))

  (in-ns 'replique.omniscient)
  (require '[replique.omniscient-runtime :as omni])

  (extend-type replique.omniscient_runtime.Tt
    omni/Pp
    (pp ([this] 2) ([this [e f]] e))
    (pp2 [this] 3))

  (omni/pp (replique.omniscient_runtime.Tt.))
  (omni/pp (replique.omniscient_runtime.Tt.) [1 2])
  
  )

;;deftype
(comment
  (deftype Tt2 [a b] #?@(:clj [:load-ns true])
    Pp
    (pp [this] a)
    (pp [this [e f]] e)
    (pp2 [this] 3)
    Ppp
    (ppp [this] 4))
  

  (comment
    (pp (Tt2. "a" "b"))
    (pp (Tt2. "a" "b") [1 2])
    )
  )

;;defrecord
(comment
  (defrecord Tt2 [a b] #?@(:clj [:load-ns true])
    Pp
    (pp [this] a)
    (pp [this [e f]] e)
    (pp2 [this] 3)
    Ppp
    (ppp [this] 4))
  

  (comment
    (pp (Tt2. "a" "b"))
    (pp (Tt2. "a" "b") [1 2])
    )
  )

