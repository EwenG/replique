(ns replique.spec-tooling
  (:refer-clojure :exclude [+ * and assert or cat def keys merge])
  (:require [clojure.spec :as s]
            [clojure.pprint :refer [pprint pp]]
            [clojure.walk :as walk]))

(alias 'c 'clojure.core)

(declare candidates)
(declare from-spec)
(declare map-spec-impl)
(declare multi-spec-impl)
(declare tuple-impl)
(declare every-impl)
(declare merge-impl)
(declare or-spec-impl)
(declare and-spec-impl)

(defprotocol Complete
  (candidates* [spec context prefix]))

(extend-protocol Complete
  clojure.lang.Var
  (candidates* [v context prefix]
    (candidates* @v context prefix)))

(extend-protocol Complete
  clojure.spec.Spec
  (candidates* [spec context prefix]
    (candidates* (from-spec spec) context prefix)))

(extend-protocol Complete
  clojure.lang.Keyword
  (candidates* [k context prefix]
    (when-let [spec (s/get-spec k)]
      (candidates* (from-spec spec) context prefix))))

(defn set-candidates [s context prefix]
  (when (c/empty? context)
    (->> (filter #(.startsWith (str %) prefix) s)
         (into #{}))))

(extend-protocol Complete
  clojure.lang.IFn
  (candidates* [f context prefix]
    (if (instance? clojure.lang.IPersistentSet f)
      (set-candidates f context prefix)
      f)))

(defn complete? [x]
  (when (instance? replique.spec_tooling.Complete x) x))

(defn from-cat [preds]
  {::s/op ::s/pcat :ps (mapv from-spec preds)})

(defn from-alt [preds]
  {::s/op ::s/alt :ps (mapv from-spec preds)})

(defn from-rep [pred]
  (let [p1 (from-spec pred)]
    {::s/op ::s/rep :p1 p1 :p2 p1}))

(defn from-amp [pred preds]
  {::s/op ::s/amp :p1 (from-spec pred)})

(defn from-keys [keys-seq]
  (-> (apply hash-map keys-seq)
      map-spec-impl))

(defn from-multi [mm]
  (multi-spec-impl (resolve mm)))

(defn from-tuple [preds]
  (tuple-impl (mapv from-spec preds)))

(defn from-every [pred opts]
  (every-impl (from-spec pred) opts))

(defn from-merge [key-specs]
  (merge-impl (mapv from-spec key-specs)))

(defn from-or [key-preds]
  (->> (drop 1 key-preds)
       (take-nth 2)
       (mapv from-spec)
       or-spec-impl))

(defn from-and [preds]
  (and-spec-impl (mapv from-spec preds)))

;; Transforms specs into a spec that can be understand by spec-tooling. Functions are transformed
;; into their original var, in order to let antone override the candidates returned by a var.
;; Specs are removed their :keys, :forms, ... since it is not needed to compute candidates
(defn from-spec [spec]
  (when spec
    (cond
      (s/regex? spec) (case (::s/op spec)
                        ::s/pcat (from-cat (:forms spec))
                        ::s/alt (from-alt (:forms spec))
                        ::s/rep (from-rep (:forms spec))
                        ::s/amp (from-amp (:p1 spec) (:forms spec)))
      (s/spec? spec)
      (let [form (s/form spec)
            [spec-sym & spec-rest] form]
        (cond (= 'clojure.spec/keys spec-sym)
              (from-keys spec-rest)
              (= 'clojure.spec/multi-spec spec-sym)
              (from-multi (first spec-rest))
              (= 'clojure.spec/tuple spec-sym)
              (from-tuple spec-rest)
              (= 'clojure.spec/every spec-sym)
              (from-every (first spec-rest) (drop 1 spec-rest))
              (= 'clojure.spec/merge spec-sym)
              (from-merge spec-rest)
              (= 'clojure.spec/or spec-sym)
              (from-or spec-rest)
              (= 'clojure.spec/and spec-sym)
              (from-and spec-rest)
              :else (eval form)))
      (c/and (symbol? spec) (namespace spec) (var? (resolve spec)))
      (resolve spec)
      (seq? spec)
      (recur (eval spec))
      :else (eval spec))))

(defprotocol Specize
  (specize* [_] [_ form]))

(declare regex-spec-impl)

(defn- reg-resolve [k]
  (if (ident? k)
    (let [reg @@#'s/registry-ref
          spec (get reg k)]
      (when spec
        (if-not (ident? spec)
          (from-spec spec)
          (-> (#'s/deep-resolve reg spec) from-spec))))
    k))

(defn- reg-resolve! [k]
  (if (ident? k)
    (c/or (reg-resolve k)
          (throw (Exception. (str "Unable to resolve spec: " k))))
    k))

(defn- accept-nil? [p]
  (let [{:keys [::s/op ps p1 p2] :as p} (reg-resolve! p)]
    (case op
      nil nil
      ::s/accept true
      ::s/amp (accept-nil? p1)
      ::s/rep (c/or (identical? p1 p2) (accept-nil? p1))
      ::s/pcat (every? accept-nil? ps)
      ::s/alt (c/some accept-nil? ps))))

(defn- dt [pred x]
  (if pred
    (if-let [spec (#'s/the-spec pred)]
      (if (s/valid? spec x)
        {::s/op ::s/accept}
        ::s/invalid)
      (if (ifn? pred)
        (if (pred x) {::s/op ::s/accept} ::s/invalid)
        ::s/invalid))
    {::s/op ::s/accept}))

(defn- accept? [{:keys [::s/op]}]
  (= ::s/accept op))

(defn- pcat* [{[p1 & pr :as ps] :ps ret :ret}]
  ;; (not (every? identity)) - for example, when a predicate was ::s/invalid
  ;;                           (and thus returned nil)
  (when (every? identity ps)
    ;; if accept and not pr, the result is flatten, in any cases, (:ret p1) is bubbled up
    (if (accept? p1)
      (if pr
        {::s/op ::s/pcat :ps pr :ret (:ret p1)}
        {::s/op ::s/accept :ret (:ret p1)})
      {::s/op ::s/pcat :ps ps :ret (:ret p1)})))

(defn ret-union [ps]
  (->> (map :ret ps)
       (apply clojure.set/union)))

(defn- alt* [ps]
  ;; (not (identity ps)) - for example, when a predicate was ::s/invalid
  ;;                       (and thus returned nil)
  (let [[p1 & pr :as ps] (filter identity ps)]
    (when ps
      (let [ret {::s/op ::s/alt :ps ps}]
        (if (nil? pr)
          (if (accept? p1)
            {::s/op ::s/accept :ret (:ret p1)}
            ret)
          (assoc ret :ret (ret-union ps)))))))

(defn- alt2 [p1 p2]
  (if (c/and p1 p2)
    {::s/op ::s/alt :ps [p1 p2] :ret (clojure.set/union (:ret p1) (:ret p2))}
    (c/or p1 p2)))

(defn- rep* [p1 p2]
  (when p1
    (let [r {::s/op ::s/rep :p2 p2}]
      (if (accept? p1)
        (assoc r :p1 p2 :ret (:ret p1))
        (assoc r :p1 p1 :ret (:ret p1))))))

(defn- deriv [p x at-point?]
  (let [{[p0 & pr :as ps] :ps :keys [::s/op p1 p2] :as p} (reg-resolve! p)]
    (when p
      (case op
        nil (if at-point?
              {::s/op ::s/accept :ret #{p}}
              (let [ret (dt p x)]
                (when-not (s/invalid? ret) ret)))
        ;; amp are ignored because it may remove useful candidates results, which also means
        ;; candidates are not totally accurates
        ::s/amp (deriv p1 x at-point?)
        ::s/pcat (alt2
                  (pcat* {:ps (cons (deriv p0 x at-point?) pr)})
                  (when (accept-nil? p0) (deriv (pcat* {:ps pr}) x at-point?)))
        ::s/alt (alt* (map #(deriv % x at-point?) ps))
        ::s/rep (alt2 (rep* (deriv p1 x at-point?) p2)
                      (when (accept-nil? p1)
                        (deriv (rep* p2 p2) x at-point?)))))))

(defn- re-candidates [p [{:keys [idx form]} & cs :as context] prefix deriv-idx]
  (if (> deriv-idx idx)
    (let [candidates-res (->> (:ret p)
                              (map #(candidates* % cs prefix))
                              (filter set?)
                              (apply clojure.set/union))]
      (with-meta candidates-res {::not-contrainable true}))
    ;; dp is nil if a predicate was ::s/invalid, which means that if the beginning of the regex
    ;; is invalid (amp are not checked), we don't try to get candidates at all
    ;; This is probably a valid behavior since collections matched by regex are often typed by
    ;; humans in order, contrary to maps
    (when-let [dp (deriv p (nth form deriv-idx) (= idx deriv-idx))]
      (recur dp context prefix (inc deriv-idx)))))

(defn regex-spec-impl [re]
  (reify
    Specize
    (specize* [s] s)
    Complete
    (candidates* [_ [{:keys [idx form]} & _ :as context] prefix]
      (when (coll? form)
        (re-candidates re context prefix 0)))))

(defn spec-impl [pred]
  (cond
    (s/regex? pred) (regex-spec-impl pred)
    :else
    (reify
      Specize
      (specize* [s] s)
      Complete
      (candidates* [_ context prefix]
        (let [ret (candidates* pred context prefix)]
          (when (set? ret) ret))))))

(defn map-spec-impl [{:keys [req-un opt-un req opt]}]
  (let [req-un->req (zipmap (map (comp keyword name) req-un) req-un)
        opt-un->opt (zipmap (map (comp keyword name) opt-un) opt-un)
        unqualified->qualified (c/merge req-un->req opt-un->opt)]
    (reify
      Specize
      (specize* [s] s)
      Complete
      (candidates* [_ [{:keys [idx map-role form]} & cs :as context] prefix]
        (when (map? form)
          (case map-role
            :value (when-let [spec (s/get-spec (c/or (get unqualified->qualified idx) idx))]
                     (-> (candidates* spec cs prefix)
                         (with-meta {::not-contrainable true})))
            :key (let [candidates-res (->> (concat (c/keys unqualified->qualified) req opt)
                                           (into #{}))]
                   (-> (candidates* candidates-res nil prefix)
                       (with-meta {::not-contrainable true})))))))))

(defn multi-spec-impl [mm]
  (reify
    Specize
    (specize* [s] s)
    Complete
    (candidates* [_ [{:keys [form]} & cs :as context] prefix]
      (let [spec (try (mm form)
                      (catch IllegalArgumentException e nil))
            specs (if spec #{spec} (->> (vals (methods @mm))
                                        (map (fn [spec-fn] (spec-fn nil)) )
                                        (into #{})))]
        (let [candidates-res (->> (map #(candidates* % context prefix) specs)
                                  (filter set?))]
          (if (some #(::not-contrainable (meta %)) candidates-res)
            (-> (apply clojure.set/union candidates-res)
                (with-meta {::not-contrainable true}))
            (apply clojure.set/union candidates-res)))))))

(defn tuple-impl [preds]
  (reify
    Specize
    (specize* [s] s)
    Complete
    (candidates* [_ [{:keys [idx form]} & cs :as context] prefix]
      (when (vector? form)
        (when-let [pred (get preds idx)]
          (-> (candidates* pred cs prefix)
              (with-meta {::not-contrainable true})))))))

(defn map-form->seq-form [{:keys [idx map-role form] :as context-item}]
  (case map-role
    :key {:idx 0 :form ['__prefix__ (get form '__prefix__)]}
    :value {:idx 1 :form [idx (get form idx)]}))

(defn every-impl [pred {:keys [kind] :as opts}]
  (reify
    Specize
    (specize* [s] s)
    Complete
    (candidates* [_ [{:keys [idx form] :as c0} & cs :as context] prefix]
      (let [kind-pred (c/or kind coll?)]
        (when (kind-pred form)
          (let [cs (if (map? form) (cons (map-form->seq-form c0) cs) cs)]
            (-> (candidates* pred cs prefix)
                (with-meta {::not-contrainable true}))))))))

(defn merge-impl [key-specs]
  (reify
    Specize
    (specize* [s] s)
    Complete
    (candidates* [_ context prefix]
      (let [candidates-res (->> (map #(candidates* % context prefix) key-specs)
                                (filter set?)
                                (apply clojure.set/union))]
        (with-meta candidates-res {::not-contrainable true})))))

(defn or-spec-impl [preds]
  (reify
    Specize
    (specize* [s] s)
    Complete
    (candidates* [_ context prefix]
      (let [candidates-res (->> (map #(candidates* % context prefix) preds)
                                (filter set?))]
        (if (some #(::not-contrainable (meta %)) candidates-res)
          (-> (apply clojure.set/union candidates-res)
              (with-meta {::not-contrainable true}))
          (apply clojure.set/union candidates-res))))))

(defn every-valid [preds x]
  (loop [[p & ps] preds]
    (if (nil? p) true
        (let [pred-res (p x)]
          (if (c/and pred-res (not (s/invalid? pred-res)))
            (recur ps)
            false)))))

(defn and-spec-impl [preds]
  (reify
    Specize
    (specize* [s] s)
    Complete
    (candidates* [_ context prefix]
      (let [preds-ands (map #(candidates* % context prefix) preds)
            and-candidates (filter set? preds-ands)
            constrainable? (not (some #(::not-contrainable (meta %)) and-candidates))
            and-candidates (apply clojure.set/intersection and-candidates)]
        (if constrainable?
          (let [ands (filter #(c/and (not (set? %)) (ifn? %)) preds-ands)]
            (->> (filter (partial every-valid ands) and-candidates)
                 (into #{})))
          and-candidates)))))

(extend-protocol Specize
  clojure.lang.Keyword
  (specize* [k] (specize* (reg-resolve! k)))

  clojure.lang.Symbol
  (specize* [s] (specize* (reg-resolve! s)))

  clojure.lang.IFn
  (specize* [ifn] (spec-impl (from-spec ifn)))

  clojure.spec.Spec
  (specize* [spec] (from-spec spec))
  )

(defn candidates [spec context prefix]
  (when (satisfies? Specize spec)
    (let [candidates-result (candidates* (specize* spec) context prefix)]
      (when (set? candidates-result)
        candidates-result))))





(comment
  (candidates "" '({:idx nil, :map-role :key, :form {__prefix__ nil}}) "eee")
  )

(comment

  ;; Regexps
  (s/def ::rr string?)
  (s/def ::ss #{11111 222222})
  
  (candidates (s/cat :a (s/cat :b #{22} :c #{1111 2})
                     :d (s/cat :e #{33} :f #{44}))
              '({:idx 2 :form [22 2 __prefix__]})
              "3")

  (candidates (s/cat :a (s/alt :b (s/cat :c #{1 2} :d #{3 4}) :e #{5 6})
                     :f (s/* #{1111})
                     :g (s/& #{2222} string?)
                     :h #{11})
              '({:idx 3, :form [6 1111 1111 __prefix__]})
              "22")
  (candidates (s/cat :a (s/alt :b (s/cat :c #{1 2} :d #{3 4}) :e #{5 6})
                     :f (s/* #{1111})
                     :g (s/& #{2222} string?)
                     :h #{11})
              '({:idx 3, :form [6 1111 1111 __prefix__]})
              "22")

  ;; Does not work !! (s/form (s/keys* :req [::ss])) does not return the right thing !
  (candidates (s/keys* :req [::ss])
              '({:idx 1, :form [::ss __prefix__]})
              "11")



  ;; keys
  
  (candidates (s/keys :req [::ss])
              '({:idx ::ss :map-role :value :form {::ss __prefix__}})
              "11")

  (candidates (s/keys :req [::ss])
              '({:idx nil :map-role :key :form {__prefix__ nil}})
              ":replique")

  (candidates (s/keys :req-un [::ss])
              '({:idx nil :map-role :key :form {__prefix__ nil}})
              ":s")



  ;; multi spec

  (defmulti test-mm :mm)
  (defmethod test-mm 33 [_]
    (s/keys :req-un [::ss]))
  (candidates (s/multi-spec test-mm :mm)
              '({:idx nil :map-role :key :form {:ss 33
                                                __prefix__ nil}})
              ":s")
  (candidates (s/multi-spec test-mm :ss)
              '({:idx :ss :map-role :value :form {:mm 33
                                                  :ss __prefix__}})
              "11")

  ;; tuple

  (candidates (s/tuple #{1111 22} #{3333})
              '({:idx 1 :form [nil __prefix__]})
              "33")

  ;; every
  
  (candidates (s/every #{33333})
              '({:idx 1 :form [nil __prefix__]})
              "33")
  (candidates (s/every-kv #{1111 2222} #{3333 4444})
              '({:idx 1111 :map-role :value :form {:mm 33
                                                   1111 __prefix__}})
              "333")
  (candidates (s/every-kv #{1111 2222} #{3333 4444})
              '({:idx 1, :form [[:mm 33] [1111 __prefix__]]}
                {:idx 1, :form [1111 __prefix__]})
              "333")

  ;; merge

  (candidates (s/merge (s/keys :req [::ss]) (s/keys :req [::ee]))
              '({:idx ::ss :map-role :value :form {::ee 3 ::ss __prefix__}})
              "111")

  ;; or
  (candidates (s/or :a #{1111 2222} :b #{333 44 11112})
              '()
              "111")

  ;;and
  (candidates (s/and #{1111 2222} #{333 44 1111} #_string?)
              '()
              "1111")

  (candidates (s/and (s/every #{33333}) string?)
              '({:idx 1 :form [nil __prefix__]})
              "33")
  

  )

(s/form (s/double-in :min 0 :max 3))
(s/form (s/conformer string?))
(s/form (s/spec string?))
(s/form (s/nilable string?))
(s/form (s/int-in 0 3))

(s/conform (s/conformer (fn [x] nil)) "e")
