(ns replique.compliment.context
  "Utilities for parsing and storing the current completion context."
  (:require [replique.compliment.core :refer [ensure-ns]]
            [clojure.walk :refer [walk]]
            [clojure.set]))

(def ^:dynamic *reader-conditionals* #{:cljs :clj})

(defn- restore-map-literals [context]
  (clojure.walk/postwalk (fn [el]
                           (if (and (sequential? el)
                                    (= (first el) 'compliment-hashmap))
                             (apply hash-map
                                    (if (even? (count el))
                                      (concat (rest el) [nil])
                                      (rest el)))
                             el)) context))

(defn- safe-read-context-string [^String context]
  ;; Bind all tag readers to the identity
  (with-redefs [default-data-readers {}]
    (binding [*default-data-reader-fn* (fn [tag val] val)]
      (let [read-fn (partial read-string {:read-cond :preserve})]
        (try (-> context
                 (.replace "{" "(compliment-hashmap ")
                 (.replace "}" ")")
                 read-fn
                 restore-map-literals)
             (catch Exception ex nil))))))

(def ^{:doc "Stores the last completion context."
       :private true}
  previous-context (atom nil))

(def ^{:doc "Special symbol which substitutes prefix in the context,
  so the former can be found unambiguously."}
  prefix-placeholder '__prefix__)

(defn parse-context
  "Takes a context which is a Lisp form and returns a transformed context.

  The result is a list of maps, each map represents a level of the
  context from inside to outside. Map has `:idx` and `:form` values,
  and `:map-role` if the level is a map. `:idx` defines the position
  of prefix (or the form containing prefix) on the current
  level (number for lists and vectors, key or value for maps).

  Example: `(dotimes [i 10] ({:foo {:baz __prefix__}, :bar 42} :quux))`

  Transformed it looks like:

  `({:idx :baz, :map-role :value, :form {:baz __prefix__}}
    {:idx :foo, :map-role :key, :form {:foo {:baz __prefix__}, :bar 42}}
    {:idx 0, :form ({:foo {:baz __prefix__}, :bar 42} :quux)}
    {:idx 2, :form (dotimes [i 10] ({:foo {:baz __prefix__}, :bar 42} :quux))})`."
  [context]
  (let [parse (fn parse [ctx]
                (cond
                  (sequential? ctx)
                  (when-let [res (first (keep-indexed (fn [idx el]
                                                        (when-let [p (parse el)]
                                                          [idx p]))
                                                      ctx))]
                    (cons {:idx (first res) :form ctx} (second res)))

                  (map? ctx)
                  (when-let [res (first (keep (fn [[k v]]
                                                (if-let [p (parse v)]
                                                  [k :value p]
                                                  (when-let [p (parse k)]
                                                    [v :key p])))
                                              ctx))]
                    (cons {:idx (first res) :map-role (second res) :form ctx}
                          (nth res 2)))

                  (string? ctx)
                  (let [idx (.indexOf ^String ctx (name prefix-placeholder))]
                    (when (>= idx 0)
                      [{:idx idx :form ctx}]))

                  (and (reader-conditional? ctx) (even? (count (:form ctx))))
                  (let [conditionals (apply array-map (:form ctx))]
                    (if-let [parsed-clj (-> conditionals :clj parse)]
                      parsed-clj
                      (let [conditional-keys (keys conditionals)]
                        (loop [[k & rest-k] conditional-keys]
                          (cond (not (keyword? k))
                                nil
                                (= :clj k)
                                (recur (rest conditional-keys))
                                :else
                                (if-let [parsed-form (-> conditionals k parse)]
                                  (do (set! *reader-conditionals*
                                            (clojure.set/intersection
                                             *reader-conditionals* (set [k])))
                                      parsed-form)
                                  (recur rest-k)))))))

                  (= ctx prefix-placeholder) ()))]
    (parse context)))

(defn cache-context
  "Parses the context, or returns one from cache if it was unchanged."
  [comp-env ns context-string]
  (let [ns (ensure-ns comp-env ns)
        context (binding [*ns* ns] (safe-read-context-string context-string))]
    (binding [*reader-conditionals* *reader-conditionals*]
      (when-not (= context :same)
        (reset! previous-context (parse-context context)))
      {:context @previous-context :reader-conditionals *reader-conditionals*})))


(comment
  (read-string "#uuid \"2631c308-5d32-4160-8a93-8bde06bdf64c\"")
  (safe-read-context-string "#uuid \"2631c308-5d32-4160-8a93-8bde06bdf64c\"")
  (parse-context (safe-read-context-string "[(__prefix__)]"))
  (parse-context (safe-read-context-string "(__prefix__ \"ee\")"))
  (parse-context (safe-read-context-string "(defn []
  #uuid __prefix__)"))
  (parse-context (safe-read-context-string "[e (__prefix__ \"ee\" [1 2])]"))
  (parse-context (safe-read-context-string "(__prefix__)"))
  (parse-context (safe-read-context-string "#?(:cljs (prn ) :cljg (print __prefix__))"))

  (with-redefs [default-data-readers {}]
    (binding [*default-data-reader-fn* (fn [tag val] val)]
      (read-string "__prefix__")))

  (read-string {:read-cond :preserve :features #{:clj}} "#?(:clj :3 :cljs :2 3 4)")

  (safe-read-context-string "#?(:clj __prefix__)")

  (parse-context '{:e __prefix__ :f 3})
  (parse-context '{__prefix__ nil})

  (safe-read-context-string "[__prefix__]")
  (parse-context '[__prefix__])
  )
