(ns replique.compliment.sources.keywords
  "Completion for keywords interned globally across the application"
  (:refer-clojure :exclude [ns-name ns-aliases find-ns])
  (:require [replique.compliment.sources :refer [defsource]]
            [replique.compliment.utils :refer [defmemoized]]
            [replique.environment :refer [ns-name resolve-namespace ns-aliases
                                            keywords find-ns]]))

(defn- tagged-candidate [c]
  {:candidate c, :type :keyword})

(defn qualified-candidates
  "Returns a list of namespace-qualified double-colon keywords (like ::foo)
  resolved for the given namespace."
  [comp-env prefix ns]
  (let [prefix (subs prefix 2)
        ns-name (str ns)]
    (for [kw (keywords comp-env)
          :when (= (namespace kw) ns-name)
          :when (.startsWith (name kw) prefix)]
      (tagged-candidate (str "::" (name kw))))))

(defn namespace-alias-candidates
  "Returns a list of namespace aliases prefixed by double colon required in the
  given namespace."
  [comp-env prefix ns]
  (let [prefix (subs prefix 2)]
    (for [[alias _] (ns-aliases comp-env ns)
          :let [aname (name alias)]
          :when (.startsWith aname prefix)]
      (tagged-candidate (str "::" aname)))))

(defn aliased-candidates
  "Returns a list of alias-qualified double-colon keywords (like ::str/foo),
  where alias has to be registered in the given namespace."
  [comp-env prefix ns]
  (let [[_ alias prefix] (re-matches #"::([^/]+)/(.*)" prefix)
        resolve-namespace (partial resolve-namespace comp-env)
        alias-ns-name (-> (symbol alias) (resolve-namespace ns) str)]
    (for [kw (keywords comp-env)
          :when (= (namespace kw) alias-ns-name)
          :when (.startsWith (name kw) prefix)]
      (tagged-candidate (str "::" alias "/" (name kw))))))

(defn candidates
  ([^String prefix, ns context]
   (candidates nil prefix ns context))
  ([comp-env ^String prefix, ns _]
   (let [single-colon? (.startsWith prefix ":")
         double-colon? (.startsWith prefix "::")
         has-slash? (> (.indexOf prefix "/") -1)]
     (cond (and double-colon? has-slash?) (aliased-candidates comp-env prefix ns)
           double-colon? (concat (qualified-candidates comp-env prefix ns)
                                 (namespace-alias-candidates comp-env prefix ns))
           single-colon? (for [kw (keywords comp-env)
                               :when (.startsWith (str kw) (subs prefix 1))]
                           (tagged-candidate (str ":" kw)))))))

(defsource ::keywords
  :candidates #'candidates
  :doc (constantly nil))

(comment
  (require '[replique.repl-cljs :refer [compiler-env]])
  (require '[replique.environment :refer [->CljsCompilerEnv]])
  (def comp-env (->CljsCompilerEnv @compiler-env))
  
  (candidates comp-env ":cljs.c" (find-ns comp-env 'replique.compliment.ns-mappings-cljs-test) nil)
  (candidates comp-env "::eee" (find-ns comp-env 'replique.compliment.ns-mappings-cljs-test) nil)
  )
