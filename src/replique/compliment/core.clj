;; ## Compliment - a completion library you deserve.
;; This library provides a fast and extensible way to complete symbols in your
;; editor. It is intended to be maximally editor-agnostic where
;; possible, to avoid duplicating implementation in different clients.

(ns replique.compliment.core
  "Core namespace. Most interactions with Compliment should happen
  through functions defined here."
  (:refer-clojure :exclude [find-ns])
  (:require (replique.compliment.sources ns-mappings
                                namespaces-and-classes
                                class-members
                                keywords
                                special-forms
                                local-bindings
                                resources)
            [replique.compliment.sources :refer [all-sources]]
            [replique.compliment.utils :refer [*extra-metadata*]]
            [replique.environment :refer [find-ns default-ns ns-var]]
            [clojure.string :refer [join]])
  (:import java.util.Comparator))

(def ^:private by-length-comparator
  (reify Comparator
    (compare [_ s1 s2]
      (let [res (compare (count s1) (count s2))]
        (if (zero? res)
          (compare s1 s2)
          res)))))

(defn sort-by-length
  "Sorts list of strings by their length first, and then alphabetically if
  length is equal. Works for tagged and non-tagged results."
  [candidates]
  (sort-by :candidate by-length-comparator candidates))

(defn ensure-ns
  "Takes either a namespace object or a symbol and returns the corresponding
  namespace if it exists, otherwise returns `user` namespace."
  [comp-env ns]
  (cond (instance? clojure.lang.Namespace ns) ns
        (symbol? ns) (or (find-ns comp-env ns)
                         (find-ns comp-env (default-ns comp-env))
                         (ns-var comp-env))
        :else (ns-var comp-env)))

(defn completions
  "Returns a list of completions for the given prefix. Options map can contain
  the following options:
  - :ns - namespace where completion is initiated;
  - :context - code form around the prefix;
  - :sort-order (either :by-length or :by-name);
  - :plain-candidates - if true, returns plain strings instead of maps;
  - :extra-metadata - set of extra fields to add to the maps;
  - :sources - list of source keywords to use;
  - :comp-env - the compilation environment to be used - nil for Clojure, the
  Clojurescript compilation environment for Clojurescript ..."
  ([prefix]
   (completions prefix {}))
  ([prefix options-map]
   (if (string? options-map)
     (completions prefix {:context options-map})
     (let [{:keys [ns context sort-order sources extra-metadata comp-env]
            :or {sort-order :by-length}} options-map
           ns (ensure-ns comp-env ns)
           options-map (assoc options-map :ns ns)
           sort-fn (if (= sort-order :by-name)
                     (partial sort-by :candidate)
                     (partial sort-by-length true))]
       (binding [*extra-metadata* extra-metadata]
         (let [candidate-fns (keep (fn [[_ src]]
                                     (when (:enabled src)
                                       (:candidates src)))
                                   (if sources
                                     (all-sources sources)
                                     (all-sources)))]
           (as-> (mapcat (fn [f] (f comp-env prefix ns context)) candidate-fns)
               candidates
             (if (= sort-order :by-name)
               (sort-by :candidate candidates)
               (sort-by :candidate by-length-comparator candidates))

             (if (:plain-candidates options-map)
               (map :candidate candidates)
               candidates)

             (doall candidates))))))))

(defn documentation
  "Returns a documentation string that describes the given symbol."
  ([symbol-str]
   (documentation symbol-str *ns*))
  ([symbol-str ns]
   (if (empty? symbol-str)
     ""
     (->> (for [[_ {:keys [doc enabled]}] (all-sources)
                :when enabled
                :let [docstr (doc symbol-str (ensure-ns ns))]
                :when docstr]
            docstr)
          (interpose "\n\n")
          join))))
