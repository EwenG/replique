(ns replique.cljs-env.completion
  (:require [replique.cljs-env.elisp-printer :as elisp]
            [clojure.string :as string :refer [split]]
            [goog.object :as o]
            [goog.string :as gstring])
  (:import [goog.events EventType]))

(def ^:const max-candidates-number 200)

;; Split on underscors or upper case letters
(defn tokenize-prefix [prefix]
  (->> (string/split prefix #"_|(?=[A-Z])")
       (filter #(not= "" %))
       distinct))

(defn is-upper-case [c]
  (and c
       (> (.-length c) 0)
       (= (.toUpperCase c) c)))

(defn matches? [candidate prefix-tokens]
  (loop [prefix-tokens (seq prefix-tokens)
         match-index 0]
    (if-let [token (first prefix-tokens)]
      (let [maybe-match-index
            (if (is-upper-case (.charAt token 0))
              (.lastIndexOf candidate token)
              (let [maybe-match-index (.lastIndexOf candidate (str "_" token))]
                (if (> maybe-match-index -1)
                  (inc maybe-match-index)
                  (if (string/starts-with? candidate token) 0 -1))))]
        (when (> maybe-match-index -1)
          (let [maybe-match-index (+ (count token) maybe-match-index)]
            (if (> maybe-match-index match-index)
              (recur (rest prefix-tokens) maybe-match-index)
              (recur (rest prefix-tokens) match-index)))))
      match-index)))

(def by-length-comparator
  (fn [s1 s2]
    (let [res (compare (count s1) (count s2))]
      (if (zero? res)
        (compare s1 s2)
        res))))

(defn js-scoped-candidates [original-scope-name munged-scope-name munged-prefix include-methods?]
  (->> (when-let [scope (if (= 0 (count munged-scope-name))
                          js/window
                          (try (js/eval munged-scope-name) (catch js/Error e nil)))]
         ;; getAllPropertyNames may not be available on old google closure versions
         ;; ~ before clojurescript 1.9.562
         (for [k (o/getAllPropertyNames scope true true)
               :when (and (or include-methods? (not (fn? (elisp/custom-unchecked-get scope k))))
                          (not (gstring/isNumeric k)))
               :let [match-index (matches? k (tokenize-prefix munged-prefix))]
               :when match-index]
           {:candidate (str original-scope-name (demunge k))
            :match-index (+ (count original-scope-name) match-index)}))
       (sort-by :candidate by-length-comparator)
       (take max-candidates-number)
       elisp/pr-str))

(defn js-fields-candidates [munged-prefix munged-param methods?]
  (->> (when-let [scope (try (js/eval munged-param) (catch js/Error e nil))]
         ;; getAllPropertyNames may not be available on old google closure versions
         ;; ~ before clojurescript 1.9.562
         (for [k (o/getAllPropertyNames scope true true)
               :when (and (if methods?
                            (fn? (elisp/custom-unchecked-get scope k))
                            (not (fn? (elisp/custom-unchecked-get scope k))))
                          (not (gstring/isNumeric k)))
               :let [match-index (matches? k (tokenize-prefix munged-prefix))]
               :when match-index]
           {:candidate (str (if methods? "." ".-") (demunge k))
            :match-index (+ (if methods? 1 2) match-index)}))
       (sort-by :candidate by-length-comparator)
       (take max-candidates-number)
       elisp/pr-str))

(comment
  (matches? "forEach" (tokenize-prefix "for"))
  )

