(ns replique.shape-test
  (:require [clojure.test :refer [deftest is testing]]
            [replique.shape :as shape]))

(defn- of-all [& xs] (reduce shape/merged (map shape/shape xs)))

(deftest test-a-map-of-keywords-is-said-key-by-key
  (is (= '{:id uuid, :n long, :status :draft, :tags (vector (or :a :b) 2), :f fn}
         (shape/shape {:id (random-uuid) :n 1 :status :draft :tags [:a :b] :f inc})))
  (testing "in the order they print in"
    (is (= [:f :id :n :status :tags]
           (keys (shape/shape {:tags [] :status :x :n 1 :id 2 :f 3})))))
  (testing "and any other map by what its keys and values look like"
    (is (= '(map-of string long 2) (shape/shape {"a" 1 "b" 2})))
    (is (= '(map-of keyword long 41)
           (shape/shape (zipmap (map #(keyword (str "k" %)) (range 41)) (range)))))))

(defrecord Point [x y])

(deftest test-a-record-is-said-with-its-name
  (is (= '(record replique.shape_test.Point {:x long :y double})
         (shape/shape (->Point 1 2.0)))))

(deftest test-a-collection-is-what-its-elements-look-like-and-how-many
  (is (= '(vector long 3) (shape/shape [1 2 3])))
  (is (= '(set long 2) (shape/shape #{1 2})))
  (is (= '(list string 1) (shape/shape '("x"))))
  (is (= '(array long 3) (shape/shape (long-array 3))))
  (is (= '(vector none 0) (shape/shape [])))
  (testing "the elements are those first ones"
    (is (= '(vector long 3) (shape/shape [1 2 "x"] {:elements 2})))))

(deftest test-a-lazy-seq-is-not-realized
  (let [unrealized (map inc [1 2 3])]
    (is (= 'lazy (shape/shape unrealized)))
    (is (not (realized? unrealized))))
  (testing "but what of it is, is said, and how much"
    (let [s (map inc (range 100))]
      (first s)
      (is (= '(lazy long 32) (shape/shape s)))))
  (is (= '(seq long 3) (shape/shape (doall (map inc [1 2 3])))))
  (testing "and an endless one is not counted to its end"
    (is (= '(seq long ?) (shape/shape (repeat 1))))))

(deftest test-nothing-is-dereferenced
  (is (= ['atom 'delay 'promise 'volatile]
         (mapv shape/shape [(atom {:a 1}) (delay 1) (promise) (volatile! 1)]))))

(deftest test-a-shape-stops-at-its-depth
  (is (= '{:a {:b ...}} (shape/shape {:a {:b {:c 1}}} {:depth 2})))
  (is (= '{:a {:b long}} (shape/shape {:a {:b 1}} {:depth 2}))))

(deftest test-the-shapes-of-many-values-merge
  (testing "a key not in every map is optional"
    (is (= '{:a long, :b (optional string)} (of-all {:a 1} {:a 2 :b "x"}))))
  (testing "values of more than one kind are alternatives, nil last"
    (is (= '(or long string nil) (of-all 1 nil "x")))
    (is (= '(or {:a long} nil) (of-all nil {:a 1}))))
  (testing "a count that varied is a range"
    (is (= '(vector (or long string) [0 2]) (of-all [] [1 2] ["x"]))))
  (testing "keywords are said until there are too many to say anything"
    (is (= '(or :a :b) (of-all :a :b :a)))
    (is (= 'keyword (of-all :a :b :c :d :e :f))))
  (testing "elements merge too"
    (is (= '(vector {:a (or long string), :b (optional long)} 2)
           (shape/shape [{:a 1} {:a "x" :b 2}]))))
  (testing "a lazy seq none of which is realized is one with nothing realized"
    (let [s (map inc (range 100))]
      (first s)
      (is (= '(lazy long [0 32]) (of-all (map inc [1]) s)))))
  (testing "what was too deep to say is said by what was not"
    (is (= '(vector {:a long} 2)
           (shape/merged '(vector ... 2) '(vector {:a long} 2)))))
  (testing "none is where merging starts"
    (is (= 'none (shape/merged)))
    (is (= 'long (shape/merged 'none 'long)))))
