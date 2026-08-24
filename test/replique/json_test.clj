(ns replique.json-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as djson]
            [replique.json :as json]))

(deftest scalars
  (is (= "null" (json/write-str nil)))
  (is (= "true" (json/write-str true)))
  (is (= "false" (json/write-str false)))
  (is (= "42" (json/write-str 42)))
  (is (= "42" (json/write-str (bigint 42))))
  (is (= "42" (json/write-str (biginteger 42))))
  (is (= "1.5" (json/write-str 1.5)))
  (is (= "1.50" (json/write-str 1.50M)))
  (is (= "\"foo\"" (json/write-str "foo")))
  (is (= "\"foo\"" (json/write-str :foo)))
  (is (= "\"my.ns/foo\"" (json/write-str :my.ns/foo)))
  (is (= "\"my.ns/foo\"" (json/write-str 'my.ns/foo)))
  (is (= "\"a\"" (json/write-str \a))))

(deftest rejects-what-json-cannot-represent
  (testing "a lossy coercion would be worse than an exception"
    (is (thrown? IllegalArgumentException (json/write-str 1/3)))
    (is (thrown? IllegalArgumentException (json/write-str Double/NaN)))
    (is (thrown? IllegalArgumentException (json/write-str Double/POSITIVE_INFINITY)))
    (is (thrown? IllegalArgumentException (json/write-str (Object.))))
    (is (thrown? IllegalArgumentException (json/write-str {(Object.) 1})))))

(deftest escaping
  (is (= "\"a\\\"b\"" (json/write-str "a\"b")))
  (is (= "\"a\\\\b\"" (json/write-str "a\\b")))
  (is (= "\"a\\nb\"" (json/write-str "a\nb")))
  (is (= "\"a\\tb\"" (json/write-str "a\tb")))
  (is (= "\"a\\u0000b\"" (json/write-str "a\u0000b")))
  (is (= "\"a\\u001fb\"" (json/write-str "a\u001fb")))
  (testing "a frame is a single line"
    (is (not (re-find #"\n" (json/write-str {:a "line1\nline2\r\n"})))))
  (testing "non ascii characters are not escaped"
    (is (= "\"éà漢\"" (json/write-str "éà漢"))))
  (testing "surrogate pairs are kept, unpaired surrogates are replaced"
    (is (= "\"😀\"" (json/write-str "\uD83D\uDE00")))
    (is (= "\"\\ufffd\"" (json/write-str (str (char 0xD83D)))))
    (is (= "\"\\ufffd\"" (json/write-str (str (char 0xDE00)))))
    (is (= "\"a\\ufffdb\"" (json/write-str (str "a" (char 0xD83D) "b"))))))

(deftest collections
  (is (= "[1,2,3]" (json/write-str [1 2 3])))
  (is (= "[1,2,3]" (json/write-str '(1 2 3))))
  (is (= "[1,2,3]" (json/write-str (map inc [0 1 2]))))
  (is (= "[1]" (json/write-str #{1})))
  (is (= "[1,2]" (json/write-str (eduction (map inc) [0 1]))))
  (is (= "[1,2]" (json/write-str (into-array Long [1 2]))))
  (is (= "[]" (json/write-str [])))
  (is (= "{}" (json/write-str {})))
  (is (= "{\"a\":1}" (json/write-str {:a 1})))
  (is (= "{\"a\":1}" (json/write-str {"a" 1})))
  (is (= "{\"a\":1}" (json/write-str (doto (java.util.HashMap.) (.put "a" 1)))))
  (is (= "{\"1\":\"x\"}" (json/write-str {1 "x"}))))

(deftest round-trip
  (let [value {:tag "reply" :id 12 :op "echo"
               :candidates ["foo" "bar"]
               :nested {:a [1 2 {:b "c"}] :s "quote \" newline \n backslash \\"}
               :empty [] :t true :f false :n nil}]
    (is (= {"tag" "reply" "id" 12 "op" "echo"
            "candidates" ["foo" "bar"]
            "nested" {"a" [1 2 {"b" "c"}] "s" "quote \" newline \n backslash \\"}
            "empty" [] "t" true "f" false "n" nil}
           (djson/read-str (json/write-str value))))))
