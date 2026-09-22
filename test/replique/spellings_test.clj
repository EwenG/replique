(ns replique.spellings-test
  (:require [clojure.test :refer [deftest is testing]]
            [replique.ops]
            [replique.test-client :as client
             :refer [control-client disconnect eval! repl-client request! with-process]]))

(def ^:private vars ["clojure.core/let" "clojure.core/fn"])

(defn- written-as
  "What the process says NS writes each of VARS as."
  [conn ns]
  (:spellings (request! conn (cond-> {:op :spellings :vars vars :id 1}
                               ns (assoc :ns ns)))))

(defn- of [spellings var] (get spellings (keyword var)))

;; The namespaces below are this file's own, and the names have to be: a test
;; runs inside the process it is testing, so a namespace one test makes is
;; still there when the next test file runs, and a second file that wrote
;; a name another file had written would be answered about that file's rather
;; than its own. Which is not a hazard that stays theoretical - it was one,
;; here, until it was found: remove_var_test makes a `probe.renamed' with no
;; :refer-clojure clause, so the ns macro refers the whole of clojure.core
;; into it, and the form below then added `lettuce' to a namespace that
;; already wrote `let'. A refer adds and never takes away, so re-evaluating an
;; ns form cannot undo what the first one did.
(deftest what-a-namespace-writes-a-var-as
  (with-process [info nil]
    (let [a (repl-client info)
          c (control-client info)
          make! (fn [& forms] (doseq [form forms] (eval! a form)) (eval! a "(in-ns 'user)"))]
      (try
        (testing "a namespace that refers core plainly writes let, and the
        qualified name is a way to write it wherever it is written"
          (make! "(ns probe.writes-plainly)")
          (is (= ["clojure.core/let" "let"] (of (written-as c "probe.writes-plainly") "clojure.core/let"))))

        (testing "an alias is another way to write it, and does not take the
        first one away"
          (make! "(ns probe.writes-aliased (:require [clojure.core :as c]))")
          (is (= ["c/let" "clojure.core/let" "let"]
                 (of (written-as c "probe.writes-aliased") "clojure.core/let"))))

        (testing "a namespace that excluded it and defined its own writes let
        for a var that is not this one, so let is not a way to write this one"
          (make! "(ns probe.writes-shadowed (:refer-clojure :exclude [let]))"
                 "(def let :something-else)")
          (let [found (written-as c "probe.writes-shadowed")]
            (is (= ["clojure.core/let"] (of found "clojure.core/let")))
            (testing "and what it did not exclude is untouched"
              (is (= ["clojure.core/fn" "fn"] (of found "clojure.core/fn"))))))

        (testing "referred under another name, it is written by that name"
          (make! "(ns probe.writes-renamed (:refer-clojure :rename {let lettuce}))")
          (is (= ["clojure.core/let" "lettuce"]
                 (of (written-as c "probe.writes-renamed") "clojure.core/let"))))

        (finally (disconnect a) (disconnect c))))))

(deftest a-namespace-the-process-does-not-have
  (with-process [info nil]
    (let [c (control-client info)]
      (try
        (testing "which is every file until it is loaded. What a namespace
        refers before it refers anything is clojure.core, so the names of core
        mean there what they mean anywhere"
          (is (= ["clojure.core/let" "let"]
                 (of (written-as c "no.such.namespace") "clojure.core/let"))))
        (testing "and none named is the same question"
          (is (= ["clojure.core/let" "let"] (of (written-as c nil) "clojure.core/let"))))
        (finally (disconnect c))))))

(deftest a-var-that-names-nothing-is-left-out
  (with-process [info nil]
    (let [c (control-client info)]
      (try
        (testing "a fact about the process, which may not have loaded the
        namespace it names, rather than a message written wrongly"
          (let [reply (request! c {:op :spellings :id 1
                                  :vars ["clojure.core/let" "no.such.ns/nope"]})]
            (is (= "reply" (:tag reply)))
            (is (= ["clojure.core/let" "let"] (of (:spellings reply) "clojure.core/let")))
            (is (nil? (of (:spellings reply) "no.such.ns/nope")))))
        (finally (disconnect c))))))

(deftest what-the-client-got-wrong
  (with-process [info nil]
    (let [c (control-client info)
          kind (fn [msg] (:error (request! c msg)))]
      (try
        (is (= "invalid-message" (kind {:op :spellings :id 1})))
        (is (= "invalid-message" (kind {:op :spellings :vars [] :id 2})))
        (is (= "invalid-message" (kind {:op :spellings :vars "clojure.core/let" :id 3})))
        (testing "unqualified, which names nothing to ask about: what is being
        asked is what a namespace calls clojure.core/let, and let is the
        answer rather than the question"
          (is (= "invalid-message" (kind {:op :spellings :vars ["let"] :id 4}))))
        (is (= "invalid-message" (kind {:op :spellings :vars [42] :id 5})))
        (is (= "invalid-message" (kind {:op :spellings :vars vars :ns 42 :id 6})))
        (testing "and the connection survives every one of them"
          (is (= "reply" (:tag (request! c {:op :echo :value 1 :id 7})))))
        (finally (disconnect c))))))

(deftest a-name-is-a-name-however-a-client-spells-it
  (with-process [info nil]
    (let [c (control-client info)]
      (try
        (testing "a client with an EDN printer writes a symbol where one
        without writes a string, and the two spell the same name"
          (is (= (:spellings (request! c {:op :spellings :vars ["clojure.core/let"] :id 1}))
                 (:spellings (request! c {:op :spellings :vars ['clojure.core/let] :id 2})))))
        (finally (disconnect c))))))
