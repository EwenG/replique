(ns replique.remove-var-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [replique.ops]
            [replique.test-client :as client
             :refer [control-client disconnect eval! repl-client request! with-process]]))

(defn- make!
  "Evaluate the forms on the repl, and leave it where it started."
  [r & forms]
  (doseq [form forms] (eval! r form))
  (eval! r "(in-ns 'user)"))

(defn- remove! [c var]
  (request! c {:op :remove-var :var var :id 1}))

(defn- resolves?
  "What NS makes of the symbol WRITTEN, as the repl says."
  [r ns written]
  (-> (eval! r (str "(ns-resolve '" ns " '" written ")"))
      (client/frame-tagged "ret")
      :value))

(deftest a-var-is-taken-away-from-everywhere-that-maps-it
  (testing "unmapping a var is not ns-unmap, because a var is rarely in one
  place: one that was referred is in every namespace that referred it, under
  whatever name that namespace referred it as"
    (with-process [info nil]
      (let [r (repl-client info)
            c (control-client info)]
        (try
          (make! r
                 "(ns probe.home)"
                 "(defn gone [] :here)"
                 "(ns probe.plain (:require [probe.home :refer [gone]]))"
                 "(ns probe.renamed (:require [probe.home :refer [gone] :rename {gone went}]))")
          (let [reply (remove! c "probe.home/gone")]
            (is (= "reply" (:tag reply)))
            (is (= "probe.home/gone" (:removed reply)))
            (testing "and the answer says where it was, by the name each of
            them wrote it as - the namespace that referred it as something else
            is the one whose code will not compile until it is edited"
              (is (= {:probe.home ["gone"]
                      :probe.plain ["gone"]
                      :probe.renamed ["went"]}
                     (:unmapped reply)))))
          (testing "which is a thing about the process and not only about the
          answer: nothing resolves it any more"
            (is (= "nil" (resolves? r "probe.home" "gone")))
            (is (= "nil" (resolves? r "probe.plain" "gone")))
            (is (= "nil" (resolves? r "probe.renamed" "went"))))
          (testing "and asking again is refused, because there is nothing left
          to remove"
            (is (= "unknown-var" (:error (remove! c "probe.home/gone")))))
          (finally (disconnect r) (disconnect c)))))))

(deftest a-var-is-named-where-it-lives
  (testing "the name at point in a buffer resolves to whatever that namespace
  maps it to, and for map or for str that is a var of clojure.core - so
  \"remove the definition I am pointing at\" must not be a way to unmap
  clojure.core from the process"
    (with-process [info nil]
      (let [r (repl-client info)
            c (control-client info)]
        (try
          (make! r "(ns probe.refers)")
          (let [reply (remove! c "probe.refers/map")]
            (is (= "error" (:tag reply)))
            (is (= "unknown-var" (:error reply)))
            (is (string/includes? (:message reply) "probe.refers/map")))
          (testing "and clojure.core still has what it always had"
            (is (= "#'clojure.core/map" (resolves? r "probe.refers" "map"))))
          (finally (disconnect r) (disconnect c)))))))

(deftest a-var-that-is-interned-nowhere
  (testing "refused rather than answered with nothing done. :spellings leaves
  such a name out, because it asks about several and one the process has not
  loaded is a fact about the process; this asks for one thing to be done to
  one var"
    (with-process [info nil]
      (let [c (control-client info)]
        (try
          (is (= "unknown-var" (:error (remove! c "no.such.ns/nope"))))
          (finally (disconnect c)))))))

(deftest a-var-must-be-named-by-a-qualified-name
  (with-process [info nil]
    (let [c (control-client info)]
      (try
        (testing "a bare name names where it is written rather than where it
        lives, which is the thing this op must not be given"
          (let [reply (remove! c "gone")]
            (is (= "invalid-message" (:error reply)))
            (is (string/includes? (:message reply) "qualified name"))))
        (testing "and something that is not a name at all"
          (let [reply (request! c {:op :remove-var :var 1 :id 1})]
            (is (= "invalid-message" (:error reply)))
            (is (string/includes? (:message reply) "the :var to remove"))))
        (testing "including none"
          (is (= "invalid-message" (:error (request! c {:op :remove-var :id 1})))))
        (finally (disconnect c))))))

(deftest a-name-is-read-the-three-ways-a-client-spells-one
  (testing "a client with an EDN printer writes a symbol, one without writes a
  string - and both spell the same name"
    (with-process [info nil]
      (let [r (repl-client info)
            c (control-client info)]
        (try
          (make! r "(ns probe.spelt)" "(def one 1)" "(def two 2)")
          (is (= "probe.spelt/one" (:removed (remove! c "probe.spelt/one"))))
          (is (= "probe.spelt/two"
                 (:removed (request! c {:op :remove-var :var 'probe.spelt/two :id 1}))))
          (finally (disconnect r) (disconnect c)))))))
