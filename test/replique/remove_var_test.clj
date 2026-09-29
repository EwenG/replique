(ns replique.remove-var-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [replique.hooks :as hooks]
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

(defn- vars-of [c ns]
  (:vars (request! c (cond-> {:op :vars :id 1} ns (assoc :ns ns)))))

(defn- written-file!
  "Write SOURCE into DIR under NAME, and answer the path of it."
  [dir name source]
  (let [f (java.io.File. (str dir) (str name))]
    (spit f source)
    (.getPath f)))

(deftest the-vars-of-a-namespace-are-what-can-be-chosen-from
  (testing "the var to remove is nearly never the one at point: renaming a
  definition and evaluating the file again leaves the process holding both,
  and the old name is by then written nowhere in the buffer"
    (with-process [info nil]
      (let [dir (client/temp-dir)
            r (repl-client info)
            c (control-client info)]
        (try
          ;; Loaded from a file rather than evaluated form by form, because
          ;; what is being checked is the order they were written in - and a
          ;; form sent to a repl was written at a socket, which has no lines.
          (let [path (written-file! dir "probe_listed.clj"
                                    (str "(ns probe.listed)\n"
                                         "(defn parse [s] s)\n"
                                         "(defn- helper [] 1)\n"
                                         "(def answer 42)\n"
                                         "(defmacro twice [x] x)\n"))]
            (eval! r (str "#replique/load " (pr-str {:file path})))
            (let [found (vars-of c "probe.listed")]
              (testing "in the order they were written, which is what makes the
              list read like the file - the definition somebody just renamed is
              where they would look for it"
                (is (= ["parse" "helper" "answer" "twice"] (mapv :name found))))
              (testing "each with what it is, from the list a completion
              candidate and a :symbol answer use"
                (is (= ["function" "function" "var" "macro"] (mapv :type found))))
              (testing "and a private one is said to be private rather than
              left out: a defn- renamed is a defn- left behind like any other"
                (is (= [nil true nil nil] (mapv :private found))))
              (testing "and one the process cannot place goes last: a var
              interned rather than written carries no file at all, and sorting
              it by the empty string would put it first - the one place it
              certainly does not go"
                (eval! r "(clojure.core/intern 'probe.listed 'made-up 1)")
                (is (= ["parse" "helper" "answer" "twice" "made-up"]
                       (mapv :name (vars-of c "probe.listed")))))))
          (finally (disconnect r) (disconnect c) (client/delete-recursively dir)))))))

(deftest what-can-be-chosen-is-what-can-be-removed
  (testing "one rule read twice: the vars offered are the interns of the
  namespace, and the interns of the namespace are what :remove-var finds"
    (with-process [info nil]
      (let [r (repl-client info)
            c (control-client info)]
        (try
          (make! r "(ns probe.agreed)" "(defn- quiet [] 1)")
          (is (= ["quiet"] (mapv :name (vars-of c "probe.agreed"))))
          (is (= "probe.agreed/quiet" (:removed (remove! c "probe.agreed/quiet"))))
          (testing "and what a namespace only refers is in neither"
            (is (not (contains? (set (mapv :name (vars-of c "probe.agreed"))) "map")))
            (is (= "unknown-var" (:error (remove! c "probe.agreed/map")))))
          (finally (disconnect r) (disconnect c)))))))

(deftest a-namespace-the-process-does-not-have-holds-no-vars
  (testing "answered with none rather than refused. Every file is one until it
  has been loaded, and holding none is a fact about the process"
    (with-process [info nil]
      (let [c (control-client info)]
        (try
          (let [reply (request! c {:op :vars :ns "no.such.namespace" :id 1})]
            (is (= "reply" (:tag reply)))
            (is (= [] (:vars reply))))
          (finally (disconnect c)))))))

(deftest the-vars-op-needs-a-namespace
  (with-process [info nil]
    (let [c (control-client info)]
      (try
        (is (= "invalid-message" (:error (request! c {:op :vars :id 1}))))
        (is (= "invalid-message" (:error (request! c {:op :vars :ns 1 :id 1}))))
        (testing "and reads the three spellings a client writes a name in"
          (is (= [] (:vars (request! c {:op :vars :ns 'no.such.ns :id 1})))))
        (finally (disconnect c))))))

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

(deftest removing-a-var-tells-whoever-is-running
  ;; A var taken away is a program changed, which is what a hook exists to hear
  ;; about - and it is the one such change no compiler reports, because this op
  ;; does the unmapping itself rather than asking a compiler to. See
  ;; `replique.hooks/removed!'.
  (with-process [info nil]
    (let [r     (repl-client info)
          c     (control-client info)
          fired (atom [])]
      (try
        (swap! hooks/clj-hooks assoc 'rmhook (fn [e] (swap! fired conj e)))
        (make! r "(ns rmhook.a)" "(def gone 1)")
        (reset! fired [])
        (remove! c "rmhook.a/gone")
        (is (= 1 (count @fired)) (pr-str @fired))
        (let [e (first @fired)]
          (is (= :clj (:dialect e)))
          (is (= ['rmhook.a] (:namespaces e)) (pr-str e))
          (is (= ['rmhook.a/gone] (:removed e)) (pr-str e))
          (is (= [] (:vars e))))
        (testing "and a var no hook covers tells nobody"
          (reset! fired [])
          (make! r "(ns rmelsewhere.a)" "(def gone 1)")
          (reset! fired [])
          (remove! c "rmelsewhere.a/gone")
          (is (= [] @fired) (pr-str @fired)))
        (finally
          (swap! hooks/clj-hooks dissoc 'rmhook)
          (disconnect r)
          (disconnect c))))))
