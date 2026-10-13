(ns replique.trace-test
  "Functions traced: their calls, as a tree."
  (:require [clojure.test :refer [deftest is testing]]
            [replique.classpath :as classpath]
            [replique.state :as state]
            [replique.test-client :as client
             :refer [control-client disconnect eval! recv repl-client send! with-process]]))

(defn- ask!
  "Send MSG and return its answer, past any event."
  [c msg]
  (send! c msg)
  (loop []
    (let [f (recv c)]
      (if (= "event" (:tag f)) (recur) f))))

(defn- ret [r code]
  (:value (client/frame-tagged (eval! r code) "ret")))

(defn- calls
  "The calls made in the call PARENT - or in none - as [var args value-or-thrown]."
  ([c] (calls c nil))
  ([c parent]
   (:calls (ask! c (cond-> {:op :trace-calls :id 10} parent (assoc :parent parent))))))

(defn- said [call]
  [(:var call) (:args call) (or (:value call) (:thrown call))])

(deftest the-calls-of-what-is-traced-are-a-tree
  (with-process [info nil]
    (let [r (repl-client info)
          c (control-client info)]
      (try
        ;; what another test traced and kept is the process's
        (ask! c {:op :untrace :id 0})
        (ask! c {:op :trace-clear :id 0})
        (ret r "(ns tr)")
        (ret r "(defn ta [x] (inc x))")
        (ret r "(defn tb [m] (+ (ta (:n m)) (ta 10)))")
        (ret r "(defn tc [n] (if (pos? n) (tc (dec n)) (throw (ex-info \"none\" {}))))")
        (ret r "(defn td [] @(future (ta 1)))")
        (ret r "(defmacro tm [x] x)")
        (ret r "(def tv 1)")
        (let [{:keys [asked traced skipped]} (ask! c {:op :trace :ns "tr" :id 1})]
          (testing "a namespace's functions are traced - its macros are no question - and
            the rest said why not"
            (is (= ["tr/ta" "tr/tb" "tr/tc" "tr/td"] traced))
            (is (= [["tr/tv" "not a function"]] (map (juxt :var :why) skipped)))
            (is (= asked (+ (count traced) (count skipped))))))
        (is (= "13" (ret r "(tb {:n 1})")))
        (let [[tb] (calls c)]
          (testing "a call, with what it was called with and returned, and how long it took"
            (is (= ["tr/tb" ["{:n 1}"] "13"] (said tb)))
            (is (= {:state "returned" :made 2 :kept 2} (select-keys tb [:state :made :kept])))
            (is (number? (:ms tb)))
            (is (= 1 (:line tb))))
          (testing "and the calls it made, in it"
            (is (= [["tr/ta" ["1"] "2"] ["tr/ta" ["10"] "11"]] (map said (calls c (:call tb))))))
          (testing "each a value to browse"
            (is (= "[{:n 1}]" (get-in (ask! c {:op :inspect :id 2
                                               :source {:trace {:call (:call tb) :args true}}})
                                      [:root :value])))
            (is (= "13" (get-in (ask! c {:op :inspect :id 3
                                         :source {:trace {:call (:call tb) :value true}}})
                                [:root :value])))
            (let [o (ask! c {:op :inspect :id 4 :source {:trace {:call (:call tb) :arg 0}}})]
              (is (= "{:n 1}" (get-in o [:root :value])))
              (testing "kept in a var of the namespace of its function"
                (is (= "tr/traced-kept"
                       (:var (ask! c {:op :inspect-def :view (:view o)
                                      :node (get-in o [:root :node])
                                      :name "traced-kept" :id 40}))))
                (is (= "{:n 1}" (ret r "tr/traced-kept"))))))
          (testing "made again, with what it was called with"
            (is (= "13" (ret r (str "(apply #'tr/tb (replique.trace/args-of " (:call tb) "))"))))
            (is (= ["tr/tb" ["{:n 1}"] "13"] (said (last (calls c)))))
            (is (re-find #"not kept" (:message (client/frame-tagged
                                                (eval! r "(replique.trace/args-of 100000)")
                                                "exception"))))))
        (testing "a call that threw, in the one it threw out of"
          (ret r "(try (tc 1) (catch Exception _ :caught))")
          (let [tc (last (calls c))]
            (is (= ["tr/tc" ["1"] "clojure.lang.ExceptionInfo: none"] (said tc)))
            (is (= [["tr/tc" ["0"] "clojure.lang.ExceptionInfo: none"]]
                   (map said (calls c (:call tc)))))))
        (testing "a call made on another thread, in the call that started it"
          (ret r "(td)")
          (let [td (last (calls c))
                [ta] (calls c (:call td))]
            (is (= ["tr/ta" ["1"] "2"] (said ta)))
            (is (not= (:thread td) (:thread ta)))))
        (testing "defined again, it is traced still"
          (ret r "(defn ta [x] (+ x 2))")
          (is (= "3" (ret r "(ta 1)")))
          (is (= ["tr/ta" ["1"] "3"] (said (last (calls c))))))
        (testing "cleared, the calls are kept from the first again"
          (is (pos? (:cleared (ask! c {:op :trace-clear :id 5}))))
          (is (= [] (calls c))))
        (testing "let go of, it is what it was"
          (is (= #{"tr/ta" "tr/tb" "tr/tc" "tr/td"}
                 (set (:untraced (ask! c {:op :untrace :id 6})))))
          (is (= "3" (ret r "(ta 1)")))
          (is (= [] (calls c)))
          (is (= [] (:traced (ask! c {:op :trace-calls :id 7})))))
        (testing "what tracing is made of is not traced"
          (is (= [{:var "clojure.core/map" :why "what tracing is made of is not traced"}]
                 (:skipped (ask! c {:op :trace :vars ["clojure.core/map"] :id 8})))))
        (finally (disconnect r) (disconnect c))))))

;;; Around a var, which needs what the compiler resolved

(defn- source-root! []
  (let [dir (java.io.File. (str (client/temp-dir)) "src")]
    (.mkdirs dir)
    (.addURL state/class-loader (.toURL (.toURI dir)))
    (state/adopt-class-loader!)
    (classpath/rescan!)
    (.getPath dir)))

(defn- written-file! [dir name source]
  (let [f (java.io.File. (str dir) (str name))]
    (.mkdirs (.getParentFile f))
    (spit f source)
    (.getPath f)))

(deftest a-var-is-traced-with-what-calls-it-and-what-it-calls
  (with-process [info nil]
    (let [r (repl-client info)
          c (control-client info)
          root (source-root!)]
      (try
        (ask! c {:op :untrace :id 0})
        (ask! c {:op :trace-clear :id 0})
        (if-not (true? (:analysis (ask! c {:op :process-info :id 1})))
          (println "replique.trace-test: no model to read callers from - run with :analysis")
          (let [a (written-file! root "aro/a.clj"
                                 (str "(ns aro.a)\n"
                                      "(defn low [x] (inc x))\n"
                                      "(defn mid [x] (map low [x]))\n"
                                      "(defn other [] 1)\n"))
                b (written-file! root "aro/b.clj"
                                 (str "(ns aro.b (:require [aro.a :as a]))\n"
                                      "(defn top [] (a/mid 1))\n"
                                      "(defn above [] (top))\n"))]
            (eval! r (str "#replique/load " (pr-str {:file a})))
            (eval! r (str "#replique/load " (pr-str {:file b})))
            (let [{:keys [callers callees traced]}
                  (ask! c {:op :trace :around "aro.a/mid" :id 2})]
              (is (= ["aro.b/top"] callers))
              (testing "what it hands to what it calls is what it calls"
                (is (= ["aro.a/low"] callees)))
              (is (= ["aro.a/low" "aro.a/mid" "aro.b/top"] traced)))
            (testing "further away"
              (is (= ["aro.b/above" "aro.b/top"]
                     (:callers (ask! c {:op :trace :around "aro.a/mid" :depth 2 :id 3})))))
            (is (= "(2)" (ret r "(aro.b/top)")))
            (let [[top] (filter #(= "aro.b/top" (:var %)) (calls c))]
              (is (= [["aro.a/mid" ["1"] "(2)"]] (map said (calls c (:call top))))))))
        (finally (ask! c {:op :untrace :id 9}) (disconnect r) (disconnect c)
                 (client/delete-recursively (.getParent (java.io.File. ^String root))))))))
