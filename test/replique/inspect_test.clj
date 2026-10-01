(ns replique.inspect-test
  (:require [clojure.test :refer [deftest is testing]]
            [replique.test-client :as client
             :refer [control-client disconnect eval! recv repl-client request! with-process]])
  (:import [java.net SocketTimeoutException]))

(defn- ret [r code]
  (:value (client/frame-tagged (eval! r code) "ret")))

(defn- inspect! [c source & {:as more}]
  (request! c (merge {:op :inspect :source source :id 1} more)))

(defn- keyed
  "The line of CHILDREN labelled K."
  [children k]
  (first (filter #(= k (:key %)) children)))

(defn- events
  "The inspect-changed events C receives in the next MS."
  [c ms]
  (let [^java.net.Socket socket (:socket c)
        deadline (+ (System/currentTimeMillis) ms)]
    (loop [seen []]
      (let [left (- deadline (System/currentTimeMillis))]
        (if (<= left 0)
          seen
          (do (.setSoTimeout socket (int (max 1 left)))
              (let [f (try (recv c) (catch SocketTimeoutException _ ::none))]
                (.setSoTimeout socket 10000)
                (if (= ::none f)
                  seen
                  (recur (cond-> seen
                           (= "inspect-changed" (:event f)) (conj f)))))))))))

(deftest a-var-holding-an-atom-is-a-view-of-what-the-atom-holds
  (with-process [info nil]
    (let [r (repl-client info)
          c (control-client info)]
      (try
        (ret r "(def state (atom {:a 1 :b (vec (range 300))}))")
        (let [o (inspect! c {:var "user/state"} :limit 10)]
          (is (= "reply" (:tag o)))
          (is (number? (:view o)))
          (testing "the root is open, and its children are its lines"
            (is (= 0 (:node (:root o))))
            (is (= "map" (:kind (:root o))))
            (is (= 2 (:total o)))
            (is (= "1" (:value (keyed (:children o) ":a")))))
          (testing "a line that does not show all of it is one to open, and
          says how much there is"
            (let [b (keyed (:children o) ":b")]
              (is (:truncated b))
              (is (= 300 (:count b)))
              (is (:expandable b))
              (let [page (request! c {:op :inspect-children :view (:view o)
                                      :node (:node b) :offset 290 :limit 5 :id 2})]
                (is (= ["290" "291" "292" "293" "294"] (mapv :key (:children page))))
                (is (= 300 (:total page)))
                (is (true? (:more page))))))
          (testing "a line that shows all of it is not one to open"
            (is (not (:expandable (keyed (:children o) ":a"))))))
        (finally (disconnect r) (disconnect c))))))

(deftest nothing-is-realized-past-the-page-asked-for
  (with-process [info nil]
    (let [r (repl-client info)
          c (control-client info)]
      (try
        (ret r "(def realized (atom 0))")
        (ret r "(def endless (map (fn [i] (swap! realized inc) i) (range)))")
        (let [o (inspect! c {:var "user/endless"} :limit 10 :width 40)]
          (is (= "seq" (:kind (:root o))))
          (is (nil? (:total o)) "how long it is is not known, and not asked")
          (is (true? (:more o)))
          (is (= 10 (count (:children o))))
          (let [page (request! c {:op :inspect-children :view (:view o) :node 0
                                  :offset 1000 :limit 3 :id 2})]
            (is (= ["1000" "1001" "1002"] (mapv :key (:children page)))))
          (testing "chunks of 32 at a time, and nothing like the whole of it"
            (is (< (Long/parseLong (ret r "@realized")) 1200))))
        (finally (disconnect r) (disconnect c))))))

(deftest a-key-that-does-not-read-back-is-a-key
  (with-process [info nil]
    (let [r (repl-client info)
          c (control-client info)]
      (try
        (ret r "(def odd {(Object.) {:inside (vec (range 100))}})")
        (let [o (inspect! c {:var "user/odd"} :width 40)
              [line] (:children o)]
          (is (re-find #"#object" (:key line)))
          (is (:expandable line))
          (let [page (request! c {:op :inspect-children :view (:view o)
                                  :node (:node line) :id 2})]
            (is (= ":inside" (:key (first (:children page))))))
          (testing "and the code that reaches it cannot be written, so there is
          only the call that brings it to the repl"
            (let [p (request! c {:op :inspect-path :view (:view o)
                                 :node (:node line) :id 3})]
              (is (nil? (:code p)))
              (is (= "{:inside [0 1 2 3]}"
                     (ret r (str "(update " (:value p) " :inside #(subvec % 0 4))")))))))
        (finally (disconnect r) (disconnect c))))))

(deftest a-watched-atom-says-it-changed-once-until-it-is-asked-for
  (with-process [info nil]
    (let [r (repl-client info)
          c (control-client info)]
      (try
        (ret r "(def counter (atom 0))")
        (let [o (inspect! c {:var "user/counter"})]
          (ret r "(dotimes [_ 1000] (swap! counter inc))")
          (let [seen (events c 500)]
            (is (= [(:view o)] (mapv :view seen))))
          (testing "and says nothing of a change after that"
            (ret r "(swap! counter inc)")
            (is (empty? (events c 300))))
          (testing "until the view has been asked for again"
            (request! c {:op :inspect-refresh :view (:view o) :id 2})
            (ret r "(swap! counter inc)")
            (is (= [(:view o)] (mapv :view (events c 500))))))
        (finally (disconnect r) (disconnect c))))))

(deftest a-refresh-keeps-what-was-open-and-says-what-changed
  (with-process [info nil]
    (let [r (repl-client info)
          c (control-client info)]
      (try
        (ret r "(def state (atom {:a {:deep (vec (range 50))} :b {:other (vec (range 50))}}))")
        (let [o (inspect! c {:var "user/state"} :history 3 :width 40)
              a (:node (keyed (:children o) ":a"))
              b (:node (keyed (:children o) ":b"))]
          (ret r "(swap! state update-in [:a :deep] conj :new)")
          (let [again (request! c {:op :inspect-refresh :view (:view o) :width 40 :id 2})]
            (testing "the same path is the same node"
              (is (= a (:node (keyed (:children again) ":a"))))
              (is (= b (:node (keyed (:children again) ":b")))))
            (testing "and what was replaced says so, what was not does not"
              (is (:changed (keyed (:children again) ":a")))
              (is (not (:changed (keyed (:children again) ":b")))))
            (is (= {:count 2} (:history again))))
          (testing "a value it held can be shown, and is still browsed by the
          same nodes"
            (let [then (request! c {:op :inspect-refresh :view (:view o) :at 0
                                    :width 40 :id 3})
                  deep (request! c {:op :inspect-children :view (:view o) :node a :id 4})]
              (is (= {:count 2 :at 0} (:history then)))
              (is (= 50 (:count (first (:children deep))))))))
        (finally (disconnect r) (disconnect c))))))

(deftest a-var-defined-again-is-watched-again
  (with-process [info nil]
    (let [r (repl-client info)
          c (control-client info)]
      (try
        (ret r "(def state (atom 1))")
        (let [o (inspect! c {:var "user/state"})]
          (ret r "(def state (atom 2))")
          (is (seq (events c 400)))
          (is (= "2" (:value (:root (request! c {:op :inspect-refresh :view (:view o) :id 2})))))
          (testing "and the new atom is the one watched"
            (ret r "(reset! state 3)")
            (is (seq (events c 400)))
            (is (= "0" (ret r "(count (.getWatches (atom 0)))")))
            (is (= "1" (ret r "(count (.getWatches state))")))))
        (finally (disconnect r) (disconnect c))))))

(deftest an-object-is-browsed-as-the-data-datafy-makes-of-it
  (with-process [info nil]
    (let [r (repl-client info)
          c (control-client info)]
      (try
        (ret r "(def failure (ex-info \"boom\" {:x 1}))")
        (let [o (inspect! c {:var "user/failure"})]
          (is (= "object" (:kind (:root o))))
          (is (= #{":cause" ":data" ":via" ":trace"} (set (map :key (:children o)))))
          (is (= "\"boom\"" (:value (keyed (:children o) ":cause")))))
        (finally (disconnect r) (disconnect c))))))

(deftest what-a-repl-returned-and-what-was-tapped-are-views-too
  (with-process [info nil]
    (let [r (repl-client info)
          c (control-client info)]
      (try
        (ret r "(tap> {:tapped 1})")
        (ret r "(range 3)")
        (ret r ":last")
        (let [o (inspect! c {:results (:connection (:hello r))})]
          (is (= ["*1" "*2" "*3"] (mapv :key (:children o))))
          (is (= ":last" (:value (keyed (:children o) "*1"))))
          (testing "the code for one of them is the name"
            (is (= "*2" (:code (request! c {:op :inspect-path :view (:view o)
                                            :node (:node (keyed (:children o) "*2"))
                                            :id 2}))))))
        (let [o (inspect! c {:taps true})]
          (is (= "{:tapped 1}" (:value (first (:children o)))))
          (ret r "(tap> :again)")
          (is (seq (events c 500)))
          (is (= [":again" "{:tapped 1}"]
                 (sort (map :value (:children (request! c {:op :inspect-refresh
                                                           :view (:view o) :id 2})))))))
        (finally (disconnect r) (disconnect c))))))

(deftest a-view-goes-with-the-connection-that-opened-it
  (with-process [info nil]
    (let [r (repl-client info)
          c (control-client info)
          other (control-client info)]
      (try
        (ret r "(def state (atom 1))")
        (let [o (inspect! c {:var "user/state"})]
          (is (= "1" (ret r "(count (.getWatches state))")))
          (disconnect c)
          (Thread/sleep 200)
          (is (= "0" (ret r "(count (.getWatches state))")))
          (is (= "unknown-view" (:error (request! other {:op :inspect-children
                                                         :view (:view o) :node 0 :id 2})))))
        (finally (disconnect r) (disconnect other))))))

(deftest what-cannot-be-a-view-says-why
  (with-process [info nil]
    (let [c (control-client info)]
      (try
        (is (= "unknown-var" (:error (inspect! c {:var "user/nope"}))))
        (is (= "unknown-var" (:error (inspect! c {:var "nope"}))))
        (is (= "invalid-message" (:error (inspect! c {:what :ever}))))
        (is (= "invalid-message" (:error (inspect! c nil))))
        (is (= "unknown-connection" (:error (inspect! c {:results "c999"}))))
        (is (= "unknown-view" (:error (request! c {:op :inspect-refresh :view 999 :id 2}))))
        (finally (disconnect c))))))
