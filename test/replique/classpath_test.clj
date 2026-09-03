(ns replique.classpath-test
  (:require [clojure.java.basis :as basis]
            [clojure.repl.deps :as deps]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [replique.classpath :as classpath]
            [replique.ops]
            [replique.state :as state]
            [replique.test-client :as client
             :refer [control-client disconnect eval! frame-tagged repl-client request!
                     temp-dir delete-recursively with-process]])
  (:import [clojure.lang DynamicClassLoader]
           [java.nio.file Files Path Paths]
           [java.nio.file.attribute FileAttribute]))

(defn- path ^Path [& names] (Paths/get (first names) (into-array String (rest names))))

(defn- write-file! [dir & names]
  (let [file (apply path dir names)]
    (Files/createDirectories (.getParent file) (make-array FileAttribute 0))
    (Files/write file (.getBytes "" "UTF-8") (make-array java.nio.file.OpenOption 0))
    file))

(defn- entry-url ^java.net.URL [dir] (.toURL (.toUri (path dir))))

(defn- returned [frames] (:value (frame-tagged frames "ret")))

(defn- add-entry!
  "Put dir on the classpath of the process, the way a library is put there.

  A loader cannot be made to forget a url, so what a test leaves behind is an
  entry that no longer exists once its directory is deleted - which is the
  entry `replique.classpath' is written to pass over."
  [dir]
  (.addURL ^DynamicClassLoader state/class-loader (entry-url dir)))

;;; The loader

(deftest every-connection-loads-through-one-loader
  (with-process [info nil]
    (let [dir (temp-dir)
          a (repl-client info)
          b (repl-client info)
          c (control-client info)
          look (str "(some? (.getResource (.getContextClassLoader (Thread/currentThread))"
                    " \"shared/probe.clj\"))")]
      (try
        (write-file! dir "shared" "probe.clj")
        (is (= "false" (returned (eval! a look))))
        (add-entry! dir)
        (testing "what one connection can load, every connection can load: a
        repl wraps the loader of the process rather than replacing it"
          (is (= "true" (returned (eval! a look))))
          (is (= "true" (returned (eval! b look)))))
        (testing "and the thread that reads the classpath is one of them"
          (request! c {:op :update-classpath :id 1})
          (is (contains? (set (map :candidate
                                   (:completions
                                    (request! c {:op :completions :position :namespace
                                                 :text "shared.probe" :id 2}))))
                         "shared.probe")))
        (finally
          (disconnect a) (disconnect b) (disconnect c)
          (delete-recursively dir)
          (classpath/rescan!))))))

(deftest the-loader-a-repl-adds-to-is-the-one-of-the-process
  (with-process [info nil]
    (let [a (repl-client info)
          highest (str "(identical? replique.state/class-loader"
                       " (loop [l (.getContextClassLoader (Thread/currentThread))]"
                       "   (if (instance? clojure.lang.DynamicClassLoader (.getParent l))"
                       "     (recur (.getParent l)) l)))")]
      (try
        (is (= "true" (returned (eval! a highest)))
            "which is the loader clojure.repl.deps walks up to and adds to")
        (finally (disconnect a))))))

;;; The ops

(deftest what-was-added-is-said-and-the-classpath-read-again
  (with-process [info nil]
    (let [dir (temp-dir)
          c (control-client info)
          thread (atom nil)]
      (try
        (write-file! dir "added" "probe.clj")
        (with-redefs [deps/add-libs (fn [libs]
                                      (reset! thread {:repl *repl*
                                                      :data-readers (thread-bound? #'*data-readers*)})
                                      (add-entry! dir)
                                      (vec (keys libs)))]
          (let [reply (request! c {:op :add-libs :libs {'my/lib {:mvn/version "1.0.0"}} :id 1})]
            (is (= "reply" (:tag reply)))
            (is (= ["my/lib"] (:added reply)))
            (is (pos? (:namespaces reply)))
            (is (pos? (:classes reply)))))
        (testing "what clojure.repl.deps asks of the thread it is called on:
        somebody having asked, and somewhere to put what it reads off the
        library it added"
          (is (= {:repl true :data-readers true} @thread)))
        (testing "read again by the op, so nothing else has to ask"
          (is (contains? (set (map :candidate
                                   (:completions
                                    (request! c {:op :completions :position :namespace
                                                 :text "added.probe" :id 2}))))
                         "added.probe")))
        (testing "and nothing added is an empty list rather than an absent key"
          (with-redefs [deps/add-libs (constantly nil)]
            (is (= [] (:added (request! c {:op :add-libs :libs {'my/lib {}} :id 3}))))))
        (finally
          (disconnect c)
          (delete-recursively dir)
          (classpath/rescan!))))))

(deftest a-sync-passes-on-the-aliases-it-was-given
  (with-process [info nil]
    (let [c (control-client info)
          asked (atom ::none)]
      (try
        (with-redefs [deps/sync-deps (fn [& {:keys [aliases]}] (reset! asked aliases) nil)]
          (request! c {:op :sync-deps :id 1})
          (is (= nil @asked) "none given is none passed on, not an empty list")
          (request! c {:op :sync-deps :aliases [:dev "test"] :id 2})
          (is (= [:dev :test] @asked)
              "spelled the way a client without an EDN printer spells them"))
        (finally (disconnect c))))))

(deftest a-library-needs-a-basis-to-be-resolved-against
  (with-process [info nil]
    (let [c (control-client info)]
      (try
        (with-redefs [basis/initial-basis (constantly nil)]
          (doseq [msg [{:op :add-libs :libs {'my/lib {}} :id 1}
                       {:op :sync-deps :id 2}]]
            (let [reply (request! c msg)]
              (is (= "error" (:tag reply)))
              (is (= "no-basis" (:error reply)))
              (is (string/includes? (:message reply) "clojure cli")))))
        (finally (disconnect c))))))

(deftest what-the-client-got-wrong
  (with-process [info nil]
    (let [c (control-client info)
          kind (fn [msg] (:error (request! c msg)))]
      (try
        (is (= "invalid-message" (kind {:op :add-libs :id 1})))
        (is (= "invalid-message" (kind {:op :add-libs :libs {} :id 2})))
        (is (= "invalid-message" (kind {:op :add-libs :libs "a-string" :id 3})))
        (is (= "invalid-message" (kind {:op :add-libs :libs {'my/lib "1.0.0"} :id 4})))
        (is (= "invalid-message" (kind {:op :add-libs :libs {42 {}} :id 5})))
        (is (= "invalid-message" (kind {:op :sync-deps :aliases :dev :id 6})))
        (is (= "invalid-message" (kind {:op :sync-deps :aliases [42] :id 7})))
        (testing "and the connection survives every one of them"
          (is (= "reply" (:tag (request! c {:op :echo :value 1 :id 8})))))
        (finally (disconnect c))))))
