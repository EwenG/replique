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
           [java.nio.file Files LinkOption Path Paths]
           [java.nio.file.attribute FileAttribute]))

(defn- path ^Path [& names] (Paths/get (first names) (into-array String (rest names))))

(defn- write-file! [dir & names]
  (let [file (apply path dir names)]
    (Files/createDirectories (.getParent file) (make-array FileAttribute 0))
    (Files/write file (.getBytes "" "UTF-8") (make-array java.nio.file.OpenOption 0))
    file))

(defn- entry-url ^java.net.URL [dir] (.toURL (.toUri (path dir))))

(defn- linked!
  "Make LINK a symbolic link to TARGET, and answer LINK."
  ^Path [^Path link ^Path target]
  (Files/createDirectories (.getParent link) (make-array FileAttribute 0))
  (Files/createSymbolicLink link target (make-array FileAttribute 0)))

(defn- real ^String [^Path p] (str (.toRealPath p (make-array LinkOption 0))))

(defn- offers?
  "Whether the process offers NAMESPACE as a namespace to require."
  [c namespace]
  (contains? (set (map :candidate
                       (:completions (request! c {:op :completions :position :namespace
                                                  :text namespace :id 99}))))
             namespace))

(defn- anchors-under
  "The anchors of the reading that name something under DIR, as pairs of what
  being under them contributes and where they are."
  [dir]
  (vec (for [[^Path p prefix] (classpath/naming-anchors)
             :when (string/starts-with? (str p) (real (path dir)))]
         [prefix (str p)])))

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

;;; Links under an entry

(deftest a-package-linked-under-an-entry-is-read-through-the-link
  (testing "an entry of the classpath is resolved once, when the process
  starts, and a link BELOW one is resolved on every lookup - which is how one
  process reads a source tree that is somewhere else, and can be made to read
  another one without being restarted. So the walk has to follow it: a
  linked directory not followed arrives as a leaf, and a source root whose
  packages are links reads as a source root holding no namespaces at all"
    (with-process [info nil]
      (let [entry (temp-dir)
            tree (temp-dir)
            c (control-client info)]
        (try
          (write-file! tree "probe" "linked.clj")
          (linked! (path entry "probe") (path tree "probe"))
          (add-entry! entry)
          (request! c {:op :update-classpath :id 1})
          (is (offers? c "probe.linked"))
          (testing "and what it crossed on the way is written down - the way
          the classpath spells that directory, and where it really is, which
          is the pair `replique.analysis/source-path' names a file with"
            (is (= [["probe/" (real (path tree "probe"))]] (anchors-under tree))))
          (finally
            (disconnect c)
            (delete-recursively entry)
            (delete-recursively tree)
            (classpath/rescan!)))))))

(deftest a-link-re-pointed-under-a-running-process-is-a-reading-due
  (testing "the one way this reading goes stale that the entries do not: an
  entry cannot move under a running process, and a link below one is moved by
  re-pointing it. So a worktree swapped for another is a reading due - and
  until it happens the anchors name where the files were, which is answering
  nothing rather than answering something else"
    (with-process [info nil]
      (let [entry (temp-dir)
            one (temp-dir)
            two (temp-dir)
            c (control-client info)]
        (try
          (write-file! one "probe" "first.clj")
          (write-file! two "probe" "second.clj")
          (linked! (path entry "probe") (path one "probe"))
          (add-entry! entry)
          (request! c {:op :update-classpath :id 1})
          (is (offers? c "probe.first"))
          (Files/delete (path entry "probe"))
          (linked! (path entry "probe") (path two "probe"))
          (testing "re-pointed and not read again is the reading it was"
            (is (offers? c "probe.first"))
            (is (not (offers? c "probe.second")))
            (is (= [["probe/" (real (path one "probe"))]] (anchors-under one))))
          (testing "and reading it again is what says otherwise, for the names
          and for the anchors both"
            (request! c {:op :update-classpath :id 2})
            (is (offers? c "probe.second"))
            (is (not (offers? c "probe.first")))
            (is (= [] (anchors-under one)))
            (is (= [["probe/" (real (path two "probe"))]] (anchors-under two))))
          (finally
            (disconnect c)
            (delete-recursively entry)
            (delete-recursively one)
            (delete-recursively two)
            (classpath/rescan!)))))))

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
