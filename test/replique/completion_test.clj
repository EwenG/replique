(ns replique.completion-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as string]
            [replique.completion :as completion]
            [replique.ops]
            [replique.protocol :as protocol]
            [replique.test-client :as client
             :refer [control-client disconnect request! temp-dir
                     delete-recursively with-process]])
  (:import [java.nio.file Files Path Paths]
           [java.nio.file.attribute FileAttribute]))

(defn- ask
  "What the op answers, asked the way the op asks it - a position written the
  way a client writes one."
  [msg]
  (completion/completions (assoc msg :position (protocol/as-keyword (:position msg)))))

(defn- candidates [msg] (mapv :candidate (:completions (ask msg))))

(defn- typed [msg] (set (candidates msg)))

(defn- error-kind [msg]
  (try (ask msg) nil
       (catch clojure.lang.ExceptionInfo t (:replique/error (ex-data t)))))

;;; A directory on the classpath, as add-libs puts one there

(defn- path ^Path [& names] (Paths/get (first names) (into-array String (rest names))))

(defn- write-file!
  "Make an empty file under dir. Empty because nothing here reads one: a
  resource is read for its name and for nothing else."
  [dir & names]
  (let [file (apply path dir names)]
    (Files/createDirectories (.getParent file) (make-array FileAttribute 0))
    (Files/write file (.getBytes "" "UTF-8") (make-array java.nio.file.OpenOption 0))
    file))

(defn- entry-url ^java.net.URL [& names] (.toURL (.toUri (apply path names))))

(defn- with-entries
  "Call f with these urls on the classpath of this thread.

  Added to a loader rather than to java.class.path, which is what `add-libs'
  does to a running process and what the scan has to read to see it."
  [urls f]
  (let [thread (Thread/currentThread)
        previous (.getContextClassLoader thread)
        loader (clojure.lang.DynamicClassLoader. previous)]
    (doseq [url urls] (.addURL loader url))
    (.setContextClassLoader thread loader)
    (try (f) (finally (.setContextClassLoader thread previous)))))

(defn- with-entry [dir f] (with-entries [(entry-url dir)] f))

;;; Namespaces

(deftest a-namespace-is-offered-from-the-classpath
  (testing "and not from what has been loaded, which is the point: a require
  is written for a namespace that has not been loaded"
    (is (nil? (find-ns 'clojure.zip)))
    (is (contains? (typed {:position :namespace :text "clojure.zi"}) "clojure.zip"))))

(deftest a-candidate-is-what-gets-written
  (testing "a prefix list writes the start of the name once, so what goes in
  the buffer under (clojure [str...]) is string and not clojure.string"
    (is (= ["string"] (candidates {:position :namespace :prefix "clojure" :text "strin"})))
    (is (= ["clojure.string"] (candidates {:position :namespace :text "clojure.strin"})))))

(deftest under-a-prefix-a-name-goes-one-level
  (testing "a lib name inside a prefix list must not hold a period, so a
  namespace below the prefix is not a name that could be written there"
    (let [found (typed {:position :namespace :prefix "clojure" :text "co"})]
      (is (contains? found "core"))
      (is (not (contains? found "core.protocols")))))
  (testing "and it is a name in that prefix, not a name that merely starts alike"
    (is (not (contains? (typed {:position :namespace :prefix "clojur" :text ""})
                        "e.string")))))

(deftest a-namespace-written-now-is-offered-now
  (let [dir (temp-dir)]
    (try
      (with-entry
        dir
        (fn []
          (is (not (contains? (typed {:position :namespace :text "made"}) "made.up")))
          (write-file! dir "made" "up.clj")
          (is (contains? (typed {:position :namespace :text "made"}) "made.up")
              "a directory is walked again every time, and it is what changes")
          (testing "the underscores a file name carries are dashes in the name"
            (write-file! dir "made" "two_words.cljc")
            (is (contains? (typed {:position :namespace :text "made"}) "made.two-words")))
          (testing "and a name is answered once however many entries provide it"
            (write-file! dir "clojure" "string.clj")
            (is (= ["clojure.string"]
                   (candidates {:position :namespace :text "clojure.string"}))))))
      (finally (delete-recursively dir)))))

(deftest an-entry-that-cannot-be-read-is-passed-over
  (testing "a classpath names directories that were never created and jars
  that arrived truncated, and the names of the entries that could be read are
  still the answer"
    (let [dir (temp-dir)]
      (try
        (write-file! dir "not-a" "jar-at-all.jar")
        (write-file! dir "real" "here.clj")
        (with-entries
          [(entry-url dir)
           (entry-url dir "not-a" "jar-at-all.jar")
           (entry-url dir "was" "never" "created")
           ;; and one that is not a path at all: a url a loader holds
           ;; unencoded is one nothing can be made of
           (java.net.URL. "file:/a name with spaces/x.jar")]
          (fn []
            (is (= ["real.here"] (candidates {:position :namespace :text "real."})))))
        (finally (delete-recursively dir))))))

(deftest a-macro-namespace-is-a-namespace-of-this-world
  (testing "what a :require-macros names is a Clojure namespace, which is what
  this process has"
    (is (= (candidates {:position :namespace :text "clojure.stri"})
           (candidates {:position :namespace-macros :text "clojure.stri"})))))

;;; Vars

(deftest a-var-comes-from-a-loaded-namespace
  (testing "and only from one: loading a namespace to see what it holds runs
  every top level form in it, which a keystroke must not do"
    (is (nil? (find-ns 'clojure.data)))
    (is (empty? (candidates {:position :var :namespace "clojure.data" :text ""})))
    (require 'clojure.data)
    (is (contains? (typed {:position :var :namespace "clojure.data" :text "dif"})
                   "diff"))))

(deftest a-var-of-a-refer-clojure-comes-from-core
  (testing "a refer-clojure names no namespace anywhere in itself"
    (is (= (typed {:position :var :namespace :refer-clojure :text "map-in"})
           #{"map-indexed"}))))

(deftest a-var-of-a-namespace-that-is-not-one-is-nothing
  (is (empty? (candidates {:position :var :namespace "not.a.namespace" :text ""}))))

(deftest a-var-is-a-public-one
  (testing "a private var is not one another namespace can refer, and core
  holds three private ones that start the way assert does"
    (is (= ["assert"] (candidates {:position :var :namespace "clojure.core"
                                   :text "assert"})))))

;;; Classes and packages

(deftest a-class-is-offered-under-its-package
  (is (= ["Date"] (candidates {:position :class :package "java.util" :text "Da"})))
  (testing "the classes in the package and not the ones below it"
    (is (not (contains? (typed {:position :class :package "java.util" :text ""})
                        "concurrent.Future")))))

(deftest an-import-written-as-one-name-is-a-package-or-a-class
  (let [found (:completions (ask {:position :package-or-class :text "java.util.Ma"}))]
    (is (contains? (set (map :candidate found)) "java.util.Map"))
    (is (= #{"class"} (set (map :type found)))))
  (testing "a package is answered as one, and before what is inside it"
    (let [found (:completions (ask {:position :package-or-class :text "java.uti"}))]
      (is (= {:candidate "java.util" :type "package"} (first found)))
      (is (contains? (set found) {:candidate "java.util.Date" :type "class"})))))

(deftest an-inner-class-waits-for-its-dollar
  (testing "there are ten of them for every class anybody imports"
    (is (= ["Map"] (candidates {:position :class :package "java.util" :text "Map"})))
    (is (= ["Map$Entry"] (candidates {:position :class :package "java.util" :text "Map$"})))
    (is (contains? (typed {:position :package-or-class :text "java.util.Map$E"})
                   "java.util.Map$Entry"))))

(deftest a-class-nobody-wrote-is-not-offered
  (let [dir (temp-dir)]
    (try
      (with-entry
        dir
        (fn []
          (write-file! dir "made" "Thing.class")
          (write-file! dir "made" "Thing$Inner.class")
          (write-file! dir "made" "Thing$1.class")
          (write-file! dir "made" "Thing$1Local.class")
          (write-file! dir "made" "package-info.class")
          (write-file! dir "module-info.class")
          (is (= ["made.Thing"] (candidates {:position :package-or-class :text "made."}))
              "a descriptor is not a class and neither is an inner one, yet")
          (testing "and what the compiler made up is not a name anybody wrote"
            (is (= ["made.Thing$Inner"]
                   (candidates {:position :package-or-class :text "made.Thing$"}))))
          (is (empty? (candidates {:position :package-or-class :text "module-info"})))))
      (finally (delete-recursively dir))))
  (testing "which is a rule about a real classpath too - clojure holds fifty
  five of them"
    (let [found (typed {:position :package-or-class :text "clojure.lang.Var$"})]
      (is (contains? found "clojure.lang.Var$Unbound"))
      (is (not-any? #(re-find #"\$\d" %) found)))))

(deftest a-class-of-a-package-nothing-exports-is-not-offered
  (testing "importing one would not compile"
    (is (empty? (candidates {:position :package-or-class :text "jdk.internal."})))))

;;; Matching

(deftest case-is-ignored-until-a-capital-is-typed
  (is (= ["Date"] (candidates {:position :class :package "java.util" :text "da"})))
  (is (= ["Date"] (candidates {:position :class :package "java.util" :text "Da"})))
  (is (empty? (candidates {:position :class :package "java.util" :text "DA"}))
      "somebody who wrote a capital said which of the two they meant"))

(deftest nothing-typed-is-every-name
  (testing "point sits after an opening bracket and everything could follow it"
    (is (seq (candidates {:position :namespace :text ""})))
    (is (= (candidates {:position :namespace :text ""})
           (candidates {:position :namespace})))))

(deftest the-answer-is-sorted-and-bounded
  (let [reply (ask {:position :package-or-class :text ""})
        found (mapv :candidate (:completions reply))]
    (is (= completion/max-completions (count found)))
    (is (= (sort found) found))
    (is (= (count (distinct found)) (count found)))
    (is (true? (:truncated reply)) "what was cut is said rather than dropped"))
  (let [reply (ask {:position :flag :text ""})]
    (is (nil? (:truncated reply)))))

;;; The keyword positions

(deftest a-keyword-candidate-carries-its-colon
  (testing "the client replaces the keyword it read point out of"
    (is (= [":refer" ":rename"] (candidates {:position :libspec-option :text ":r"})))
    (is (= [":refer-clojure" ":require"] (candidates {:position :dependency-type :text ":re"})))
    (is (= [":rename"] (candidates {:position :libspec-option-refer :text ":r"})))
    (is (= [":reload" ":reload-all"] (candidates {:position :flag :text ":rel"})))))

(deftest what-a-refer-takes-is-what-is-offered-to-a-refer
  (testing "the position of a refer-clojure as well as of a :use libspec, and
  an :as means nothing in the first of them"
    (is (empty? (candidates {:position :libspec-option-refer :text ":a"})))
    (is (seq (candidates {:position :libspec-option :text ":a"})))))

;;; Load paths

(deftest a-load-path-names-the-file
  (let [found (typed {:position :load-path :text "/clojure/core"})]
    (is (contains? found "/clojure/core"))
    (is (contains? found "/clojure/core_deftype")
        "a load takes a path, so the underscores stay")))

;;; What the client got wrong

(deftest a-position-that-cannot-be-answered-is-refused
  (is (= :invalid-message (error-kind {:position :something-else :text ""})))
  (is (= :invalid-message (error-kind {:text ""})))
  (is (= :invalid-message (error-kind {:position :var :text ""})))
  (is (= :invalid-message (error-kind {:position :class :text ""})))
  (is (= :invalid-message (error-kind {:position :namespace :text 42})))
  (is (= :invalid-message (error-kind {:position :namespace :prefix 42 :text ""}))))

;;; Over a connection

(deftest the-op
  (with-process [info nil]
    (let [client (control-client info)]
      (try
        (let [reply (request! client {:op :completions :position :namespace
                                      :text "clojure.strin" :id 1})]
          (is (= "reply" (:tag reply)))
          (is (= "completions" (:op reply)))
          (is (= [{:candidate "clojure.string" :type "namespace"}] (:completions reply)))
          (is (not (contains? reply :truncated))
              "an absent value is an absent key"))
        (testing "a position spelled the way a client without an EDN printer spells it"
          (is (= [{:candidate "clojure.string" :type "namespace"}]
                 (:completions (request! client {:op :completions :position "namespace"
                                                 :text "clojure.strin" :id 2})))))
        (testing "and one nothing can be answered at"
          (let [reply (request! client {:op :completions :position :nowhere :id 3})]
            (is (= "error" (:tag reply)))
            (is (= "invalid-message" (:error reply)))
            (is (string/includes? (:message reply) "nowhere"))))
        (testing "the connection survives it"
          (is (= "reply" (:tag (request! client {:op :completions :position :flag :id 4})))))
        (finally (disconnect client))))))
