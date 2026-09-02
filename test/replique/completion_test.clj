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

(defn- found [msg name]
  (first (filter #(= name (:candidate %)) (:completions (ask msg)))))

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

(deftest a-piece-of-a-name-at-a-time
  (testing "a name is written in pieces, and what was typed is split the same"
    (is (contains? (typed {:position :namespace :text "c.s"}) "clojure.string"))
    (is (contains? (typed {:position :package-or-class :text "j.u.c.Atomic"})
                   "java.util.concurrent.atomic.AtomicInteger"))
    (is (contains? (typed {:position :package-or-class :text "ABQ"})
                   "java.util.concurrent.ArrayBlockingQueue")
        "three letters, and no separator written between them"))
  (testing "a piece is looked for wherever a piece of the name starts"
    (is (contains? (typed {:position :namespace :text "str"}) "clojure.string"))
    (is (contains? (typed {:position :package-or-class :text "HashMap"})
                   "java.util.LinkedHashMap")))
  (testing "and nowhere else"
    (is (not (contains? (typed {:position :namespace :text "tring"}) "clojure.string"))))
  (testing "in the order they were typed"
    (is (not (contains? (typed {:position :namespace :text "string.clojure"})
                        "clojure.string")))))

(deftest how-far-the-match-reached-is-said
  (testing "so that a client can show which of the candidate was matched"
    (is (= {:candidate "clojure.string" :type "namespace" :match-index 9}
           (found {:position :namespace :text "c.s"} "clojure.string")))
    (is (= 34 (:match-index (found {:position :package-or-class :text "j.u.c.Atomic"}
                                   "java.util.concurrent.atomic.AtomicInteger")))))
  (testing "and nothing typed reaches nought, which every name is matched by"
    (is (= 0 (:match-index (first (:completions (ask {:position :flag}))))))))

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
  (is (= ["Date" "LocaleISOData"]
         (candidates {:position :class :package "java.util" :text "Da"})))
  (testing "the classes in the package and not the ones below it"
    (is (not (contains? (typed {:position :class :package "java.util" :text ""})
                        "concurrent.Future")))))

(deftest an-import-written-as-one-name-is-a-package-or-a-class
  (let [answered (:completions (ask {:position :package-or-class :text "java.util.Ma"}))]
    (is (contains? (set (map :candidate answered)) "java.util.Map"))
    (is (= #{"class"} (set (map :type answered)))))
  (testing "a package is answered as one, and before what is inside it"
    (let [answered (:completions (ask {:position :package-or-class :text "java.uti"}))]
      (is (= {:candidate "java.util" :type "package" :match-index 8} (first answered)))
      (is (= {:candidate "java.util.Date" :type "class" :match-index 8}
             (found {:position :package-or-class :text "java.uti"} "java.util.Date"))))))

(deftest an-inner-class-waits-for-its-dollar
  (testing "there are ten of them for every class anybody imports"
    (let [without (candidates {:position :class :package "java.util" :text "Map"})
          with (candidates {:position :class :package "java.util" :text "Map$"})]
      (is (= "Map" (first without)))
      (is (not-any? #(string/includes? % "$") without))
      (is (= "Map$Entry" (first with)))
      (is (every? #(string/includes? % "$") with))))
  (is (contains? (typed {:position :package-or-class :text "java.util.Map$E"})
                 "java.util.Map$Entry")))

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
          (is (= ["made" "made.Thing"] (candidates {:position :package-or-class :text "made."}))
              "the package, and one class: a descriptor is not a class and
              neither is an inner one, yet")
          (testing "and what the compiler made up is not a name anybody wrote"
            (is (= ["made.Thing$Inner"]
                   (candidates {:position :package-or-class :text "made.Thing$"}))))
          (is (not (contains? (typed {:position :package-or-class :text "module-info"})
                              "module-info")))))
      (finally (delete-recursively dir))))
  (testing "which is a rule about a real classpath too - clojure holds fifty
  five of them"
    (let [found (typed {:position :package-or-class :text "clojure.lang.Var$"})]
      (is (contains? found "clojure.lang.Var$Unbound"))
      (is (not-any? #(re-find #"\$\d" %) found)))))

(deftest a-class-of-a-package-nothing-exports-is-not-offered
  (testing "importing one would not compile"
    (let [answered (typed {:position :package-or-class :text "jdk.internal.ref.Cleaner"})]
      (is (not (contains? answered "jdk.internal.ref.Cleaner")))
      (is (not-any? #(string/starts-with? % "jdk.internal.") answered)))))

;;; Matching

(deftest case-is-ignored-until-a-capital-is-typed
  (is (contains? (typed {:position :class :package "java.util" :text "da"}) "Date"))
  (is (contains? (typed {:position :class :package "java.util" :text "Da"}) "Date"))
  (testing "somebody who wrote a capital said which of the two they meant"
    (is (empty? (candidates {:position :class :package "java.util" :text "DA"})))
    (is (contains? (typed {:position :var :namespace "clojure.core" :text "boolean"})
                   "boolean-array"))
    (is (not (contains? (typed {:position :var :namespace "clojure.core" :text "Boolean"})
                        "boolean-array"))))
  (testing "and it is said piece by piece, not once for the whole of it"
    (is (contains? (typed {:position :package-or-class :text "j.u.Date"}) "java.util.Date"))
    (is (not (contains? (typed {:position :package-or-class :text "j.U.Date"})
                        "java.util.Date")))))

(deftest nothing-typed-is-every-name
  (testing "point sits after an opening bracket and everything could follow it"
    (is (seq (candidates {:position :namespace :text ""})))
    (is (= (candidates {:position :namespace :text ""})
           (candidates {:position :namespace})))))

(deftest the-shortest-is-first
  (testing "somebody who typed map wants map before map-indexed"
    (is (= ["map" "map?" "mapv" "mapcat"]
           (vec (take 4 (candidates {:position :var :namespace "clojure.core"
                                     :text "map"}))))))
  (testing "and alphabetically among the names of a length"
    (is (= [":reload" ":verbose" ":reload-all"]
           (candidates {:position :flag :text ""})))))

(deftest the-answer-is-ordered-and-bounded
  (let [reply (ask {:position :package-or-class :text ""})
        answered (mapv :candidate (:completions reply))]
    (is (= completion/max-completions (count answered)))
    (is (= (sort-by (juxt count identity) answered) answered))
    (is (= (count (distinct answered)) (count answered)))
    (is (true? (:truncated reply)) "what was cut is said rather than dropped")
    (testing "and what is cut is the longest, not the last of the alphabet"
      (is (contains? (set answered) "java.util.Map"))))
  (let [reply (ask {:position :flag :text ""})]
    (is (nil? (:truncated reply)))))

;;; The keyword positions

(deftest a-keyword-candidate-carries-its-colon
  (testing "the client replaces the keyword it read point out of"
    (is (= [":refer" ":rename"] (candidates {:position :libspec-option :text ":r"})))
    (is (= [":require" ":refer-clojure"] (candidates {:position :dependency-type :text ":re"})))
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
          (is (= [{:candidate "clojure.string" :type "namespace" :match-index 13}]
                 (:completions reply)))
          (is (not (contains? reply :truncated))
              "an absent value is an absent key"))
        (testing "a position spelled the way a client without an EDN printer spells it"
          (is (= [{:candidate "clojure.string" :type "namespace" :match-index 13}]
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
