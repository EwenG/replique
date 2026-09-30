(ns replique.lint-test
  "What is wrong with a Clojure file, as the compiler saw it.

  Read out of the forked compiler's model, so every test here asks the process
  whether it records one and, where it does not, checks that it says so:

    clojure -M:test:analysis ...

  The files are written into temporary directories put on the classpath, which
  is what `replique.analysis-test' does and for its reason."
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [replique.classpath :as classpath]
            [replique.ops]
            [replique.state :as state]
            [replique.test-client :as client
             :refer [control-client disconnect eval! recv repl-client request!
                     with-process]]))

;;; A project to look at

(defn- written-file! [dir name source]
  (let [f (java.io.File. (str dir) (str name))]
    (.mkdirs (.getParentFile f))
    (spit f source)
    (.getPath f)))

(defn- edited-file!
  "SOURCE written over the file at PATH, and dated ahead of now - a file written
  twice in the second the process compiled it would otherwise read as unchanged."
  [path source]
  (spit path source)
  (.setLastModified (java.io.File. ^String path) (+ 30000 (System/currentTimeMillis)))
  path)

(defn- source-root! []
  (let [dir (client/temp-dir)]
    (.addURL state/class-loader (.toURL (.toURI (java.io.File. (str dir)))))
    (state/adopt-class-loader!)
    (classpath/rescan!)
    dir))

(defn- load! [r path]
  (eval! r (str "#replique/load " (pr-str {:file path}))))

(defn- lints! [c path]
  (request! c {:op :lints :id 1 :file path}))

(defn- analysing? [c]
  (true? (:analysis (request! c {:op :process-info :id 1}))))

(defn- said
  "The lints of ANSWER as [type line column message]."
  [answer]
  (mapv (juxt :type :line :column :message) (:lints answer)))

(def ^:private util-clj
  (str "(ns lint.util)\n"
       "(defn bar [x] x)\n"
       "(defn- secret [] 1)\n"
       "(defn ^{:deprecated \"1.2\"} old [] 1)\n"
       "(defn bar [x] x)\n"))

(def ^:private core-clj
  (str "(ns lint.core\n"
       "  (:require [lint.util :as u]\n"
       "            [clojure.string :as str]\n"
       "            [clojure.set :refer [union]])\n"
       "  (:import [java.io File]))\n"
       "(set! *warn-on-reflection* true)\n"
       "(defn- unused-priv [] 1)\n"
       "(defn- rec [n] (rec n))\n"
       "(defn f [x y]\n"
       "  (let [z 1 _w 2]\n"
       "    (u/bar 1 2)\n"
       "    (-> x (u/bar 3))\n"
       "    (u/old)\n"
       "    (.length x)\n"
       "    (assoc {})))\n"))

(deftest what-clj-kondo-would-say-the-compiler-says
  (testing "the same linters and the same words, out of what the compiler resolved"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (written-file! root "lint/util.clj" util-clj)
          (let [core (written-file! root "lint/core.clj" core-clj)]
            (load! r core)
            (if-not (analysing? c)
              (testing "a process that records nothing says so"
                (is (string/includes? (str (:message (lints! c core))) "does not record")))
              (let [found (lints! c core)]
                (is (= [["unused-namespace" 3 14 "namespace clojure.string is required but never used"]
                        ["unused-namespace" 4 14 "namespace clojure.set is required but never used"]
                        ["unused-referred-var" 4 34 "#'clojure.set/union is referred but never used"]
                        ["unused-import" 5 21 "Unused import File"]
                        ["unused-private-var" 7 8 "Unused private var lint.core/unused-priv"]
                        ["unused-private-var" 8 8 "Unused private var lint.core/rec"]
                        ["unused-binding" 9 12 "unused binding y"]
                        ["unused-binding" 10 9 "unused binding z"]
                        ["invalid-arity" 11 5 "lint.util/bar is called with 2 args but expects 1"]
                        ["invalid-arity" 12 11 "lint.util/bar is called with 2 args but expects 1"]
                        ["deprecated-var" 13 6 "#'lint.util/old is deprecated since 1.2"]
                        ["reflection" 14 5 "reference to field length can't be resolved."]
                        ["invalid-arity" 15 5 "clojure.core/assoc is called with 1 arg but expects 3 or more"]]
                       (said found)))
                (testing "the version it is about is the one on disk"
                  (is (= (.lastModified (java.io.File. ^String core)) (:mtime found))))
                (testing "a verdict about the whole file says so, and one about a
                form says that"
                  (is (= #{"file"} (set (map :scope (filter #(string/starts-with? (:type %) "unused-n")
                                                            (:lints found))))))
                  (is (= #{"form"} (set (map :scope (filter #(= "invalid-arity" (:type %))
                                                            (:lints found)))))))
                (testing "the other file: a private var nothing calls, and a var
                defined twice"
                  (is (= [["unused-private-var" 3 8 "Unused private var lint.util/secret"]
                          ["redefined-var" 5 7 "redefined var #'lint.util/bar"]]
                         (said (lints! c (str root "/lint/util.clj")))))))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))

(deftest a-lint-is-about-the-version-on-disk
  (testing "a file edited on disk since it was compiled has no lints, unless the
  new version does not compile - which is then the one lint"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (let [util (written-file! root "disk/util.clj"
                                    "(ns disk.util)\n(defn bar [x] x)\n(defn baz [] 1)\n")
                core (written-file! root "disk/core.clj"
                                    (str "(ns disk.core (:require [disk.util :as u]))\n"
                                         "(defn f [] (u/bar 1 2) (u/baz))\n"))]
            (load! r core)
            (when (analysing? c)
              (is (= [["invalid-arity" 2 12 "disk.util/bar is called with 2 args but expects 1"]]
                     (said (lints! c core))))

              (testing "the callee changed on disk and was not loaded: what it
              says about its callers is about a version nobody can see"
                (edited-file! util "(ns disk.util)\n(defn bar [x y] x)\n(defn baz [] 1)\n")
                (is (= [] (said (lints! c core)))))

              (testing "a var removed by a reload is a use of nothing"
                (edited-file! util "(ns disk.util)\n(defn bar [x y] x)\n")
                (eval! r "#replique/reload {}")
                (is (= [["unresolved-var" 2 25 "Unresolved var: u/baz"]]
                       (said (lints! c core)))))

              (testing "the file itself changed on disk"
                (edited-file! core (str "(ns disk.core (:require [disk.util :as u]))\n"
                                        "(defn f [] (u/bar 1 2))\n"))
                (let [found (lints! c core)]
                  (is (= [] (said found)))
                  (is (true? (:changed found)))))

              (testing "and does not compile: the failure is the lint, and it is
              about the version on disk"
                (edited-file! core (str "(ns disk.core (:require [disk.util :as u]))\n"
                                        "(defn f [] (nope 1))\n"))
                (load! r core)
                (let [found (lints! c core)]
                  (is (= [["unresolved-symbol" 2 12 "Unresolved symbol: nope"]] (said found)))
                  (is (= "file" (:scope (first (:lints found)))))
                  (is (= (.lastModified (java.io.File. ^String core)) (:mtime found)))))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))

(deftest a-lint-can-be-told-it-is-wrong
  (testing "the project's levels"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (let [core (written-file! root "quiet/core.clj"
                                    (str "(ns quiet.core)\n"
                                         "(defn one [x] x)\n"
                                         "(defn f []\n"
                                         "  (one 1 2)\n"
                                         "  (let [a 1] (one 1 2)))\n"))]
            (load! r core)
            (when (analysing? c)
              (is (= [["invalid-arity" 4 3 "quiet.core/one is called with 2 args but expects 1"]
                      ["unused-binding" 5 9 "unused binding a"]
                      ["invalid-arity" 5 14 "quiet.core/one is called with 2 args but expects 1"]]
                     (said (lints! c core))))
              (testing "a level from .clj-kondo/config.edn, and :off"
                (written-file! (:directory (state/info)) ".clj-kondo/config.edn"
                               "{:linters {:invalid-arity {:level :warning} :unused-binding {:level :off}}}")
                (is (= [["invalid-arity" "warning"] ["invalid-arity" "warning"]]
                       (mapv (juxt :type :level) (:lints (lints! c core))))))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))

(deftest a-client-is-told-when-the-answer-may-have-changed
  (testing "a load changes the model, and every control connection hears of it"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (let [core (written-file! root "told/core.clj" "(ns told.core)\n(defn f [] 1)\n")]
            (when (analysing? c)
              (load! r core)
              (let [e (recv c)]
                (is (= ["event" "analysis"] [(:tag e) (:event e)]))
                (is (pos-int? (:generation e))))
              (testing "and an evaluation that changes nothing says nothing"
                (eval! r "(+ 1 2)")
                (is (= "reply" (:tag (request! c {:op :echo :id 2 :value 1})))))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))

(deftest what-an-expansion-hides-the-source-still-wrote
  (testing "a protocol implemented, a const, and a loop that destructures: the
  compiler sees the interface, the value and a second binding, and the source
  wrote the alias, the name and one binding that is used"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (written-file! root "hidden/p.clj"
                         "(ns hidden.p)\n(defprotocol P (m [this]))\n(def ^:const k 42)\n")
          (let [core (written-file! root "hidden/core.clj"
                                    (str "(ns hidden.core (:require [hidden.p :as p]))\n"
                                         "(defrecord R [] p/P (m [_] p/k))\n"
                                         "(def ^:private ^:const c 1)\n"
                                         "(defn f [xs] (loop [[a & b] xs] (if a (recur b) c)))\n"))]
            (load! r core)
            (when (analysing? c)
              (is (= [] (said (lints! c core))))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))

(deftest what-the-source-wrote-the-compiler-kept
  (testing "a #' and a var-ized symbol name a private var legally, a declare is
  not a definition, and a use is reported the way it was spelled"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (let [util (written-file! root "wrote/util.clj"
                                    (str "(ns wrote.util)\n"
                                         "(defn- secret [] 1)\n"
                                         "(def ^:private ^:dynamic *p* 1)\n"
                                         "(defn gone [] 1)\n"))
                core (written-file! root "wrote/core.clj"
                                    (str "(ns wrote.core (:require [wrote.util :as u :refer [gone]]))\n"
                                         "(declare later)\n"
                                         "(defn f [] (later) #'u/secret (binding [u/*p* 2] (gone)))\n"
                                         "(defn later [] 1)\n"))]
            (load! r core)
            (when (analysing? c)
              (is (= [] (said (lints! c core))))
              (testing "a referred var removed by a reload is a symbol that means nothing"
                (edited-file! util (str "(ns wrote.util)\n"
                                        "(defn- secret [] 1)\n"
                                        "(def ^:private ^:dynamic *p* 1)\n"))
                (eval! r "#replique/reload {}")
                (is (some #{["unresolved-symbol" 3 51 "Unresolved symbol: gone"]}
                          (said (lints! c core)))))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))

(deftest a-var-redefined-at-a-prompt-is-not-the-one-on-disk
  (testing "a call of it is not judged by an arity the disk does not have, until a
  load defines it again - and the clients are told both times"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (let [util (written-file! root "redef/util.clj" "(ns redef.util)\n(defn one [x] x)\n")
                core (written-file! root "redef/core.clj"
                                    (str "(ns redef.core (:require [redef.util :as u]))\n"
                                         "(defn f [] (u/one 1 2))\n"))
                wrong [["invalid-arity" 2 12 "redef.util/one is called with 2 args but expects 1"]]]
            (load! r core)
            (when (analysing? c)
              (is (= wrong (said (lints! c core))))
              (eval! r "(in-ns 'redef.util)")
              (eval! r "(defn one [x y] x)")
              (eval! r "(in-ns 'user)")
              (let [e (recv c)]
                (is (= ["event" "analysis"] [(:tag e) (:event e)])))
              (is (= [] (said (lints! c core))))
              (testing "and once the file is loaded, the disk's arity is the var's again"
                (load! r util)
                (is (= wrong (said (lints! c core)))))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))
