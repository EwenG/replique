(ns replique.analysis-test
  "Where a name is used.

  Read out of what the compiler resolved, which not every clojure writes
  down: it is a fork of the compiler that does, and a process is running one
  or the other. So every test here is written for both - what a process that
  records it must answer, and what a process that does not must say instead -
  and it asks the process which it is rather than assuming.

  Under the one that does:

    clojure -M:test:analysis ...

  The files are written into temporary directories put on the classpath as
  the process runs, which is what `:add-libs' does to it as well. A file that
  the classpath cannot name is analysed by nothing, so a test that wrote its
  files anywhere else would be a test of the fallback only - and one of these
  is exactly that, on purpose."
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [replique.classpath :as classpath]
            [replique.ops]
            [replique.state :as state]
            [replique.test-client :as client
             :refer [control-client disconnect eval! repl-client request! with-process]]))

;;; A project to look at

(defn- written-file!
  "Write SOURCE into DIR under NAME, and answer the path of it."
  [dir name source]
  (let [f (java.io.File. (str dir) (str name))]
    (.mkdirs (.getParentFile f))
    (spit f source)
    (.getPath f)))

(defn- source-root!
  "A directory the process reads names off the classpath, and its path.

  Added to the loader the whole process shares rather than to the thread the
  test is on: what loads a file is the repl connection's thread, and what
  reads the classpath back is whichever thread answers the op. Which is the
  loader `add-libs' adds to, so a source root appearing under a running
  process is a thing that happens for real and not only here.

  And read again afterwards, because what says which entries of the classpath
  are directories is that reading - see `replique.classpath/directories'. A
  client that put a directory there does the same thing: `:add-libs' and
  `:sync-deps' both end in a reading, and `:update-classpath' is one asked
  for by itself."
  []
  (let [dir (client/temp-dir)]
    (.addURL state/class-loader (.toURL (.toURI (java.io.File. (str dir)))))
    ;; Through the loader the process shares, which is what every thread of a
    ;; running process reads the classpath through - a connection adopts it
    ;; before it answers anything, and the thread a test runs on is the one
    ;; thread of this process that has not. Without it the reading walks up
    ;; from whatever loader the test runner was left holding and finds no
    ;; source root at all.
    (state/adopt-class-loader!)
    (classpath/rescan!)
    dir))

(defn- load! [r path]
  (eval! r (str "#replique/load " (pr-str {:file path}))))

(defn- usages!
  "Ask where the name TEXT, read in NS, is used."
  [c ns text]
  (request! c {:op :usages :id 1 :position :code :ns ns :text text}))

(defn- analysing?
  "Whether the process running this test records what the compiler resolved."
  [c]
  (true? (:analysis (request! c {:op :process-info :id 1}))))

(defn- at
  "The usages FOUND holds, as the places they are - the file's own name, the
  line and the column - so that a test says where it expects them without
  writing out the temporary directory they are under."
  [found]
  (mapv (fn [{:keys [file line column from-ns]}]
          [(.getName (java.io.File. ^String file)) line column from-ns])
        (:usages found)))

(defn- refused
  "What the process said it could not do, or nil when it did it."
  [found]
  (when (= "error" (:tag found)) (:message found)))

;;; What only the compiler knows

(def ^:private util-clj
  (str "(ns probe.util)\n"
       "\n"
       "(defn twice [x] (* 2 x))\n"
       "\n"
       "(defn thrice [x] (+ x (twice x)))\n"))

(def ^:private core-clj
  (str "(ns probe.core\n"
       "  (:require [probe.util :as u]))\n"
       "\n"
       "(defn run []\n"
       "  (+ (u/twice 1)\n"
       "     (u/thrice 2)\n"
       "     (u/twice 3)))\n"
       "\n"
       "(def marker ::tag)\n"))

(deftest where-a-name-is-used-is-not-where-it-is-written
  (testing "clojure.core/let is written let where core is referred and c/let
  where it is aliased - so what a name is called is a different question from
  what it is, and only the process can join them"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (written-file! root "probe/util.clj" util-clj)
          (load! r (written-file! root "probe/core.clj" core-clj))
          (let [found (usages! c "probe.core" "u/twice")]
            (if (analysing? c)
              (do
                (testing "the name resolves the way :symbol resolves it, which is
                what the answer has to be headed with: twelve usages of
                probe.util/twice rather than twelve usages of u/twice"
                  (is (= {:type "function" :name "twice" :ns "probe.util"}
                         (select-keys (:symbol found) [:type :name :ns]))))
                (testing "both the ones written through the alias and the one
                written bare in the file that defines it - three spellings of
                nothing, one var"
                  (is (= [["core.clj" 5 7 "probe.core"]
                          ["core.clj" 7 7 "probe.core"]
                          ["util.clj" 5 24 "probe.util"]]
                         (at found))))
                (testing "each one spans the name as it is written there rather
                than the name of the var: seven characters where it is written
                u/twice and five where it is written twice, which is what lets
                a client replace it where it stands"
                  (is (= [7 7 5] (mapv (fn [{:keys [line column end-line end-column]}]
                                         (when (= line end-line) (- end-column column)))
                                       (:usages found)))))
                (testing "the file it defines is analysed too, without anything
                asking for it: a require compiles what it requires, and the
                compiler is what is being read"
                  (is (some (fn [{:keys [file]}] (string/ends-with? file "util.clj"))
                            (:usages found)))))
              (testing "a process whose compiler records nothing says so, and
              says what to start it on - rather than answering that the name is
              used nowhere, which is what an empty list would be read as"
                (is (string/includes? (or (refused found) "") "does not record")))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))

(deftest a-keyword-is-found-by-what-it-is-rather-than-by-how-it-is-written
  (testing "::tag is a keyword of whatever namespace the file is, and
  ::alias/tag of whatever that alias stands for - so the text is not the name"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (written-file! root "probe/util.clj" util-clj)
          (load! r (written-file! root "probe/core.clj" core-clj))
          (let [found (usages! c "probe.core" "::tag")]
            (if (analysing? c)
              (do
                (is (= {:type "keyword" :name "tag" :ns "probe.core"}
                       (select-keys (:symbol found) [:type :name :ns])))
                (is (= [["core.clj" 9 13 "probe.core"]] (at found))))
              (is (string/includes? (or (refused found) "") "does not record"))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))

(deftest loading-a-file-again-replaces-what-it-said
  (testing "a model that added to itself every time somebody pressed the load
  key would count one call as many times as the file has been loaded, and
  would go on holding a call that was deleted"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (written-file! root "probe/util.clj" util-clj)
          (let [path (written-file! root "probe/core.clj" core-clj)]
            (load! r path)
            (load! r path)
            (when (analysing? c)
              (testing "loaded twice and used as many times as it is written"
                (is (= 3 (count (:usages (usages! c "probe.core" "u/twice"))))))
              (testing "and a call taken out of the file is gone once the file
              has been loaded again - which is the whole reason the model is
              replaced a file at a time rather than added to"
                (written-file! root "probe/core.clj"
                               (string/replace core-clj "     (u/twice 3)" "     3"))
                (load! r path)
                (is (= 2 (count (:usages (usages! c "probe.core" "u/twice"))))))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))

(deftest what-a-file-means-does-not-change-because-it-was-analysed
  (testing "a compiler that records where every name was written has to write
  that down somewhere, and where it writes it is the metadata of the forms it
  read - so the risk is that it reaches the values the program itself can
  read.  A quoted symbol is the case that bites: it is a constant, and one
  carrying four more keys is a constant the compiler emits a map and a
  withMeta for.  A file that is mostly quoted symbols pays that thousands of
  times over, which is how sci/impl/namespaces.cljc - already split by a macro
  named avoid-method-too-large - went past the 64K method limit"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)]
        (try
          (load! r (written-file! root "probe/values.clj"
                                  (str "(ns probe.values)\n"
                                       "(defn quoted [] 'a-name)\n"
                                       "(defn quoted-list [] '(a b c))\n"
                                       "(defn a-vector [] [1 2 3])\n"
                                       "(defn a-map [] {:a 1})\n"
                                       "(defn annotated [] ^{:user :kept} [1 2])\n")))
          (let [answer (fn [code] (:value (client/frame-tagged (eval! r code) "ret")))]
            (testing "nothing the reader wrote down is on what the program gets
            back, which is what stock clojure gives for every one of these"
              (is (= ["nil" "nil" "nil" "nil"]
                     [(answer "(meta (probe.values/quoted))")
                      (answer "(meta (first (probe.values/quoted-list)))")
                      (answer "(meta (probe.values/a-vector))")
                      (answer "(meta (probe.values/a-map))")])))
            (testing "while what the program wrote is still there"
              (is (= "{:user :kept}" (answer "(meta (probe.values/annotated))")))))
          (finally
            (disconnect r)
            (client/delete-recursively root)))))))

(deftest a-file-the-classpath-cannot-name-is-loaded-all-the-same
  (testing "a scratch file, a file in a directory the project has not put on
  its paths, a file being tried out before it is saved where it belongs - all
  of them load, and none of them is analysed: the model names a file the way
  the classpath does, and there is no such name for these"
    (with-process [info nil]
      (let [outside (client/temp-dir)
            r (repl-client info)
            c (control-client info)]
        (try
          (load! r (written-file! outside "loose.clj"
                                  (str "(ns probe.loose)\n"
                                       "(defn twice [x] (* 2 x))\n"
                                       "(defn four [x] (twice (twice x)))\n")))
          (testing "loaded, which is what was asked for"
            (is (= "8" (-> (eval! r "(probe.loose/four 2)")
                           (client/frame-tagged "ret")
                           :value))))
          (when (analysing? c)
            (testing "and used nowhere, because nothing recorded it"
              (is (= [] (:usages (usages! c "probe.loose" "twice"))))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively outside)))))))

(deftest a-source-root-is-found-once-the-classpath-has-been-read-again
  (testing "which entries of the classpath are directories comes out of the
  same reading everything else does, so a directory put there under a running
  process is one this knows about once that reading has happened - and every
  way of putting one there ends in one: `:add-libs' and `:sync-deps' both do,
  and `:update-classpath' is a client asking for one by itself"
    (with-process [info nil]
      (let [dir (client/temp-dir)
            r (repl-client info)
            c (control-client info)]
        (try
          (.addURL state/class-loader (.toURL (.toURI (java.io.File. (str dir)))))
          (let [path (written-file! dir "probe/late.clj"
                                    (str "(ns probe.late)\n"
                                         "(defn twice [x] (* 2 x))\n"
                                         "(defn four [x] (twice (twice x)))\n"))]
            (testing "before the reading it is a directory the classpath cannot
            name a file under, so the file is loaded the way any file off the
            classpath is - which is loaded, and not analysed"
              (load! r path)
              (is (= "8" (-> (eval! r "(probe.late/four 2)")
                             (client/frame-tagged "ret")
                             :value)))
              (when (analysing? c)
                (is (= [] (:usages (usages! c "probe.late" "twice"))))))
            (testing "and after it the same load records where the name is used
            - both of them, which are the two calls written on one line"
              (is (pos? (:namespaces (request! c {:op :update-classpath :id 1}))))
              (load! r path)
              (when (analysing? c)
                (is (= 2 (count (:usages (usages! c "probe.late" "twice"))))))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively dir)))))))

(deftest a-file-shadowed-on-the-classpath-is-read-where-it-was-named
  (testing "two source roots holding the same relative path is an ordinary way
  to lay a project out, and only one of them is what that name reaches. The
  other is a file the classpath cannot name - and loading it under that name
  would load the first one, which is a good deal worse than not analysing it"
    (with-process [info nil]
      (let [first-root (source-root!)
            second-root (source-root!)
            r (repl-client info)]
        (try
          (written-file! first-root "probe/shadow.clj"
                         "(ns probe.shadow)\n(def which :the-first-root)\n")
          (load! r (written-file! second-root "probe/shadow.clj"
                                  "(ns probe.shadow)\n(def which :the-second-root)\n"))
          (is (= ":the-second-root" (-> (eval! r "probe.shadow/which")
                                        (client/frame-tagged "ret")
                                        :value)))
          (finally
            (disconnect r)
            (client/delete-recursively first-root)
            (client/delete-recursively second-root)))))))
