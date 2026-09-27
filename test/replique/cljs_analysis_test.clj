(ns replique.cljs-analysis-test
  "Where a ClojureScript name is used, and what the compiler has fallen behind.

  `replique.analysis-test' asked of the other compiler. The two questions are
  the same two and the ops are the same two ops - what tells them apart is the
  `:dialect' the request carries - but the model behind them is a model of its
  own (`clojure.cljs.analysis'), so none of it is covered by the Clojure tests.

  Written for both processes, as every ClojureScript test here is: the compiler
  is not a dependency of replique, and a process is running with it or without
  it. With it:

    clojure -M:test:cljs ...

  ONE PROCESS FOR THE WHOLE FILE, for `replique.cljs-ops-test's reason: the
  first ClojureScript question compiles cljs.core and the first repl connection
  starts node on top of it, which is seconds each. What that gives up is
  isolation, and THE MODEL IS PROCESS WIDE - one test's edited file is in every
  other test's answer to `:stale'. So every test works in a source root of its
  own and reads the answers back through `under', which keeps the files of that
  root and drops everybody else's. A test that asserted on the whole list would
  be a test of what the tests before it happened to leave behind.

  The files go into temporary directories put on the classpath as the process
  runs, which is where the ClojureScript driver looks for sources: its source
  paths default to the directory entries of the classpath, and a namespace it
  cannot find there it looks for as a resource."
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [replique.classpath :as classpath]
            [replique.cljs :as cljs]
            [replique.cljs-analysis :as cljs-analysis]
            [replique.core :as core]
            [replique.ops]
            [replique.state :as state]
            [replique.test-client :as client
             :refer [control-client disconnect eval! repl-client request!]]))

(defn- compiling?
  "Whether the process running this test has a ClojureScript compiler."
  []
  (cljs/available?))

;;; One process, and the handshake that is slow

(def ^:private the-process (atom nil))

(def ^:private slow
  "How long a client waits for a handshake here. A ClojureScript one compiles
  cljs.core and starts node before it answers."
  180000)

(defn- with-one-process [f]
  (let [dir (client/temp-dir)
        out *out*
        err *err*]
    (reset! the-process (core/start! {:directory dir :init false}))
    (try
      (binding [*out* out *err* err] (f))
      (finally (core/stop!) (reset! the-process nil)
               (client/delete-recursively dir)))))

(use-fixtures :once with-one-process)

;;; A project to look at

(defn- written-file!
  "Write SOURCE into DIR under NAME, and answer the path of it."
  [dir name source]
  (let [f (java.io.File. (str dir) (str name))]
    (.mkdirs (.getParentFile f))
    (spit f source)
    (.getPath f)))

(defn- edited-file!
  "SOURCE written over the file at PATH, and dated ahead of now.

  DATED, because a file edited in the same second the process compiled it is a
  file whose mtime says nothing changed - which is true of every file a test
  writes twice. A person editing in an editor takes longer than that and needs
  no help; a test has to say so."
  [path source]
  (spit path source)
  (.setLastModified (java.io.File. ^String path) (+ 30000 (System/currentTimeMillis)))
  path)

(defn- source-root!
  "A directory the process reads names off the classpath, and its path.

  `replique.analysis-test/source-root!' - added to the loader the whole process
  shares, and the classpath read again afterwards. The ClojureScript driver
  finds a namespace under it the same way: a source path it cannot name it
  looks for as a resource, and this is one."
  []
  (let [dir (client/temp-dir)]
    (.addURL state/class-loader (.toURL (.toURI (java.io.File. (str dir)))))
    (state/adopt-class-loader!)
    (classpath/rescan!)
    dir))

;;; Asking

(defn- ask [msg]
  (let [c (control-client @the-process)]
    (try (request! c (assoc msg :id 1))
         (finally (disconnect c)))))

(defn- about
  "The same request, asked about ClojureScript on node."
  [msg]
  (ask (assoc msg :dialect :cljs :target :node)))

(defmacro ^:private with-cljs-repl
  [sym & body]
  `(let [~sym (repl-client @the-process {:dialect :cljs :target :node} slow)]
     (try ~@body (finally (disconnect ~sym)))))

(defn- at
  "The usages FOUND holds, as the places they are: the file's name inside ROOT,
  the line, the column, and the namespace the name was written from."
  [root found]
  (mapv (fn [{:keys [file line column from-ns]}]
          [(subs (str file) (inc (count (str root)))) line column from-ns])
        (:usages found)))

(defn- under
  "The files FOUND lists under KEY that are inside ROOT, by their names there.

  WHICH IS HOW A TEST READS A PROCESS WIDE MODEL. `:stale' answers about every
  file the process has compiled, and the process compiled the other tests'
  files too - so what a test can assert on is its own root and nothing else."
  [root found key]
  (let [prefix (str root "/")]
    (vec (sort (keep (fn [{:keys [file]}]
                       (when (and file (string/starts-with? (str file) prefix))
                         (subs (str file) (count prefix))))
                     (get found key))))))

(defn- refused
  "What the process said it could not do, or nil when it did it."
  [found]
  (when (= "error" (:tag found)) (:message found)))

;;; Where a name is used

(deftest where-a-clojurescript-name-is-used-is-not-where-it-is-written
  (testing "u/twice is probe.util/twice written under an alias, and the two
  places it is called are the compiler's answer rather than a search's"
    (when (compiling?)
      (let [root (source-root!)]
        (written-file! root "usages/util.cljs"
                       (str "(ns usages.util)\n"
                            "(defn twice [x] (* 2 x))\n"))
        (written-file! root "usages/core.cljs"
                       (str "(ns usages.core\n"
                            "  (:require [usages.util :as u]))\n"
                            "(defn run []\n"
                            "  (+ (u/twice 1) (u/twice 2)))\n"))
        (with-cljs-repl r
          (eval! r "(require 'usages.core)")
          (let [found (about {:op :usages :position :code
                             :ns "usages.core" :text "u/twice"})]
            (is (nil? (refused found)))
            (is (= "usages.util" (:ns (:symbol found))))
            (is (= [["usages/core.cljs" 4 7 "usages.core"]
                    ["usages/core.cljs" 4 19 "usages.core"]]
                   (at root found)))
            (testing "and the definition is not among them: a def is where the
            name is, not a use of it"
              (is (not-any? #(= 2 (second %)) (at root found))))))))))

(deftest the-two-models-are-not-one
  (testing "a ClojureScript var and a Clojure var of the same name are two
  things, and each model answers only about its own"
    (when (compiling?)
      (let [root (source-root!)]
        (written-file! root "two/side.clj"
                       (str "(ns two.side)\n"
                            "(defn shared [x] x)\n"
                            "(defn caller [] (shared 1))\n"))
        (written-file! root "two/side.cljs"
                       (str "(ns two.side)\n"
                            "(defn shared [x] x)\n"
                            "(defn caller [] (shared 2))\n"))
        (with-cljs-repl cljs-r
          (eval! cljs-r "(require 'two.side)")
          (let [clj-r (repl-client @the-process)]
            (try
              (eval! clj-r (str "#replique/load "
                                (pr-str {:file (str root "/two/side.clj")})))
              (let [in-cljs (about {:op :usages :position :code
                                    :ns "two.side" :text "shared"})
                    in-clj (ask {:op :usages :position :code
                                 :ns "two.side" :text "shared"})]
                (is (nil? (refused in-cljs)))
                (is (nil? (refused in-clj)))
                (is (= [["two/side.cljs" 3 18 "two.side"]] (at root in-cljs)))
                (is (= [["two/side.clj" 3 18 "two.side"]] (at root in-clj))))
              (finally (disconnect clj-r)))))))))

(deftest a-keyword-is-found-by-what-it-is-and-not-by-what-it-is-written-as
  (testing "::tag written in usages.core is :usages.core/tag, which is the one
  thing about a keyword no reader of the text can work out"
    (when (compiling?)
      (let [root (source-root!)]
        (written-file! root "kw/one.cljs"
                       (str "(ns kw.one)\n"
                            "(def marker ::tag)\n"))
        (written-file! root "kw/two.cljs"
                       (str "(ns kw.two\n"
                            "  (:require [kw.one :as one]))\n"
                            "(def echoed ::one/tag)\n"))
        (with-cljs-repl r
          (eval! r "(require 'kw.two)")
          (eval! r "(require 'kw.one)")
          (let [found (about {:op :usages :position :code :ns "kw.one" :text "::tag"})]
            (is (nil? (refused found)))
            (is (= "keyword" (:type (:symbol found))))
            (is (= [["kw/one.cljs" 2 13 "kw.one"]
                    ["kw/two.cljs" 3 13 "kw.two"]]
                   (at root found)))))))))

(deftest a-class-is-a-question-for-clojure-only
  (testing "a .cljs file has no classes, so a ClojureScript question about one
  reaches neither the name nor any usage of it - and the same question asked of
  Clojure, in the process holding both files, is answered in full"
    (when (compiling?)
      (let [root (source-root!)]
        (written-file! root "cls/one.clj"
                       (str "(ns cls.one)\n"
                            "(defn now [] (java.util.Date.))\n"))
        (written-file! root "cls/one.cljs" "(ns cls.one)\n")
        (with-cljs-repl cljs-r
          (eval! cljs-r "(require 'cls.one)")
          (let [clj-r (repl-client @the-process)]
            (try
              (eval! clj-r (str "#replique/load "
                                (pr-str {:file (str root "/cls/one.clj")})))
              (let [in-cljs (about {:op :usages :position :code
                                    :ns "cls.one" :text "java.util.Date"})
                    in-clj (ask {:op :usages :position :code
                                 :ns "cls.one" :text "java.util.Date"})]
                (is (nil? (refused in-cljs))
                    "answered rather than refused: the process can answer, and
                    a name that means nothing is an ordinary answer")
                (is (nil? (:symbol in-cljs))
                    "the name itself reaches nothing - what a .cljs buffer
                    writes before a dot is a JavaScript object, so the jvm's
                    classpath is not asked at all (replique.names/class-named)")
                (is (= [] (at root in-cljs)))
                (is (= "class" (:type (:symbol in-clj))))
                (is (= [["cls/one.clj" 2 15 "cls.one"]] (at root in-clj))))
              (finally (disconnect clj-r)))))))))

(deftest a-repl-started-on-a-main-recorded-the-program-it-compiled
  (testing "the :main compile is the one that reads the whole dependency graph
  off disk, and it goes straight to the driver rather than through the
  compiler's repl - so it is the one compilation that has to install the sink
  itself, and a repl that did not would have built the program and recorded
  none of it"
    (when (compiling?)
      (let [root (source-root!)]
        (written-file! root "boot/dep.cljs"
                       (str "(ns boot.dep)\n"
                            "(defn used [] 1)\n"))
        (written-file! root "boot/program.cljs"
                       (str "(ns boot.program\n"
                            "  (:require [boot.dep :as d]))\n"
                            "(defn go [] (d/used))\n"))
        (let [r (repl-client @the-process
                            {:dialect :cljs :target :node :main "boot.program"}
                            slow)]
          (try
            (is (= "boot.program" (:main (:hello r))) (pr-str (:hello r)))
            (let [found (about {:op :usages :position :code
                                :ns "boot.program" :text "d/used"})]
              (is (nil? (refused found)))
              (is (= [["boot/program.cljs" 3 14 "boot.program"]] (at root found))))
            (finally (disconnect r))))))))

(deftest a-macro-call-is-recorded-although-no-name-reaches-it-yet
  (testing "the model holds the calls the source WROTE to a macro, keyed by the
  JVM var that expanded it, beside the uses of the ClojureScript var of the same
  name - one name can be both, as cljs.core/str is when it is called and when
  it is passed to map"
    ;; ASKED OF THE MODEL RATHER THAN THROUGH `:usages', because nothing can
    ;; reach it through the op yet: `:symbol' resolves no macro at all in a
    ;; .cljs buffer - the macros of a ClojureScript namespace live in the
    ;; compile environment's macro view and not in what `ns-map' answers, so
    ;; there is no name to hand the op. The model's half is right and is what
    ;; this pins; the naming half is `replique.names's to grow.
    (when (compiling?)
      (let [root (source-root!)]
        (written-file! root "mac/defs.clj"
                       (str "(ns mac.defs)\n"
                            "(defmacro shout [x] (list 'cljs.core/str x \"!\"))\n"))
        (written-file! root "mac/uses.cljs"
                       (str "(ns mac.uses\n"
                            "  (:require-macros [mac.defs :as d]))\n"
                            "(defn loud [] (d/shout \"a\"))\n"))
        (with-cljs-repl r
          (eval! r "(require 'mac.uses)")
          (let [found (cljs-analysis/usages 'mac.defs/shout)]
            (is (= #{["mac/uses.cljs" 3 16 'mac.uses]}
                   (into #{} (map (juxt :source :line :column :from-ns)) found)))))))))

;;; What has moved on

(deftest an-edited-file-is-changed-and-a-file-that-expands-its-macro-is-stale
  (testing "the two lists are two different facts, and the second is the half
  nobody can work out by looking at their buffers: a .cljs file holds the
  expansion the macro made, so editing the .clj it expands from leaves the
  .cljs wrong with nothing in it edited"
    (when (compiling?)
      (let [root (source-root!)
            macros (written-file! root "stale/macros.clj"
                                  (str "(ns stale.macros)\n"
                                       "(defmacro twice [x] (list 'cljs.core/+ x x))\n"))
            core (written-file! root "stale/core.cljs"
                                (str "(ns stale.core\n"
                                     "  (:require-macros [stale.macros :as m]))\n"
                                     "(defn run [] (m/twice 1))\n"))]
        (with-cljs-repl r
          (eval! r "(require 'stale.core)")
          (testing "nothing edited, nothing to load"
            (let [found (about {:op :stale})]
              (is (= [] (under root found :changed)))
              (is (= [] (under root found :stale)))))

          (testing "the file itself edited"
            (edited-file! core (str "(ns stale.core\n"
                                    "  (:require-macros [stale.macros :as m]))\n"
                                    "(defn run [] (m/twice 2))\n"))
            (let [found (about {:op :stale})]
              (is (= ["stale/core.cljs"] (under root found :changed)))
              (is (= [] (under root found :stale)))))

          (testing "and a reload puts it back in step, here and in the runtime"
            (let [frames (eval! r "#replique/reload {}")
                  value (:value (first (filter #(= "ret" (:tag %)) frames)))]
              (is (string/includes? (str value) "stale/core.cljs"))
              (is (= ["4"] (mapv :value (filter #(= "ret" (:tag %))
                                                (eval! r "(stale.core/run)"))))
                  "the runtime is running the file that is on disk"))
            (let [found (about {:op :stale})]
              (is (= [] (under root found :changed)))
              (is (= [] (under root found :stale)))))

          (testing "the MACRO file edited, which changes no .cljs file at all"
            (edited-file! macros (str "(ns stale.macros)\n"
                                      "(defmacro twice [x] (list 'cljs.core/* x 10))\n"))
            (let [found (about {:op :stale})]
              (is (= [] (under root found :changed))
                  "no .cljs file on disk is newer than what was compiled")
              (is (= ["stale/core.cljs"] (under root found :stale))
                  "and the one that expands the macro is stale all the same")))

          (testing "and the reload loads the macro file before recompiling it"
            (eval! r "#replique/reload {}")
            (is (= ["20"] (mapv :value (filter #(= "ret" (:tag %))
                                               (eval! r "(stale.core/run)"))))
                "the new expansion is what is running")
            (let [found (about {:op :stale})]
              (is (= [] (under root found :stale))))))))))

(deftest a-definition-a-reload-removed-stops-resolving
  (testing "a def deleted from a file is pruned from the compile environment,
  which is what makes a file still using it warn rather than compile"
    (when (compiling?)
      (let [root (source-root!)
            core (written-file! root "prune/core.cljs"
                               (str "(ns prune.core)\n"
                                    "(defn kept [] 1)\n"
                                    "(defn dropped [] 2)\n"))]
        (with-cljs-repl r
          (eval! r "(require 'prune.core)")
          (is (= ["2"] (mapv :value (filter #(= "ret" (:tag %))
                                            (eval! r "(prune.core/dropped)")))))
          (edited-file! core (str "(ns prune.core)\n"
                                  "(defn kept [] 1)\n"))
          (eval! r "#replique/reload {}")
          (let [frames (eval! r "(prune.core/dropped)")]
            (is (= ["No such var: prune.core/dropped"]
                   (mapv :message (filter #(= "exception" (:tag %)) frames)))
                "the name no longer resolves in the compile environment")))))))

(deftest a-macro-a-reload-removed-stops-expanding
  (testing "the jvm half of it: the Clojure macro files a ClojureScript reload
  loads on the way are pruned the way a Clojure reload prunes them, so a macro
  deleted from a file this process analysed is gone from the namespace rather
  than left there to expand a body nothing holds any more"
    (when (compiling?)
      (let [root (source-root!)
            macros (written-file! root "expanded/macros.clj"
                                  (str "(ns expanded.macros)\n"
                                       "(defmacro kept [] 1)\n"
                                       "(defmacro dropped [] 2)\n"))
            said (fn [r code]
                   (mapv :value (filter #(= "ret" (:tag %)) (eval! r code))))]
        (written-file! root "expanded/core.cljs"
                       (str "(ns expanded.core\n"
                            "  (:require-macros [expanded.macros :refer [kept]]))\n"
                            "(def a (kept))\n"))
        (with-cljs-repl r
          (let [clj-r (repl-client @the-process)]
            (try
              ;; analysed on the Clojure side as well, which is the session this
              ;; is about - a macro file the Clojure model has never seen is
              ;; loaded again and not pruned, having no record of what it defined
              (eval! clj-r (str "#replique/load " (pr-str {:file macros})))
              (eval! r "(require 'expanded.core)")
              (is (= ["1"] (said r "expanded.core/a")))
              (is (= ["#'expanded.macros/dropped"]
                     (said clj-r "(resolve 'expanded.macros/dropped)")))
              (edited-file! macros (str "(ns expanded.macros)\n"
                                        "(defmacro kept [] 1)\n"))
              (eval! r "#replique/reload {}")
              (testing "the macro the file stopped defining is unmapped on the jvm"
                (is (= ["nil"] (said clj-r "(resolve 'expanded.macros/dropped)"))))
              (testing "and the one it still defines is where it was"
                (is (= ["#'expanded.macros/kept"]
                       (said clj-r "(resolve 'expanded.macros/kept)"))))
              (finally (disconnect clj-r)))))))))

;;; A process that cannot be asked at all is `replique.cljs-ops-test's, with
;;; every other op it cannot answer: the refusal is `:usages' and `:stale'
;;; getting the same "no-cljs" as `:namespaces', and the point of it is the list
;;; rather than one more test of one more op.
