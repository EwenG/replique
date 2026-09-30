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

(deftest a-clojurescript-namespace-is-used-where-an-ns-form-requires-it
  (testing "the one place usages.util is used is the :require that loads it;
  u/twice is a use of twice, asked about as twice"
    (when (compiling?)
      (let [root (source-root!)]
        (written-file! root "nsused/util.cljs"
                       (str "(ns nsused.util)\n"
                            "(defn twice [x] (* 2 x))\n"))
        (written-file! root "nsused/core.cljs"
                       (str "(ns nsused.core\n"
                            "  (:require [nsused.util :as u]))\n"
                            "(defn run [] (u/twice 1) (nsused.util/twice 2))\n"))
        (with-cljs-repl r
          (eval! r "(require 'nsused.core)")
          (let [found (about {:op :usages :position :code
                              :ns "nsused.core" :text "nsused.util"})]
            (is (nil? (refused found)))
            (is (= "namespace" (:type (:symbol found))))
            (is (= [["nsused/core.cljs" 2 14 "nsused.core"]] (at root found)))
            (is (= ["require"] (mapv :declaration (:usages found))))))))))

(deftest what-a-clojurescript-name-is-written-as-a-part-of-is-said
  (testing "a defmethod and a :keys symbol, as in Clojure"
    (when (compiling?)
      (let [root (source-root!)]
        (written-file! root "roles/core.cljs"
                       (str "(ns roles.core)\n"
                            "(defmulti area :shape)\n"
                            "(defmethod area :square [{:keys [side]}] (* side side))\n"
                            "(defn total [xs] (map area xs))\n"))
        (with-cljs-repl r
          (eval! r "(require 'roles.core)")
          (let [places (fn [text]
                         (let [found (about {:op :usages :position :code :ns "roles.core" :text text})]
                           (is (nil? (refused found)))
                           (mapv (juxt :line :column :role) (:usages found))))]
            (is (= [[3 12 "defmethod"] [4 23 nil]] (places "area")))
            (is (= [[3 34 "destructuring"]] (places ":side")))))))))

(deftest a-clojurescript-local-is-asked-about-where-it-is-written
  (when (compiling?)
    (let [root (source-root!)
          path (written-file! root "locs/core.cljs"
                              (str "(ns locs.core)\n"
                                   "(defn f [x y]\n"
                                   "  (let [x (inc x)]\n"
                                   "    (+ x y)))\n"))]
      (with-cljs-repl r
        (eval! r "(require 'locs.core)")
        (let [at (fn [line column]
                   (about {:op :usages :position :code :ns "locs.core" :text "x"
                           :file path :line line :column column}))]
          (is (= {:type "local" :name "x"} (:symbol (at 4 8))))
          (is (= [[3 9 "binding"] [4 8 nil]]
                 (mapv (juxt :line :column :declaration) (:usages (at 4 8)))))
          (is (= [[2 10 "binding"] [3 16 nil]]
                 (mapv (juxt :line :column :declaration) (:usages (at 2 10))))))))))

(deftest what-the-ns-form-writes-is-a-place-and-says-it-is-one
  (testing "a :refer is where the name is written without being used, and the
  answer says which of the places is which - a rename walks them all, a list of
  call sites is the ones with nothing on them"
    (when (compiling?)
      (let [root (source-root!)]
        (written-file! root "decl/util.cljs"
                       (str "(ns decl.util)\n"
                            "(defn twice [x] (* 2 x))\n"))
        (written-file! root "decl/core.cljs"
                       (str "(ns decl.core\n"
                            "  (:require [decl.util :as u :refer [twice]]))\n"
                            "(defn run []\n"
                            "  (+ (twice 1) (u/twice 2)))\n"))
        (with-cljs-repl r
          (eval! r "(require 'decl.core)")
          (let [found (about {:op :usages :position :code
                              :ns "decl.core" :text "u/twice"})]
            (is (nil? (refused found)))
            (is (= [[2 38 "refer"] [4 7 nil] [4 17 nil]]
                   (mapv (juxt :line :column :declaration) (:usages found))))
            (testing "and the one that is not a call site is the one in the ns form"
              (is (= ["decl/core.cljs" 2 38 "decl.core"]
                     (first (at root (update found :usages
                                             #(filterv :declaration %)))))))))))))

(deftest a-name-written-in-code-that-does-not-run-says-so
  (testing "a use inside a #_ or a (comment ...) is a place the name is written
  and is not a call site, and the compiler is the only thing that can tell them
  apart: it resolves dead code without compiling it"
    (when (compiling?)
      (let [root (source-root!)]
        (written-file! root "cdead/util.cljs"
                       (str "(ns cdead.util)\n"
                            "(defn helper [x] x)\n"))
        (written-file! root "cdead/core.cljs"
                       (str "(ns cdead.core\n"
                            "  (:require [cdead.util :as u]))\n"
                            "(defn run [] (u/helper 1))\n"
                            "#_(u/helper 2)\n"
                            "(comment (u/helper 3))\n"))
        (with-cljs-repl r
          (eval! r "(require 'cdead.core)")
          (let [found (about {:op :usages :position :code
                              :ns "cdead.core" :text "u/helper"})
                live (remove #(or (:dead %) (:declaration %)) (:usages found))]
            (is (nil? (refused found)))
            (is (= [[3 15 nil] [4 4 "discard"] [5 11 "comment"]]
                   (mapv (juxt :line :column :dead) (:usages found))))
            (testing "the one that runs is the one with nothing on it"
              (is (= [[3 15]] (mapv (juxt :line :column) live))))))))))

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

(deftest implementing-a-protocol-is-a-use-of-the-protocol
  (testing "who implements this is the same question as where is this used,
  asked with the same op - which on the jvm falls out of how a protocol is
  compiled, and here had to be arranged: a ClojureScript protocol becomes a
  munged property name, so the symbol the source wrote is gone before the
  analyzer sees anything, and its compiler says so while it still has both"
    (when (compiling?)
      (let [root (source-root!)]
        (written-file! root "prot/defs.cljs"
                       (str "(ns prot.defs)\n"
                            "(defprotocol Shape\n"
                            "  (area [s]))\n"))
        (written-file! root "prot/uses.cljs"
                       (str "(ns prot.uses\n"
                            "  (:require [prot.defs :as p :refer [Shape area]]))\n"
                            "(defrecord Square [n]\n"
                            "  Shape\n"
                            "  (area [_] (* n n)))\n"
                            "(deftype Dot []\n"
                            "  p/Shape\n"
                            "  (area [_] 0))\n"
                            "(defn measure [s] (area s))\n"))
        (with-cljs-repl r
          (eval! r "(require 'prot.uses)")
          (let [found (about {:op :usages :position :code
                              :ns "prot.uses" :text "Shape"})]
            (is (nil? (refused found)))
            (is (= [["prot/uses.cljs" 2 38 "prot.uses"]
                    ["prot/uses.cljs" 4 3 "prot.uses"]
                    ["prot/uses.cljs" 7 3 "prot.uses"]]
                   (at root found))
                "the defrecord and the deftype, however each names it, beside
                what the ns form wrote")
            (is (= [2] (mapv :line (filter :declaration (:usages found))))
                "and only the ns form's is a declaration - the other two are
                places the protocol is put to use"))
          (let [found (about {:op :usages :position :code
                              :ns "prot.uses" :text "area"})]
            (is (= [["prot/uses.cljs" 2 44 "prot.uses"]
                    ["prot/uses.cljs" 9 20 "prot.uses"]]
                   (at root found))
                "a method answers its call sites and not the bodies that
                implement it: a type need not implement every method it
                could, which is where the two questions part company")))))))

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
                   (into #{} (map (juxt :source :line :column :from-ns)) found)))
            ;; AND EACH SAYS IT IS A MACRO CALL. In this model what makes a
            ;; place one is which index it was filed under, and the union
            ;; `usages' answers with is exactly where that would stop being
            ;; visible - so it is written onto each place as it is merged,
            ;; which is where Clojure carries it too.
            (is (every? :macro found))))))))

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
              (is (= ["stale/macros.clj"] (under root found :changed))
                  "the .clj is not a file this compiles, but it is the file that
  changed - and a reload loads it, so it is part of what a reload would do")
              (is (= ["stale/core.cljs"] (under root found :stale))
                  "and the one that expands the macro is stale all the same")
              (is (= [] (filter #{"stale/macros.clj"} (under root found :stale)))
                  "in one list or the other, never both")))

          (testing "and the reload loads the macro file before recompiling it"
            (eval! r "#replique/reload {}")
            (is (= ["20"] (mapv :value (filter #(= "ret" (:tag %))
                                               (eval! r "(stale.core/run)"))))
                "the new expansion is what is running")
            (let [found (about {:op :stale})]
              (is (= [] (under root found :stale))))))))))

(defn- gone
  "The file this test is about, where FOUND lists it as one the disk no longer
  has."
  [found]
  (filterv #{"vanished/lib.cljs"} (:deleted found)))

(deftest a-cljs-file-the-disk-no-longer-has-is-a-list-of-its-own
  (testing "a reload does not only compile: it drops the files this process
  compiled that are gone, and takes what they defined out of the compile
  environment.  Which is what switching a branch mostly does, and it is in
  neither list beside it - a file nothing answers to is not a file that
  changed, and there is nothing to compile it from"
    (when (compiling?)
      (let [root (source-root!)
            lib (written-file! root "vanished/lib.cljs"
                               (str "(ns vanished.lib)\n"
                                    "(def y 1)\n"))
            core (written-file! root "vanished/core.cljs"
                                (str "(ns vanished.core\n"
                                     "  (:require [vanished.lib :as l]))\n"
                                     "(def a l/y)\n"))]
        (with-cljs-repl r
          (eval! r "(require 'vanished.core)")
          ;; Its own file and not the whole list, for the reason `under' gives
          ;; about the others: the model is one per process.
          (is (= [] (gone (about {:op :stale}))))
          (.delete (java.io.File. ^String lib))
          ;; and the file that required it says what the branch says, which is
          ;; what a checkout does to both of them at once
          (edited-file! core (str "(ns vanished.core)\n"
                                  "(def a 1)\n"))
          (let [found (about {:op :stale})]
            (is (= ["vanished/core.cljs"] (under root found :changed)))
            (is (= [] (under root found :stale)))
            (testing "named as the model names it, which is the only way there
            is: what is gone is what nothing answers for"
              (is (= ["vanished/lib.cljs"] (gone found)))))
          (testing "and the reload drops it, so the list empties the way the
          others do"
            (eval! r "#replique/reload {}")
            (is (= [] (gone (about {:op :stale}))))))))))

(deftest the-cljs-staleness-answer-says-whether-there-is-anywhere-to-put-it
  (testing "a Clojure reload ends when the files have been loaded on this jvm;
  a ClojureScript one has a second act - the bodies have to be RUN in the
  runtime - so whether a runtime is there is part of what would happen if a
  reload were asked for, and belongs in the answer to what one would do"
    (when (compiling?)
      (testing "answered without starting anything, so a client asking what is
      stale does not open a port or start a node process by asking - which is
      also why it is false where no repl has ever been opened on the target"
        (is (contains? (about {:op :stale}) :connected)))
      (with-cljs-repl r
        (eval! r "(+ 1 1)")
        (testing "and true of a node runtime that exists, which is a node
        runtime that has dialled back"
          (is (true? (:connected (about {:op :stale}))))))
      (testing "the Clojure answer has no such key: the question does not
      exist there"
        (is (not (contains? (ask {:op :stale}) :connected)))))))

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

;;; Where a name of the host's is used

(deftest a-name-of-the-host-is-found-by-what-it-is
  (testing "a Closure var, a global and an export of a module are found however
  they were written - an alias, a :refer, an :import, in full - because the
  model files each under what it names"
    (when (compiling?)
      (let [root (source-root!)
            ;; A JavaScript module, where the bundler would have put one. There
            ;; is no node_modules here to build npm/ out of, and a module that
            ;; is not there fails the load and leaves the runtime a load that
            ;; failed - which every test after this one then runs in.
            out-dir (binding [cljs/*target* :node] (:out-dir (cljs/environment)))]
        (written-file! out-dir "npm/hostmod.js"
                       "export const $module = { readText(x) { return x; } };\n")
        (written-file! root "host/core.cljs"
                       (str "(ns host.core\n"
                            "  (:require [goog.string :as gstr :refer [trim]] [\"hostmod\" :as hm :refer [readText]])\n"
                            "  (:import [goog.math Long]))\n"
                            "(defn a [x] (gstr/trim x) (trim x) (goog.string/trim x) (gstr/startsWith x \"a\"))\n"
                            "(defn b [] (js/console.log 1) (.log js/console 2) (Long.fromNumber 3) (Long/fromNumber 4) Long)\n"
                            "(defn c [] (hm/readText \"x\") (readText \"y\") hm)\n"))
        (with-cljs-repl r
          (is (not-any? #(= "exception" (:tag %)) (eval! r "(require 'host.core)")))
          (let [usages (fn [text]
                         (let [found (about {:op :usages :position :code
                                             :ns "host.core" :text text})]
                           (is (nil? (refused found)))
                           found))
                places (fn [found] (mapv (juxt :line :column :member) (remove :declaration (:usages found))))
                declared (fn [found] (mapv (juxt :line :column :member :declaration)
                                           (filter :declaration (:usages found))))]
            (testing "one var of a Closure namespace, three ways of writing it"
              (doseq [text ["gstr/trim" "trim" "goog.string/trim"]]
                (let [found (usages text)]
                  (is (= {:type "host" :kind "goog-var" :ns "goog.string" :name "trim"}
                         (:symbol found)))
                  (is (= [[4 14 nil] [4 28 nil] [4 37 nil]] (places found)))
                  (testing "and the :refer that names it, which a rename rewrites too"
                    (is (= [[2 43 nil "refer"]] (declared found)))))))
            (testing "a global, and the one under it that is a name of its own"
              (is (= [[5 13 nil]] (places (usages "js/console.log")))))
            (testing "an export, through the alias and through the :refer"
              (is (= [[6 13 nil] [6 31 nil]] (places (usages "hm/readText"))))
              (is (= [[6 13 nil] [6 31 nil]] (places (usages "readText"))))
              (is (= [[2 76 nil "refer"]] (declared (usages "readText")))))
            (testing "and a package, which is every use of anything in it, each
            saying which"
              (is (= [[4 14 "trim"] [4 28 "trim"] [4 37 "trim"] [4 58 "startsWith"]]
                     (places (usages "gstr"))))
              (testing "and the :require that loads it, a namespace's own use"
                (is (= [[2 14 nil "require"] [2 43 "trim" "refer"]]
                       (declared (usages "gstr")))))
              (is (= [[6 13 "readText"] [6 31 "readText"] [6 45 nil]]
                     (places (usages "hm"))))
              (is (= [[2 76 "readText" "refer"]] (declared (usages "hm"))))
              (is (= [[5 13 "console.log"] [5 37 nil]] (places (usages "js/console"))))
              (testing "where an :import is written as a value, it is where it
              was written - it was filed at a line of another file, off the name
              of the namespace it stands for"
                (is (= [[5 52 "fromNumber"] [5 72 "fromNumber"] [5 91 nil]]
                       (places (usages "Long")))))
              (testing "and the :import is where the class is named, and says so"
                (is (= [[3 23 nil "import"]] (declared (usages "Long"))))))
            (testing "and asking adds nothing to the namespace: a Closure name
            written in full is its own require to the analyzer, which is not
            the question's to add"
              (usages "goog.math.Long/fromNumber")
              (usages "goog.object/get")
              (is (not (contains? (binding [cljs/*target* :node]
                                    ((requiring-resolve 'clojure.cljs.env/requires)
                                     (:cenv (cljs/environment)) 'host.core))
                                  'goog.object))))
            (testing "and the module is completed out of the runtime, as js/ is"
              (is (some #{"hm/readText"}
                        (map :candidate (:completions (about {:op :completions :position :code
                                                              :ns "host.core" :text "hm/rea"}))))))))))))
