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

(defn- source-root!* [dir]
  (let [dir (str dir)]
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

(defn- source-root!
  "A directory the process reads names off the classpath, and its path.

  Added to the loader the whole process shares rather than to the thread the
  test is on: what loads a file is the repl connection's thread, and what
  reads the classpath back is whichever thread answers the op. Which is the
  loader `add-libs' adds to, so a source root appearing under a running
  process is a thing that happens for real and not only here.

  And read again afterwards, because what says which places a path can be
  named under is that reading - see `replique.classpath/naming-anchors'. A
  client that put a directory there does the same thing: `:add-libs' and
  `:sync-deps' both end in a reading, and `:update-classpath' is one asked
  for by itself.

  UNDER A DIRECTORY GIVEN, where a test needs the source root to be part of
  the project rather than beside it: what a process answers about its own
  application is scoped to the directory it was started on - see
  `replique.analysis/unread' - so a test of that answer has to write the
  application where the process would look for it."
  ([] (source-root!* (client/temp-dir)))
  ([parent]
   (let [dir (java.io.File. (str parent) "src")]
     (.mkdirs dir)
     (source-root!* (.getPath dir)))))

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

(deftest what-the-ns-form-writes-is-a-place-and-says-it-is-one
  (testing "a :refer and an :import are where a name is written without being
  used - what makes the short name mean the var, and what a rename has to rewrite
  along with the call sites - so they are in the list and carry :declaration"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (written-file! root "decl/lib.clj"
                         (str "(ns decl.lib)\n"
                              "(defn twice [x] (* 2 x))\n"))
          (load! r (written-file! root "decl/a.clj"
                                  (str "(ns decl.a\n"
                                       "  (:require [decl.lib :as lib :refer [twice]])\n"
                                       "  (:import [java.io File]))\n"
                                       "(defn run [] (twice 1))\n"
                                       "(def f (File. \"x\"))\n")))
          (if (analysing? c)
            (let [var (usages! c "decl.a" "twice")
                  class (usages! c "decl.a" "File")
                  decl (fn [found] (mapv (juxt :line :column :declaration)
                                         (:usages found)))]
              (testing "the :refer is in one list with the call, marked for what
              it is"
                (is (= [[2 39 "refer"] [4 15 nil]] (decl var))))
              (testing "and so is the :import, at the name the spec writes"
                (is (= [[3 21 "import"] [5 9 nil]] (decl class))))
              (testing "a client that wants the call sites alone can have them,
              which is the whole point of saying which is which"
                (is (= [[4 15 nil]] (remove #(nth % 2) (decl var))))))
            (is (string/includes? (or (refused (usages! c "decl.a" "twice")) "")
                                  "does not record")))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))

(deftest a-name-written-in-code-that-does-not-run-says-so
  (testing "a use inside a #_ or a (comment ...) is a place the name is written
  and is not a call site, and the process is the only thing that can tell them
  apart - it resolves dead code without compiling it, and already acts on the
  difference where it prunes"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (written-file! root "dead/lib.clj"
                         (str "(ns dead.lib)\n"
                              "(defn helper [x] x)\n"))
          (load! r (written-file! root "dead/a.clj"
                                  (str "(ns dead.a\n"
                                       "  (:require [dead.lib :as lib]))\n"
                                       "(defn run [] (lib/helper 1))\n"
                                       "#_(lib/helper 2)\n"
                                       "(comment (lib/helper 3))\n")))
          (if (analysing? c)
            (let [found (usages! c "dead.a" "lib/helper")]
              (is (= [[3 15 nil] [4 4 "discard"] [5 11 "comment"]]
                     (mapv (juxt :line :column :dead) (:usages found))))
              (testing "the one that runs is the one with nothing on it"
                (is (= [[3 15]] (->> (:usages found)
                                     (remove :dead)
                                     (mapv (juxt :line :column)))))))
            (is (string/includes? (or (refused (usages! c "dead.a" "lib/helper")) "")
                                  "does not record")))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))

(deftest a-protocol-is-used-where-it-is-implemented
  (testing "a deftype, a defrecord and a reify name the interface the protocol
  generated by the time the compiler sees it, so which types implement a
  protocol is a thing only the process knows - and it is the same question as
  where the protocol is used"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (written-file! root "probe/proto.clj"
                         (str "(ns probe.proto)\n"
                              "(defprotocol Shape\n"
                              "  (area [this]))\n"))
          (load! r (written-file! root "probe/shapes.clj"
                                  (str "(ns probe.shapes\n"
                                       "  (:require [probe.proto :as p]))\n"
                                       "(deftype Square [side]\n"
                                       "  p/Shape\n"
                                       "  (area [this] (* side side)))\n"
                                       "(defn measure [x] (p/area x))\n")))
          (if (analysing? c)
            (do
              (testing "the type implementing it is a place the protocol is used,
              named where the protocol is written rather than where the type is"
                (is (= [["shapes.clj" 4 3 "probe.shapes"]]
                       (at (usages! c "probe.proto" "Shape")))))
              (testing "while a method is used where it is called: a type need not
              implement every method it could, so a method answered with its
              protocol's implementations would be answering something else"
                (is (= [["shapes.clj" 6 20 "probe.shapes"]]
                       (at (usages! c "probe.shapes" "p/area"))))))
            (testing "a process that recorded nothing has nothing to say about
            either of them"
              (is (= [] (at (usages! c "probe.proto" "Shape"))))))
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
          (let [said (client/printed
                      (load! r (written-file! outside "loose.clj"
                                              (str "(ns probe.loose)\n"
                                                   "(defn twice [x] (* 2 x))\n"
                                                   "(defn four [x] (twice (twice x)))\n")))
                      "err")]
            (when (analysing? c)
              (testing "and says it was not analysed, where this process could
              have analysed it: the fallback is otherwise invisible until
              something is asked about the file and answered with nothing"
                (is (string/includes? said "without analysing it")))))
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

(defn- linked!
  "Make LINK a symbolic link to TARGET, and answer LINK.

  The three shapes a link takes around a project are all here: a link to a
  source tree, which is how one is worked on under a name that outlives the
  worktree behind it; a link to one file inside a source tree, which is how a
  file is shared between several of them; and a link to a package under a
  source root, which is how one process reads a tree that can be swapped for
  another without restarting it."
  [link target]
  (let [^java.io.File f (java.io.File. ^String (str link))]
    (.mkdirs (.getParentFile f))
    (java.nio.file.Files/createSymbolicLink
     (.toPath f)
     (java.nio.file.Paths/get (str target) (make-array String 0))
     (make-array java.nio.file.attribute.FileAttribute 0))
    (str link)))

(defn- probe-clj
  "A file naming the namespace NAME, with one function used twice in another."
  [name]
  (str "(ns probe." name ")\n"
       "(defn twice [x] (* 2 x))\n"
       "(defn four [x] (twice (twice x)))\n"))

(deftest a-project-reached-through-a-link-is-named-the-way-the-classpath-names-it
  (testing "the directory a process was started in is resolved by the kernel
  before the process ever sees it, and an editor naming a file resolves
  nothing - so a project worked on through a link to it is one whose sources
  the process holds under a directory it spells differently. Every file loaded
  from that editor would be a file the classpath cannot name, and every load
  would record nothing at all: silently, since the code still loads"
    (with-process [info nil]
      (let [root (source-root!)
            elsewhere (client/temp-dir)
            link (linked! (java.io.File. (str elsewhere) "link") root)
            r (repl-client info)
            c (control-client info)]
        (try
          (written-file! root "probe/linked.clj" (probe-clj "linked"))
          (load! r (str (java.io.File. ^String link "probe/linked.clj")))
          (testing "loaded"
            (is (= "8" (-> (eval! r "(probe.linked/four 2)")
                           (client/frame-tagged "ret")
                           :value))))
          (when (analysing? c)
            (testing "and recorded, both calls of it"
              (is (= [["linked.clj" 3 17 "probe.linked"]
                      ["linked.clj" 3 24 "probe.linked"]]
                     (at (usages! c "probe.linked" "twice"))))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively elsewhere)
            (client/delete-recursively root)))))))

(deftest a-file-linked-into-a-source-root-is-named-under-that-root
  (testing "the other way a link stands between a name and a file, and the
  reason the two names are tried in the order they are: this file is under a
  source root by the name it was opened and loaded under, and somewhere else
  entirely by the name it resolves to. Resolving first would lose it - which
  is how one file is shared between several worktrees"
    (with-process [info nil]
      (let [root (source-root!)
            elsewhere (client/temp-dir)
            real (written-file! elsewhere "shared.clj" (probe-clj "shared"))
            link (linked! (java.io.File. (str root) "probe/shared.clj") real)
            r (repl-client info)
            c (control-client info)]
        (try
          (load! r link)
          (testing "loaded"
            (is (= "8" (-> (eval! r "(probe.shared/four 2)")
                           (client/frame-tagged "ret")
                           :value))))
          (when (analysing? c)
            (testing "and recorded under the name the classpath gives it, which
            is the one it was loaded under - a namespace of its own, so that
            what the model holds about it was put there by this load and not by
            another test's"
              (is (= [["shared.clj" 3 17 "probe.shared"]
                      ["shared.clj" 3 24 "probe.shared"]]
                     (at (usages! c "probe.shared" "twice"))))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively elsewhere)
            (client/delete-recursively root)))))))

(deftest a-file-linked-into-a-project-reached-through-a-link-is-named-all-the-same
  (testing "the two links at once, which is neither of the two above and is
  what a worktree opened under a name that outlives it looks like when it also
  shares a file with its siblings. Written, the file is under no source root,
  because the project is spelt by the link to it; resolved, it is under none
  either, because the file resolves out of the tree altogether. So both
  readings give up on a file the classpath holds perfectly well, and the load
  records nothing - silently, and for everything that load required, which is
  a whole system where the file is the one that starts it"
    (with-process [info nil]
      (let [root (source-root!)
            elsewhere (client/temp-dir)
            real (written-file! elsewhere "both.clj" (probe-clj "both"))
            _ (linked! (java.io.File. (str root) "probe/both.clj") real)
            link (linked! (java.io.File. (str elsewhere) "link") root)
            r (repl-client info)
            c (control-client info)]
        (try
          (load! r (str (java.io.File. ^String link "probe/both.clj")))
          (testing "loaded"
            (is (= "8" (-> (eval! r "(probe.both/four 2)")
                           (client/frame-tagged "ret")
                           :value))))
          (when (analysing? c)
            (testing "and recorded, under the name the classpath gives it -
            which is neither of the two names it was reached by"
              (is (= [["both.clj" 3 17 "probe.both"]
                      ["both.clj" 3 24 "probe.both"]]
                     (at (usages! c "probe.both" "twice"))))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively elsewhere)
            (client/delete-recursively root)))))))

(deftest a-file-under-a-package-linked-below-a-source-root-is-named-through-it
  (testing "the shape no resolution can find, and the one a process reads a
  swappable tree through: the source root is a real directory that stays where
  it is - an entry of the classpath is resolved once and never again - and the
  package under it is a link into a tree somewhere else, which is resolved on
  every lookup and can therefore be made to point somewhere else. A file
  opened where it really is, which is where it is edited and where its git is,
  is under the root by neither of its names: the root does not resolve to the
  tree, and the file resolves out of the root. What names it is the pair the
  classpath walk wrote down as it crossed the link"
    (with-process [info nil]
      (let [root (source-root!)
            elsewhere (client/temp-dir)
            far (client/temp-dir)
            staged (written-file! elsewhere "probe/staged.clj" (probe-clj "staged"))
            shared (linked! (java.io.File. (str elsewhere) "probe/inboth.clj")
                            (written-file! far "inboth.clj" (probe-clj "inboth")))
            _ (linked! (java.io.File. (str root) "probe")
                       (java.io.File. (str elsewhere) "probe"))
            r (repl-client info)
            c (control-client info)]
        (try
          ;; The link was made after this root was read, and a link under an
          ;; entry moves without the classpath moving - see
          ;; `replique.classpath/naming-anchors'
          (classpath/rescan!)
          (load! r staged)
          (testing "loaded"
            (is (= "8" (-> (eval! r "(probe.staged/four 2)")
                           (client/frame-tagged "ret")
                           :value))))
          (when (analysing? c)
            (testing "and recorded under the name the classpath gives it,
            which is the way down to the link with the rest of the path after
            it - neither end of that name is a piece of the path it was loaded
            under"
              (is (= [["staged.clj" 3 17 "probe.staged"]
                      ["staged.clj" 3 24 "probe.staged"]]
                     (at (usages! c "probe.staged" "twice"))))))
          (testing "and a file shared between the trees, which is a link
          inside the one the link leads to: two links deep, and under the root
          by no reading that resolves the file itself"
            (load! r shared)
            (is (= "8" (-> (eval! r "(probe.inboth/four 2)")
                           (client/frame-tagged "ret")
                           :value)))
            (when (analysing? c)
              (is (= [["inboth.clj" 3 17 "probe.inboth"]
                      ["inboth.clj" 3 24 "probe.inboth"]]
                     (at (usages! c "probe.inboth" "twice"))))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively far)
            (client/delete-recursively elsewhere)
            (client/delete-recursively root)))))))

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

;;; Loading again what changed

(defn- edited-file!
  "Write SOURCE into DIR under NAME, as a file edited since it was read.

  The time it was last modified is pushed forward rather than left to the
  clock, because that time is the whole of what says a file changed and a
  filesystem writes it as coarsely as it likes. Two writes inside one tick of
  it carry one time, and the second edit would be a file nothing had
  touched - which is a test that passes or fails on how fast the machine is."
  [dir name source]
  (let [path (written-file! dir name source)]
    (.setLastModified (java.io.File. ^String path) (+ (System/currentTimeMillis) 10000))
    path))

(defn- deleted-file!
  "Delete DIR/NAME, and answer the path it was at."
  [dir name]
  (let [f (java.io.File. (str dir) (str name))]
    (.delete f)
    (.getPath f)))

(defn- value!
  "What the repl answered CODE with."
  [r code]
  (:value (client/frame-tagged (eval! r code) "ret")))

(defn- reloaded!
  "Ask for everything that changed to be loaded, and answer what was said.

  The files, printed the way a repl prints a value, or the refusal where this
  process kept no track of what it compiled - one string either way, because
  what a test wants to say about both is the same sentence."
  [r]
  (let [frames (eval! r "#replique/reload {}")]
    (or (:value (client/frame-tagged frames "ret"))
        (:message (client/frame-tagged frames "exception")))))

(deftest a-file-edited-since-it-was-read-is-loaded-again
  (testing "which files those are is a question about the disk and the model
  together: what was loaded, and what has been written since. Nobody names
  the files - the point of asking is not knowing which they are"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (written-file! root "probe/edited.clj"
                         "(ns probe.edited)\n(defn twice [x] (* 2 x))\n")
          (load! r (written-file! root "probe/uses_edited.clj"
                                  (str "(ns probe.uses-edited\n"
                                       "  (:require [probe.edited :as e]))\n"
                                       "(defn four [x] (e/twice (e/twice x)))\n")))
          (is (= "8" (value! r "(probe.uses-edited/four 2)")))
          (edited-file! root "probe/edited.clj"
                        "(ns probe.edited)\n(defn twice [x] (* 3 x))\n")
          (if (analysing? c)
            (do
              (testing "the file that changed, and that one only: the file
              calling it goes through the var every time it runs, so what it
              finds is the new definition without being compiled at all"
                (is (= "[\"probe/edited.clj\"]" (reloaded! r)))
                (is (= "18" (value! r "(probe.uses-edited/four 2)"))))
              (testing "and asking again loads nothing, because loading a file
              is what records the time it was last modified: a file read a
              moment ago is a file that has not changed since"
                (is (= "[]" (reloaded! r)))))
            (testing "a process whose compiler kept no track of what it
            compiled cannot know what changed, and says so rather than
            answering that nothing did"
              (is (string/includes? (reloaded! r) "keep track of what it compiled"))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))

(deftest a-reload-says-what-it-is-loading-while-it-loads-it
  (testing "the value is the list and the value arrives when it stops being
  useful: a reload of forty files is half a minute of a repl that looks
  stopped, and the file it is inside is the one thing worth knowing about it -
  both while it is working and when it is not coming back"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (written-file! root "probe/said.clj" "(ns probe.said)\n(def x 1)\n")
          (load! r (written-file! root "probe/says.clj"
                                  (str "(ns probe.says\n"
                                       "  (:require [probe.said :as s]))\n"
                                       "(def y s/x)\n")))
          (edited-file! root "probe/said.clj" "(ns probe.said)\n(def x 2)\n")
          (when (analysing? c)
            (let [frames (eval! r "#replique/reload {}")
                  said (client/printed frames "out")]
              (testing "one line per file, named before it is loaded rather
              than after: the file that never finishes compiling is then the
              last line the client received"
                (is (= ["  1/1 probe/said.clj"]
                       (filterv #(re-matches #"\s+\d+/\d+ .*" %)
                                (string/split-lines said)))))
              (testing "and the value is still the value"
                (is (= "[\"probe/said.clj\"]"
                       (:value (client/frame-tagged frames "ret")))))
              (testing "a reload with nothing to load names no file"
                (is (empty? (filterv #(re-matches #"\s+\d+/\d+ .*" %)
                                     (string/split-lines
                                      (client/printed (eval! r "#replique/reload {}")
                                                      "out"))))))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))

(deftest a-file-that-expands-a-macro-of-an-edited-file-is-loaded-too
  (testing "a macro is expanded where it is used, so a file that uses one
  holds the old expansion until it is compiled again - editing a macro leaves
  every file that expands it wrong, and not one of those files changed. Which
  only the compiler can know, because what expanded what is not written in
  either file"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (written-file! root "probe/macro.clj"
                         (str "(ns probe.macro)\n"
                              "(defmacro twice [x] `(* 2 ~x))\n"))
          (load! r (written-file! root "probe/expands.clj"
                                  (str "(ns probe.expands\n"
                                       "  (:require [probe.macro :as m]))\n"
                                       "(defn four [x] (m/twice (m/twice x)))\n")))
          (is (= "8" (value! r "(probe.expands/four 2)")))
          (edited-file! root "probe/macro.clj"
                        (str "(ns probe.macro)\n"
                             "(defmacro twice [x] `(* 3 ~x))\n"))
          (if (analysing? c)
            (do
              (testing "both files, the macro first - a file is loaded before
              the files that expand what it defines, or they would expand the
              old one again and the reload would have changed nothing"
                (is (= "[\"probe/macro.clj\" \"probe/expands.clj\"]" (reloaded! r))))
              (testing "and the expansion the second file holds is the new one"
                (is (= "18" (value! r "(probe.expands/four 2)")))))
            (testing "where nothing recorded which form expanded which macro,
            there is no such thing to ask for"
              (is (string/includes? (reloaded! r) "keep track of what it compiled"))
              (is (= "8" (value! r "(probe.expands/four 2)")))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))

(deftest a-definition-a-file-no-longer-has-is-unmapped-by-the-reload
  (testing "loading a file defines what the file says and cannot un-define
  what it no longer says, so a var whose def form was deleted would stay
  interned and go on answering for a name the codebase does not have"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (written-file! root "probe/pruned.clj"
                         (str "(ns probe.pruned)\n"
                              "(defn kept [] 1)\n"
                              "(defn dropped [] 2)\n"))
          (load! r (written-file! root "probe/uses_pruned.clj"
                                  (str "(ns probe.uses-pruned\n"
                                       "  (:require [probe.pruned :as p]))\n"
                                       "(defn both [] [(p/kept) (p/dropped)])\n")))
          (is (= "[1 2]" (value! r "(probe.uses-pruned/both)")))
          (edited-file! root "probe/pruned.clj"
                        (str "(ns probe.pruned)\n"
                             "(defn kept [] 1)\n"))
          (if (analysing? c)
            (let [frames (eval! r "#replique/reload {}")]
              (testing "the file is loaded, and the name it stopped defining
              resolves to nothing afterwards"
                (is (= "[\"probe/pruned.clj\"]"
                       (:value (client/frame-tagged frames "ret"))))
                (is (= "nil" (value! r "(resolve 'probe.pruned/dropped)"))))
              (testing "while what the file still defines is untouched: what
              goes is what the model recorded this file defining and no longer
              finds defined anywhere"
                (is (= "1" (value! r "(probe.pruned/kept)"))))
              (testing "a var something still used goes all the same, and says
              so on this repl - what uses one may be something nothing
              recorded, so a usage is a thing to be told about rather than a
              veto"
                (let [said (client/printed frames "err")]
                  (is (string/includes? said "probe.pruned/dropped"))
                  (is (string/includes? said "probe.uses-pruned"))))
              (testing "and it is the name that goes rather than the code that
              already compiled: a form that resolved the var before holds the
              var itself, and goes on working until it is loaded again"
                (is (= "[1 2]" (value! r "(probe.uses-pruned/both)")))))
            (testing "a process that kept no track of what it compiled has
            nothing to unmap, because it has no idea what the file used to
            define"
              (is (string/includes? (reloaded! r) "keep track of what it compiled"))
              (is (= "[1 2]" (value! r "(probe.uses-pruned/both)")))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))

(deftest a-protocol-method-the-protocol-no-longer-names-is-unmapped-too
  (testing "a method var is interned and never def-ed, so nothing recorded it to
  be missed - what says one has gone is the protocol itself, which the reload
  has just written again without it"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (load! r (written-file! root "probe/methods.clj"
                                  (str "(ns probe.methods)\n"
                                       "(defprotocol P\n"
                                       "  (kept [this])\n"
                                       "  (dropped [this]))\n")))
          (is (= "true" (value! r "(some? (resolve 'probe.methods/dropped))")))
          (edited-file! root "probe/methods.clj"
                        (str "(ns probe.methods)\n"
                             "(defprotocol P\n"
                             "  (kept [this]))\n"))
          (if (analysing? c)
            (do
              (is (= "[\"probe/methods.clj\"]" (reloaded! r)))
              (testing "the method the protocol stopped naming resolves to
              nothing, where a reload on its own would have left it answering
              for a name the file does not have"
                (is (= "nil" (value! r "(resolve 'probe.methods/dropped)"))))
              (testing "and the method it still names is where it was, as is the
              protocol - what goes is what the protocol stopped naming"
                (is (= "true" (value! r "(some? (resolve 'probe.methods/kept))")))
                (is (= "true" (value! r "(some? (resolve 'probe.methods/P))")))))
            (testing "a process that kept no track of what it compiled has
            nothing to reload and nothing to unmap"
              (is (string/includes? (reloaded! r) "keep track of what it compiled"))
              (is (= "true" (value! r "(some? (resolve 'probe.methods/dropped))")))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))

(deftest a-name-another-namespace-referred-is-unmapped-with-the-definition
  (testing "a :refer is a mapping of its own, in the namespace that took it and
  under whatever name it took it as - so a var unmapped only where it was
  defined would go on resolving in every file that referred it"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (written-file! root "probe/referred.clj"
                         (str "(ns probe.referred)\n"
                              "(defn kept [] 1)\n"
                              "(defn dropped [] 2)\n"))
          (load! r (written-file! root "probe/refers.clj"
                                  (str "(ns probe.refers\n"
                                       "  (:require [probe.referred :refer [kept dropped]\n"
                                       "                             :rename {dropped elsewhere}]))\n"
                                       "(defn both [] [(kept) (elsewhere)])\n")))
          (is (= "[1 2]" (value! r "(probe.refers/both)")))
          (edited-file! root "probe/referred.clj"
                        (str "(ns probe.referred)\n"
                             "(defn kept [] 1)\n"))
          (if (analysing? c)
            (do
              (is (= "[\"probe/referred.clj\"]" (reloaded! r)))
              (testing "the name goes from the namespace that referred it, under
              the name that namespace referred it as - which is the name whose
              code has stopped being compilable"
                (is (= "false" (value! r "(some? (ns-resolve 'probe.refers 'elsewhere))")))
                (is (= "false" (value! r "(some? (resolve 'probe.referred/dropped))"))))
              (testing "while what the file still defines is still referred, and
              still the same var: the sweep goes by the var and not by the name"
                (is (= "true" (value! r "(= (ns-resolve 'probe.refers 'kept) (resolve 'probe.referred/kept))")))))
            (testing "a process that kept no track of what it compiled has
            nothing to unmap, here as anywhere"
              (is (string/includes? (reloaded! r) "keep track of what it compiled"))
              (is (= "true" (value! r "(some? (ns-resolve 'probe.refers 'elsewhere))")))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))

(deftest a-file-the-disk-no-longer-has-is-dropped-by-a-reload
  (testing "a file that is gone cannot be loaded again, so a reload has nothing
  to do about it and everything to undo: what it defined is still defined here,
  and this is the one moment anything will ever notice that the file behind it
  is not there any more"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (load! r (written-file! root "probe/vanishes.clj"
                                  (str "(ns probe.vanishes)\n"
                                       "(defn answer [] 42)\n")))
          (is (= "42" (value! r "(probe.vanishes/answer)")))
          (is (= "true" (value! r "(contains? (loaded-libs) 'probe.vanishes)")))
          (deleted-file! root "probe/vanishes.clj")
          (if (analysing? c)
            (do
              (testing "nothing is loaded - there is nothing to load it from"
                (is (= "[]" (reloaded! r))))
              (testing "and what the file defined stops resolving, as it does
              when a def is taken out of a file that is still there"
                (is (= "false" (value! r "(some? (resolve 'probe.vanishes/answer))"))))
              (testing "while the namespace stops being a loaded lib, so that
              requiring it says what the disk says rather than finding the dead
              entry and doing nothing"
                (is (= "false" (value! r "(contains? (loaded-libs) 'probe.vanishes)")))))
            (testing "a process that kept no track of what it compiled has
            nothing to drop, here as anywhere"
              (is (string/includes? (reloaded! r) "keep track of what it compiled"))
              (is (= "true" (value! r "(some? (resolve 'probe.vanishes/answer))")))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))

(deftest a-file-that-implements-a-protocol-is-loaded-again-when-the-protocol-is
  (testing "a `defprotocol' generates an interface, and what implements it was
  compiled against the one the edit has just replaced - so loading the protocol's
  file alone leaves a type that no longer satisfies the protocol, in a file where
  nothing changed and which no macro of the edited file reaches"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (written-file! root "probe/iface.clj"
                         (str "(ns probe.iface)\n"
                              "(defprotocol P\n"
                              "  (greet [this]))\n"))
          (load! r (written-file! root "probe/typed.clj"
                                  (str "(ns probe.typed\n"
                                       "  (:require [probe.iface :as p]))\n"
                                       "(deftype T []\n"
                                       "  p/P\n"
                                       "  (greet [this] \"hello\"))\n"
                                       "(def t (T.))\n")))
          (is (= "\"hello\"" (value! r "(probe.iface/greet probe.typed/t)")))
          (edited-file! root "probe/iface.clj"
                        (str "(ns probe.iface)\n"
                             "(defprotocol P\n"
                             "  (greet [this])\n"
                             "  (farewell [this]))\n"))
          (if (analysing? c)
            (do
              (testing "the file that implements it is loaded too, and after the
              file it implements"
                (is (= "[\"probe/iface.clj\" \"probe/typed.clj\"]" (reloaded! r))))
              (testing "so the type implements the interface the protocol has now,
              rather than failing with no implementation of a method of a protocol
              - which reads like a missing `extend-type' and is nothing of the sort"
                (is (= "\"hello\"" (value! r "(probe.iface/greet probe.typed/t)")))))
            (testing "a process that kept no track of what it compiled knows of no
            dependency to follow, here as anywhere"
              (is (string/includes? (reloaded! r) "keep track of what it compiled"))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))

(defn- jar-on-classpath!
  "A jar holding ENTRIES, put on the classpath behind everything on it, and
  the directory it was written into.

  Behind, which is the whole of what makes it worth writing: the loader hands
  back the first entry of the classpath that holds a name, so a directory
  added before this one wins the moment a file of that name is written into
  it. A jar nothing could ever come before is a jar there is nothing to say
  about."
  [entries]
  (let [dir (client/temp-dir)
        path (str (java.io.File. (str dir) "library.jar"))]
    (with-open [out (java.util.jar.JarOutputStream. (java.io.FileOutputStream. path))]
      (doseq [[entry source] entries]
        (.putNextEntry out (java.util.jar.JarEntry. ^String entry))
        (.write out (.getBytes ^String source "UTF-8"))
        (.closeEntry out)))
    (.addURL state/class-loader (.toURL (.toURI (java.io.File. path))))
    (state/adopt-class-loader!)
    (classpath/rescan!)
    dir))

(defn- stale!
  "Ask what would be loaded, without anything being loaded.

  Without the unread count, which is the asking most clients do: it is what
  the client asks before a question of its own, to know whether to offer a
  reload first."
  [c]
  (request! c {:op :stale :id 1}))

(defn- stale-unread!
  "Ask what would be loaded AND how much of this project is unread.

  The other asking, which is the one the client showing the answer does - see
  `replique.analysis/stale'."
  [c]
  (request! c {:op :stale :unread true :id 1}))

(defn- named-files
  "The files FOUND lists under K, as their own names."
  [found k]
  (mapv (fn [{:keys [file]}] (.getName (java.io.File. ^String file))) (k found)))

(deftest what-would-be-loaded-is-answered-without-loading-anything
  (testing "what a reload is about to do is a thing to look at before it does
  it - and \"this one file makes that other one need compiling\" is not
  something anybody can work out by looking at their buffers"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (written-file! root "probe/gone_stale.clj"
                         (str "(ns probe.gone-stale)\n"
                              "(defmacro twice [x] `(* 2 ~x))\n"))
          (load! r (written-file! root "probe/uses_stale.clj"
                                  (str "(ns probe.uses-stale\n"
                                       "  (:require [probe.gone-stale :as g]))\n"
                                       "(defn four [x] (g/twice (g/twice x)))\n")))
          (if (analysing? c)
            (do
              (testing "a file read a moment ago is a file nothing has to do
              anything about"
                (let [found (stale! c)]
                  (is (= [] (named-files found :changed)))
                  (is (= [] (named-files found :stale)))))
              (edited-file! root "probe/gone_stale.clj"
                            (str "(ns probe.gone-stale)\n"
                                 "(defmacro twice [x] `(* 3 ~x))\n"))
              (let [found (stale! c)]
                (testing "the file whose disk copy is newer than what was read"
                  (is (= ["gone_stale.clj"] (named-files found :changed))))
                (testing "and, apart from it, the file that did not change and
                is out of date all the same: it holds the expansion the old
                macro made.  Two facts about two files, so two lists"
                  (is (= ["uses_stale.clj"] (named-files found :stale)))))
              (testing "and nothing was loaded by the asking, which is the
              whole of what makes it a question: the old expansion is still
              the one that runs"
                (is (= "8" (value! r "(probe.uses-stale/four 2)"))))
              (testing "loading them is what changes the answer, and it empties
              it - what was asked about is what was loaded"
                (is (= "[\"probe/gone_stale.clj\" \"probe/uses_stale.clj\"]"
                       (reloaded! r)))
                (is (= "18" (value! r "(probe.uses-stale/four 2)")))
                (let [found (stale! c)]
                  (is (= [] (named-files found :changed)))
                  (is (= [] (named-files found :stale))))))
            (testing "a process that kept no track of what it compiled has no
            such question to be asked, and says so rather than answering that
            there is nothing to do"
              (is (string/includes? (or (refused (stale! c)) "")
                                    "keep track of what it compiled"))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))

(defn- gone
  "The file this test is about, where FOUND lists it as one the disk no longer
  has."
  [found]
  (filterv #{"probe/vanished.clj"} (:deleted found)))

(deftest a-file-the-disk-no-longer-has-is-a-list-of-its-own
  (testing "a reload does not only load. It drops the files this process read
  that the disk no longer has, and unmaps what they defined - which is the one
  thing a reload does that nobody asked for by editing anything, and it is
  what switching a branch mostly does. In neither list above, so an answer
  without this one is two empty lists and an application reported as up to
  date"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (let [path (written-file! root "probe/vanished.clj"
                                    (str "(ns probe.vanished)\n"
                                         "(defn value [] 1)\n"))]
            (load! r path)
            (when (analysing? c)
              (is (= "1" (value! r "(probe.vanished/value)")))
              ;; Its own file and not the whole list, for the reason `under'
              ;; gives about the others: the model is one per process, and
              ;; every other test here deletes the root it wrote under when it
              ;; is done - so what this process has read and cannot open is
              ;; mostly other tests' files.
              (is (= [] (gone (stale! c))))
              (.delete (java.io.File. ^String path))
              (let [found (stale! c)]
                (testing "not a changed file, and not a stale one: there is
                nothing to load it from"
                  (is (= [] (named-files found :changed)))
                  (is (= [] (named-files found :stale))))
                (testing "and named as the model names it, which is the only
                way there is - asking the classpath where it is is asking
                about the one thing that is not so any more"
                  (is (= ["probe/vanished.clj"] (gone found)))))
              (testing "and a reload is what drops it, so the list empties the
              way the other two do"
                (reloaded! r)
                (is (= [] (gone (stale! c))))
                (is (nil? (ns-resolve 'probe.vanished 'value))))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))

(deftest how-many-files-have-been-read-is-answered-beside-what-changed
  (testing "the two ways the lists can be empty are not the same fact and read
  the same. \"Nothing has changed since this process read these files\" says the
  program is up to date; \"this process has read no files\" says it knows of
  nothing and will go on saying nothing whatever is edited. A client that
  cannot tell them apart can only report the wrong one of the two - so the
  count of what has been read is in the answer.

  Counted from where this test starts rather than from zero, because a test
  runs inside the process it is testing (see `with-process') and the model is
  one per jvm: what another test read is in it, which is the one thing a
  suite has that a fresh process does not."
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (when (analysing? c)
            (let [before (:analysed (stale! c))]
              (is (integer? before))
              (written-file! root "probe/unread.clj"
                             (str "(ns probe.unread)\n"
                                  "(defn value [] 1)\n"))
              (testing "`require' IS NOT LOADING IT THROUGH THIS PROCESS. The
              model holds what the compiler read under the sink, which is what
              a load and a reload push - so the namespace is loaded, is
              running, and the model has not heard of it. Which is the state a
              repl whose application was required from an init script is in,
              and the whole reason the count is worth answering"
                (is (= "1" (value! r (str "(do (require 'probe.unread)"
                                          " (probe.unread/value))"))))
                (let [found (stale! c)]
                  (is (= before (:analysed found)))
                  (is (= [] (named-files found :changed)))))
              (testing "a load is, and from then on the empty lists mean what
              they say about that file"
                (load! r (written-file! root "probe/read.clj"
                                        (str "(ns probe.read)\n"
                                             "(defn value [] 2)\n")))
                (let [found (stale! c)]
                  (is (= (inc before) (:analysed found)))
                  (is (= [] (named-files found :changed)))))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))

(deftest what-is-running-here-and-unread-is-answered-beside-what-has-been-read
  (testing "a count of what has been read tells the two empty answers apart
  only where it is nothing, and the case that matters is not that one. A
  process whose application came up by `require' and was then loaded from
  once has read a file, so it is not nothing, reads as a model and answers
  nothing whatever is edited - which is a client saying \"nothing has changed\"
  to somebody who has just changed branch. What says so is the other count:
  how much of this project is running here that no model holds"
    (with-process [info nil]
      (let [root (source-root! (:directory info))
            r (repl-client info)
            c (control-client info)]
        (try
          (when (analysing? c)
            ;; Counted from where this test starts rather than from zero, for
            ;; `how-many-files-have-been-read-is-answered-beside-what-changed's
            ;; reason: the model is one per jvm and a test runs inside the
            ;; process it is testing. `:unread' needs no such allowance - it is
            ;; scoped to this process's directory, and every test that came
            ;; before it was a process with a directory of its own.
            (let [before (:analysed (stale-unread! c))]
              (testing "nothing of this project is loaded, so there is nothing
              it has not read"
                (is (= 0 (:unread (stale-unread! c)))))
              (written-file! root "probe/required.clj"
                             (str "(ns probe.required)\n"
                                  "(defn value [] 1)\n"))
              (written-file! root "probe/loaded.clj"
                             (str "(ns probe.loaded)\n"
                                  "(defn value [] 2)\n"))
              (testing "a namespace required rather than loaded is running
              here and is in no model, which is the whole of what this counts"
                (is (= "1" (value! r (str "(do (require 'probe.required)"
                                          " (probe.required/value))"))))
                (let [found (stale-unread! c)]
                  (is (= 1 (:unread found)))
                  (is (= before (:analysed found)))))
              (testing "loading another file does not answer for it. Which is
              the state the count exists for: one file read, so the count of
              what has been read is not nothing - and the empty lists beside
              it are about that file and about nothing else"
                (load! r (str root "/probe/loaded.clj"))
                (let [found (stale-unread! c)]
                  (is (= (inc before) (:analysed found)))
                  (is (= 1 (:unread found)))
                  (is (= [] (named-files found :changed)))))
              (testing "and loading it does: a file the model holds is a file
              this answers for, so it is counted in one place and not in both"
                (load! r (str root "/probe/required.clj"))
                (let [found (stale-unread! c)]
                  (is (= (+ 2 before) (:analysed found)))
                  (is (= 0 (:unread found)))))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))

(deftest what-is-unread-is-counted-only-where-it-was-asked-for
  (testing "the count of what is running here unread is the dear half of this
  answer - it walks every var of every namespace in the process, on every
  asking, and remembers none of it - and most of what asks is not going to
  read it. A client asking before a question of its own, only to know whether
  to offer a reload, waits for it and throws it away. So it is asked for, and
  an answer nobody asked it of does not carry it at all"
    (with-process [info nil]
      (let [root (source-root! (:directory info))
            r (repl-client info)
            c (control-client info)]
        (try
          (when (analysing? c)
            (written-file! root "probe/uncounted.clj"
                           (str "(ns probe.uncounted)\n"
                                "(defn value [] 1)\n"))
            (is (= "1" (value! r (str "(do (require 'probe.uncounted)"
                                      " (probe.uncounted/value))"))))
            (testing "there is something to count, and asking says so"
              (is (= 1 (:unread (stale-unread! c)))))
            (testing "and the same question asked without it comes back with
            no such key - which is not the same as a count of nothing"
              (let [found (stale! c)]
                (is (not (contains? found :unread)))
                ;; Not the lists themselves, which are the process's and not
                ;; this test's: the model is one per jvm and every test that
                ;; ran before this one is in it - see
                ;; `how-many-files-have-been-read-is-answered-beside-what-changed'.
                ;; What is being told apart here is a key that is there from a
                ;; key that is not.
                (testing "and the rest of the answer is the answer"
                  (is (integer? (:analysed found)))
                  (is (contains? found :changed))
                  (is (contains? found :stale))
                  (is (contains? found :deleted))))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))

(deftest what-is-running-outside-this-project-is-not-counted-against-it
  (testing "a file of a library is not what the question is about, and a
  library brought in by `:local/root' is a directory of real files like any
  source root - so a count that took every loaded namespace with a file
  behind it would never be nothing, and a fact that is never nothing is a
  fact nobody reads. What is asked about is what is under the directory this
  process was started on"
    (with-process [info nil]
      (let [elsewhere (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (when (analysing? c)
            (written-file! elsewhere "probe/beside.clj"
                           (str "(ns probe.beside)\n"
                                "(defn value [] 3)\n"))
            (is (= "3" (value! r (str "(do (require 'probe.beside)"
                                      " (probe.beside/value))"))))
            (testing "loaded, running, in no model, and none of this
            process's business"
              (is (= 0 (:unread (stale-unread! c))))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively elsewhere)))))))

(deftest a-project-that-is-a-tree-of-links-is-still-this-project
  (testing "a checkout is pointed at without the classpath moving by making
  the project directory a tree of links into a worktree - so the files under
  it are named here and are real somewhere else entirely. Asking where a file
  really is would lose every one of them, and this process would answer that
  it has nothing it has not read while it holds the whole application"
    (with-process [info nil]
      (let [worktree (client/temp-dir)
            linked (java.io.File. (str (:directory info)) "src")
            r (repl-client info)
            c (control-client info)]
        (try
          (when (analysing? c)
            (written-file! worktree "probe/linked.clj"
                           (str "(ns probe.linked)\n"
                                "(defn value [] 4)\n"))
            (java.nio.file.Files/createSymbolicLink
             (.toPath linked)
             (.toPath (java.io.File. (str worktree)))
             (make-array java.nio.file.attribute.FileAttribute 0))
            (source-root!* (.getPath linked))
            (is (= "4" (value! r (str "(do (require 'probe.linked)"
                                      " (probe.linked/value))"))))
            (testing "named under this project, so it is one of this
            project's, whatever the file it opens turns out to be"
              (is (= 1 (:unread (stale-unread! c))))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively worktree)))))))

(deftest a-file-written-over-one-in-a-jar-is-seen-once-the-classpath-is-read-again
  (testing "a file inside a jar is not a file anybody edits, so whether it is
  one is asked once and remembered - which is most of the work of asking what
  changed, on a codebase whose model is mostly library namespaces. And it
  stops being true the moment somebody writes a file of that name into a
  directory earlier on the classpath, which is how a library's namespace is
  patched"
    (with-process [info nil]
      (let [root (source-root!)
            jar (jar-on-classpath! {"probe/library.clj"
                                    "(ns probe.library)\n(defn value [] 1)\n"})
            r (repl-client info)
            c (control-client info)]
        (try
          (load! r (written-file! root "probe/uses_library.clj"
                                  (str "(ns probe.uses-library\n"
                                       "  (:require [probe.library :as l]))\n"
                                       "(defn twice [] (* 2 (l/value)))\n")))
          (is (= "2" (value! r "(probe.uses-library/twice)")))
          (if (analysing? c)
            (do
              (testing "the file in the jar was compiled on the way and is in
              the model, and nothing about it has changed"
                (let [found (stale! c)]
                  (is (= [] (named-files found :changed)))
                  (is (= [] (named-files found :stale)))))
              (written-file! root "probe/library.clj"
                             "(ns probe.library)\n(defn value [] 5)\n")
              (testing "and writing a file over it changes nothing yet - the
              same rule the rest of the classpath is read by, which is that a
              classpath this process has not read is one it says nothing new
              about.  See `replique.classpath/naming-anchors'"
                (is (= [] (named-files (stale! c) :changed))))
              (request! c {:op :update-classpath :id 1})
              (testing "reading it again is what says otherwise: the name that
              was answered by a jar is answered by a file now, and a file that
              this process has never read is a file that changed"
                (is (= ["library.clj"] (named-files (stale! c) :changed))))
              (testing "and what is loaded is the file, not the entry it was
              written over"
                (is (= "[\"probe/library.clj\"]" (reloaded! r)))
                (is (= "10" (value! r "(probe.uses-library/twice)")))))
            (testing "a process that recorded nothing has nothing to say about
            it either way"
              (is (string/includes? (or (refused (stale! c)) "")
                                    "keep track of what it compiled"))))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)
            (client/delete-recursively jar)))))))

(deftest a-file-that-is-gone-and-comes-back-is-loaded-again
  (testing "which is a branch changed under a running process: the file the
  model holds is nowhere for a moment, and then it is somewhere again with
  something else in it.  A name nothing answers to is nothing to remember -
  what was asked was where a file is, and the answer was that it is not
  anywhere yet"
    (with-process [info nil]
      (let [root (source-root!)
            r (repl-client info)
            c (control-client info)]
        (try
          (load! r (written-file! root "probe/vanishing.clj"
                                  "(ns probe.vanishing)\n(defn value [] 1)\n"))
          (is (= "1" (value! r "(probe.vanishing/value)")))
          (if (analysing? c)
            (do
              (.delete (java.io.File. (str root) "probe/vanishing.clj"))
              (testing "a file that is not there is not a file to load again -
              loading it would only fail"
                (is (= [] (named-files (stale! c) :changed))))
              (edited-file! root "probe/vanishing.clj"
                            "(ns probe.vanishing)\n(defn value [] 7)\n")
              (testing "and one that is there again is one this process has
              never read"
                (is (= ["vanishing.clj"] (named-files (stale! c) :changed)))
                (is (= "[\"probe/vanishing.clj\"]" (reloaded! r)))
                (is (= "7" (value! r "(probe.vanishing/value)")))))
            (is (string/includes? (or (refused (stale! c)) "")
                                  "keep track of what it compiled")))
          (finally
            (disconnect r)
            (disconnect c)
            (client/delete-recursively root)))))))
