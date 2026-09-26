(ns replique.completion-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as string]
            [replique.classpath :as classpath]
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
  does to a running process and what the reading has to look at to see it.

  Read on the way in and on the way out, since the classpath is read once and
  kept: what a test leaves behind is what the next one would be answered."
  [urls f]
  (let [thread (Thread/currentThread)
        previous (.getContextClassLoader thread)
        loader (clojure.lang.DynamicClassLoader. previous)]
    (doseq [url urls] (.addURL loader url))
    (.setContextClassLoader thread loader)
    (classpath/rescan!)
    (try (f) (finally (.setContextClassLoader thread previous) (classpath/rescan!)))))

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

(deftest a-namespace-written-now-waits-for-the-classpath-to-be-read-again
  (let [dir (temp-dir)]
    (try
      (with-entry
        dir
        (fn []
          (write-file! dir "made" "up.clj")
          (is (not (contains? (typed {:position :namespace :text "made"}) "made.up"))
              "the classpath is read once and kept, so a file written after
              the reading is not one this knows about")
          (classpath/rescan!)
          (is (contains? (typed {:position :namespace :text "made"}) "made.up"))
          (testing "the underscores a file name carries are dashes in the name"
            (write-file! dir "made" "two_words.cljc")
            (classpath/rescan!)
            (is (contains? (typed {:position :namespace :text "made"}) "made.two-words")))
          (testing "and a name is answered once however many entries provide it"
            (write-file! dir "clojure" "string.clj")
            (classpath/rescan!)
            (is (= ["clojure.string"]
                   (candidates {:position :namespace :text "clojure.string"}))))))
      (finally (delete-recursively dir)))))

(deftest what-is-under-a-hidden-directory-is-not-a-name
  (testing "a name with an empty piece in it is neither a namespace anybody
  wrote nor a class anybody can import, and skipping those is what keeps the
  walk to the size of a source tree"
    (let [dir (temp-dir)]
      (try
        (write-file! dir ".git" "objects" "probehidden.clj")
        (write-file! dir "seen" "probehidden.clj")
        (with-entry
          dir
          (fn []
            (is (= ["seen.probehidden"]
                   (candidates {:position :namespace :text "probehidden"})))))
        (finally (delete-recursively dir))))))

(deftest a-subtree-that-cannot-be-read-does-not-lose-the-entry
  (testing "losing the entry answers a whole source tree as if it were empty"
    (let [dir (temp-dir)]
      (try
        (write-file! dir "readable" "probehere.clj")
        (write-file! dir "closed" "probethere.clj")
        (Files/setPosixFilePermissions (path dir "closed") (java.util.HashSet.))
        (with-entry
          dir
          (fn []
            (let [answered (typed {:position :namespace :text "probe"})]
              (is (contains? answered "readable.probehere"))
              ;; and not where the test runs as somebody permissions do not
              ;; apply to, which is nobody this is written for
              (when-not (Files/isReadable (path dir "closed"))
                (is (not (contains? answered "closed.probethere")))))))
        (finally
          (Files/setPosixFilePermissions
           (path dir "closed")
           (java.nio.file.attribute.PosixFilePermissions/fromString "rwx------"))
          (delete-recursively dir))))))

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
            (is (= ["real" "real.here"]
                   (candidates {:position :namespace :text "real."})))))
        (finally (delete-recursively dir))))))

(deftest a-piece-of-a-name-at-a-time
  (testing "a name is written in pieces, and what was typed is split the same"
    (is (contains? (typed {:position :namespace :text "c.s"}) "clojure.string"))
    (is (contains? (typed {:position :package-or-class :text "j.u.c.Atomic"})
                   "java.util.concurrent.atomic.AtomicInteger"))
    (is (contains? (typed {:position :package-or-class :text "ABQ"})
                   "java.util.concurrent.ArrayBlockingQueue")
        "three letters, and no separator written between them"))
  (testing "a name holding two capitals in a row is reached by writing it
  out, which is a name split into a piece for each of them"
    (is (contains? (typed {:position :package-or-class :text "UUID"})
                   "java.util.UUID"))
    (is (contains? (typed {:position :package-or-class :text "java.util.UUID"})
                   "java.util.UUID"))
    (is (contains? (typed {:position :code :ns "clojure.core" :text "Integer/SIZE"})
                   "Integer/SIZE")))
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

(deftest a-namespace-made-at-a-repl-is-offered
  (testing "what the process has loaded as well as what is on the classpath: a
  namespace made at a repl has no file anywhere and is a namespace all the same"
    (try
      (is (not (contains? (typed {:position :namespace :text "made.at"})
                          "made.at.the.repl")))
      (create-ns 'made.at.the.repl)
      (is (contains? (typed {:position :namespace :text "made.at"}) "made.at.the.repl"))
      (finally (remove-ns 'made.at.the.repl)))))

(deftest a-piece-of-a-namespace-is-answered-as-a-piece
  (testing "the head of a prefix list is a name no file carries - requiring it
  alone would fail, so it is answered as what it is rather than left out"
    (is (= "namespace-prefix"
           (:type (found {:position :namespace :text "clojure.core.spec"}
                         "clojure.core.specs"))))
    (is (= "namespace"
           (:type (found {:position :namespace :text "clojure.core.spec"}
                         "clojure.core.specs.alpha")))))
  (testing "and a name that is both is the namespace"
    (is (= "namespace"
           (:type (found {:position :namespace :text "clojure.core"} "clojure.core")))))
  (testing "under a prefix as under none"
    (is (contains? (typed {:position :namespace :prefix "clojure" :text "sp"}) "spec"))))

;;; Vars

(deftest a-var-comes-from-a-loaded-namespace
  (testing "and only from one: loading a namespace to see what it holds runs
  every top level form in it, which a keystroke must not do"
    ;; One that brings nothing else with it, and that no other test is
    ;; written about. What this loads stays loaded for every test after it,
    ;; here and in the other files - and more than one of them is written
    ;; about a namespace that has not been loaded.
    (is (nil? (find-ns 'clojure.datafy)))
    (is (empty? (candidates {:position :var :namespace "clojure.datafy" :text ""})))
    (require 'clojure.datafy)
    (is (contains? (typed {:position :var :namespace "clojure.datafy" :text "dataf"})
                   "datafy"))))

(deftest a-var-of-a-refer-clojure-comes-from-core
  (testing "a refer-clojure names no namespace anywhere in itself"
    (is (= (typed {:position :var :namespace :refer-clojure :text "map-in"})
           #{"map-indexed"}))))

(deftest a-var-of-a-namespace-that-is-not-one-is-nothing
  (is (empty? (candidates {:position :var :namespace "not.a.namespace" :text ""}))))

(deftest a-var-says-what-it-is-and-where-it-is-from
  (testing "so that a client can annotate it without asking a second time"
    (is (= {:candidate "mapv" :type "function" :ns "clojure.core" :match-index 4}
           (found {:position :var :namespace "clojure.core" :text "mapv"} "mapv")))
    (is (= "var" (:type (found {:position :var :namespace "clojure.core" :text "*ns*"}
                               "*ns*"))))
    (testing "a macro before a function, since a macro has arglists too"
      (is (= "macro" (:type (found {:position :var :namespace "clojure.core" :text "defn"}
                                   "defn"))))))
  (testing "and a refer-clojure says core, which it names nowhere itself"
    (is (= "clojure.core"
           (:ns (found {:position :var :namespace :refer-clojure :text "map-in"}
                       "map-indexed"))))))

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

(defrecord ThingMadeHere [a b])

(deftest a-class-made-at-a-repl-is-offered
  (testing "a defrecord makes a class the moment it is evaluated and no file
  of it, and the package it lands in is the namespace that made it"
    (is (not (contains? (set (:classes (classpath/scan)))
                        "replique.completion_test.ThingMadeHere")))
    (is (= ["ThingMadeHere"]
           (candidates {:position :class :package "replique.completion_test"
                        :text "ThingMade"}))))
  (testing "under the package as it is written, which is the munged one - the
  other would not import"
    (is (empty? (candidates {:position :class :package "replique.completion-test"
                             :text "ThingMade"}))))
  (testing "and under a namespace whose own name carries an underscore, which
  is a name that munges to itself"
    (try
      (binding [*ns* (create-ns 'made_under.core)]
        (refer-clojure)
        (eval '(deftype Thing [])))
      (is (= ["Thing"] (candidates {:position :class :package "made_under.core"
                                    :text "Thi"})))
      (finally (remove-ns 'made_under.core)))))

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
      (write-file! dir "made" "Thing.class")
      (write-file! dir "made" "Thing$Inner.class")
      (write-file! dir "made" "Thing$1.class")
      (write-file! dir "made" "Thing$1Local.class")
      (write-file! dir "made" "package-info.class")
      (write-file! dir "module-info.class")
      (with-entry
        dir
        (fn []
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

(deftest a-load-path-is-read-from-where-it-is-written
  (testing "a path that starts with a slash is read from the root of the classpath"
    (is (contains? (typed {:position :load-path :text "/clojure/core"}) "/clojure/core")))
  (testing "and one that does not is read from the namespace it is written in"
    (let [answered (typed {:position :load-path :ns "clojure.core" :text ""})]
      (is (contains? answered "protocols"))
      (is (contains? answered "specs/alpha") "a load goes as deep as the directories do")
      (is (not (contains? answered "/clojure/core/protocols")))))
  (testing "and where the client said no namespace there is no other answer"
    (is (contains? (typed {:position :load-path :text ""}) "/clojure/core"))))

(deftest a-load-path-names-the-file
  (let [found (typed {:position :load-path :text "/clojure/core"})]
    (is (contains? found "/clojure/core"))
    (is (contains? found "/clojure/core_deftype")
        "a load takes a path, so the underscores stay")))

;;; A name written in code

(def ^:private probe
  "A namespace made the way a file makes one, for the code position to be
  asked about.

  Evaluated rather than built out of create-ns and intern, since what is
  being asked about is what an ns form leaves behind: an alias, an import,
  and a var referred under a name of this namespace's choosing."
  (delay
    (binding [*ns* *ns*]
      ;; Nothing is required that the process has not loaded already, since
      ;; loading one here would be loading it for every test after this one.
      (eval '(ns replique.completion-test.probe
               (:require [clojure.java.io :as io]
                         [clojure.string :as string :refer [join] :rename {join joined}])
               (:import [java.util Date])))
      ;; Read here, which is what interns a keyword: nothing declares one,
      ;; and what exists is what has been written somewhere.
      (eval '(def probe-keywords [:probe-plain
                                  :replique.completion-test.probe/probe-own
                                  :clojure.string/probe-aliased]))
      (eval '(def probe-value 1))
      (eval '(defn probe-fn [] 1))
      (eval '(defmacro probe-macro [] 1))
      ;; a class of its own, whose fields are public and are not static
      (eval '(deftype ProbeType [probe-field]))
      ;; and a var that declares what it holds, which is the one thing a var
      ;; says about its value without the value being looked at
      (eval '(def ^java.util.Date probe-tagged nil))
      (str (ns-name *ns*)))))

(defn- in-code
  "What is offered where TEXT is being written in the probe namespace."
  [text]
  {:position :code :ns @probe :text text :locals [{:name "probe-local"}]})

(deftest a-name-in-code-is-answered-as-everything-that-could-be-written-there
  (testing "which is every kind of name at once, where a dependency form
  takes one kind and no other"
    (is (contains? (typed (in-code "probe-l")) "probe-local")
        "a local, which only the client could have said")
    (is (contains? (typed (in-code "probe-f")) "probe-fn")
        "a var of the namespace")
    (is (contains? (typed (in-code "joine")) "joined")
        "a var it referred, under the name it is written as here")
    (is (not (contains? (typed (in-code "joi")) "join"))
        "and not under the name it has where it is public")
    (is (contains? (typed (in-code "Dat")) "Date")
        "a class it imported")
    (is (contains? (typed (in-code "strin")) "string")
        "an alias")
    (is (contains? (typed (in-code "clojure.strin")) "clojure.string")
        "a namespace that has been loaded, which is how a var of one that was
        never required is written")
    (is (contains? (typed (in-code "recu")) "recur")
        "and a special form")))

(deftest what-each-name-in-code-is-said-beside-it
  (let [what (fn [text name] (dissoc (found (in-code text) name) :match-index))]
    (is (= {:candidate "probe-local" :type "local"} (what "probe-l" "probe-local")))
    (is (= {:candidate "probe-fn" :type "function" :ns @probe} (what "probe-f" "probe-fn")))
    (is (= {:candidate "probe-macro" :type "macro" :ns @probe} (what "probe-m" "probe-macro")))
    (is (= {:candidate "probe-value" :type "var" :ns @probe} (what "probe-v" "probe-value")))
    (testing "the namespace a var is public in, which is the one thing its own
    name does not say - what was referred is written without it"
      (is (= {:candidate "joined" :type "function" :ns "clojure.string"}
             (what "joine" "joined"))))
    (testing "the package of a class, for the same reason"
      (is (= {:candidate "Date" :type "class" :package "java.util"} (what "Dat" "Date"))))
    (testing "and what an alias stands for, which is the whole of what an
    alias is worth saying"
      (is (= {:candidate "string" :type "namespace" :ns "clojure.string"}
             (what "strin" "string"))))
    (is (= {:candidate "recur" :type "special-form"} (what "recu" "recur")))))

(deftest a-local-is-what-the-name-means-where-it-is-bound
  (let [asked (fn [locals] (found {:position :code :ns "clojure.core" :text "map" :locals locals}
                                  "map"))]
    (is (= "function" (:type (asked nil)))
        "the var, where nothing binds the name")
    (testing "and the local, where something does. A name is answered once,
    and a let that binds map is what map means inside it"
      (is (= "local" (:type (asked [{:name "map"}]))))
      (is (nil? (:ns (asked [{:name "map"}]))) "a local is public in no namespace"))
    (testing "what else the client knows about one has somewhere to go, and
    nothing reads it yet"
      (is (= "local" (:type (asked [{:name "map" :tag "java.lang.String"}])))))))

(deftest a-name-written-under-a-scope
  (testing "the scope written back on, since a candidate is what goes in the
  buffer and the scope is part of what is written there"
    (is (= ["string/join"] (candidates (in-code "string/joi"))))
    (is (= "clojure.string" (:ns (found (in-code "string/joi") "string/join")))
        "with the namespace the alias stands for"))
  (testing "a namespace is written under its own name as well as under an
  alias of it"
    (is (contains? (typed (in-code "clojure.string/joi")) "clojure.string/join")))
  (testing "a slash at the front is not a scope: that is the var named / being
  written, and the whole of it is the name"
    (is (contains? (typed (in-code "/")) "/")))
  (testing "what resolves to neither is a namespace nobody required, and a
  process that has not loaded it has nothing to say about what is in it"
    (is (empty? (candidates (in-code "nope/joi")))))
  (testing "and no local is offered under one, however much it looks like
  what is being written: a name with a slash in it is not a local"
    (is (empty? (candidates {:position :code :ns @probe :text "string/prob"
                             :locals [{:name "string-probe"}]})))))

(deftest a-class-that-was-not-imported-is-written-in-full
  (is (contains? (typed (in-code "java.util.Da")) "java.util.Date"))
  (testing "until the text holds a dot, the classes offered are the ones the
  namespace imported and no others - two letters typed in code would
  otherwise answer with a thousand class names, and the vars they were meant
  to reach would be underneath them"
    (is (not (contains? (typed (in-code "Str")) "java.lang.String")))
    (is (contains? (typed (in-code "Str")) "String")
        "which the imports answer, under the name they are written as")))

(deftest a-namespace-the-process-does-not-have-is-answered-as-clojure-core
  (testing "which is every file until it is loaded. What a namespace refers
  before it refers anything is clojure.core"
    (is (contains? (typed {:position :code :ns "no.such.namespace" :text "redu"}) "reduce")))
  (testing "and none named is the same question"
    (is (contains? (typed {:position :code :text "redu"}) "reduce"))))

(deftest a-special-form-is-offered-where-one-can-be-written
  (testing "which is the head of a form and nowhere else: an if written at an
  argument of something resolves to nothing at all"
    (is (contains? (typed (assoc (in-code "recu") :argument 0)) "recur"))
    (is (not (contains? (typed (assoc (in-code "recu") :argument 1)) "recur"))))
  (testing "and nothing else is held back at an argument, since the head is a
  place only a special form has to be written at"
    (is (contains? (typed (assoc (in-code "probe-f") :argument 1)) "probe-fn"))
    (is (contains? (typed (assoc (in-code "Dat") :argument 1)) "Date")))
  (testing "a client that said nothing about the form the name is written in
  is a client that did not read one, not one saying it is at an argument"
    (is (contains? (typed (in-code "recu")) "recur"))))

(deftest what-a-code-completion-carries-wrongly
  (is (= :invalid-message (error-kind {:position :code :text "" :ns 42})))
  (is (= :invalid-message (error-kind {:position :code :text "" :locals "probe-local"})))
  (testing "which argument of a form the name is at is a whole number of them"
    (is (= :invalid-message (error-kind {:position :code :text "" :argument "1"})))
    (is (= :invalid-message (error-kind {:position :code :text "" :argument -1})))
    (is (= :invalid-message (error-kind {:position :code :text "" :argument 1.5}))))
  (testing "a local is a map holding its name, so that what else is known
  about one has somewhere to go"
    (is (= :invalid-message (error-kind {:position :code :text "" :locals ["probe-local"]})))
    (is (= :invalid-message (error-kind {:position :code :text "" :locals [{:name 42}]}))))
  (testing "and none of them is none rather than a message written wrongly"
    (is (seq (candidates {:position :code :text "redu"})))))

(deftest a-class-is-answered-with-the-members-of-it

  (testing "a static method, written Class/name"
    (is (= {:candidate "Date/from" :type "method"}
           (dissoc (found (in-code "Date/fro") "Date/from") :match-index))))

  (testing "a class written out in full, which is one the process may never
  have touched - it is loaded to be found, and not initialized"
    (is (contains? (typed (in-code "java.util.Date/fro")) "java.util.Date/from")))

  (testing "an instance method, written Class/.name, and a constructor,
  written Class/new - the spellings clojure reads since 1.12, which is the
  least this process runs on"
    (is (= {:candidate "String/.length" :type "method"}
           (dissoc (found (in-code "String/.leng") "String/.length") :match-index)))
    (is (= {:candidate "String/new" :type "constructor"}
           (dissoc (found (in-code "String/ne") "String/new") :match-index))))

  (testing "a static field"
    (is (= {:candidate "Integer/MAX_VALUE" :type "field"}
           (dissoc (found (in-code "Integer/") "Integer/MAX_VALUE") :match-index))))

  (testing "and not an instance field: what reads one is written on the thing
  rather than on the class, and there is no spelling of it behind a slash"
    (let [answered (typed (in-code "ProbeType/"))]
      (is (contains? answered "ProbeType/new"))
      (is (not (contains? answered "ProbeType/probe_field")))))

  (testing "a scope that is neither a namespace nor a class answers nothing"
    (is (empty? (candidates (in-code "nope.Nope/x"))))))

(deftest a-member-is-written-on-the-thing-it-is-read-from
  (let [on (fn [text & {:as said}]
             (merge {:position :code :ns @probe :text text} said))]

    (testing "a method is written .name, and the class comes from the tag the
    client read out of the text"
      (is (= {:candidate ".length" :type "method"}
             (dissoc (found (on ".leng" :tag "String") ".length") :match-index))))

    (testing "a field is written .-name, and a method is not offered there: a
    field is not readable by the spelling a method is called with"
      (is (= {:candidate ".-probe_field" :type "field"}
             (dissoc (found (on ".-probe" :tag "ProbeType") ".-probe_field")
                     :match-index)))
      (is (empty? (candidates (on ".-leng" :tag "String")))))

    (testing "a static member is not one either - it is written on the class"
      (is (not (contains? (typed (on ".parse" :tag "Integer")) ".parseInt")))
      (is (empty? (candidates (on ".-max" :tag "Integer")))))

    (testing "a var declares its class with a :tag of its own"
      (is (contains? (typed (on ".getT" :on "probe-tagged")) ".getTime")))

    (testing "and a literal is its own class"
      (is (contains? (typed (on ".leng" :on "\"abc\"")) ".length")))

    (testing "what nothing says the class of is answered with nothing: what an
    expression would return is not knowable without running it, and running
    somebody's code is what a keystroke must not do"
      (is (empty? (candidates (on ".leng"))))
      (is (empty? (candidates (on ".leng" :on "(make-a-thing)"))))
      (is (empty? (candidates (on ".coun" :on "probe-keywords")))
          "a var that declares nothing among them: what it holds now is what
          it holds now, and reading a var to find out is reading it"))

    (testing "and the target is read rather than evaluated, so a form the
    reader would run is a form nothing runs"
      (is (empty? (candidates (on ".leng" :on "#=(str \"abc\")")))))))

(deftest a-constructor-is-written-with-a-dot-on-the-class
  (testing "and every candidate carries one, since a candidate without it
  would be written over the dot and take it away - which is what somebody who
  typed (Date. would watch happen"
    (is (= {:candidate "Date." :type "constructor"}
           (dissoc (found (in-code "Date.") "Date.") :match-index)))
    (is (contains? (typed (in-code "Date.")) "java.util.Date.")))
  (testing "only where what stands before the dot is already a class:
  java.util. is somebody halfway through writing a class name rather than a
  constructor of a package"
    (let [answered (typed (in-code "java.util."))]
      (is (contains? answered "java.util.Date"))
      (is (not (contains? answered "java.util.Date."))))))

;;; A keyword written in code

(deftest a-keyword-written-with-one-colon-is-read-as-it-stands
  (testing "so what is offered is every keyword this process has interned,
  the qualified ones under the name they are written out in full as"
    (is (contains? (typed (in-code ":probe-pla")) ":probe-plain"))
    (is (contains? (typed (in-code ":probe-ow"))
                   ":replique.completion-test.probe/probe-own")))
  (testing "and keywords only: the colon says which kind of name is being
  written, where a var of that name is not one that could be written there"
    (is (empty? (candidates (in-code ":probe-fn"))))))

(def ^:private probe-unwritable
  "Keywords nothing could write, interned the way anything interns one.

  Nothing declares a keyword. `keyword' makes one out of whatever string it is
  handed, so a process that has read a document holds one for every key in it,
  and half of those are names nobody could write back.

  Held in a var, because the table clojure interns keywords in holds them
  weakly - a test that only made them would be asking about whatever the
  collector had left."
  [(keyword "probe-space one")
   (keyword "probe-semicolon;one")
   (keyword ":probe-colon")
   (keyword "probe-trailing:")
   (keyword "probe::double")
   ;; a slash inside the name rather than between the two, which is the one
   ;; of these that `keyword' has to be handed in two pieces to make
   (keyword "probe-ns" "probe-slash/inside")
   (keyword "")
   (keyword "replique.completion-test.probe" "1probe-digit")
   ;; and one of the same shape that is written fine, so that what is left
   ;; out is left out for its shape rather than for the word in it
   (keyword "1probe-writable")])

(deftest a-keyword-nothing-could-write-is-not-offered
  (is (= 9 (count probe-unwritable)) "the keywords are held rather than collected")
  (testing "a name the reader stops partway through is a candidate somebody
  watches turn into a shorter keyword than the one they picked"
    (is (empty? (candidates (in-code ":probe-spac"))))
    (is (empty? (candidates (in-code ":probe-semic")))))
  (testing "and a colon at either end of one is a token it refuses outright,
  as are two of them anywhere in it: :::name is not how any keyword is
  written"
    (is (empty? (candidates (in-code ":probe-col"))))
    (is (empty? (candidates (in-code ":probe-trail"))))
    (is (empty? (candidates (in-code ":probe-doub")))))
  (testing "a slash inside the name is a keyword written under two
  namespaces, which is one more than the reader takes"
    (is (empty? (candidates (in-code ":probe-slash-insi")))))
  (testing "and a keyword with no name at all is written as a lone colon,
  which names nothing - offered, it would be the shortest of them and so the
  first thing anybody saw"
    (is (not (contains? (typed (in-code ":")) ":"))))
  (testing "a digit at the front of the name is read as a number where a
  namespace stands before it, under either spelling"
    (is (empty? (candidates (in-code ":probe/1probe-dig"))))
    (is (empty? (candidates (in-code "::1probe-dig")))))
  (testing "though :1 on its own is a token the reader takes, so one written
  that way is offered"
    (is (contains? (typed (in-code ":1probe-writ")) ":1probe-writable"))))

(deftest a-keyword-written-with-two-colons-is-read-against-the-namespace
  (testing "::name is a keyword of the namespace it is written in"
    (is (= ["::probe-own"] (candidates (in-code "::probe-ow")))))
  (testing "::alias/name is one of the namespace that alias stands for, with
  the alias written back on - a candidate is what goes in the buffer"
    (is (= ["::string/probe-aliased"] (candidates (in-code "::string/probe-al"))))
    (is (= "clojure.string"
           (:ns (found (in-code "::string/probe-al") "::string/probe-aliased")))))
  (testing "an alias and nothing else, since the reader takes nothing else
  there: ::clojure.string/x is an invalid token in a namespace that required
  clojure.string without aliasing it"
    (is (empty? (candidates (in-code "::clojure.string/probe-al")))))
  (testing "and the aliases themselves, which is what stands before the slash
  of an ::alias/name - answered as the namespaces they open"
    (is (= {:candidate "::string" :type "namespace" :ns "clojure.string"}
           (dissoc (found (in-code "::strin") "::string") :match-index)))))

;;; A path written in a string

(defn- in-string
  "What is offered where TEXT is being written inside a string, in a form
  headed by CALL."
  [call argument text]
  (cond-> {:position :string :ns @probe :text text}
    call (assoc :call call)
    argument (assoc :argument argument)))

(deftest a-resource-is-what-is-neither-a-class-nor-a-source
  (let [found (set (:resources (classpath/scan)))]
    (is (contains? found "clojure/version.properties"))
    (testing "a source is on the classpath as the namespace it provides, and
    a class as the class it is - a path to one is not how either is asked for,
    and neither is one of a class no name can be read out of"
      (is (not-any? (fn [^String name] (string/ends-with? name ".class")) found))
      (is (not-any? (fn [^String name] (or (string/ends-with? name ".clj")
                                           (string/ends-with? name ".cljc")))
                    found)))
    (testing "a jar holds an entry for each directory in it, and a name with
    nothing at the end of it is not a resource anybody reads"
      (is (not-any? (fn [^String name] (string/ends-with? name "/")) found)))))

(deftest a-string-is-a-path-where-the-call-it-is-written-in-reads-one
  (let [dir (temp-dir)]
    (try
      (with-entry
        dir
        (fn []
          (write-file! dir "probe" "written.edn")
          (classpath/rescan!)
          (is (contains? (typed (in-string "io/resource" 1 "probe/writ"))
                         "probe/written.edn")
              "the call written under the alias the namespace gave it, which
              is a name only this side can resolve")
          (is (contains? (typed (in-string "clojure.java.io/resource" 1 "probe/writ"))
                         "probe/written.edn")
              "and written out in full, which is the same var")
          (testing "and nothing where the call does not read one, since most
          strings are text rather than paths"
            (is (empty? (candidates (in-string "str" 1 "probe/writ"))))
            (is (empty? (candidates (in-string "slurp" 1 "probe/writ"))))
            (is (empty? (candidates (in-string "io/file" 1 "probe/writ")))))
          (testing "nothing at an argument that is not the one it reads"
            (is (empty? (candidates (in-string "io/resource" 2 "probe/writ"))))
            (is (empty? (candidates (in-string "io/resource" 0 "probe/writ")))))
          (testing "and nothing where the client read no call around the
          string, which a string at the top of a file is written at"
            (is (empty? (candidates (in-string nil nil "probe/writ"))))
            (is (empty? (candidates (in-string nil 1 "probe/writ")))))
          (testing "a call that resolves to nothing is one more way of naming
          nothing, and so is one reaching for a class that is not there"
            (is (empty? (candidates (in-string "no-such-fn" 1 "probe/writ"))))
            (is (empty? (candidates (in-string "no.such.Class/of" 1 "probe/writ")))))))
      (finally (delete-recursively dir)))))

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
        (testing "reading the classpath again, which is what a file written
        after the process started waits for"
          (let [dir (temp-dir)
                property (System/getProperty "java.class.path")
                ask (fn [id] (mapv :candidate
                                   (:completions
                                    (request! client {:op :completions :position :namespace
                                                      :text "overthewire" :id id}))))]
            (try
              (write-file! dir "made" "overthewire.clj")
              ;; the property rather than a loader, because the op is answered
              ;; on the thread of the connection and a loader is this one's
              (System/setProperty "java.class.path"
                                  (str property (System/getProperty "path.separator") dir))
              (is (empty? (ask 5)))
              (let [reply (request! client {:op :update-classpath :id 6})]
                (is (= "reply" (:tag reply)))
                (is (pos? (:namespaces reply)))
                (is (pos? (:classes reply))))
              (is (= ["made.overthewire"] (ask 7)))
              (finally
                (System/setProperty "java.class.path" property)
                (classpath/rescan!)
                (delete-recursively dir)))))
        (finally (disconnect client))))))

(deftest the-op-answers-a-name-written-in-code
  (with-process [info nil]
    (let [client (control-client info)]
      (try
        (testing "the locals travel with the request, since a name bound by
        the form being written is one the process has never seen"
          (let [reply (request! client {:op :completions :position :code :ns "clojure.core"
                                        :text "ma" :locals [{:name "map-of-mine"}] :id 1})]
            (is (= "reply" (:tag reply)))
            (is (contains? (set (map :candidate (:completions reply))) "map-of-mine"))))
        (testing "and a local written as something that is not one is refused"
          (let [reply (request! client {:op :completions :position :code
                                        :text "" :locals ["map-of-mine"] :id 2})]
            (is (= "error" (:tag reply)))
            (is (= "invalid-message" (:error reply)))))
        (testing "the connection survives it"
          (is (= "reply" (:tag (request! client {:op :completions :position :code
                                                 :text "redu" :id 3})))))
        (finally (disconnect client))))))
