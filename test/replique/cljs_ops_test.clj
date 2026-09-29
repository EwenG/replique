(ns replique.cljs-ops-test
  "The reading ops, asked about ClojureScript.

  `:namespaces', `:vars', `:symbol', `:completions' and `:remove-var' are one
  question each and every one of them has two answers, because a process
  running the compiler holds two symbol tables. Which of them a request is
  about is the `:dialect' it carries, and these are the tests of that key -
  what each op says when it is set, and what it says when the process cannot
  answer for ClojureScript at all.

  Written for both processes, as `replique.cljs-test' is: the compiler is not
  a dependency of replique, and a process is running with it or without it.
  With one:

    clojure -M:test:cljs ...

  ONE PROCESS FOR THE WHOLE FILE, for `replique.cljs-repl-test's reason: the
  first ClojureScript question compiles cljs.core, and the first repl
  connection starts node on top of it - seconds each, and both of them once
  per process rather than once per test. What is given up for that is
  isolation between namespaces, and it is bought back the way that file buys
  it: every test here works in namespaces of its own.

  The namespaces are named ops.* and the name is this file's own. A test runs
  inside the process it tests, so a namespace one test makes outlives it - see
  `replique.spellings-test', where two files sharing a name was a red test for
  a while."
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [replique.cljs :as cljs]
            [replique.core :as core]
            [replique.test-client :as client
             :refer [control-client disconnect eval! recv repl-client request!]]))

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

;;; Asking

(defn- ask
  "Put one request and read its reply."
  [msg]
  (let [c (control-client @the-process)]
    (try (request! c (assoc msg :id 1))
         (finally (disconnect c)))))

(defn- about
  "The same request, asked about ClojureScript on node."
  [msg]
  (ask (assoc msg :dialect :cljs :target :node)))

(defmacro ^:private with-cljs-repl
  "Body with a ClojureScript repl on node, which is how a namespace is made."
  [sym & body]
  `(let [~sym (repl-client @the-process {:dialect :cljs :target :node} slow)]
     (try ~@body (finally (disconnect ~sym)))))

(defmacro ^:private with-clj-repl
  [sym & body]
  `(let [~sym (repl-client @the-process nil slow)]
     (try ~@body (finally (disconnect ~sym)))))

(defn- candidates
  "The candidates of a completion reply, as they would be written."
  [reply]
  (mapv :candidate (:completions reply)))

(defn- named
  "The candidate NAMED, with what was said about it, or nil."
  [reply named]
  (first (filter #(= named (:candidate %)) (:completions reply))))

(defn- source-of
  "The file an answer names, as a path that carries its extension.

  The `entry' where there is one, since a source inside a jar answers with
  the path of the jar and the entry inside it - and it is the entry that is
  the .clj."
  [answer]
  (str (or (:entry answer) (:file answer))))

;;; A process that cannot be asked at all

(deftest test-a-process-without-the-compiler-refuses-a-clojurescript-question
  ;; Refused rather than answered with nothing, which is the distinction
  ;; `cljs/refuse-unless-available!' exists to keep: a process with no
  ;; compiler has no ClojureScript namespaces AT ALL, so an empty list would
  ;; read as "there is nothing in it" rather than as "this cannot say".
  (when-not (compiling?)
    (let [r (about {:op :namespaces})]
      (is (= "error" (:tag r)))
      (is (= "no-cljs" (:error r)))
      (is (string/includes? (:message r) "ClojureScript compiler"))
      (is (string/includes? (:message r) "classpath")))
    (testing "and every one of them the same way, since none of them can be
    answered and the reason is the same one"
      (doseq [msg [{:op :vars :ns "cljs.core"}
                   {:op :symbol :position :code :ns "cljs.core" :text "first"}
                   {:op :completions :position :code :ns "cljs.core" :text "fir"}
                   {:op :spellings :ns "cljs.core" :vars ["cljs.core/map"]}
                   {:op :remove-var :var "cljs.core/first"}
                   ;; and the two that read the analysis rather than a symbol
                   ;; table, whose refusal is still this one: a process with no
                   ;; compiler has compiled nothing, so what is missing is the
                   ;; compiler and not what it would have recorded
                   {:op :usages :position :code :ns "cljs.core" :text "first"}
                   {:op :stale}]]
        (is (= "no-cljs" (:error (about msg))) (pr-str (:op msg)))))
    (testing "including the positions that would have answered without reading
    a symbol table at all, which is why the refusal is at the top of the op and
    not where a namespace is looked for: what a :flag takes is a list of three
    keywords, and answering it would be this process saying something about a
    language it cannot compile"
      (doseq [msg [{:op :completions :position :flag :text ":rel"}
                   {:op :completions :position :dependency-type :text ":req"}
                   {:op :symbol :position :string :text "clojure/core.clj"}]]
        (is (= "no-cljs" (:error (about msg))) (pr-str (:position msg)))))))

;;; What the client got wrong

(deftest test-a-dialect-this-process-does-not-speak-is-refused-by-name
  ;; Whether or not there is a compiler: a dialect nothing can answer is a
  ;; client that believes this process reads a third way, and it is wrong
  ;; about that on every process.
  (let [r (ask {:op :namespaces :dialect :fortran})]
    (is (= "error" (:tag r)))
    ;; the kind the repl handshake refuses a dialect with, because it is the
    ;; same mistake asked on another connection
    (is (= "invalid-dialect" (:error r)))
    (is (string/includes? (:message r) "fortran"))
    (is (string/includes? (:message r) "cljs"))))

(deftest test-a-target-there-is-no-such-thing-as-is-refused-by-name
  (when (compiling?)
    (let [r (ask {:op :namespaces :dialect :cljs :target :toaster})]
      (is (= "error" (:tag r)))
      (is (= "invalid-target" (:error r)))
      (is (string/includes? (:message r) "toaster"))
      (is (string/includes? (:message r) "node")))))

(deftest test-a-question-with-no-dialect-is-a-clojure-question
  ;; Which is what makes this key one a client can leave out. Every message
  ;; written before there was a second answer goes on meaning what it meant.
  (let [r (ask {:op :namespaces})]
    (is (= "reply" (:tag r)))
    (is (some #{"replique.ops"} (:namespaces r)))
    ;; cljs.user is the repl's starting namespace on the ClojureScript side
    ;; and is nothing at all on this one
    (is (not-any? #{"cljs.user"} (:namespaces r)))))

;;; The namespaces of the two worlds

(deftest test-the-namespaces-of-the-two-worlds-are-two-different-lists
  (when (compiling?)
    (with-cljs-repl r
      (eval! r "#replique/ns ops.listed\n(def here 1)")
      (let [cljs (:namespaces (about {:op :namespaces}))
            clj (:namespaces (ask {:op :namespaces}))]
        (testing "the ClojureScript one holds what the compiler compiled"
          (is (some #{"cljs.core"} cljs))
          (is (some #{"ops.listed"} cljs)))
        (testing "and not what the jvm loaded, which is a different set and
        not a subset - a process holds both and neither is the other's"
          (is (not-any? #{"replique.ops"} cljs))
          (is (not-any? #{"ops.listed"} clj)))
        (testing "a name in both lists is two namespaces and not one shared:
        cljs/core.cljc is a ClojureScript namespace AND the jvm namespace its
        macros are written in, so cljs.core is a name each world has its own
        of - which is the whole reason a question has to say which it means"
          (is (some #{"cljs.core"} cljs))
          (is (some #{"cljs.core"} clj)))
        (testing "sorted, as the Clojure list is"
          (is (= (sort cljs) cljs)))))))

;;; What a namespace holds

(deftest test-the-vars-of-a-clojurescript-namespace-in-the-order-they-were-written
  (when (compiling?)
    (with-cljs-repl r
      ;; One `eval!' per form, because `eval!' reads up to the next prompt and
      ;; this repl writes one prompt per form. A directive is the exception -
      ;; it moves the repl and reads on, so it shares the prompt of the form
      ;; under it.
      (eval! r "#replique/ns ops.held\n(def first-one 1)")
      (eval! r "(defn second-one [x] x)")
      (eval! r "(def ^:private third-one 3)")
      (let [vars (:vars (about {:op :vars :ns "ops.held"}))]
        (is (= ["first-one" "second-one" "third-one"] (mapv :name vars)))
        (testing "each one annotated the way a Clojure var is - a def is a
        var and a defn is a function, read off the same metadata"
          (is (= ["var" "function" "var"] (mapv :type vars))))
        (testing "and a private one said to be private, since what can be
        chosen here is what :remove-var can be asked about"
          (is (= [nil nil true] (mapv :private vars))))))))

(deftest test-a-clojurescript-namespace-the-compiler-does-not-have-holds-no-vars
  ;; Answered with none rather than refused, which is what the Clojure side
  ;; means by it too: every file is a namespace the process does not have
  ;; until something loads it.
  (when (compiling?)
    (let [r (about {:op :vars :ns "ops.never-compiled"})]
      (is (= "reply" (:tag r)))
      (is (= [] (:vars r))))))

;;; What a name means

(deftest test-a-core-name-resolves-although-the-namespace-maps-nothing-of-it
  ;; THE RULE THAT IS NOT A MAPPING. ClojureScript refers cljs.core into every
  ;; namespace whether it says so or not, and the compiler honours that with a
  ;; rule rather than with a thousand entries - so a reader that looked only
  ;; at what the namespace maps would answer that `map' means nothing here.
  (when (compiling?)
    (with-cljs-repl r
      (eval! r "#replique/ns ops.plain\n(def mine 1)")
      (let [found (:symbol (about {:op :symbol :position :code
                                   :ns "ops.plain" :text "map"}))]
        (is (= "function" (:type found)))
        (is (= "map" (:name found)))
        (is (= "cljs.core" (:ns found)))
        (testing "with the file it was written in, which is the .cljs and not
        the .clj of the same name"
          (is (string/ends-with? (str (:file found)) "core.cljs"))
          (is (integer? (:line found))))))))

(deftest test-an-arglist-reads-the-same-way-in-both-dialects
  ;; ClojureScript writes :arglists QUOTED, because metadata there is data
  ;; that has to survive into the emitted program. Read as it stands, the
  ;; first way to call `first' would come back as `quote'.
  (when (compiling?)
    (let [cljs (:symbol (about {:op :symbol :position :code
                                :ns "cljs.core" :text "map"}))
          clj (:symbol (ask {:op :symbol :position :code
                             :ns "clojure.core" :text "map"}))]
      (is (= ["[f]" "[f coll]"] (take 2 (:arglists cljs))))
      (is (= (:arglists clj) (:arglists cljs)))
      (is (not-any? #{"quote"} (:arglists cljs))))))

(deftest test-a-name-the-namespace-excluded-is-not-cores
  ;; :refer-clojure :exclude is the one thing standing between what cljs.core
  ;; holds and what a namespace can write without a slash, and a reader
  ;; applying the compiler's rule has to apply that half of it too.
  (when (compiling?)
    (with-cljs-repl r
      (eval! r "(ns ops.excluding (:refer-clojure :exclude [map]))")
      (is (nil? (:symbol (about {:op :symbol :position :code
                                 :ns "ops.excluding" :text "map"}))))
      (testing "and what it did not exclude is untouched"
        (is (= "cljs.core" (:ns (:symbol (about {:op :symbol :position :code
                                                 :ns "ops.excluding"
                                                 :text "filter"})))))))))

(deftest test-a-scope-is-a-namespace-the-namespace-required
  ;; Where Clojure answers for anything the process has loaded, ClojureScript
  ;; answers only for what this namespace reached: an alias, its own name, or
  ;; something it required. A namespace that was merely compiled is a typo
  ;; away from resolving, and the compiler refuses it.
  (when (compiling?)
    (with-cljs-repl r
      (eval! r "#replique/ns ops.away\n(defn far [] :far)")
      (eval! r "(ns ops.near (:require [ops.away :as away]))")
      (testing "through the alias it gave it"
        (is (= "away/far" (:candidate (named (about {:op :completions :position :code
                                                     :ns "ops.near" :text "away/f"})
                                             "away/far"))))
        (is (= "ops.away" (:ns (:symbol (about {:op :symbol :position :code
                                                :ns "ops.near" :text "away/far"}))))))
      (testing "and under its own name, which it required"
        (is (= "ops.away" (:ns (:symbol (about {:op :symbol :position :code
                                                :ns "ops.near"
                                                :text "ops.away/far"}))))))
      (testing "but not from a namespace that never required it"
        (eval! r "#replique/ns ops.stranger\n(def unrelated 1)")
        (is (nil? (:symbol (about {:op :symbol :position :code
                                   :ns "ops.stranger" :text "ops.away/far"}))))
        (is (= [] (candidates (about {:op :completions :position :code
                                      :ns "ops.stranger" :text "ops.away/"}))))))))

;;; What could be written

(deftest test-a-completion-offers-the-core-names-a-namespace-can-write
  (when (compiling?)
    (with-cljs-repl r
      (eval! r "#replique/ns ops.typing\n(def partly 1)")
      (let [reply (about {:op :completions :position :code
                          :ns "ops.typing" :text "part"})]
        (testing "what cljs.core holds, although the namespace maps none of it"
          (is (= "cljs.core" (:ns (named reply "partial"))))
          (is (= "function" (:type (named reply "partial")))))
        (testing "beside what the namespace does map"
          (is (= "ops.typing" (:ns (named reply "partly")))))))))

(deftest test-a-completion-offers-a-namespace-nobody-has-compiled
  ;; Read off the classpath rather than out of the compiler, which is what
  ;; makes it an answer for a namespace a require has not reached yet - and
  ;; the classpath is read twice, once for each extension. See
  ;; `classpath/cljs-source-extensions'.
  (when (compiling?)
    (let [reply (about {:op :completions :position :namespace :text "rt.main"})]
      (is (some #{"rt.main-program"} (candidates reply))))
    (testing "and the Clojure list does not hold it, because a .cljs is a
    namespace of one world only"
      (is (not-any? #{"rt.main-program"}
                    (candidates (ask {:op :completions :position :namespace
                                      :text "rt.main"})))))))

(deftest test-a-file-that-is-both-is-a-namespace-of-both-worlds
  ;; A .cljc, which is the one extension in both lists. One file, read once by
  ;; each of the two compilers.
  (let [cljs? (compiling?)
        clj (candidates (ask {:op :completions :position :namespace :text "rt.shared"}))]
    (is (some #{"rt.shared-thing"} clj))
    (when cljs?
      (is (some #{"rt.shared-thing"}
                (candidates (about {:op :completions :position :namespace
                                    :text "rt.shared"})))))))

(deftest test-a-class-is-not-an-answer-in-clojurescript
  ;; A name before a slash or after a dot reaches a JavaScript object there,
  ;; and answering it out of this jvm's classpath would be answering a
  ;; question about one language with a fact about another.
  (when (compiling?)
    (testing "nothing at the position an :import is written at"
      (is (= [] (candidates (about {:op :completions :position :package-or-class
                                    :text "String"}))))
      (is (nil? (:symbol (about {:op :symbol :position :package-or-class
                                 :text "java.util.Date"})))))
    (testing "and no class where code is written, although the jvm has one of
    that name and the Clojure side answers with it"
      (is (nil? (:symbol (about {:op :symbol :position :code :ns "cljs.core"
                                 :text "java.util.Date"}))))
      (is (= "class" (:type (:symbol (ask {:op :symbol :position :code
                                           :ns "clojure.core"
                                           :text "java.util.Date"}))))))
    (testing "and a string literal is not a java.lang.String to read members
    off, which is what reading what it is written on as this jvm would make
    of it"
      (is (nil? (:symbol (about {:op :symbol :position :code :ns "cljs.core"
                                 :on "\"x\"" :text ".length"})))))
    (testing "and none is offered either, although a name with a dot in it is
    exactly where the Clojure side starts offering them"
      (is (= [] (candidates (about {:op :completions :position :code
                                    :ns "cljs.core" :text "java.util.Da"}))))
      (is (some #{"java.util.Date"}
                (candidates (ask {:op :completions :position :code
                                  :ns "clojure.core" :text "java.util.Da"})))))))

(deftest test-what-a-member-is-written-on-and-the-runtime-are-two-keys
  ;; THEY WERE ONE, AND ONE MESSAGE CARRIES BOTH. A ClojureScript question says
  ;; which runtime it is about, and a name written on something says what it is
  ;; written on - and while both were `:target', a .cljs buffer asking about a
  ;; member wrote the key twice and the process refused the whole line as
  ;; unreadable EDN. Which named neither of them: the client had asked a
  ;; perfectly good question and was told its message would not read.
  ;;
  ;; SENT AS THE TEXT OF A LINE rather than as a map, because that is where the
  ;; failure was. A map with one key twice is a map with one key - `assoc' takes
  ;; the second and nothing is ever wrong - so a test that built one would pass
  ;; against the collision as happily as against the fix.
  (when (compiling?)
    (let [c (control-client @the-process)]
      (try
        (let [reply (request! c (str "{:op :symbol :id 1 :position :code"
                                     " :ns \"cljs.core\" :text \".length\""
                                     " :on \"\\\"x\\\"\""
                                     " :dialect :cljs :target :node}"))]
          (is (= "reply" (:tag reply)) (pr-str reply))
          (is (nil? (:symbol reply))
              "answered, and answered with nothing: a literal is not a class
              there"))
        (finally (disconnect c))))))

(deftest test-a-macro-namespace-is-read-in-the-clojure-world
  ;; The one name in a .cljs buffer that is not a ClojureScript name. The
  ;; macros of a ClojureScript namespace are written in Clojure and live in
  ;; this jvm, so a :require-macros is asked of this world although everything
  ;; around it is asked of the other.
  (when (compiling?)
    ;; clojure.java.io and not clojure.string, which would prove nothing:
    ;; there is a clojure/string.cljs beside the clojure/string.clj, so a name
    ;; in both worlds is answered either way round. A namespace only this jvm
    ;; has is what says which world was read.
    ;;
    ;; AND NOT clojure.walk EITHER, WHICH IS WHAT THIS USED TO SAY. It was a
    ;; jvm-only name on the day it was written and it is not one now: the
    ;; ClojureScript standard library grew a clojure/walk.cljs, so the name
    ;; this test leant on moved into both worlds and the second half of it
    ;; started failing. The property is right and the witness was perishable -
    ;; a `java' in the name is what makes this one not perish, since a
    ;; namespace wrapping java.io is not a namespace ClojureScript will ever
    ;; have.
    (testing "a namespace only the jvm has is offered, and is what it says"
      (is (some #{"clojure.java.io"}
                (candidates (about {:op :completions :position :namespace-macros
                                    :text "clojure.java.i"}))))
      (is (= "namespace" (:type (:symbol (about {:op :symbol :position :namespace-macros
                                                 :text "clojure.java.io"}))))))
    (testing "and it is not offered at the position beside it, which is the
    same request with one key changed"
      (is (not-any? #{"clojure.java.io"}
                    (candidates (about {:op :completions :position :namespace
                                        :text "clojure.java.i"}))))
      (is (nil? (:symbol (about {:op :symbol :position :namespace
                                 :text "clojure.java.io"})))))
    (testing "a name both worlds have is answered with the file of whichever
    world the position names - one request, one key apart"
      (is (string/ends-with? (source-of (:symbol (about {:op :symbol :position :namespace
                                                         :text "clojure.string"})))
                             ".cljs"))
      (is (string/ends-with? (source-of (:symbol (about {:op :symbol
                                                         :position :namespace-macros
                                                         :text "clojure.string"})))
                             ".clj")))))


;;; What a namespace calls a var

(deftest test-what-a-clojurescript-namespace-writes-a-core-var-as
  ;; The same question `:spellings' answers for Clojure, asked of the other
  ;; table - and the answer has to come out of the compiler's rule rather than
  ;; out of the mappings, because cljs.core is referred by a rule.
  (when (compiling?)
    (with-cljs-repl r
      (eval! r "#replique/ns ops.spelt\n(def unrelated 1)")
      (let [spellings (:spellings (about {:op :spellings :ns "ops.spelt"
                                          :vars ["cljs.core/map"]}))]
        (is (= ["cljs.core/map" "map"] (:cljs.core/map spellings))))
      (testing "and a name the namespace excluded is written only in full,
      which is the one way left to write it there"
        (eval! r "(ns ops.spelt-not (:refer-clojure :exclude [map]))")
        (is (= ["cljs.core/map"]
               (:cljs.core/map (:spellings (about {:op :spellings :ns "ops.spelt-not"
                                                   :vars ["cljs.core/map"]}))))))
      (testing "an alias is another way to write it, as it is in Clojure"
        (eval! r "(ns ops.spelt-aliased (:require [cljs.core :as c]))")
        (is (= ["c/map" "cljs.core/map" "map"]
               (:cljs.core/map (:spellings (about {:op :spellings :ns "ops.spelt-aliased"
                                                   :vars ["cljs.core/map"]})))))))))

(deftest test-a-clojure-var-is-not-a-spelling-in-the-other-world
  ;; clojure.core/map and cljs.core/map are two vars, and a question about one
  ;; of them asked of the other world names nothing to answer about.
  (when (compiling?)
    (let [spellings (:spellings (about {:op :spellings :ns "cljs.user"
                                        :vars ["clojure.core/map" "cljs.core/map"]}))]
      (is (nil? (:clojure.core/map spellings)))
      (is (= ["cljs.core/map" "map"] (:cljs.core/map spellings))))
    (testing "and the other way round, on the Clojure side"
      (let [spellings (:spellings (ask {:op :spellings :ns "clojure.core"
                                        :vars ["clojure.core/map" "cljs.core/map"]}))]
        (is (= ["clojure.core/map" "map"] (:clojure.core/map spellings)))))))

;;; Taking a definition away

(deftest test-a-clojurescript-var-is-taken-away-from-every-namespace-that-had-it
  (when (compiling?)
    (with-cljs-repl r
      (eval! r "#replique/ns ops.home\n(defn gone [] :here)")
      (eval! r "(ns ops.calls (:require [ops.home :refer [gone]]))")
      (eval! r "(ns ops.renames (:require [ops.home :refer [gone] :rename {gone went}]))")
      (let [reply (about {:op :remove-var :var "ops.home/gone"})]
        (is (= "reply" (:tag reply)))
        (is (= "ops.home/gone" (:removed reply)))
        (testing "named where each namespace writes it, which is what says
        whose code will not compile until somebody edits it"
          (is (= {:ops.home ["gone"] :ops.calls ["gone"] :ops.renames ["went"]}
                 (:unmapped reply))))
        (testing "and it resolves nowhere afterwards"
          (is (nil? (:symbol (about {:op :symbol :position :code
                                     :ns "ops.home" :text "gone"}))))
          (is (nil? (:symbol (about {:op :symbol :position :code
                                     :ns "ops.renames" :text "went"})))))))))

(deftest test-removing-a-clojurescript-var-leaves-the-clojure-one-alone
  ;; The two worlds hold the same name at once, and a request that named one
  ;; of them must not reach into the other.
  (when (compiling?)
    (with-cljs-repl cljs-repl
      (with-clj-repl clj-repl
        (eval! cljs-repl "#replique/ns ops.twice\n(defn both [] :in-javascript)")
        (eval! clj-repl "(ns ops.twice)")
        (eval! clj-repl "(defn both [] :on-the-jvm)")
        (is (= "ops.twice/both" (:removed (about {:op :remove-var :var "ops.twice/both"}))))
        (testing "the Clojure var of that very name is still there"
          (is (= "function" (:type (:symbol (ask {:op :symbol :position :code
                                                  :ns "ops.twice" :text "both"}))))))
        (testing "and removing that one is a second request"
          (is (= "ops.twice/both"
                 (:removed (ask {:op :remove-var :var "ops.twice/both"}))))
          (is (nil? (:symbol (ask {:op :symbol :position :code
                                   :ns "ops.twice" :text "both"})))))))))

(deftest test-a-clojurescript-var-that-is-interned-nowhere-is-refused
  ;; As the Clojure side refuses it, and for the same reason: this asks for
  ;; one thing to be done to one var, and a request that found nothing to do
  ;; has not been carried out.
  (when (compiling?)
    (let [r (about {:op :remove-var :var "ops.nowhere/nothing"})]
      (is (= "error" (:tag r)))
      (is (= "unknown-var" (:error r))))))

;;; The module a page loads

(deftest test-the-main-module-is-written-where-the-client-said
  (when (compiling?)
    ;; THE OTHER HALF OF :main. That compiles a program into the output
    ;; directory and stops, because on the browser the runtime is a page
    ;; somebody opens - and this is the file that page includes in order to be
    ;; the runtime and to load the program. Neither side knows both things:
    ;; where it goes is a fact about the application's assets, and what goes in
    ;; it is a port that is different every time.
    (let [out (io/file (client/temp-dir) "assets" "js" "main.js")
          r   (ask {:op :main-js :file (str out) :main "ops.main-module"})]
      (try
        (is (= "reply" (:tag r)) (pr-str r))
        (is (= (str out) (:file r)))
        (is (string/starts-with? (str (:url r)) "http://"))
        (testing "the directories under it were made"
          (is (.isFile out)))
        (let [text (slurp out)]
          (testing "it begins with the marker a client finds it again by"
            (is (string/starts-with? text "//replique-2 main module")))
          (testing "and that marker is not one replique 1 would claim"
            ;; Master matches its own by comparing this many characters, so a
            ;; first line beginning with it would be rewritten with master's
            ;; port - two repliques fighting over one file.
            (is (not (string/starts-with?
                      text "//main-js-file autogenerated by replique"))))
          (testing "the four constants are each on a line of their own"
            (doseq [c ["host" "port" "mainNs" "mainPath"]]
              (is (re-find (re-pattern (str "(?m)^const " c " = .*;$")) text) c)))
          (testing "the path is the one the compiler emits, not a second spelling"
            ;; ops.main-module, with the hyphen kept: this compiler does not
            ;; munge a namespace into its file name, and a client computing the
            ;; path itself would get ops/main_module.js and a 404.
            (is (string/includes? text "\"ns/ops/main-module.js\"") text))
          (testing "and it imports the runtime and then the program"
            (is (string/includes? text "runtime_browser.js"))
            (is (string/includes? text "await connect();"))
            (is (string/includes? text "import(base + mainPath)"))))
        (finally (client/delete-recursively (.getParentFile (.getParentFile out))))))))

(deftest test-a-main-module-can-name-no-program
  (when (compiling?)
    ;; A repl in a page of yours with no program in it, which is a thing to
    ;; want - and the page still has to connect, so the file is still written.
    (let [out (io/file (client/temp-dir) "main.js")
          r   (ask {:op :main-js :file (str out)})]
      (try
        (is (= "reply" (:tag r)) (pr-str r))
        (is (nil? (:main r)))
        (let [text (slurp out)]
          (is (re-find #"(?m)^const mainNs = null;$" text) text)
          (is (re-find #"(?m)^const mainPath = null;$" text) text)
          (is (string/includes? text "await connect();")))
        (finally (client/delete-recursively (.getParentFile out)))))))

(deftest test-a-main-module-for-something-that-is-not-a-name-is-refused
  (when (compiling?)
    (let [out (io/file (client/temp-dir) "main.js")
          r   (ask {:op :main-js :file (str out) :main 42})]
      (try
        (is (= "error" (:tag r)))
        (is (= "invalid-main" (:error r)))
        (testing "and nothing was written"
          (is (not (.exists out))))
        (finally (client/delete-recursively (.getParentFile out)))))))

(deftest test-a-main-module-with-nowhere-to-go-is-refused
  (when (compiling?)
    (let [r (ask {:op :main-js})]
      (is (= "error" (:tag r)))
      (is (= "invalid-message" (:error r))))))

(deftest test-refreshing-the-main-modules-again
  ;; THE PORT IS NOT THE ONLY HALF THAT GOES STALE. A browser runtime refreshes
  ;; every main module under this directory when it starts, which is the moment
  ;; the port changes and the only such moment the process can find by itself.
  ;; It is not the only moment the answer changes: a project directory that is
  ;; a tree of links into a checkout elsewhere has ANOTHER checkout's modules
  ;; under it the moment those links move, naming whichever port was current
  ;; the day they were last written. The process cannot see that happen, so
  ;; this is the op whoever moved them says it with.
  (when (compiling?)
    (let [dir (io/file (:directory (ask {:op :process-info})))
          a   (io/file dir "refresh-me" "main.js")]
      (try
        ;; Writing one starts the browser runtime, which is the port every
        ;; refresh after this moves them to.
        (let [url     (:url (ask {:op :main-js :file (str a) :main "ops.refreshed"}))
              current (slurp a)]
          (testing "a module already naming this port is found and left where it is"
            ;; Only where it changed: a file already naming this port is a file
            ;; whose modification time means something to somebody's build.
            (let [r (ask {:op :refresh-main-js})
                  m (first (filter #(= (str a) (:file %)) (:modules r)))]
              (is (= "reply" (:tag r)) (pr-str r))
              (is (= url (:url r)))
              (is (some? m) (pr-str (:modules r)))
              (is (false? (:refreshed m)))
              (testing "and it is answered beside the program its page loads"
                (is (= "ops.refreshed" (:main m))))))
          (testing "and one naming yesterday's port is moved to this one"
            (spit a (string/replace current #"const port = \"[^\"]*\";"
                                    "const port = \"1\";"))
            (let [r (ask {:op :refresh-main-js})
                  m (first (filter #(= (str a) (:file %)) (:modules r)))]
              (is (true? (:refreshed m)) (pr-str m))
              (testing "which is the file as the process itself wrote it"
                ;; The host and the port and nothing else: `mainNs' and
                ;; `mainPath' are the choice of whoever wrote the page.
                (is (= current (slurp a)))))))
        (finally (when (.exists (.getParentFile a))
                   (client/delete-recursively (.getParentFile a))))))))

(deftest test-which-programs-this-project-has
  ;; THE OTHER HALF OF `mainNs'. A main module names the namespace its page
  ;; loads for whoever finds the file - the browser imports `mainPath' and
  ;; never reads the name - and this is the op that reader asks. What it
  ;; answers is a menu: which programs this project can be started on, which is
  ;; what a client offers as the `:main' of a ClojureScript repl so that the
  ;; page's import lands on a program rather than on a 404. Replique 1 built
  ;; the same list by walking the project from the editor.
  (when (compiling?)
    (let [dir (io/file (:directory (ask {:op :process-info})))
          a   (io/file dir "menu-a" "main.js")
          b   (io/file dir "menu-b" "main.js")]
      (try
        (ask {:op :main-js :file (str a) :main "ops.menu"})
        (ask {:op :main-js :file (str b)})
        (let [r       (ask {:op :main-modules})
              by-file (into {} (map (juxt :file :main)) (:modules r))]
          (is (= "reply" (:tag r)) (pr-str r))
          (testing "both were found, and each is named beside its program"
            (is (= "ops.menu" (get by-file (str a))))
            (testing "and the one naming no program is answered all the same"
              (is (contains? by-file (str b)))
              (is (nil? (get by-file (str b))))))
          (testing "they are the files this process wrote and not a list it kept"
            ;; Nothing is remembered between asks: these live in an
            ;; application's own assets and are edited by whoever edits those.
            (client/delete-recursively (.getParentFile a))
            (let [again (into #{} (map :file) (:modules (ask {:op :main-modules})))]
              (is (not (contains? again (str a))))
              (is (contains? again (str b))))))
        (finally (doseq [^java.io.File d [(.getParentFile a) (.getParentFile b)]]
                   (when (.exists d) (client/delete-recursively d))))))))

(deftest test-a-macro-is-read-in-the-other-world
  ;; A ClojureScript namespace maps its vars and none of its macros: those are
  ;; Clojure vars of this jvm, reached through the compiler's macro view and
  ;; through cljs.core's macros by the rule that reaches its functions. So
  ;; `defn' and `when', which are macros there and nothing else, were names
  ;; that meant nothing - no eldoc, no completion, no binding forms.
  (when (compiling?)
    (with-cljs-repl c
      (eval! c (str "#replique/ns ops.macros\n"
                    "(ns ops.macros (:refer-clojure :exclude [when])"
                    " (:require-macros [cljs.test :as t :refer [deftest]]))"))
      (eval! c "#replique/ns ops.macros\n(defn when [x] x)")
      (let [symbol-of (fn [ns text]
                        (:symbol (about {:op :symbol :position :code :ns ns :text text})))]
        (testing "a core macro, bare and written in full either way"
          (is (= ["macro" "cljs.core" "defn"]
                 ((juxt :type :ns :name) (symbol-of "cljs.user" "defn"))))
          (is (seq (:arglists (symbol-of "cljs.user" "when"))))
          (is (= "macro" (:type (symbol-of "cljs.user" "cljs.core/when"))))
          (is (= "cljs.core" (:ns (symbol-of "cljs.user" "clojure.core/when")))))
        (testing "after the var, which wins a name that is both"
          (is (= "function" (:type (symbol-of "cljs.user" "str")))))
        (testing "a :refer-clojure :exclude, and a var of the namespace's own"
          (is (= ["function" "ops.macros"]
                 ((juxt :type :ns) (symbol-of "ops.macros" "when")))))
        (testing "a :refer-macros and an alias of a :require-macros"
          (is (= ["macro" "cljs.test"] ((juxt :type :ns) (symbol-of "ops.macros" "deftest"))))
          (is (= ["macro" "cljs.test"] ((juxt :type :ns) (symbol-of "ops.macros" "t/is"))))))
      (testing "offered where a name is written"
        (is (some #{"when-let"} (candidates (about {:op :completions :position :code
                                                    :ns "cljs.user" :text "whe"}))))
        (is (some #{"cljs.core/when-let"}
                  (candidates (about {:op :completions :position :code
                                      :ns "cljs.user" :text "cljs.core/whe"}))))
        (is (some #{"deftest"} (candidates (about {:op :completions :position :code
                                                   :ns "ops.macros" :text "deft"}))))
        (is (some #{"t/is"} (candidates (about {:op :completions :position :code
                                                :ns "ops.macros" :text "t/"}))))
        (is (some #{"t"} (candidates (about {:op :completions :position :code
                                             :ns "ops.macros" :text "t"})))))
      (testing "and spelled, which is how a client finds the forms that bind"
        (is (= {:clojure.core/let ["cljs.core/let" "let"]}
               (:spellings (about {:op :spellings :ns "cljs.user"
                                   :vars ["clojure.core/let"]}))))
        (is (= {:clojure.core/when ["cljs.core/when"]
                :cljs.test/is ["cljs.test/is" "t/is"]}
               (:spellings (about {:op :spellings :ns "ops.macros"
                                   :vars ["clojure.core/when" "cljs.test/is"]}))))))))

(deftest test-a-name-of-the-host-is-asked-of-the-runtime
  ;; What a JavaScript object holds is whatever the runtime made it, so a name
  ;; written on one is completed by asking the runtime - see
  ;; `replique.cljs/host-names'. Only one that is already there: a completion
  ;; is not a reason to start node or wait for a page.
  (when (compiling?)
    (with-cljs-repl c
      (eval! c (str "#replique/ns ops.host\n"
                    "(ns ops.host (:require [goog.string :as gstr]) (:import [goog.math Long]))"))
      (let [offered (fn [msg]
                      (candidates (about (merge {:op :completions :position :code :ns "ops.host"}
                                                msg))))]
        (testing "under js, and down a path of it"
          (is (some #{"js/console"} (offered {:text "js/con"})))
          (is (some #{"js/console.log"} (offered {:text "js/console.lo"})))
          (is (some #{"js/Math.max"} (offered {:text "js/Math.ma"}))))
        (testing "and not what every object has, which would bury what this one has"
          (is (not-any? #{"js/constructor" "js/hasOwnProperty"} (offered {:text "js/con"}))))
        (testing "a Closure provide, under the alias of a :require, in full, and as an :import"
          (is (some #{"gstr/trim"} (offered {:text "gstr/tri"})))
          (is (some #{"goog.string/trim"} (offered {:text "goog.string/tri"})))
          (is (some #{"Long/fromNumber"} (offered {:text "Long/fromN"})))
          (is (some #{"Long.fromNumber"} (offered {:text "Long.fromN"}))))
        (testing "what a member is written on, where that is the host's or a literal"
          (is (some #{".toUpperCase"} (offered {:text ".toUpp" :on "\"x\""})))
          (is (some #{".-length"} (offered {:text ".-leng" :on "\"x\""})))
          (is (not-any? #{".-length"} (offered {:text ".leng" :on "\"x\""})))
          (is (some #{".log"} (offered {:text ".lo" :on "js/console"})))
          (is (some #{".toFixed"} (offered {:text ".toFix" :on "1.5"}))))
        (testing "and nothing on a local, which is no object the runtime has"
          (is (= [] (offered {:text ".lo" :on "console"
                              :locals [{:name "console"}]}))))
        (testing "nor on a ClojureScript namespace, whose vars are known without asking"
          (is (not-any? #(string/includes? % "$") (offered {:text "cljs.core/ma"}))))))
    (testing "and nothing where no page is there to ask, which is not waited for
    and no server is started for"
      ;; whatever another test here left running, which this must not change
      (let [before (cljs/browser-runtime-url)]
        (is (= [] (candidates (ask {:op :completions :position :code :ns "cljs.user"
                                    :text "js/con" :dialect :cljs :target :browser}))))
        (is (= before (cljs/browser-runtime-url)))))))
