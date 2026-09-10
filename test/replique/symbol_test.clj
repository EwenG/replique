(ns replique.symbol-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as string]
            [replique.completion :as completion]
            [replique.ops]
            [replique.protocol :as protocol]
            [replique.symbol :as sym]
            [replique.test-client :as client
             :refer [control-client disconnect request! with-process]]))

(defn- ask
  "What the op answers, asked the way the op asks it - a position written the
  way a client writes one."
  [msg]
  (:symbol (sym/named (assoc msg :position (protocol/as-keyword (:position msg))))))

(defn- written [text & {:as msg}] (ask (merge {:position :code :text text} msg)))

(defn- error-kind [msg]
  (try (ask msg) nil
       (catch clojure.lang.ExceptionInfo t (:replique/error (ex-data t)))))

;;; What a name is

(deftest a-var-is-answered-with-what-it-is-and-where-it-came-from
  (let [found (written "map" :ns "clojure.core")]
    (is (= "function" (:type found)))
    (testing "the name it has rather than the name it was written as"
      (is (= "map" (:name found)))
      (is (= "clojure.core" (:ns found))))
    (testing "the arglists, one string each, since a client shows them"
      (is (= ["[f]" "[f coll]" "[f c1 c2]" "[f c1 c2 c3]" "[f c1 c2 c3 & colls]"]
             (:arglists found))))
    (is (string/starts-with? (:doc found) "Returns a lazy sequence"))))

(deftest a-macro-is-answered-as-one
  (is (= "macro" (:type (written "when" :ns "clojure.core"))))
  (testing "and a var that is neither is a var"
    (is (= "var" (:type (written "*ns*" :ns "clojure.core"))))))

(deftest a-var-is-answered-under-whatever-name-it-was-written-as
  (let [under (fn [text] (select-keys (written text :ns "replique.symbol-test") [:ns :name]))]
    (testing "referred, aliased, and written out in full are one var"
      (is (= {:ns "clojure.string" :name "join"} (under "string/join")))
      (is (= {:ns "clojure.string" :name "join"} (under "clojure.string/join"))))
    (testing "and the var named / is written with a slash of its own"
      (is (= {:ns "clojure.core" :name "/"} (under "/")))
      (is (= {:ns "clojure.core" :name "/"} (under "clojure.core//"))))))

(deftest a-local-is-what-the-name-means-where-it-is-bound
  (testing "which is what the client sends the locals for: a let that binds
  map is answered as that local and not as clojure.core/map"
    (is (= {:type "local" :name "map"}
           (written "map" :ns "clojure.core" :locals [{:name "map"}]))))
  (testing "and a local of another name takes nothing away"
    (is (= "function" (:type (written "map" :ns "clojure.core" :locals [{:name "x"}]))))))

(deftest a-namespace-is-answered-with-where-requiring-it-would-read-it
  (let [found (written "clojure.string")]
    (is (= "namespace" (:type found)))
    (is (= "clojure.string" (:name found)))
    (is (= "clojure/string.clj" (:entry found)))
    (is (string/starts-with? (:doc found) "Clojure String utilities")))
  (testing "an alias is answered as the namespace it stands for, which is the
  one thing its own name does not say"
    (is (= {:type "namespace" :name "clojure.string"}
           (select-keys (written "string" :ns "replique.symbol-test") [:type :name])))))

(deftest a-namespace-nobody-has-required-is-answered-all-the-same
  (testing "read out of the classpath rather than out of what it holds, which
  is what a name written in an ns form needs: it is required by being written"
    (is (nil? (find-ns 'clojure.core.reducers))
        "the test needs a namespace nothing here has loaded")
    (let [found (written "clojure.core.reducers" :position :namespace)]
      (is (= {:type "namespace" :name "clojure.core.reducers"}
             (select-keys found [:type :name])))
      (is (= "clojure/core/reducers.clj" (:entry found)))
      (testing "and a file that was never read has not said what it is for"
        (is (nil? (:doc found)))))))

(deftest a-special-form-is-answered-with-what-the-compiler-takes
  (let [found (written "if")]
    (is (= "special-form" (:type found)))
    (is (= "if" (:name found)))
    (testing "the head dropped, since an arglist is what is written after the
    name rather than the whole call"
      (is (= ["[test then else?]"] (:arglists found))))
    (is (some? (:doc found))))
  (testing "catch and finally are two the doc command says nothing about -
  what they take is written in what try takes"
    (is (= {:type "special-form" :name "catch"} (written "catch"))))
  (testing "and a starred form is not offered by a completion and not read
  here either"
    (is (nil? (written "let*")))))

(deftest a-class-is-answered-with-its-package-beside-it
  (is (= {:type "class" :name "String" :package "java.lang"} (written "String")))
  (testing "written out in full, which is how one that was not imported is
  written"
    (is (= {:type "class" :name "Date" :package "java.util"} (written "java.util.Date"))))
  (testing "and an inner class keeps the dollar that says what it is inside of"
    (is (= {:type "class" :name "Map$Entry" :package "java.util"}
           (written "java.util.Map$Entry")))))

;;; The members of a class

(deftest a-static-member-is-written-behind-a-slash
  (testing "a field before a method, since a name written behind a slash and
  called nothing is read as a field where the class has one"
    (is (= {:type "field" :name "SIZE" :class "java.lang.Integer" :tag "int"}
           (written "Integer/SIZE"))))
  (let [found (written "Integer/parseInt")]
    (is (= "method" (:type found)))
    (is (= "parseInt" (:name found)))
    (is (= "java.lang.Integer" (:class found)))
    (testing "the return tagged on the parameter vector, which is the notation
    the language already has for it"
      (is (= ["^int [String]" "^int [String int]" "^int [CharSequence int int int]"]
             (:arglists found))))))

(deftest an-instance-method-is-written-on-a-class-or-on-a-thing
  (let [behind-a-slash (written "String/.substring")
        on-the-thing (written ".substring" :tag "String")]
    (is (= behind-a-slash on-the-thing))
    (is (= "method" (:type behind-a-slash)))
    (is (= "substring" (:name behind-a-slash)))
    (is (= "java.lang.String" (:class behind-a-slash)))
    (testing "fewest arguments first, which is how the arglists of a var read"
      (is (= ["^String [int]" "^String [int int]"] (:arglists behind-a-slash)))))
  (testing "what the thing is comes from the tag or from the target, and
  nothing is evaluated to find out"
    (is (= "java.lang.String" (:class (written ".length" :target "\"a string\""))))
    (is (nil? (written ".length" :target "(make-a-thing)")))
    (is (nil? (written ".length")))))

(deftest an-instance-field-is-only-written-on-the-thing
  (testing "which is (.-field x), and there is no spelling of one behind a
  slash for a completion to offer or for this to read"
    (is (= {:type "field" :name "x" :class "java.awt.Point" :tag "int"}
           (written ".-x" :tag "java.awt.Point")))
    (is (nil? (written "java.awt.Point/.-x")))))

(deftest a-constructor-is-one-name-however-many-arities
  (let [behind-a-slash (written "java.util.Date/new")
        behind-a-dot (written "java.util.Date.")]
    (is (= behind-a-slash behind-a-dot))
    (is (= "constructor" (:type behind-a-slash)))
    (is (= "new" (:name behind-a-slash)))
    (is (= "java.util.Date" (:class behind-a-slash)))
    (testing "no return tagged on them: a constructor gives back the class the
    name has already said"
      (is (= "[]" (first (:arglists behind-a-slash))))
      (is (every? #(string/starts-with? % "[") (:arglists behind-a-slash)))))
  (testing "a text ending in a dot whose start is not a class is somebody
  halfway through writing a class name"
    (is (nil? (written "java.util.")))))

(def ^:private shadowing
  "A namespace that aliases clojure.string as String, so that one scope is a
  namespace and a class at once. Nothing forbids it, and what is written
  under it is read as the compiler reads it: the namespace where the
  namespace has that name, and the class only where it does not."
  (let [named 'replique.symbol-test.shadowing]
    (or (find-ns named)
        (let [made (create-ns named)]
          (binding [*ns* made] (alias 'String 'clojure.string))
          made))))

(deftest a-var-under-a-scope-before-a-member-of-a-class-spelled-the-same
  (testing "which is the order the compiler reads a name in"
    (is (= {:ns "clojure.string" :name "join"}
           (select-keys (written "String/join" :ns (str (ns-name shadowing)))
                        [:ns :name])))
    (testing "and a name the namespace does not hold is the class after all"
      (is (= {:type "method" :name "valueOf" :class "java.lang.String"}
             (select-keys (written "String/valueOf" :ns (str (ns-name shadowing)))
                          [:type :name :class]))))
    (testing "where nothing aliases it, the class is what it was all along"
      (is (= "java.lang.String" (:class (written "String/valueOf")))))))

;;; Keywords

(deftest a-keyword-written-with-two-colons-is-read-against-the-namespace
  (testing "which only the process can say"
    (is (= {:type "keyword" :name "foo" :ns "clojure.string"}
           (written "::foo" :ns "clojure.string")))
    (is (= {:type "keyword" :name "foo" :ns "clojure.string"}
           (written "::string/foo" :ns "replique.symbol-test"))))
  (testing "an alias and nothing else, since the reader takes nothing else there"
    (is (nil? (written "::no.such.alias/foo" :ns "replique.symbol-test"))))
  (testing "and one written with a single colon is read as it stands"
    (is (= {:type "keyword" :name "foo"} (written ":foo")))
    (is (= {:type "keyword" :name "bar" :ns "foo"} (written ":foo/bar"))))
  (testing "colons and nothing after them are a keyword nobody has written yet"
    (is (nil? (written "::")))
    (is (nil? (written ":")))))

;;; Where a source is

(deftest a-file-of-a-directory-is-a-file-and-one-inside-a-jar-is-an-entry
  (testing "a var of this project, which is read from a directory on the
  classpath: there is a path to it and it is opened as one"
    (let [found (written "replique.names/namespace-named")]
      (is (string/ends-with? (:file found) "src/replique/names.clj"))
      (is (nil? (:entry found)))
      (is (pos-int? (:line found)))
      (is (pos-int? (:column found)))))
  (testing "a var of clojure itself, which is read out of a jar: there is no
  path to a file inside an archive, so both halves travel"
    (let [found (written "clojure.core/map")]
      (is (string/ends-with? (:file found) ".jar"))
      (is (= "clojure/core.clj" (:entry found)))))
  (testing "a namespace is found under the name its file is written as, which
  is the name it munges to and not the name it is written in"
    (is (string/ends-with? (:file (written "replique.test-client"))
                           "test/replique/test_client.clj"))))

(deftest a-name-that-reaches-nothing-is-answered-with-nothing
  (testing "which is half of what somebody writes: a name they have not
  finished writing"
    (is (nil? (written "no-such-name")))
    (is (nil? (written "no.such.ns/nope")))
    (is (nil? (written "java.util.NoSuchClass")))
    (is (nil? (written "String/noSuchMember")))
    (is (nil? (written "")))
    (is (nil? (written "  ")))))

;;; The positions of a dependency form

(deftest the-slots-of-a-dependency-form
  (testing "a namespace under a prefix, where the prefix is written once"
    (is (= "clojure.string"
           (:name (ask {:position :namespace :prefix "clojure" :text "string"}))))
    (is (= "clojure.string" (:name (ask {:position :namespace :text "clojure.string"}))))
    (is (= "clojure.string" (:name (ask {:position :namespace-macros
                                         :text "clojure.string"})))))
  (testing "a var of a namespace, which is what a refer names"
    (is (= {:ns "clojure.string" :name "join"}
           (select-keys (ask {:position :var :namespace "clojure.string" :text "join"})
                        [:ns :name])))
    (testing "and a refer-clojure names no namespace anywhere in itself"
      (is (= "clojure.core"
             (:ns (ask {:position :var :namespace :refer-clojure :text "map"})))))
    (is (nil? (ask {:position :var :namespace "clojure.string" :text "nope"})))
    (testing "a var that is not public is not a var that a refer can name"
      (is (nil? (ask {:position :var :namespace "replique.symbol" :text "of-var"})))))
  (testing "a class or a package"
    (is (= {:type "class" :name "Date" :package "java.util"}
           (ask {:position :package-or-class :text "java.util.Date"})))
    (is (= {:type "package" :name "java.util"}
           (ask {:position :package-or-class :text "java.util"})))
    (is (= {:type "class" :name "Date" :package "java.util"}
           (ask {:position :class :package "java.util" :text "Date"}))))
  (testing "a path, read from the namespace it is written in or from the root"
    (let [found (ask {:position :load-path :ns "replique" :text "names"})]
      (is (= "path" (:type found)))
      (is (= "replique/names" (:name found)))
      (is (string/ends-with? (:file found) "src/replique/names.clj")))
    (is (= "replique/names"
           (:name (ask {:position :load-path :ns "no.such.ns" :text "/replique/names"}))))
    (is (nil? (ask {:position :load-path :ns "replique" :text "nope"}))))
  (testing "and the keyword slots, where what is written is a keyword the form
  itself gives a meaning to"
    (is (nil? (ask {:position :dependency-type :text ":require"})))
    (is (nil? (ask {:position :libspec-option :text ":as"})))
    (is (nil? (ask {:position :libspec-option-refer :text ":only"})))
    (is (nil? (ask {:position :flag :text ":reload"})))))

;;; What a completion offers is a name this reads back

(deftest what-a-completion-offers-can-be-read-back
  (testing "the two are one question asked twice: what a client writes into a
  buffer from a completion is a name point is then sitting in"
    (doseq [msg [{:position :code :ns "replique.symbol-test" :text "ma"}
                 {:position :code :ns "replique.symbol-test" :text "string/j"}
                 {:position :code :ns "replique.symbol-test" :text "Integer/M"}
                 {:position :code :ns "replique.symbol-test" :text "String/.sub"}
                 {:position :code :ns "replique.symbol-test" :text "java.util.Date."}
                 {:position :code :ns "replique.symbol-test" :text ".sub" :tag "String"}
                 {:position :code :ns "replique.symbol-test" :text "clojure.st"}]]
      (let [offered (:completions (completion/completions msg))]
        (is (seq offered) (pr-str msg))
        (doseq [{:keys [candidate]} offered]
          (is (some? (ask (assoc msg :text candidate)))
              (str "offered by " (pr-str msg) ", read back as nothing: " candidate)))))))

;;; What the client got wrong

(deftest a-message-written-wrongly-is-refused
  (is (= :invalid-message (error-kind {:text "map"})))
  (is (= :invalid-message (error-kind {:position :something-else :text "map"})))
  (is (= :invalid-message (error-kind {:position :code :text 42})))
  (is (= :invalid-message (error-kind {:position :code :text "map" :ns 42})))
  (is (= :invalid-message (error-kind {:position :code :text "map" :locals "x"})))
  (is (= :invalid-message (error-kind {:position :code :text "map" :locals [{:name 42}]})))
  (is (= :invalid-message (error-kind {:position :class :text "Date"})))
  (is (= :invalid-message (error-kind {:position :var :text "join"}))))

;;; Over the wire

(deftest the-op-answers-a-client
  (with-process [info nil]
    (let [c (control-client info)]
      (try
        (let [reply (request! c {:op :symbol :position :code :ns "clojure.core"
                                 :text "map" :id 1})]
          (is (= "reply" (:tag reply)))
          (is (= "function" (get-in reply [:symbol :type])))
          (is (= "clojure.core" (get-in reply [:symbol :ns])))
          (is (= "clojure/core.clj" (get-in reply [:symbol :entry]))))
        (testing "a name that means nothing here is an absent key rather than
        a null, which is what every other absent value is"
          (let [reply (request! c {:op :symbol :position :code :text "no-such-name"
                                   :id 2})]
            (is (= "reply" (:tag reply)))
            (is (not (contains? reply :symbol)))))
        (testing "and a message written wrongly is refused without taking the
        connection with it"
          (is (= "invalid-message" (:error (request! c {:op :symbol :text "map" :id 3}))))
          (is (= "reply" (:tag (request! c {:op :echo :value 1 :id 4})))))
        (finally (disconnect c))))))
