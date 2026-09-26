(ns replique.name-fuzz-test
  "Messages nobody would write, against the rules every answer keeps.

  The completion reads text out of somebody's buffer, and a buffer holds
  whatever has been typed into it so far - half a name, a colon on its own,
  three slashes, a dot at the end of a word that is not a class. So the
  strings that reach it are not the strings the other tests are written
  around, and what breaks on one of them breaks in front of somebody who was
  only typing.

  Random rather than listed, because the interesting text is the text nobody
  thought of. What is asserted is not an answer - there is no expected answer
  to a random message - but the rules that hold whatever the answer is: it
  came back, it is shaped the way the protocol says it is, and every
  candidate in it is a name the text reaches.

  Seeded and fixed, so a run that fails fails again. A fuzz test that finds
  something once and never twice has told nobody anything: what it reports is
  the message, and a message is a test that can be written.

  The symbol op is fuzzed with the same messages, because it is asked with
  the same message: the same position, the same text, the same namespace and
  locals and tag around it. What differs is where point was left - at the end
  of a name rather than partway through it - so the text is written out the
  rest of the way half the time, and the answer is one name rather than a
  list of them."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.string :as string]
            [replique.completion :as completion]
            [replique.names :as names]
            [replique.protocol :as protocol]
            [replique.symbol :as sym]
            [replique.test-client :as client
             :refer [control-client disconnect request! with-process]]))

;;; What a client might write

(def ^:private characters
  "What a fuzzed text is spelled out of.

  The separators are in it several times over, because they are what the
  reading branches on: a dot, a slash and a colon mean something to it where
  a letter means nothing but itself, so a uniform alphabet would spend a
  whole run on names with nothing in them to read."
  (vec (str "abcdefghijklmnopqrstuvwxyz"
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            "0123456789"
            "./-_:$" "./-_:$" "./-_:$"
            " ()[]{}\"\\;#'^@`~"
            ;; a character written with two of them, so that a piece taken
            ;; off a name can be taken off in the middle of one
            "😀")))

(def ^:private fragments
  "The pieces of text that reach something.

  Random characters find the shape of a name by accident and almost never
  find a name that exists, so the spellings a reader would recognise are
  written out and generated beside them - alone, in pairs, and in front of
  random text, which is what a name half typed looks like."
  ["" "." ".." "-" "/" "//" ":" "::" ":::" ".-" "$" "Map$"
   "map" "clojure" "clojure.string" "clojure.string/" "clojure.string/jo"
   "str/" "::str" "::string/" "clojure.core//" "MAX_VALUE" "clojure.zip"
   "java" "java.util" "java.util." "java.util.Date" "java.util.Date." "Date."
   "String" "String/" "String/." "String/new" "Integer/MAX" ".toStr" ".-value"
   "#=(println :x)" "^String" "nil" "1" "\"" "\\" "  " " "])

(defn- pick [^java.util.Random random coll]
  (nth coll (.nextInt random (count coll))))

(defn- letters
  "A string of up to twelve characters, which is about as far as anybody gets
  before they stop typing and read what is being offered."
  [^java.util.Random random]
  (let [length (.nextInt random 13)]
    (apply str (repeatedly length #(pick random characters)))))

;; A class with a field on it. A field is written .-name and almost nothing
;; in the jdk has a public one - the answer to .-x on a String is correctly
;; nothing - so a run that only asked about those would never read the field
;; branch at all. A deftype is also a class no file on the classpath carries,
;; which is the other half of what is being asked here.
(deftype Fuzzed [field-of-mine])

(def ^:private fielded "replique.completion_fuzz_test.Fuzzed")

(def ^:private existing
  "Names of things that are there.

  Half of what a completion does it only does once a name resolves - the
  members of a class, the vars of a namespace, the classes of a package - so
  a generator that only ever writes names of its own reaches the shallow half
  of the code and reports that nothing is wrong with it. These are what the
  other half is reached through."
  {:namespaces ["clojure.core" "clojure.string" "clojure.set" "clojure.edn"
                "replique.completion" "replique.name-fuzz-test" "user"]
   ;; the aliases this namespace holds, which is how a fuzzed text reaches
   ;; the reading that only an alias opens: str/join, ::string/name
   :aliases ["string" "completion" "protocol" "client" "edn"]
   :packages ["java.util" "java.lang" "java.util.concurrent" "clojure.lang"]
   :classes ["String" "Integer" "java.util.Date" "java.util.Map$Entry"
             "Object" "int" "clojure.lang.PersistentVector" "nosuch.Class"
             fielded]
   ;; a macro and a var that is neither, since what a var is answered as is
   ;; read off its own metadata and the three answers are three branches
   :vars ["map" "reduce" "str" "join" "split" "blank?" "starts-with?"
          "when" "*ns*"]
   :members ["toString" "length" "substring" "getTime" "hashCode" "size"
             "getName" "MAX_VALUE" "TYPE" "new" "field_of_mine"]
   :keywords ["require" "as" "refer" "keys" "name" "replique/error"]
   :paths ["clojure/string.clj" "replique/completion.clj" "/clojure" "string"]})

(defn- pool [key] (get existing key))

(def ^:private spellings
  "The ways a name is written where code is. Every one of them is read
  differently - a colon says a keyword, a dot says a member, a slash says a
  scope - so a text drawn from these is a text that reaches one of the
  readings rather than falling out of all of them."
  (vec (concat (pool :namespaces) (pool :aliases) (pool :classes) (pool :vars)
               (names/special-forms)
               (map #(str "." %) (pool :members))
               (map #(str ".-" %) (pool :members))
               (map #(str ":" %) (pool :keywords))
               (map #(str "::" %) (pool :keywords))
               (map #(str "::string/" %) (pool :keywords))
               (for [scope ["clojure.string" "str" "string" "String" "Integer"
                            "java.util.Date"]
                     name (concat (pool :vars) (pool :members) ["." "new"])]
                 (str scope "/" name))
               (map #(str % ".") (pool :classes))
               ;; a protocol method, whose metadata carries no file and no
               ;; line of its own - what it carries is the protocol, and a
               ;; name answered out of somewhere other than itself is a
               ;; branch nothing else here reaches
               ["clojure.core.protocols/coll-reduce" "clojure.core.protocols"])))

(defn- half-typed
  "A name of NAMES, as far as somebody has got with typing it.

  Which is what a completion is asked about. Random characters look like a
  name that was half typed about as often as they look like anything else,
  and a run that never matches anything is a run that never checks what
  matching answers."
  [^java.util.Random random names]
  (let [^String name (pick random names)]
    (subs name 0 (.nextInt random (inc (.length name))))))

(defn- written
  "The text of a message: what has been typed where the name is going.

  Half of it a name half typed, since that is what somebody asking for a
  completion has in front of them, and half of it text that is nothing of the
  sort - which is what the reading has to hold up against, because a buffer
  holds whatever was typed into it."
  [^java.util.Random random]
  (case (.nextInt random 8)
    0 (pick random fragments)
    1 (str (pick random fragments) (pick random fragments))
    2 (str (pick random fragments) (letters random))
    (3 4 5) (half-typed random spellings)
    6 (str (half-typed random spellings) (letters random))
    (letters random)))

(defn- typed-at
  "The text of a message asked at POSITION.

  Drawn from the names that position answers with as often as from anywhere
  else. A namespace position is asked with a namespace half written and a
  load path with a path half written, and a generator that did not know that
  would spend its run being told, correctly, that nothing matches."
  [^java.util.Random random position]
  (if (zero? (.nextInt random 2))
    (written random)
    (case position
      (:namespace :namespace-macros) (half-typed random (pool :namespaces))
      :var (half-typed random (pool :vars))
      (:package-or-class :class) (half-typed random (pool :classes))
      :load-path (half-typed random (pool :paths))
      (written random))))

(defn- named
  "A value where the message wants a name, written the three ways a client
  writes one and the once in eight it is written as no name at all.

  Once in eight rather than half the time, because a message with a junk key
  in it is refused before anything reads the rest of it - so a generator
  that writes junk freely spends its run proving that junk is refused, which
  is one thing worth knowing and not the only one."
  [^java.util.Random random names]
  (let [text (if (zero? (.nextInt random 2)) (pick random names) (written random))]
    (case (.nextInt random 8)
      0 (symbol text)
      1 (keyword text)
      2 nil
      3 (pick random [42 [text] {:name text} true])
      text)))

(def ^:private targets
  "What a member is written on, as the client read it out of the buffer: a
  local, a var, a literal, and text that is no expression at all."
  ["s" "x" "map" "clojure.string/join" "*ns*" "\"abc\"" "1" "1.0" ":keyword"
   "[1 2]" "(make-thing)" "#=(println :x)" "^String s" "" "  " "nil"])

(defn- locals [^java.util.Random random]
  (case (.nextInt random 8)
    0 nil
    1 []
    2 [{:name (named random (pool :vars))}]
    3 (pick random [[(written random)] (written random) {:name "x"} [nil]])
    (mapv (fn [_] {:name (half-typed random spellings)})
          (range (.nextInt random 4)))))

(def ^:private positions
  "Every position that is answered, and some that are not: a client asking at
  a position this does not know has to be told so.

  Code several times over, since it is where a client asks while somebody is
  typing. The others are asked inside a form that is nearly always written
  already."
  [:namespace :namespace-macros :var :package-or-class :class :load-path
   :dependency-type :libspec-option :libspec-option-refer :flag
   :code :code :code :code :code
   :nowhere "code" 'code nil 7])

(defn- message [^java.util.Random random]
  (let [position (pick random positions)
        text (typed-at random (protocol/as-keyword position))
        maybe (fn [msg key value]
                (if (zero? (.nextInt random 3)) msg (assoc msg key value)))]
    (-> {:position position}
        (maybe :text text)
        (maybe :ns (named random (pool :namespaces)))
        (maybe :prefix (named random (pool :packages)))
        (maybe :namespace (named random (pool :namespaces)))
        (maybe :package (named random (pool :packages)))
        ;; A member is answered through the class of what it is written on
        ;; and through nothing else, so a text that starts with a dot is
        ;; mostly given one - and one that starts with a dash after it is
        ;; mostly given the class that has a field, since the classes that
        ;; have none are answered with nothing however the question is
        ;; spelled. Not because a client always knows the class - it often
        ;; does not, and the answer then is nothing - but because the reading
        ;; behind it is only reached when it does.
        (maybe :tag (cond
                      (not (string/starts-with? text ".")) (named random (pool :classes))
                      (zero? (.nextInt random 4)) (named random (pool :classes))
                      (string/starts-with? text ".-") fielded
                      :else (pick random (pool :classes))))
        (maybe :on (if (zero? (.nextInt random 2))
                         (pick random targets)
                         (written random)))
        (maybe :locals (locals random)))))

;;; What must be true of the answer

(def ^:private types
  "The kinds of name a client is told about. A client annotates each of them
  and knows no others, so one that is not here arrives at a client with
  nothing to say about it."
  #{"namespace" "namespace-prefix" "macro" "function" "var" "class" "package"
    "path" "keyword" "local" "special-form" "method" "field" "constructor"})

(def ^:private keys-of-a-candidate
  #{:candidate :type :match-index :ns :package})

(defn- ill-formed
  "What the reply holds that the protocol does not say it holds."
  [reply]
  (let [{:keys [completions truncated]} reply]
    (or (when-not (vector? completions)
          (str "the completions are " (pr-str completions)))
        (when-not (contains? #{nil true} truncated)
          (str "truncated is " (pr-str truncated)))
        (when (> (count completions) completion/max-completions)
          (str (count completions) " candidates came back"))
        (when (and truncated (< (count completions) completion/max-completions))
          "truncated with room left in the reply")
        (some (fn [{:keys [candidate type match-index] :as found}]
                (or (when-not (string? candidate)
                      (str "a candidate is " (pr-str candidate)))
                    (when-not (contains? types type)
                      (str "the type of " (pr-str candidate) " is " (pr-str type)))
                    (when-not (integer? match-index)
                      (str "the match index of " (pr-str candidate) " is "
                           (pr-str match-index)))
                    (when-not (<= 0 match-index (count candidate))
                      (str "the match index of " (pr-str candidate) " is " match-index))
                    (when-let [extra (seq (remove keys-of-a-candidate (keys found)))]
                      (str (pr-str candidate) " carries " (pr-str extra)))))
              completions)
        (let [candidates (map :candidate completions)]
          (when-not (= (count candidates) (count (set candidates)))
            "the same candidate came back twice")))))

(defn- out-of-order
  "Whether the candidates are not shortest first, alphabetically among the
  ones of a length, which is the order that makes the truncation worth
  having."
  [reply]
  (let [candidates (map :candidate (:completions reply))]
    (when-not (= candidates (sort-by (juxt count identity) candidates))
      "the candidates are out of order")))

(defn- reached
  "Whether the characters of TEXT are written in CANDIDATE in the order they
  were typed.

  Read here rather than asked of the matching, so that this is a second
  opinion and not the same one twice. The separators are dropped because the
  matching splits on them, and case is ignored because the matching ignores
  it wherever nothing was typed in capitals - which makes this the weaker
  rule of the two, and a weaker rule is what an independent check can afford
  to be."
  [^String text ^String candidate]
  (let [separators #{\. \- \/ \_ \:}]
    (loop [typed 0 index 0]
      (cond
        (= typed (.length text)) true
        (contains? separators (.charAt text typed)) (recur (inc typed) index)
        (= index (.length candidate)) false
        (= (Character/toLowerCase (.charAt text typed))
           (Character/toLowerCase (.charAt candidate index)))
        (recur (inc typed) (inc index))
        :else (recur typed (inc index))))))

(defn- unreached
  "A candidate the text is not written in, up to where the match is said to
  end."
  [^String text reply]
  (some (fn [{:keys [^String candidate match-index]}]
          (when-not (reached text (subs candidate 0 match-index))
            (str (pr-str candidate) " does not hold " (pr-str text)
                 " before " match-index)))
        (:completions reply)))

(defn- misspelled
  "A candidate that could not be written where the text is being written.

  What is answered goes in the buffer over what was typed, so the spelling of
  the text says what the spelling of a candidate has to be: a keyword answers
  keywords, a member answers members, a name under a scope answers names
  under that scope. A candidate that breaks it is one that would be written
  into the buffer and read as something else."
  [msg reply]
  (let [text (or (:text msg) "")
        candidates (map :candidate (:completions reply))
        all (fn [rule message] (when-not (every? rule candidates) message))]
    (or (some (fn [{:keys [^String candidate type]}]
                (when (and (= "constructor" type)
                           (not (or (string/ends-with? candidate ".")
                                    (string/ends-with? candidate "/new"))))
                  (str "the constructor " (pr-str candidate)
                       " is written as neither a dot nor a new")))
              (:completions reply))
        (when (= :code (protocol/as-keyword (:position msg)))
          (cond
            (string/starts-with? text "::")
            (all #(string/starts-with? % "::") "a keyword came back without its colons")
            (string/starts-with? text ":")
            (all #(string/starts-with? % ":") "a keyword came back without its colon")
            (string/starts-with? text ".-")
            (all #(string/starts-with? % ".-") "a field came back written as a method")
            (string/starts-with? text ".")
            (all #(string/starts-with? % ".") "a member came back written on nothing")
            :else
            (let [slash (.lastIndexOf text (int \/))]
              (when (pos? slash)
                (all #(string/starts-with? % (subs text 0 (inc slash)))
                     "a name came back from under another scope"))))))))

(defn- answered
  "What the completion answers MSG, as [reply problem].

  Refusing is an answer: a message that names nothing a completion can be
  asked at has to come back as one the client is told about, and that is the
  invalid message. Anything else thrown is the process failing at a
  keystroke."
  [msg]
  (try
    [(completion/completions (assoc msg :position (protocol/as-keyword (:position msg)))) nil]
    (catch clojure.lang.ExceptionInfo t
      (if (= :invalid-message (:replique/error (ex-data t)))
        [nil nil]
        [nil (str "threw " (.getName (class t)) ": " (ex-message t))]))
    (catch Throwable t
      [nil (str "threw " (.getName (class t)) ": " (ex-message t))])))

(defn- problem-with
  "What is wrong with the way MSG was answered, or nil when nothing is."
  [msg]
  (let [[reply thrown] (answered msg)]
    (or thrown
        (when reply
          (or (ill-formed reply)
              (out-of-order reply)
              (unreached (or (:text msg) "") reply)
              (misspelled msg reply))))))

(defn- failing
  "The first of COUNT messages of SEED that is answered wrongly, said as a
  map, or nil when none of them is.

  The first rather than all of them, because the second failure of a run is
  nearly always the first one again in another spelling, and what is wanted
  is a message small enough to read."
  [seed count]
  (let [random (java.util.Random. seed)]
    (loop [remaining count]
      (when (pos? remaining)
        (let [msg (message random)]
          (if-let [problem (problem-with msg)]
            {:seed seed :message msg :problem problem}
            (recur (dec remaining))))))))

;;; The runs

(deftest nothing-a-client-can-write-breaks-the-completion
  (doseq [seed (range 8)]
    (is (nil? (failing seed 1000)))))

(defn- unwritable
  "A candidate of MSG that cannot be written out in full to reach itself.

  A candidate is what goes in the buffer, so finishing one has to answer with
  the name that was half typed a keystroke ago - a candidate that stops
  matching once it is written is one somebody watches disappear as they type
  it.

  Only where nothing was cut: what the truncation dropped is not something
  the answer claimed to hold."
  [msg]
  (let [[reply _] (answered msg)]
    (when (and reply (not (:truncated reply)))
      (some (fn [{:keys [candidate]}]
              (let [[again _] (answered (assoc msg :text candidate))]
                (when (and again
                           (not (:truncated again))
                           (not (contains? (set (map :candidate (:completions again)))
                                           candidate)))
                  {:message msg :candidate candidate
                   :problem "is offered, and typing it out reaches nothing"})))
            (take 4 (:completions reply))))))

(deftest what-is-offered-can-be-written
  (let [random (java.util.Random. 99)]
    (is (nil? (loop [remaining 400]
                (when (pos? remaining)
                  (or (unwritable (message random))
                      (recur (dec remaining)))))))))

;;; The same message, asked what the one name written there is

(defn- finished
  "MSG, with the name it carries written out rather than half written.

  A completion is asked about a name somebody is partway through typing, and
  the symbol op about one they have finished - so half the messages here
  carry a whole name, drawn from the names that position answers with. A
  generator that only ever wrote half of one would spend its run being told,
  correctly, that half a name names nothing.

  A local is drawn out of the message's own locals, since a name is a local
  only where the client said it was one: text drawn from anywhere else is a
  local by coincidence and hardly ever."
  [^java.util.Random random msg]
  (let [locals (vec (for [local (:locals msg)
                          :when (map? local)
                          :let [name (:name local)]
                          :when (string? name)]
                      name))]
    (cond
      (pos? (.nextInt random 2)) msg
      (and (seq locals) (zero? (.nextInt random 4))) (assoc msg :text (pick random locals))
      :else (assoc msg :text
                   (pick random (case (protocol/as-keyword (:position msg))
                                  (:namespace :namespace-macros) (pool :namespaces)
                                  :var (pool :vars)
                                  (:package-or-class :class) (pool :classes)
                                  :load-path (pool :paths)
                                  spellings))))))

(def ^:private keys-of-a-name
  #{:type :name :ns :package :class :tag :arglists :doc :file :entry :line :column})

(defn- unnamed
  "What is wrong with FOUND, the one name a symbol op answered with, or nil
  when nothing is.

  There is no expected answer to a random message, so what is checked is what
  holds of every answer: it says what kind of thing it found, out of the same
  list a candidate is annotated from, and it says the name - which is what a
  client shows, and what everything else in the answer hangs off. The rest is
  shape: the strings are strings, the numbers are numbers, and there is
  nothing in it a client has never been told to read.

  And what a client does with a source: it opens the file and goes to the
  line. So the file is a file that is there to open, and the line and the
  column are numbers a place can be counted to - a client counts from them,
  and one counted from nought lands a line above where it was told."
  [found]
  (let [strings [:name :ns :package :class :tag :doc :file :entry]
        numbers [:line :column]]
    (cond
      (not (map? found)) (str "answered " (pr-str found))
      (not (contains? types (:type found))) (str "the type is " (pr-str (:type found)))
      (string/blank? (:name found)) (str "the name is " (pr-str (:name found)))
      (seq (remove keys-of-a-name (keys found)))
      (str "answered keys nobody reads: " (pr-str (remove keys-of-a-name (keys found))))
      :else
      (or (first (for [key strings
                       :let [value (get found key)]
                       :when (and (some? value) (not (string? value)))]
                   (str "the " key " is " (pr-str value))))
          (first (for [key numbers
                       :let [value (get found key)]
                       :when (and (some? value) (not (integer? value)))]
                   (str "the " key " is " (pr-str value))))
          (when-let [arglists (:arglists found)]
            (when-not (and (vector? arglists) (every? string? arglists))
              (str "the arglists are " (pr-str arglists))))
          ;; an entry names something inside the file, so a file is what it
          ;; is named inside of
          (when (and (:entry found) (nil? (:file found)))
            "an entry with no file to read it out of")
          (first (for [key numbers
                       :let [value (get found key)]
                       :when (and (integer? value) (not (pos? value)))]
                   (str "the " key " is " value)))
          (when-let [^String file (:file found)]
            (when-not (.isFile (java.io.File. file))
              (str "the file is not there to open: " file)))))))

(defn- misnamed
  "What is wrong with the way MSG was answered by the symbol op, or nil when
  nothing is.

  Refusing is an answer, the same way it is for a completion: a message that
  names no position has to come back as one the client is told about."
  [msg]
  (try
    (when-let [found (:symbol (sym/named (assoc msg :position
                                                (protocol/as-keyword (:position msg)))))]
      (unnamed found))
    (catch clojure.lang.ExceptionInfo t
      (when-not (= :invalid-message (:replique/error (ex-data t)))
        (str "threw " (.getName (class t)) ": " (ex-message t))))
    (catch Throwable t
      (str "threw " (.getName (class t)) ": " (ex-message t)))))

(defn- unnamed-by
  "The first of COUNT messages of SEED the symbol op answers wrongly, said as
  a map, or nil when none of them is."
  [seed count]
  (let [random (java.util.Random. seed)]
    (loop [remaining count]
      (when (pos? remaining)
        (let [msg (finished random (message random))]
          (if-let [problem (misnamed msg)]
            {:seed seed :message msg :problem problem}
            (recur (dec remaining))))))))

(deftest nothing-a-client-can-write-breaks-the-symbol
  (doseq [seed (range 8)]
    (is (nil? (unnamed-by seed 1000)))))

(defn- printable
  "A message that survives being printed and read back.

  What a generated symbol prints as is not always a symbol - one holding a
  semicolon prints a comment and one holding a newline prints two lines - and
  a message like that never arrives as a message at all. What the process
  does with a line it cannot read is the protocol's to answer and is answered
  where the protocol is tested; what is being asked here is what it does with
  a completion it can read."
  [^java.util.Random random]
  (loop [remaining 20]
    (let [msg (message random)]
      (if (or (zero? remaining)
              (= msg (try (edn/read-string (pr-str msg)) (catch Throwable _ nil))))
        msg
        (recur (dec remaining))))))

(deftest the-connection-survives-what-a-client-writes
  (testing "over the wire, where a message that is refused comes back as a
  frame rather than as a closed socket"
    (with-process [info nil]
      (let [client (control-client info)
            random (java.util.Random. 11)]
        (try
          ;; Both ops, turn about. They take the same message, and what is
          ;; being asked here is of the wire rather than of either of them:
          ;; a docstring is the one answer with a newline and a quote in it,
          ;; and it has to come back as one line of json like anything else.
          (is (nil? (loop [id 1]
                      (when (<= id 150)
                        (let [msg (assoc (finished random (printable random))
                                         :op (if (odd? id) :completions :symbol)
                                         :id id)
                              reply (request! client msg)]
                          (or (when-not (contains? #{"reply" "error"} (:tag reply))
                                {:message msg :reply reply})
                              (when-not (= id (:id reply))
                                {:message msg :problem "the reply is another request's"})
                              (recur (inc id))))))))
          (testing "and answers the next one"
            (is (= "reply" (:tag (request! client {:op :completions :position :flag
                                                   :id 999})))))
          (finally (disconnect client)))))))
