(ns replique.completion
  "The names that could be written where a name is being written.

  The client says which slot of which form point is in - a namespace under a
  prefix, a var of a namespace, a class of a package - and what has been typed
  there. This answers with what could be written in its place, and a candidate
  is exactly that: what goes in the buffer, not what it names. Under a prefix
  list the two are different, since the prefix is already written.

  Matched a piece of a name at a time. A name is written in pieces - at a dot,
  a dash, a slash, an underscore, and at every capital - and what was typed is
  split the same way, so that c.s reaches clojure.string and ABQ reaches
  ArrayBlockingQueue. Each piece has to be written where a piece of the name
  starts, and after the one before it: the pieces were typed in an order, and
  a name holding them in another order is not the name that was typed. Case is
  ignored piece by piece until one of them has a capital in it - somebody who
  typed da wants Date, and somebody who typed Da has said which of the two
  they meant.

  Matched here rather than by the client. What is on a classpath runs to a
  hundred thousand names and they are not going over a socket, so the process
  is where the set is cut down - and a client that received the first few
  hundred of a hundred thousand could not have applied a rule of its own to
  the rest anyway.

  A var is answered out of the namespace the process has loaded, and out of
  nothing else. Loading one to see what it holds runs every top level form in
  it, and a keystroke is not a thing that should run anybody's code.

  The locals travel the other way. A name bound by the form being written is
  a name this process has never seen, so the client sends the ones in scope
  along with the request - and they are answered here, beside the vars,
  rather than added to the answer afterwards. Which is what makes a local
  shadow: a let that binds map is answered as that local and not as the var,
  and working that out where the two lists meet is the only place it can be
  worked out at all."
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [replique.classpath :as classpath]
            [replique.names :as names]))

(def max-completions
  "How many candidates travel in one reply. A client is showing them to
  somebody who is still typing, and the rest of a hundred thousand names is
  not what they are about to pick from. What was cut is said rather than
  quietly dropped, so a client can ask for more to be typed instead of
  showing an answer that looks whole."
  500)

;;; Matching

(def ^:private separators
  "What a name is written in pieces with. A capital starts one too, and that
  is a place to split before rather than a character to split on.

  The colon is one of them so that a keyword is written in pieces like
  everything else: :my.app/na is three pieces and reaches
  :my.app/name, and the colons of ::alias/name are not a piece of
  anything themselves."
  #{\. \- \/ \_ \:})

(defn- tokenize
  "TEXT in the pieces it was typed in, each with whether its case is to be
  ignored.

  Split at a separator and before a capital, so c.s is two pieces and ABQ is
  three - which is what lets three letters reach ArrayBlockingQueue. A piece
  can only carry a capital at its front, since a capital is where the piece
  before it ended, so what says a piece was written in a case that matters is
  that same capital."
  [^String text]
  (let [length (.length text)
        piece (fn [tokens start end]
                (if (< start end)
                  (let [token (subs text start end)]
                    (conj tokens [token (= token (string/lower-case token))]))
                  tokens))]
    (loop [index 0 start 0 tokens []]
      (if (= index length)
        (piece tokens start index)
        (let [character (.charAt text index)]
          (cond
            (contains? separators character)
            (recur (inc index) (inc index) (piece tokens start index))
            (and (Character/isUpperCase character) (> index start))
            (recur (inc index) index (piece tokens start index))
            :else (recur (inc index) start tokens)))))))

(def ^:private boundaries
  "What a piece of a name is written behind. The dollar is here and not among
  the separators: what is behind one is the inner class of the class in front
  of it, and Map$ is written to reach it."
  #{\. \- \/ \_ \$ \:})

(defn- boundary?
  "Whether a piece of the name CANDIDATE starts at INDEX.

  Where a capital does, which is the rule that reads a name written with no
  separator in it at all.

  Every capital, the ones inside an acronym included. `tokenize' splits
  before each of them, so a name holding two in a row is written in pieces
  that only these boundaries let it be matched by: UUID is typed U, U, I, D
  and java.util.UUID is a name somebody must be able to reach by writing it
  out. A capital typed inside an acronym matches more names than it used to -
  T reaches HTTPServer at its second letter - and a name that cannot be
  written at all is the worse of the two."
  [^String candidate ^long index]
  (or (zero? index)
      (contains? boundaries (.charAt candidate (dec index)))
      (Character/isUpperCase (.charAt candidate index))))

(defn- token-index
  "Where TOKEN is written in CANDIDATE at or after FROM, or -1 for nowhere."
  ^long [^String candidate ^String token ignore-case ^long from]
  (let [limit (- (.length candidate) (.length token))
        ;; The first character before anything else. Where a piece of a name
        ;; starts is the question that reads three characters of the name
        ;; around it, and asking it at every index of every name on a
        ;; classpath is most of the work; one character tells almost all of
        ;; them apart first.
        wanted (int (if ignore-case (Character/toLowerCase (.charAt token 0)) (.charAt token 0)))]
    (loop [index from]
      (cond
        (> index limit) -1
        (and (let [character (.charAt candidate index)]
               (== wanted (int (if ignore-case (Character/toLowerCase character) character))))
             (boundary? candidate index)
             (.regionMatches candidate ^boolean ignore-case index token 0 (.length token)))
        index
        :else (recur (inc index))))))

(defn- match-end
  "How far into CANDIDATE the TOKENS reach, or nil when one of them is written
  nowhere in it.

  Each of them after the one before it, and no tokens at all reach nought,
  which every name is matched by: nothing has been typed, and everything could
  still be written."
  [^String candidate tokens]
  (loop [tokens (seq tokens) from 0]
    (if-let [[^String token ignore-case] (first tokens)]
      (let [index (token-index candidate token ignore-case from)]
        (when-not (neg? index)
          (recur (next tokens) (+ index (.length token)))))
      from)))

(def ^:private shortest-first
  "Shortest first, and alphabetically among the names of a length.

  The shortest is the likeliest - somebody who typed map wants map before
  map-indexed - and it is also what makes the bound worth having: the five
  hundred that travel are the five hundred shortest, where in alphabetical
  order they would all have begun with an a."
  (reify java.util.Comparator
    (compare [_ one another]
      (let [^String candidate (:candidate one)
            ^String other (:candidate another)
            difference (- (.length candidate) (.length other))]
        (if (zero? difference) (.compareTo candidate other) difference)))))

(defn- matching
  "The names of GROUPS that TEXT is written in, each of them once, in order.

  Ordered at the end and not before: a group holds every name of its kind on
  the classpath, and putting a hundred thousand of them in order to answer
  about the handful that match is the work this is arranged to avoid. Keeping
  only the best as they arrive, in a queue that drops its worst, was measured
  at a millisecond less on the one request where every name matches and at the
  same everywhere else - so this is the one that reads."
  [^String text groups]
  (let [tokens (tokenize text)
        ;; a name is on the classpath as many times as an entry provides it,
        ;; and add is what says this is the first of them
        seen (java.util.HashSet.)
        found (java.util.ArrayList.)]
    (doseq [{:keys [type names] :as group} groups
            ^String name names]
      (when-let [end (match-end name tokens)]
        (when (.add seen name)
          (.add found (cond-> {:candidate name :type type :match-index end}
                        (:ns group) (assoc :ns (:ns group))
                        (:package group) (assoc :package (:package group)))))))
    (.sort found ^java.util.Comparator shortest-first)
    found))

;;; The positions

(defn- under
  "The names under PREFIX, with the prefix taken off, or all of them when
  there is no prefix.

  What is written under one is a name and not a path through several: a
  prefix list is one level deep - (clojure [string]) and never
  (clojure [core.protocols]) - and so is a package, whose classes are the ones
  directly in it. So a name that goes on below the prefix is not a name that
  could be written where this is being written."
  [^String prefix names]
  (if (string/blank? prefix)
    names
    (let [start (str prefix ".")
          length (.length start)]
      (eduction (filter (fn [^String name] (.startsWith name start)))
                (map (fn [^String name] (subs name length)))
                (remove (fn [^String name] (string/includes? name ".")))
                names))))

(defn- inside
  "The paths inside DIRECTORY, with the directory taken off.

  Unlike `under', what is left may go on below: a load takes a path and a
  path goes as deep as the directories do, where a lib name inside a prefix
  list and a class inside a package are one piece and no more."
  [^String directory names]
  (let [start (str directory "/")
        length (.length start)]
    (eduction (filter (fn [^String name] (.startsWith name start)))
              (map (fn [^String name] (subs name length)))
              names)))

(defn- importable
  "The classes worth offering to somebody who has typed TEXT.

  An inner class is written with a dollar in it and there are ten of them for
  every class anybody imports: most are the machinery of the class they are
  written inside, and none of them is what somebody who typed the start of an
  outer name is reaching for. So they are held back until the text says one is
  wanted, which is until it holds the dollar - Map$Entry is reached by writing
  Map$, the class it is in, which is how the name is written anyway.

  Whether a class is public is not asked, because asking means loading it, and
  that is the thing a keystroke must not do. A package private class does get
  offered, and importing one fails where it is written rather than quietly."
  [^String text classes]
  (if (string/includes? text "$")
    classes
    (eduction (remove (fn [^String name] (string/includes? name "$"))) classes)))

(defmulti ^:private groups
  "The names that could be written at the position MSG names, in groups of one
  kind each. Every one of them, unmatched: `matching' is what cuts them down."
  :position)

(defmethod groups :default [msg]
  (throw (names/invalid (if (nil? (:position msg))
                          "A completion needs the :position it is being asked at"
                          (str "Unknown completion position: "
                               (pr-str (:position msg)))))))

(defn- namespaces
  "Every namespace that could be required.

  What is on the classpath and what the process has loaded. The second is not
  the first: a namespace made at a repl, or by a tool that called `create-ns',
  has no file anywhere and is a namespace all the same - and it is the half of
  this that is not read once and kept, since asking for it costs nothing."
  []
  (map (comp name ns-name) (all-ns)))

(defmethod groups :namespace [msg]
  (let [prefix (names/named-argument msg :prefix)
        {:keys [namespace-prefixes] :as read} (classpath/scan)
        loaded (namespaces)
        known (concat (:namespaces read) loaded)]
    [{:type "namespace" :names (under prefix known)}
     ;; The head of a prefix list, which is a name no file carries: what is
     ;; written in (clojure.core.specs [alpha]) is a piece of a namespace and
     ;; not one of its own, so requiring it alone would fail. Answered as what
     ;; it is rather than left out, and answered after the namespaces so that
     ;; a name which is both is the namespace.
     ;;
     ;; Those of the classpath were worked out when it was read, as the
     ;; packages of a class were. Those of what has been loaded are worked out
     ;; here, because what has been loaded is asked for again every time.
     {:type "namespace-prefix"
      :names (under prefix (concat namespace-prefixes (classpath/prefixes loaded)))}]))

;; What a :require-macros names is a namespace of this world. ClojureScript
;; compiles with two of them and the macros of a ClojureScript namespace are
;; written in Clojure, so this is the process that has the answer - and it
;; stays the process that has it once there is a ClojureScript side answering
;; :namespace.
(defmethod groups :namespace-macros [msg] (groups (assoc msg :position :namespace)))

(defmethod groups :var [msg]
  (let [named (:namespace msg)
        found (if (= :refer-clojure named)
                ;; a refer-clojure names no namespace anywhere in itself, and
                ;; the one it refers from is the one every namespace refers
                (find-ns 'clojure.core)
                (find-ns (symbol (names/required-argument msg :namespace))))]
    ;; One group of each kind rather than one of vars, so that a client can
    ;; say which is which without asking again. The namespace rides along for
    ;; the same reason: what is offered under a :refer is written without it,
    ;; and it is the one thing that says where the name came from.
    (for [[kind vars] (group-by (comp names/var-kind val) (when found (ns-publics found)))]
      {:type kind
       :ns (str (ns-name found))
       :names (map (comp name key) vars)})))

(defmethod groups :package-or-class [msg]
  (let [{:keys [classes packages]} (classpath/scan)]
    [{:type "class" :names (importable (names/text msg) classes)}
     {:type "package" :names packages}]))

(defn- generated-classes
  "The classes of PACKAGE that no file on the classpath carries.

  A deftype and a defrecord make a class the moment they are evaluated, and
  until something compiles them ahead of time there is no file of one to find.
  What there is is the namespace that made it, which imported it into itself,
  and the package of such a class is that namespace's name with the dashes
  munged out - so the namespace is looked for under the package as written and
  under the name that munges to it.

  Only where the package is written, which is the list form of an import.
  Finding them where it is not means reading the imports of every namespace
  loaded, ninety of which are java.lang's in every one of them.

  Everything the namespace imported comes back, and nothing is checked. What
  `under' does with these is keep the ones in the package as written, which is
  the same check twice, and what `ns-imports' answers is the mappings whose
  value is a class, which is the only thing asked of one here."
  [^String package]
  (when-let [found (or (find-ns (symbol package))
                       (find-ns (symbol (.replace package \_ \-))))]
    (for [[_ ^Class imported] (ns-imports found)]
      (.getName imported))))

(defmethod groups :class [msg]
  (let [package (names/required-argument msg :package)]
    [{:type "class"
      :names (importable (names/text msg)
                         (under package (concat (:classes (classpath/scan))
                                                (generated-classes package))))}]))

(defn- load-root
  "The directory a load written in NAMESPACE is read from.

  What `clojure.core/load' resolves a path that does not start with a slash
  against, which is the directory the namespace itself is named by."
  ^String [^String namespace]
  (str "/" (-> namespace (.replace \- \_) (.replace \. \/))))

(defmethod groups :load-path [msg]
  (let [paths (:paths (classpath/scan))
        namespace (names/named-argument msg :ns)]
    ;; A path that starts with a slash is read from the root of the classpath
    ;; and one that does not is read from the namespace it is written in, so
    ;; which of the two is being written is what the slash says. Absent where
    ;; the client did not say which namespace that is, since without one there
    ;; is no other answer to give.
    (if (or (string/starts-with? (names/text msg) "/") (string/blank? namespace))
      [{:type "path" :names paths}]
      [{:type "path" :names (inside (load-root namespace) paths)}])))

;; The keyword positions. A candidate carries its colon because the client
;; replaces the keyword it read point out of, and a keyword without its colon
;; is not something that can be written where that keyword is.

(defmethod groups :dependency-type [_]
  [{:type "keyword"
    :names [":gen-class" ":import" ":load" ":refer-clojure" ":require" ":use"]}])

(defmethod groups :libspec-option [_]
  [{:type "keyword" :names [":as" ":as-alias" ":exclude" ":only" ":refer" ":rename"]}])

;; What a refer takes, and only that. This is the position of a :use libspec
;; as well, which does take an :as - but it is the position of a refer-clojure
;; too, where an :as means nothing, and a set that is right everywhere it is
;; asked beats one that is complete in one of the three places.
(defmethod groups :libspec-option-refer [_]
  [{:type "keyword" :names [":exclude" ":only" ":rename"]}])

;; The options of the whole form rather than of a spec in it. :require, :use,
;; :refer, :as and :as-alias are taken here too and left out: written as a
;; flag each of them stands for true, which is not a thing any of the five
;; means.
(defmethod groups :flag [_]
  [{:type "keyword" :names [":reload" ":reload-all" ":verbose"]}])

;;; A name written in code

;; Every position above is a slot of a dependency form, where one kind of
;; name goes. Ordinary code is where all of them go at once: a local, a var
;; the namespace maps, a class it imported, an alias, a namespace, a special
;; form. They are answered together and in one order, so that what somebody
;; reads is one list rather than six.
;;
;; A keyword is the exception, and it is one because the text says so: a
;; colon is written in front of one and in front of nothing else, so what is
;; being written there is known before anything is looked for.

(defn- at-the-head?
  "Whether a name written where MSG says is written at the head of a form.

  Which is what a special form has to be: (if a b) is a form the compiler
  reads itself, and an if written at an argument of something is a name that
  resolves to nothing at all. So that is where they are offered and nowhere
  else.

  True as well where the client said nothing about the form the name is
  written in. That is a client that does not read one, not a client saying
  the name is written at an argument - and holding names back on the strength
  of a key nobody wrote would be an answer read into somebody's silence."
  [msg]
  (let [argument (names/argument msg)]
    (or (nil? argument) (zero? argument))))

(defn- class-package
  "The package CLASS is in, or nil when it is in none. An array class is one
  of those."
  ^String [^Class class]
  (when-let [package (.getPackage class)]
    (.getName package)))

(defn- mapping-groups
  "What the namespace NS maps, in a group for each kind of name.

  Under the name it is written as here rather than the name it has: a
  :rename writes a var under a name of the namespace's choosing, and what
  goes in the buffer is the name that works where it is being written.

  A var carries the namespace it is public in, which is the one thing its own
  name does not say - what is referred is written without it. A class carries
  the package it is in for the same reason."
  [ns]
  (let [mapped (ns-map ns)
        vars (for [[written found] mapped :when (var? found)] [(str written) found])
        classes (for [[written found] mapped :when (class? found)] [(str written) found])]
    (concat
     (for [[[kind from] found]
           (group-by (fn [[_ ^clojure.lang.Var var]]
                       [(names/var-kind var) (str (ns-name (.ns var)))])
                     vars)]
       {:type kind :ns from :names (map first found)})
     (for [[package found] (group-by (fn [[_ class]] (class-package class)) classes)]
       (cond-> {:type "class" :names (map first found)}
         package (assoc :package package))))))

(defn- alias-groups
  "The aliases NS holds, written behind PREFIX, each with the namespace it
  stands for.

  Which is what an alias is worth saying: the name is one somebody of this
  namespace chose, and nothing about it says what it reaches.

  PREFIX is what stands in front of one where it is being written - nothing
  where a var is, and the two colons of an ::alias/name."
  [ns ^String prefix]
  (for [[aliased found] (group-by val (ns-aliases ns))]
    {:type "namespace"
     :ns (str (ns-name aliased))
     :names (map (fn [[alias _]] (str prefix alias)) found)}))

(defn- scoped-groups
  "The vars that could be written under SCOPE in NS, written under it.

  An alias of the namespace or the name of a namespace, since the two are
  written the same way and both of them resolve. Nothing when it is neither:
  what does not resolve is a namespace nobody has required, and a process
  that has not loaded it has nothing to say about what is in it.

  The candidate carries the scope back, because a candidate is what goes in
  the buffer and the scope is part of what is written there.

  No locals are offered beside these, and no classes: a name with a slash in
  it is neither."
  [ns ^String scope]
  (when-let [found (or (get (ns-aliases ns) (symbol scope))
                       (find-ns (symbol scope)))]
    (for [[kind vars] (group-by (comp names/var-kind val) (ns-publics found))]
      {:type kind
       :ns (str (ns-name found))
       :names (map (fn [[named _]] (str scope "/" (name named))) vars)})))

(def ^:private keyword-table
  "The map clojure interns keywords in, or nil where this jvm will not show
  it.

  Read reflectively because there is nowhere else to read it. Nothing
  declares a keyword - one exists because something wrote it, in a namespace
  that was loaded or a form that was evaluated - so the table clojure keeps
  to intern them in is the only list of them there is.

  clojure.lang is on the classpath rather than in a module of the runtime, so
  this needs nothing opened to it. A clojure that renamed the field is
  answered with no keywords at all rather than with a broken process: a
  completion is not the place to fail over a name it could not find."
  (delay
    (try
      (let [field (.getDeclaredField clojure.lang.Keyword "table")]
        (.setAccessible field true)
        (.get field nil))
      (catch Throwable _ nil))))

(defn- writable-part?
  "Whether PART can be written as the namespace or the name of a keyword.

  Walked a character at a time rather than matched against a pattern, because
  this is asked of every keyword the process holds each time one is being
  written - a table of fifty thousand is a document somebody parsed, and the
  answer is owed before the next keystroke."
  [^String part]
  (let [length (.length part)]
    (and (pos? length)
         ;; a colon at either end, or two of them anywhere, is a token the
         ;; reader refuses: :::name is not how any keyword is written
         (not (.startsWith part ":"))
         (not (.endsWith part ":"))
         (not (.contains part "::"))
         ;; and the slash is where the namespace ends, so a second one is a
         ;; keyword written with two namespaces
         (not (.contains part "/"))
         (loop [index 0]
           (or (= index length)
               ;; what ends a token or begins a form of its own. A name
               ;; written with one of them in it is a name the reader stops
               ;; partway through, and what it reads is a shorter keyword
               ;; than the one that was offered - or no keyword at all.
               (let [character (.charAt part index)]
                 (and (case character
                        (\" \; \@ \^ \` \~ \( \) \[ \] \{ \} \\ \,) false
                        true)
                      (not (Character/isWhitespace character))
                      (recur (inc index)))))))))

(defn- writable-keyword?
  "Whether NAMED is a keyword somebody could write.

  Nothing says a keyword was written to exist. `keyword' makes one out of
  whatever string it is handed, so a process that has read a document has
  interned one for every key in it - a keyword with a space in its name, or a
  quote, or nothing at all. None of those can be written back: the reader
  stops at the space, or refuses the token, and what a client put in the
  buffer is not the keyword it was offered.

  So they are not offered. What is asked here is the shape the reader takes,
  drawn a little tighter than the reader draws it - a name that begins with a
  slash under a namespace is read back fine and is left out all the same,
  because nothing writes one on purpose and offering less is the error that
  costs nobody anything. A name that is a slash and nothing else is kept,
  since that is the one keyword anybody writes with one in it."
  [^clojure.lang.Symbol named]
  (let [scope (.getNamespace named)
        ^String name (.getName named)]
    (if (nil? scope)
      (or (= "/" name) (writable-part? name))
      (and (writable-part? scope)
           (or (= "/" name)
               (and (writable-part? name)
                    ;; a digit at the front of the name is read as a number
                    ;; where a namespace stands before it, and :a/1 is a
                    ;; token the reader refuses - though :1 on its own is one
                    ;; it takes
                    (not (Character/isDigit (.charAt name 0)))))))))

(defn- interned-keywords
  "Every keyword this process has interned that could be written, as the
  symbol each is named by."
  []
  (when-let [^java.util.Map table @keyword-table]
    (filter writable-keyword? (.keySet table))))

(defn- keywords-of
  "The keywords interned in the namespace named NAMED."
  [^String named]
  (filter (fn [^clojure.lang.Symbol keyword] (= named (.getNamespace keyword)))
          (interned-keywords)))

(defn- keyword-groups
  "The keywords that could be written where TEXT is being written in NS.

  One written with a single colon is read as it stands, so what is offered is
  every keyword this process has interned - including the qualified ones,
  which are written out in full where they are written that way.

  One written with two is read against the namespace it is written in: ::name
  is a keyword of this namespace, and ::alias/name one of the namespace that
  alias stands for. An alias and nothing else, since the reader takes nothing
  else there - ::clojure.string/x is an invalid token in a namespace that
  required clojure.string without aliasing it.

  A candidate carries its colons, because a keyword without them is not what
  can be written where that keyword is."
  [ns ^String written]
  (if (string/starts-with? written "::")
    (if-let [scope (names/scope-of (subs written 2))]
      (when-let [aliased (get (ns-aliases ns) (symbol scope))]
        [{:type "keyword"
          :ns (str (ns-name aliased))
          :names (map (fn [named] (str "::" scope "/" (name named)))
                      (keywords-of (str (ns-name aliased))))}])
      (cons {:type "keyword"
             :names (map (fn [named] (str "::" (name named)))
                         (keywords-of (str (ns-name ns))))}
            ;; and the aliases, which is what stands before the slash of an
            ;; ::alias/name. Offered as the namespaces they are rather than
            ;; as keywords: ::alias is a keyword of this namespace that
            ;; happens to be spelled like one, and what somebody writing it
            ;; is reaching for is the namespace it opens.
            (alias-groups ns "::")))
    [{:type "keyword"
      :names (map (fn [named] (str ":" named)) (interned-keywords))}]))

(defn- static?
  [^java.lang.reflect.Member member]
  (java.lang.reflect.Modifier/isStatic (.getModifiers member)))

(defn- class-groups
  "The members of the class SCOPE names in NS, written under it.

  A static method and a static field are written Class/name. An instance
  method is written Class/.name and a constructor Class/new, which are the
  spellings clojure reads since 1.12 - and 1.12 is the least this process
  runs on, so they are always what can be written here.

  The public members, the inherited ones included, which is what getMethods
  and getFields answer: what can be written is what can be seen from outside
  the class. An overload is one name however many arities it has, since a
  name is what goes in the buffer.

  Instance fields are not among them. What reads one is (.-field x), written
  on the thing rather than on the class, and there is no spelling of it
  behind a slash."
  [ns ^String scope]
  (when-let [^Class class (names/class-named ns scope)]
    (let [methods (seq (.getMethods class))]
      [{:type "method"
        :names (concat (for [^java.lang.reflect.Method method methods
                             :when (static? method)]
                         (str scope "/" (.getName method)))
                       (for [^java.lang.reflect.Method method methods
                             :when (not (static? method))]
                         (str scope "/." (.getName method))))}
       {:type "field"
        :names (for [^java.lang.reflect.Field field (.getFields class)
                     :when (static? field)]
                 (str scope "/" (.getName field)))}
       {:type "constructor" :names [(str scope "/new")]}])))


(defn- member-groups
  "The members of what TEXT is being written on, written as they are called.

  A method is written .name and a field .-name, and which of the two is being
  written is in the text: what follows the dash is a field and nothing else,
  since a field is not readable by the spelling a method is called with.

  The instance members only. A static one is written on the class rather than
  on a thing - Integer/MAX_VALUE, not (.MAX_VALUE x) - and there is no
  spelling of it here."
  [ns msg ^String written]
  (when-let [^Class class (names/target-class ns msg)]
    (if (string/starts-with? written ".-")
      [{:type "field"
        :names (for [^java.lang.reflect.Field field (.getFields class)
                     :when (not (static? field))]
                 (str ".-" (.getName field)))}]
      [{:type "method"
        :names (for [^java.lang.reflect.Method method (.getMethods class)
                     :when (not (static? method))]
                 (str "." (.getName method)))}])))

(defn- constructor-groups
  "The constructor calls TEXT is the start of, or nil where it is none.

  A class name with a dot written after it, which is how a constructor is
  written where Class/new is the spelling clojure reads since 1.12. The dot
  is what says one is being written rather than a class being named, so it
  has to be written first - and once it is, every candidate carries one:
  a candidate without it would be written over the dot and take it away,
  which is what somebody who typed (Date. would watch happen.

  Only where what stands before the dot is already a class. java.util. is
  somebody halfway through writing a class name rather than a constructor of
  a package, and the classes are what is offered there."
  [ns ^String text]
  (when (string/ends-with? text ".")
    (let [named (subs text 0 (dec (.length text)))]
      (when (and (seq named) (names/class-named ns named))
        [{:type "constructor"
          :names (map (fn [^String found] (str found "."))
                      (cons named (importable text (:classes (classpath/scan)))))}]))))

(defmethod groups :code [msg]
  (let [ns (names/namespace-named msg)
        written (names/text msg)]
    ;; A keyword before a scope, since ::alias/name is written with a slash
    ;; as well as with colons, and it is the colons that say what the slash
    ;; means there
    (if (string/starts-with? written ":")
      (keyword-groups ns written)
      ;; A member before anything else, since a name written on a thing is
      ;; read against that thing rather than against the namespace
      (if (string/starts-with? written ".")
        (member-groups ns msg written)
        (if-let [constructed (constructor-groups ns written)]
          constructed
          (if-let [scope (names/scope-of written)]
            ;; A namespace and a class are written the same way and answered
            ;; together, since a scope that is both - which nothing forbids - is
            ;; a question about both.
            (concat (scoped-groups ns scope) (class-groups ns scope))
            (concat
             ;; First, which is what makes a local shadow. A name in two groups is
             ;; answered once, as what the first of them says it is - and a local
             ;; named map is what map means where it is bound, whatever
             ;; clojure.core has to say about the name.
             [{:type "local" :names (names/locals-named msg)}]
             ;; Before the aliases, because that is the order the language reads
             ;; them in: a name written with no slash after it is the mapping and
             ;; not the alias that is spelled the same.
             (mapping-groups ns)
             (alias-groups ns "")
             ;; The namespaces that have been loaded, which is how a var of one
             ;; that this namespace never required is written: in full, and the
             ;; full name starts with one of these.
             [{:type "namespace" :names (namespaces)}]
             (when (at-the-head? msg)
               [{:type "special-form" :names names/special-forms}])
             ;; A class that was not imported is written in full, which means
             ;; written with a dot in it - so until the text holds one, the classes
             ;; offered are the ones the namespace imported and no others.
             ;; Otherwise two letters typed anywhere in code would answer with a
             ;; thousand class names, and the vars they were meant to reach would
             ;; be underneath them.
             (when (string/includes? written ".")
               [{:type "class" :names (importable written (:classes (classpath/scan)))}]))))))))

;;; A path written in a string

;; Most strings are text and a few of them are paths, and what tells the two
;; apart is the call the string is written in: what goes in (io/resource "...")
;; is a name on the classpath, and what goes in (str "...") is a message
;; somebody is writing. So the client sends the call it read around the string
;; and which argument of it this is, and the rest is worked out here - the call
;; being written under whatever alias the namespace gave clojure.java.io, and
;; resolving a name in a namespace being the half only a process has.

(defn- called
  "What the call MSG names resolves to in NS, or nil when it names nothing.

  Nothing is loaded to find out and nothing is evaluated: `ns-resolve' reads
  what the namespace already maps, and what it does not map is one more way
  of naming nothing. A name that reaches for a class it has not got is one of
  those, which is what the catch is for."
  [ns msg]
  (when-let [written (names/call-named msg)]
    (try (ns-resolve ns (symbol written)) (catch Throwable _ nil))))

(defmethod groups :string [msg]
  ;; `clojure.java.io/resource' and nothing else. It is the one of these that
  ;; reads a name against the classpath - what `slurp' and `io/reader' and
  ;; `io/file' take is a file, and a bare name written at one of those is a
  ;; path against the directory the process was started in, which is a
  ;; directory rather than a list of names.
  (when (and (= 1 (names/argument msg))
             (= #'io/resource (called (names/namespace-named msg) msg)))
    [{:type "path" :names (:resources (classpath/scan))}]))

;;; The answer

(defn completions
  "The candidates for the position MSG names, as the reply frame carries them."
  [msg]
  (let [found (matching (names/text msg) (groups msg))
        kept (into [] (take max-completions) found)]
    {:completions kept
     :truncated (when (> (count found) (count kept)) true)}))
