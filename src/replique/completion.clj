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
  it, and a keystroke is not a thing that should run anybody's code."
  (:require [clojure.string :as string]
            [replique.classpath :as classpath]))

(def max-completions
  "How many candidates travel in one reply. A client is showing them to
  somebody who is still typing, and the rest of a hundred thousand names is
  not what they are about to pick from. What was cut is said rather than
  quietly dropped, so a client can ask for more to be typed instead of
  showing an answer that looks whole."
  500)

;;; What the client wrote

(defn- invalid [message]
  (ex-info message {:replique/error :invalid-message}))

(defn- named-argument
  "The string value of KEY in MSG, or nil when there is none.

  A client with an EDN printer writes a symbol or a keyword where one without
  writes a string, and the three spell the same name."
  ^String [msg key]
  (let [value (get msg key)]
    (cond
      (nil? value) nil
      (string? value) value
      (or (symbol? value) (keyword? value)) (name value)
      :else (throw (invalid (str "The " key " of a completion must be a name, got: "
                                 (pr-str value)))))))

(defn- required-argument ^String [msg key]
  (let [value (named-argument msg key)]
    (when (string/blank? value)
      (throw (invalid (str "A completion at " (pr-str (:position msg))
                           " needs the " key " to look in"))))
    value))

(defn- text
  "What has been typed at the position, which is what the candidates replace.

  Absent means nothing has been typed yet, which is every name rather than
  none: point sits after an opening bracket and everything could follow it."
  ^String [msg]
  (let [value (:text msg)]
    (cond
      (nil? value) ""
      (string? value) value
      :else (throw (invalid (str "The :text of a completion must be a string, got: "
                                 (pr-str value)))))))

;;; Matching

(def ^:private separators
  "What a name is written in pieces with. A capital starts one too, and that
  is a place to split before rather than a character to split on."
  #{\. \- \/ \_})

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
  separator in it at all. The capital of an acronym is one only where the
  acronym ends - the S of HTTPServer and not the T of HTTP - so that HS
  reaches it and H does not reach it four times over."
  [^String candidate ^long index]
  (or (zero? index)
      (contains? boundaries (.charAt candidate (dec index)))
      (let [character (.charAt candidate index)]
        (and (Character/isUpperCase character)
             (or (not (Character/isUpperCase (.charAt candidate (dec index))))
                 (and (< (inc index) (.length candidate))
                      (Character/isLowerCase (.charAt candidate (inc index)))))))))

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
                        (:ns group) (assoc :ns (:ns group)))))))
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
  (throw (invalid (if (nil? (:position msg))
                    "A completion needs the :position it is being asked at"
                    (str "Unknown completion position: " (pr-str (:position msg)))))))

(defn- namespaces
  "Every namespace that could be required.

  What is on the classpath and what the process has loaded. The second is not
  the first: a namespace made at a repl, or by a tool that called `create-ns',
  has no file anywhere and is a namespace all the same - and it is the half of
  this that is not read once and kept, since asking for it costs nothing."
  []
  (map (comp name ns-name) (all-ns)))

(defmethod groups :namespace [msg]
  (let [prefix (named-argument msg :prefix)
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

(defn- var-kind
  "What VAR is, as a client annotates it with. A macro before a function
  because a macro has arglists too."
  [var]
  (let [{:keys [arglists macro]} (meta var)]
    (cond macro "macro" arglists "function" :else "var")))

(defmethod groups :var [msg]
  (let [named (:namespace msg)
        found (if (= :refer-clojure named)
                ;; a refer-clojure names no namespace anywhere in itself, and
                ;; the one it refers from is the one every namespace refers
                (find-ns 'clojure.core)
                (find-ns (symbol (required-argument msg :namespace))))]
    ;; One group of each kind rather than one of vars, so that a client can
    ;; say which is which without asking again. The namespace rides along for
    ;; the same reason: what is offered under a :refer is written without it,
    ;; and it is the one thing that says where the name came from.
    (for [[kind vars] (group-by (comp var-kind val) (when found (ns-publics found)))]
      {:type kind
       :ns (str (ns-name found))
       :names (map (comp name key) vars)})))

(defmethod groups :package-or-class [msg]
  (let [{:keys [classes packages]} (classpath/scan)]
    [{:type "class" :names (importable (text msg) classes)}
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
  (let [package (required-argument msg :package)]
    [{:type "class"
      :names (importable (text msg)
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
        namespace (named-argument msg :ns)]
    ;; A path that starts with a slash is read from the root of the classpath
    ;; and one that does not is read from the namespace it is written in, so
    ;; which of the two is being written is what the slash says. Absent where
    ;; the client did not say which namespace that is, since without one there
    ;; is no other answer to give.
    (if (or (string/starts-with? (text msg) "/") (string/blank? namespace))
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

;;; The answer

(defn completions
  "The candidates for the position MSG names, as the reply frame carries them."
  [msg]
  (let [found (matching (text msg) (groups msg))
        kept (into [] (take max-completions) found)]
    {:completions kept
     :truncated (when (> (count found) (count kept)) true)}))
