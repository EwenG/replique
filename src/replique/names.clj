(ns replique.names
  "What a client wrote, and what it names.

  Two ops read a name out of somebody's buffer. `:completions' is asked what
  could be written where one is being written, and `:symbol' what the one
  written there is - and both of them are handed the same thing: a position, a
  text, and the namespace, the locals and the type the client read around it.
  Reading those keys is one job rather than two, and so is resolving what they
  name - and so is knowing which language to resolve them in, which is what
  `*dialect*' and the handful of functions under it are for.

  Nothing here evaluates anything. What an expression would return is not
  knowable without running it, and running somebody's code is what a keystroke
  must not do."
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [replique.cljs :as cljs]
            [replique.protocol :as protocol]))

;;; Refusing a message

(defn invalid
  "The exception a message written wrongly is refused with."
  [message]
  (ex-info message {:replique/error :invalid-message}))

;;; Which language it is written in

(def dialects
  "The languages a name can be written in, and so the worlds one can be read
  against.

  The same two the repl handshake takes, spelled the same way - see
  `replique.repl'. Which of them a buffer is in is the client's half of this
  and always was: the file has an extension, and this process has only what
  the message says."
  #{:clj :cljs})

(def ^:dynamic *dialect*
  "The language this thread's question is about.

  DYNAMIC, and for `replique.cljs/*target*'s reason: it is a property of who
  is asking rather than of what is being asked, and every rule that reads it
  sits deep under the one place that knows. Threading it through would put an
  argument on `namespace-named', on everything that calls it and on everything
  that calls those, to say one thing that is true for the whole of a request.

  Clojure where nothing says otherwise, which is what every client wrote
  before there was a second answer."
  :clj)

(defn cljs?
  "Whether this thread's question is about ClojureScript."
  []
  (= :cljs *dialect*))

(defn dialect-named
  "The dialect MSG says it is asking about.

  Clojure where the message does not say, so that a client written before
  there was a second one goes on being answered - and so that the key is
  absent from every message about Clojure rather than written on every one.

  Refused where it says something else. A dialect nothing here can answer is
  not a question with an empty answer: it is a client that believes this
  process reads .cljc a third way, and being refused is how it finds out."
  [msg]
  (if (nil? (:dialect msg))
    :clj
    (let [named (protocol/as-keyword (:dialect msg))]
      (or (dialects named)
          ;; The kind the handshake refuses a dialect with, because it is the
          ;; same mistake: a client that wrote one dialect this process does
          ;; not speak is a client that will write it again, and a kind it can
          ;; branch on is worth more than a sentence it has to read.
          (throw (ex-info (str "A name is written in one of these dialects, and the"
                               " :dialect named none of them: "
                               (pr-str (:dialect msg)) " - it is one of "
                               (pr-str (vec (sort (map name dialects)))))
                          {:replique/error :invalid-dialect}))))))

(defn with-dialect*
  "Call F reading MSG's dialect, and MSG's target where that is ClojureScript.

  ONE PLACE, at the top of an op, rather than where each name is looked up -
  which is what `replique.cljs/refuse-unless-available!' is for. A process
  with no compiler has no ClojureScript namespaces AT ALL, so a request it
  cannot answer has to be refused before anything is looked for: otherwise a
  namespace that does not exist and a process that cannot say come back as the
  same empty list.

  The target rides along because it says which compile environment answers,
  and two of them hold two different sets of namespaces - a browser build and
  a node build resolve the same npm specifier to different files. A question
  that does not say is answered about `replique.cljs/default-target', the way
  a repl that does not say is started on it."
  [msg f]
  (if (= :clj (dialect-named msg))
    (binding [*dialect* :clj] (f))
    (do
      (cljs/refuse-unless-available! "answer a ClojureScript question")
      (let [target (cljs/as-target (or (:target msg) cljs/default-target))]
        (when (nil? target)
          (throw (ex-info (str "A ClojureScript question is asked about one of these"
                               " targets, and the :target named none of them: "
                               (pr-str (:target msg)) " - it is one of "
                               (pr-str (vec (sort (map name cljs/targets)))))
                          {:replique/error :invalid-target})))
        (binding [*dialect* :cljs
                  cljs/*target* target]
          (f))))))

(defmacro with-dialect
  "Body read in the dialect MSG names. See `with-dialect*'."
  [msg & body]
  `(with-dialect* ~msg (fn [] ~@body)))

;;; Which world a name is read against
;;
;; The whole of what a dialect changes about finding a name, and it is this
;; short for one reason: a ClojureScript namespace in this compiler is a real
;; `clojure.lang.Namespace' holding real Vars, so `ns-map', `ns-publics',
;; `ns-interns', `ns-aliases' and `meta' are already the functions that read
;; one. What they are not is a way to FIND one - `find-ns' and `all-ns' read
;; the world the jvm keeps, which holds none of these - so finding is the
;; thing written twice and reading is written once.
;;
;; Resolving is the other half, and it is a rule rather than a lookup because
;; of the one namespace a ClojureScript namespace does not map: cljs.core.
;; Every namespace refers it whether it says so or not, and the compiler
;; honours that with a rule rather than with a thousand mappings - so a reader
;; that looked only at what a namespace maps would say that `map' means
;; nothing in a namespace where it plainly means cljs.core's.

(defn all-namespaces
  "Every namespace of this dialect that the process holds."
  []
  (if (cljs?) (cljs/namespaces) (all-ns)))

(defn find-namespace
  "The namespace of this dialect called NAMED, or nil when there is none."
  [named]
  (let [sym (symbol (str named))]
    (if (cljs?) (cljs/find-namespace sym) (find-ns sym))))

(defn core-namespace
  "The namespace every namespace of this dialect refers.

  clojure.core, or cljs.core where the question is about ClojureScript. What
  a `:refer-clojure' refers from, and what a namespace this process does not
  have is answered as."
  []
  (find-namespace (if (cljs?) cljs/core 'clojure.core)))

(defn resolve-plain
  "The var the bare name NAMED means in NS, or nil when it means none.

  What the namespace maps, and then - for ClojureScript only - cljs.core,
  because that is where the rule lives that no mapping records. A
  `:refer-clojure :exclude' is honoured on the way, since a namespace that
  said a name is not core's meant it.

  Clojure needs no rule of its own here: clojure.core is referred into a
  namespace as a thousand ordinary mappings, so what the namespace maps is
  already the whole answer.

  AND THEN A MACRO, for ClojureScript, where the macros are in another world
  - see `macro-refers'. After the var and never instead of one, which is
  `clojure.cljs.analyzer-api/resolve's order: `str' is a macro and a function
  there, and what somebody reading it wants is the function, whose arglists
  are the ones the macro takes as well."
  [ns ^String named]
  (let [sym (symbol named)
        mapped (get (ns-map ns) sym)]
    (cond
      (var? mapped) mapped
      (not (cljs?)) nil
      :else (let [here (str (ns-name ns))]
              (or (when-not (or (= cljs/core (symbol here)) (cljs/excluded? here sym))
                    (get (ns-interns (core-namespace)) sym))
                  (cljs/macro-var here sym))))))

(defn resolve-macro
  "The macro the qualified name SCOPE/NAMED means in NS, or nil.

  Nil for Clojure, where a macro is a var like any other and the namespace
  SCOPE resolves to already holds it. A ClojureScript namespace does not: its
  macros are in the jvm namespace of the same name or of another one, which is
  a rule of the compiler's - see `replique.cljs/macro-var'."
  [ns ^String scope ^String named]
  (when (cljs?)
    (cljs/macro-var (ns-name ns) (symbol scope named))))

(defn interned-macro
  "The macro the qualified name NAMED names, or nil.

  `interned' for the other world: nil for Clojure, where that already finds a
  macro. A ClojureScript macro is a var of this jvm, found by the compiler's
  rule for a name written in full - clojure.core/let is cljs.core's let there,
  and so is cljs.core/let - read from cljs.core, where no alias of anybody's
  can stand in the way."
  [named]
  (when (cljs?)
    (let [sym (symbol (str named))]
      (when (namespace sym)
        (cljs/macro-var cljs/core sym)))))

(defn macro-refers
  "What NS can write as a bare name and mean a macro, by that name.

  Nothing for Clojure, where a macro is a var the namespace maps like any
  other. A ClojureScript namespace maps none of them: they are Clojure vars
  of this jvm, which the compiler reaches through a view of its own - what a
  :refer-macros referred, and cljs.core's macros by the rule that reaches
  cljs.core's functions. So `defn', `when' and `let', which are macros there
  and nothing else, are in no table but this one."
  [ns]
  (when (cljs?)
    (cljs/macro-refers (ns-name ns))))

(defn macro-scope
  "The jvm namespace of macros SCOPE names from inside NS, or nil.

  What `resolve-scope' is for a var, for a macro: the alias of a
  :require-macros, the namespace an alias of the namespace stands for, or the
  name in full. Nil for Clojure, where the namespace `resolve-scope' answers
  holds its macros."
  [ns ^String scope]
  (when (cljs?)
    (cljs/macro-namespace (ns-name ns) scope)))

(defn macro-aliases
  "The aliases NS holds for namespaces of macros, by alias.

  Nothing for Clojure, where `ns-aliases' has them all. A :require-macros
  aliases into the other world, and `ns-aliases' of a ClojureScript namespace
  does not see it."
  [ns]
  (when (cljs?)
    (cljs/macro-aliases (ns-name ns))))

(defn interned
  "The var the qualified name NAMED names where it lives, or nil.

  Looked up in the interns of the namespace the name is qualified by, rather
  than resolved. Both refuse a name that a namespace only refers - a referred
  var is not interned there - but `resolve' asks the namespace the CALLING
  THREAD happens to be in what the qualifier means, and answers through an
  alias of it. The thread answering an op is in whatever namespace it was left
  in, which has nothing to do with the request, and a message has to mean the
  same thing whichever thread reads it.

  Which world it is looked for in is the dialect's to say, and that is the
  other half of why this is here rather than written out at each caller."
  [named]
  (let [sym (symbol (str named))]
    (when-let [home (some-> (namespace sym) find-namespace)]
      (get (ns-interns home) (symbol (name sym))))))

(defn core-refers
  "What NS can write as a bare name and mean a var of core, by that name.

  `resolve-plain' asked of every name at once, which is what a completion
  needs: the list of what could be written, where that resolves one thing
  somebody has written already.

  Nothing for Clojure. Core is referred into a namespace there as a thousand
  ordinary mappings, so `ns-map' already holds every one of them and
  answering them again would offer each name twice.

  The publics rather than the interns, which is the one place this is
  narrower than the compiler: `resolve-plain' follows the compiler and finds a
  private var of core by name, because that is what the compiler does and a
  name somebody has written has to be answered as what it means. Offering one
  is a different thing - it is this process suggesting that somebody write a
  name core did not publish."
  [ns]
  (when (cljs?)
    (let [here (ns-name ns)]
      (when-not (= cljs/core here)
        (let [mapped (ns-map ns)]
          (into {}
                (remove (fn [[sym _]]
                          (or (contains? mapped sym)
                              (cljs/excluded? here sym))))
                (ns-publics (core-namespace))))))))

(defn resolve-scope
  "The namespace SCOPE names from inside NS, or nil when it names none.

  What stands before the slash of a qualified name, or the head of an
  ::alias/name. An alias of the namespace first in both dialects; after that
  the two part company, and ClojureScript is the stricter of them - a
  namespace is reachable there by having been REQUIRED, where clojure answers
  for anything the process has loaded at all."
  [ns ^String scope]
  (or (get (ns-aliases ns) (symbol scope))
      (if (cljs?)
        (cljs/resolve-namespace (ns-name ns) scope)
        (find-ns (symbol scope)))))

;;; What the client wrote

(defn named-argument
  "The string value of KEY in MSG, or nil when there is none.

  Which of the three spellings a client wrote it in is `protocol/as-name's
  to know. What is said here is only that this one had to be a name."
  ^String [msg key]
  (let [value (get msg key)]
    (if (nil? value)
      nil
      (or (protocol/as-name value)
          (throw (invalid (str "The " key " must be a name, got: " (pr-str value))))))))

(defn required-argument
  "The string value of KEY in MSG, which the position needs to be answered."
  ^String [msg key]
  (let [value (named-argument msg key)]
    (when (string/blank? value)
      (throw (invalid (str "A name written at " (pr-str (:position msg))
                           " needs the " key " to look in"))))
    value))

(defn text
  "What has been typed at the position, which is the name being asked about.

  Absent means nothing has been typed yet. Which is every name rather than
  none where the question is what could be written - point sits after an
  opening bracket and everything could follow it - and no name at all where
  the question is what the name written there is."
  ^String [msg]
  (let [value (:text msg)]
    (cond
      (nil? value) ""
      (string? value) value
      :else (throw (invalid (str "The :text of a request must be a string, got: "
                                 (pr-str value)))))))

(defn namespace-named
  "The namespace MSG says the name is being written in.

  A namespace the process does not have - a file whose ns form has not been
  evaluated yet, which is every file until it is loaded - is answered as the
  core namespace itself. What a namespace refers before it refers anything is
  core, so the names of core mean there what they mean here, and half an
  answer beats none. Which core that is, is the dialect's to say: clojure.core
  for a .clj buffer and cljs.core for a .cljs one.

  The :spellings op reads the key the same way and means the same thing by
  it, so what a namespace a client named is, is one rule rather than three."
  [msg]
  (or (when-let [written (named-argument msg :ns)]
        (find-namespace written))
      (core-namespace)))

(defn locals-named
  "The locals the client says are in scope where the name is being written.

  Only the client can know them. A local is bound by the form being written,
  which the process has never seen - so a request that did not carry them
  would answer out of a namespace and leave out the names nearest to hand.

  Each is written as a map holding its :name rather than as the name itself,
  so that what else the client knows about one - the type a ^String on it
  declares, which is what says what can be called on it - has somewhere to go
  without the shape changing. Nothing reads anything but the name yet."
  [msg]
  (let [value (:locals msg)]
    (when (some? value)
      (when-not (sequential? value)
        (throw (invalid (str "The :locals of a request must be a list, got: "
                             (pr-str value)))))
      (mapv (fn [local]
              (or (and (map? local) (protocol/as-name (:name local)))
                  (throw (invalid (str "A local must be a map holding its :name, got: "
                                       (pr-str local))))))
            value))))

(defn call-named
  "The name of the call MSG says the name is being written inside, or nil
  where the client said it is being written inside none.

  What stands at the head of the enclosing form - the io/resource of
  (io/resource \"config.edn\"). Only a client can read it: it is written
  beside the name being asked about, in a buffer this process has never seen.

  It travels as it is written there, under whatever alias the namespace gave
  the namespace it is public in, and is resolved against the namespace the
  client named. Which is the half only this process has: an alias is a
  mapping of a namespace, and reading one means holding the namespace."
  ^String [msg]
  (named-argument msg :call))

(defn argument
  "Which argument of that call the name is being written at, or nil.

  Nought at the head of the form itself, one at the first argument, and so on
  - it is how many arguments of the form end before the name being written,
  and at the head none of them do.

  Absent where the client read no form around the name. Absent as well from a
  client that does not read one at all, which is why nothing is held back on
  the strength of it being missing: a key nobody wrote is a client that did
  not look, and answering that with less than was asked for would be reading
  an answer into somebody's silence."
  [msg]
  (let [value (:argument msg)]
    (cond
      (nil? value) nil
      (and (integer? value) (not (neg? value))) value
      :else (throw (invalid (str "The :argument of a request must be a whole number, got: "
                                 (pr-str value)))))))

;;; What it names

(def ^:private monitors
  "The two special forms a JavaScript runtime has nothing to lock.

  ClojureScript reads everything else in the list below, and reads it the same
  way. These two it does not read at all: they take the lock of a jvm object,
  and a program compiled to JavaScript has neither."
  #{"monitor-enter" "monitor-exit"})

(defn special-forms
  "The forms the compiler reads itself, as they are written.

  The starred ones are left out: let* and fn* and loop* are written let and
  fn and loop, which are macros and are answered as the vars they are. So are
  the dot and the ampersand - one is written on the thing it is called on and
  the other where a parameter vector says the rest of the arguments go, and
  neither is a name written on its own.

  nil, true and false are not here either. They are shorter than asking for
  them would be."
  []
  (cond->> ["catch" "def" "do" "finally" "if" "monitor-enter" "monitor-exit"
            "new" "quote" "recur" "set!" "throw" "try" "var"]
    (cljs?) (remove monitors)))

(defn arglists
  "What the metadata of a var says it is called with, as the forms it was
  written as, or nil where it says nothing.

  ONE UNWRAPPING, and it is ClojureScript's: a .cljs var carries
  (quote ([coll])) where a .clj var carries ([coll]). Which is not the
  compiler being careless - metadata in ClojureScript is data that has to
  survive into the emitted program, so a form that would be evaluated there is
  written quoted - but it means a reader that took the value as it stands
  would show somebody `quote' as the first way to call `first'.

  Read through here by everything that reads arglists at all, so that the two
  dialects answer the same shape and nothing downstream has to know there was
  ever a difference."
  [metadata]
  (let [written (:arglists metadata)]
    (if (and (seq? written) (= 'quote (first written)))
      (second written)
      written)))

(defn var-kind
  "What VAR is, as a client annotates it with. A macro before a function
  because a macro has arglists too."
  [var]
  (let [metadata (meta var)]
    (cond (:macro metadata) "macro"
          (arglists metadata) "function"
          :else "var")))

(defn scope-of
  "What TEXT is written under, or nil when it is written under nothing.

  Which is whatever stands before the last slash: str/jo is written under
  str, and clojure.string/jo under clojure.string. A slash at the front is
  not one - that is the var named / being written."
  ^String [^String text]
  (let [index (.lastIndexOf text (int \/))]
    (when (pos? index) (subs text 0 index))))

(defn class-named
  "The class SCOPE names in NS, or nil when it names none.

  What the namespace imported, which is a class the process already holds,
  and a class written out in full, which is one it may never have touched.
  The second is loaded to be found and is not initialized: what runs a static
  initializer is using a class, and reading the names of its members is not a
  use. Loading the one class somebody has named is not a reading of the
  classpath - that one is loading a hundred thousand classes to see what they
  are.

  Nothing at all where the question is about ClojureScript. A name written
  before a slash or after a dot there reaches a JavaScript object rather than
  a class, and answering it out of this jvm's classpath would be answering a
  question about one language with a fact about another."
  ^Class [ns ^String scope]
  (when-not (cljs?)
    (or (let [mapped (get (ns-map ns) (symbol scope))]
          (when (class? mapped) mapped))
        (try (Class/forName scope false (clojure.lang.RT/baseLoader))
             (catch Throwable _ nil)))))

(defn target-class
  "The class a member is being written on, or nil where nothing says what it
  is.

  The tag first, which is a ^String the client read out of the text - off the
  local the name is written on, or off the thing it is written on where that
  was written at the call site. Then `:on', the thing as it is written: a var
  declares its class with a :tag of its own, and a literal is its own class.

  `:ON' AND NOT `:TARGET', WHICH IS WHAT IT WAS AND WHICH COLLIDED. A
  ClojureScript question carries the runtime it is about under `:target' - a
  browser build and a node build are two compilations of one source - and a
  name written on something carried the thing it was written on under the same
  key. Both go in one message, so a .cljs buffer asking about a member sent
  `:target' twice and the whole line was refused as unreadable EDN, which is a
  failure that names neither of them. One of the two had to move and this is
  the one: the runtime's `:target' is in the handshake, in every prompt and in
  every reading op, and this one is in three.

  Nothing is evaluated to find out. What an expression would return is not
  knowable without running it, and running somebody's code is what a
  keystroke must not do - so (.getT (make-thing)) is answered with nothing
  rather than by making one. What it is written on is read rather than
  evaluated for the same reason, and read as edn: what a client sent is text
  out of somebody's buffer, and #= in it is a form the reader would run.

  Nothing where the question is about ClojureScript, for `class-named's
  reason - and here it takes saying, because a literal IS a class on this side
  of the compiler: the \"x\" somebody wrote in a .cljs buffer would otherwise be
  answered with java.lang.String and the members of it."
  ^Class [ns msg]
  (when-not (cljs?)
    (or (when-let [tag (named-argument msg :tag)]
          (class-named ns tag))
        (when-let [target (named-argument msg :on)]
          (let [value (try (edn/read-string target) (catch Throwable _ nil))]
            (cond
              (symbol? value)
              (let [found (try (ns-resolve ns value) (catch Throwable _ nil))
                    tag (when (var? found) (:tag (meta found)))]
                (cond (class? tag) tag
                      (symbol? tag) (class-named ns (str tag))))
              (some? value) (class value)))))))
