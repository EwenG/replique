(ns replique.symbol
  "What the name written here is.

  The other half of `replique.completion'. A completion is asked what could be
  written where somebody is typing; this is asked what the one thing written
  there already is - the var a symbol resolves to in this namespace, the class
  a name is imported as, the method a dot is calling. It is read out of the
  same request: the client says which slot of which form point is in, and what
  is written there, and this answers what that names.

  Which is what an editor needs twice over. An arglist and a docstring are
  what it shows while somebody is writing a call, and a file and a line are
  what it opens when they ask where a name came from - and both of them are
  the same question, asked of the same name, so they are answered together and
  in one message.

  One name is one thing, so one is what comes back. Where a completion offers
  everything that could be written, this picks the way the language would: a
  local before what the namespace maps, a mapping before an alias, and a var
  under a scope before a member of a class spelled the same.

  Nothing is evaluated to find out, and nothing is loaded either. A name that
  resolves to nothing is answered with nothing rather than by requiring
  something to make it resolve, since a keystroke is not a thing that should
  run anybody's code."
  (:require [clojure.java.io :as io]
            [clojure.repl]
            [clojure.string :as string]
            [replique.classpath :as classpath]
            [replique.names :as names])
  (:import [java.lang.reflect Constructor Field Method Modifier]
           [java.net JarURLConnection URL]
           [java.nio.file Path]))

;;; Where a source is

(defn- at-url
  "Where URL is, as a client opens it.

  A file of a directory answers with the path of that file, and one inside a
  jar answers with the path of the jar and the entry inside it. There is no
  path to a file in an archive: opening one means opening the archive and
  reading the entry out of it, so both halves have to travel for a client to
  be able to do it - and an absent :entry is what says the :file is a file
  and can be opened as one.

  A url written against anything else is answered with nothing. Reading a
  name is not a reason to reach the network, and a http: one parses as
  happily as the rest - so what is asked is the protocol rather than what
  answers it. Nothing is opened here either way: a connection is made to be
  read apart, and reading the jar it names and the entry it names out of it
  is reading the url rather than fetching what is at the end of it."
  [^URL url]
  (condp = (.getProtocol url)
    "file" {:file (str (Path/of (.toURI url)))}
    "jar" (let [connection (.openConnection url)]
            (when (instance? JarURLConnection connection)
              (let [^JarURLConnection connection connection]
                {:file (str (Path/of (.toURI (.getJarFileURL connection))))
                 :entry (.getEntryName connection)})))
    nil))

(defn- written-url
  "Where the url NAMED already is, as a client opens it.

  A name that carries a protocol has said where it is rather than what to look
  for: file:/tmp/notes.clj is a file, and the jar:file:/...!/clojure/string.clj
  that a classpath hands back is an entry of an archive. Both are answered the
  way every other source is, so that a client opens one the way it opens the
  rest.

  Looked for last, after the classpath and the filesystem, because a path is
  what nearly every name here is and a url is what almost none of them are.

  Whether what it names is there is asked, which nothing asked of the other
  two: a name found on the classpath was found because something is at it, and
  a path is answered only where a file is at it - where a url is somebody's
  text and says where a file would be rather than that one is.

  An entry means opening the archive to ask. A jar is a file with a list
  inside it and the list is read by opening it, so the url is opened and
  nothing is read out of what that hands back: what is being asked is whether
  there was anything to hand back.

  Nothing here says that a name is no url at all. `io/as-url' throws where one
  cannot be made, and `source-of' reads a throw as the nothing it is - this
  being the last thing it asks, there is nothing after it for a throw to
  skip."
  [^String named]
  (when-let [^URL url (io/as-url named)]
    (when-let [source (at-url url)]
      (when (try
              (if (:entry source)
                (with-open [_ (.openStream url)] true)
                (.isFile (java.io.File. ^String (:file source))))
              (catch Throwable _ false))
        source))))

(defn- source-of
  "Where the file NAMED holds is, as a client opens it.

  NAMED is what the metadata of a var carries: a path read against the
  classpath, which is what loaded it, or an absolute path where something was
  loaded from a file directly. Both are looked for, in that order, since the
  first is what a name written in a namespace nearly always is. And then the
  name as a url of its own, which is what a name that is already one is.

  Nothing where the name reaches nothing, which is what a source deleted
  since the process loaded it is."
  [^String named]
  (when-not (string/blank? named)
    (try
      (or (when-let [^URL url (or (io/resource named)
                                  (let [file (java.io.File. named)]
                                    (when (.isFile file) (.toURL (.toURI file)))))]
            (at-url url))
          (written-url named))
      ;; A path is text somebody's process put in a var, and a name that no
      ;; url can be made of - or no path out of the url, which a jar: written
      ;; over a http: is - is one more way of naming nothing.
      (catch Throwable _ nil))))

(defn- source-at
  "Where the definition METADATA describes is, as a client opens it.

  The line and the column as they were recorded, which is where the form
  starts rather than where the name inside it is. Absent where the metadata
  carries none: something evaluated at a repl has a file and no line, and a
  var made by a tool may have neither."
  [{:keys [file line column]}]
  (when-let [source (source-of file)]
    (cond-> source
      (integer? line) (assoc :line line)
      (integer? column) (assoc :column column))))

(defn- loadable
  "Where the source PATH names is, PATH being written without its extension.

  Which is how a namespace and a load name one: clojure.core/load reads
  clojure/string as clojure/string.clj, and reads a .cljc where there is no
  .clj - so the two are looked for in that order, the order the language
  looks in."
  [^String path]
  (some (fn [extension] (source-of (str path extension))) [".clj" ".cljc"]))

(defn- namespace-source
  "Where the file that requiring the namespace NAMED would load is.

  Read out of the classpath rather than out of what the namespace holds,
  which is what makes it an answer for a namespace nobody has required yet -
  a name written in an ns form is exactly that. The dashes are munged out on
  the way, since a file is named the way a namespace munges and not the way
  it is written."
  [^String named]
  (loadable (-> named (.replace \- \_) (.replace \. \/))))

;;; What a name is

(defn- source-of-var
  "Where the var VAR was written, as a client opens it.

  A protocol method carries no file and no line of its own - what it carries
  is the protocol, which is the form it was written in. So that is where it
  is: (defprotocol P (a-method [this])) writes a-method inside P, and where P
  was written is the nearest thing there is to where a-method was."
  [^clojure.lang.Var var]
  (let [metadata (meta var)]
    (or (source-at metadata)
        (when-let [protocol (:protocol metadata)]
          (when (var? protocol) (source-at (meta protocol)))))))

(defn- of-var
  "What the var VAR is, as the answer carries it.

  The name it has rather than the name it was written as: what is being asked
  is what the name written there means, and clojure.core/map is the answer
  whether somebody wrote map, c/map or a name a :rename gave it.

  The arglists as they are written, one string each, since what a client does
  with them is show them to somebody. The docstring as it stands."
  [^clojure.lang.Var var]
  (let [{:keys [arglists doc]} (meta var)]
    (cond-> {:type (names/var-kind var)
             :name (str (.sym var))
             :ns (str (ns-name (.ns var)))}
      arglists (assoc :arglists (mapv pr-str arglists))
      doc (assoc :doc doc)
      true (merge (source-of-var var)))))

(defn- var-of-a-fn
  "The var the class NAMED was compiled out of, or nil when it was not
  compiled out of one.

  A function compiles to a class named after the var it was defined in and
  the dollars of whatever was written inside it - clojure.main$repl for
  clojure.main/repl, and clojure.main$repl$read_eval_print__9206 for a
  function written inside that. The first two pieces are the var, which is
  what a name like this is worth resolving for: it is the name a stack trace
  prints and the name somebody pastes into a buffer to look up."
  [^String named]
  (let [pieces (string/split named #"\$")]
    (when (> (count pieces) 1)
      (let [written (clojure.repl/demunge (string/join "/" (take 2 pieces)))]
        (when-let [found (try (resolve (symbol written)) (catch Throwable _ nil))]
          (when (var? found) found))))))

(defn- var-of-a-type
  "The var written beside the class CLASS, or nil when none was.

  A deftype and a defrecord make a class in the package their namespace
  munges to - my.app.Point for a Point of my.app - and a ->Point beside it,
  written in the same form. So the namespace is looked for under the package
  as written and under the name that munges to it, which is what a completion
  does to find these classes in the first place."
  [^Class class]
  (when-let [package (.getPackage class)]
    (let [^String named (.getName package)]
      (when-let [found (or (find-ns (symbol named))
                           (find-ns (symbol (.replace named \_ \-))))]
        (get (ns-publics found) (symbol (str "->" (.getSimpleName class))))))))

(defn- source-of-class
  "Where the class CLASS was written, or nil when nothing says.

  What is on a classpath is compiled, and the file a java class was written
  in is not there to open. A class clojure made is the exception: it was
  written in a form of a namespace, and a var written in that same form
  remembers where the form is."
  [^Class class]
  (when-let [var (or (var-of-a-fn (.getName class)) (var-of-a-type class))]
    (source-of-var var)))

(defn- of-class
  "What the class CLASS is, as the answer carries it.

  The name without the package in front of it, and the package beside it,
  which is how a completion writes a class as well. Written back together
  they are the name the runtime knows the class by: an inner class keeps the
  dollar that says which class it is inside of."
  [^Class class]
  (merge (if-let [package (.getPackage class)]
           (let [^String named (.getName class)]
             {:type "class"
              :name (subs named (inc (.length (.getName package))))
              :package (.getName package)})
           {:type "class" :name (.getName class)})
         (source-of-class class)))

(defn- of-namespace
  "What the namespace NAMED is, as the answer carries it, or nil when nothing
  of that name is one.

  A namespace the process has loaded and a namespace that is only a file on
  the classpath are both answered - the second is what a name written in an
  ns form is, and where it is is the thing worth knowing about it. The
  docstring is the loaded half: a file that has not been read has not said
  what it is for."
  [^String named]
  (let [found (find-ns (symbol named))
        doc (:doc (meta found))
        source (namespace-source named)]
    (when (or found source)
      (cond-> {:type "namespace" :name named}
        doc (assoc :doc doc)
        true (merge source)))))

(def ^:private special-doc
  "What clojure.repl says about the forms the compiler reads itself.

  Read out of a private var of clojure.repl because that is where it is
  written down. There is nowhere else: a special form is not a var and holds
  no metadata, so the only account of what `if' takes is the one the doc
  command prints.

  A clojure that moved it is answered with the names alone rather than with
  a broken process, the same way the keyword table is."
  (delay
    (try
      (into {} (for [[named {:keys [doc forms]}] @(resolve 'clojure.repl/special-doc-map)]
                 ;; The forms written with this name at the head, and the head
                 ;; dropped: (if test then else?) says what `if' is written
                 ;; with, and an arglist is what is written after the name
                 ;; rather than the whole call. The others are other names -
                 ;; what clojure.repl says about the dot is mostly what
                 ;; .instanceMember and Classname/staticField are written as.
                 (let [written (filter (fn [form] (and (seq? form) (= named (first form))))
                                       forms)]
                   [(str named)
                    (cond-> {}
                      (seq written) (assoc :arglists (mapv (comp pr-str vec rest) written))
                      doc (assoc :doc doc))])))
      (catch Throwable _ {}))))

(def ^:private written-in-a-try
  "What catch and finally take, which clojure.repl does not say.

  The doc command writes what they take into what try takes, since that is
  the only form they are written in - so there is nowhere to read them from
  and they are written down here. Which is a table, and a table drifts from
  what it describes; this one cannot. The compiler has read a catch clause
  this way since there was a compiler, and it is not a form the language can
  change now."
  {"catch" ["[classname name expr*]"]
   "finally" ["[expr*]"]})

(defn- of-special-form
  "What the special form NAMED is, as the answer carries it, or nil when
  nothing of that name is one."
  [^String named]
  (when (some #{named} names/special-forms)
    (merge {:type "special-form" :name named}
           (when-let [arglists (get written-in-a-try named)] {:arglists arglists})
           ;; last, so that a clojure which starts saying what a catch takes
           ;; is what says it
           (get @special-doc named))))

;;; The members of a class

(defn- static? [^java.lang.reflect.Member member]
  (Modifier/isStatic (.getModifiers member)))

(defn- type-named
  "What a class is called where a signature is being read.

  The name without its package, which is what makes a signature readable at
  the end of a line somebody is typing on: [CharSequence int] says what a
  call takes and java.lang.CharSequence says it twice."
  ^String [^Class class]
  (.getSimpleName class))

(defn- parameters
  "The parameter types of one overload, written as a vector is."
  ^String [classes]
  (str "[" (string/join " " (map type-named classes)) "]"))

(defn- overloads
  "The lines WRITTEN holds, fewest arguments first.

  WRITTEN is each overload as a pair of how many arguments it takes and the
  line it is written as, so that the answer reads down the way the arglists
  of a var do: fewest first, and alphabetically among the ones of an arity.
  Two overloads written the same way - which is what a type the notation does
  not show makes of them - are one line rather than two."
  [written]
  (->> written sort (map second) distinct vec))

(defn- method-arglists
  "The overloads of METHODS, each written as it would be written in clojure.

  A parameter vector with the return type tagged on it, which is the notation
  the language already has for exactly this: ^int [] is a call that takes
  nothing and gives back an int."
  [methods]
  (overloads (for [^Method method methods]
               [(.getParameterCount method)
                (str "^" (type-named (.getReturnType method)) " "
                     (parameters (.getParameterTypes method)))])))

(defn- of-method
  "The method NAMED of CLASS, static or not as STATIC says, or nil when the
  class has none of that name.

  The public methods, the inherited ones included, which is what getMethods
  answers: what can be written on a thing is what can be seen from outside
  the class it is of."
  [^Class class ^String named static]
  (let [found (for [^Method method (.getMethods class)
                    :when (and (= named (.getName method)) (= static (static? method)))]
                method)]
    (when (seq found)
      {:type "method"
       :name named
       :class (.getName class)
       :arglists (method-arglists found)})))

(defn- of-field
  "The field NAMED of CLASS, static or not as STATIC says, or nil when the
  class has none of that name.

  Its type travels as a :tag, which is the notation a client already writes
  one in: ^int is what a form declaring this field's type would say."
  [^Class class ^String named static]
  (when-let [^Field field (first (for [^Field field (.getFields class)
                                       :when (and (= named (.getName field))
                                                  (= static (static? field)))]
                                   field))]
    {:type "field"
     :name named
     :class (.getName class)
     :tag (type-named (.getType field))}))

(defn- of-constructor
  "The constructors of CLASS, or nil when it has none that can be called.

  One answer for all of them, since they are one name: what is being asked
  about is Date/new, and its arglists are the arities that name can be
  written with. No return type is tagged on them - a constructor of a class
  gives back that class, and the name has already said which."
  [^Class class]
  (let [found (seq (.getConstructors class))]
    (when found
      {:type "constructor"
       :name "new"
       :class (.getName class)
       :arglists (overloads (for [^Constructor constructor found]
                              [(.getParameterCount constructor)
                               (parameters (.getParameterTypes constructor))]))})))

(defn- of-member
  "The member of CLASS that NAMED names where it is written behind a slash.

  Which of them is in the spelling, and they are the spellings clojure has
  read since 1.12: Class/new is a constructor, Class/.name an instance
  method, and Class/name a static one. A static field before a static
  method, since a name written behind a slash and called nothing is read as
  a field where the class has one."
  [^Class class ^String named]
  (cond
    (= "new" named) (of-constructor class)
    (string/starts-with? named ".") (of-method class (subs named 1) false)
    :else (or (of-field class named true) (of-method class named true))))

;;; A name written in code

(defn- of-keyword
  "What the keyword TEXT is, as the answer carries it.

  Which is worth answering for the one written with two colons, since only
  the process can say what it is a keyword of: ::name is of the namespace the
  code is in, and ::alias/name of the namespace that alias stands for. One
  written with a single colon is read as it stands and is its own answer.

  What qualifies it rides along as the :ns, which is what a completion
  carries on a keyword as well. It is a namespace where the colons made it
  one, and whatever somebody wrote in front of the slash where they did
  not - a keyword takes any name there, and nothing has to exist for it to
  be a keyword."
  [ns ^String text]
  (let [written (if (string/starts-with? text "::") (subs text 2) (subs text 1))
        scope (names/scope-of written)
        named (if scope (subs written (inc (.length scope))) written)]
    (when-not (string/blank? named)
      (if (string/starts-with? text "::")
        (if scope
          (when-let [aliased (get (ns-aliases ns) (symbol scope))]
            {:type "keyword" :name named :ns (str (ns-name aliased))})
          {:type "keyword" :name named :ns (str (ns-name ns))})
        (cond-> {:type "keyword" :name named}
          scope (assoc :ns scope))))))

(defn- of-written-on
  "The member of what a text starting with a dot is written on.

  A method is written .name and a field .-name, and only the instance
  members are answered: a static one is written on the class. What the thing
  is comes from the :tag and the :target the client sent, which is
  `names/target-class's to read."
  [ns msg ^String written]
  (when-let [class (names/target-class ns msg)]
    (if (string/starts-with? written ".-")
      (of-field class (subs written 2) false)
      (of-method class (subs written 1) false))))

(defn- of-constructed
  "The constructor a text ending in a dot names, or nil where it names none.

  (java.util.Date. now) is how one is written where Date/new is the 1.12
  spelling, and what stands before the dot has to be a class already:
  java.util. is somebody halfway through writing a class name."
  [ns ^String written]
  (when (string/ends-with? written ".")
    (let [named (subs written 0 (dec (.length written)))]
      (when (seq named)
        (when-let [class (names/class-named ns named)]
          (of-constructor class))))))

(defn- under-a-scope
  "TEXT as what it is written under and the name written there, or nil when
  it is written under nothing.

  Whatever stands before the last slash, which is `names/scope-of's rule -
  except where the name is itself a slash. clojure.core// is the var named /
  of clojure.core, and the last slash of it is the name rather than what
  separates the name from the scope."
  [^String text]
  (if (string/ends-with? text "//")
    (let [scope (subs text 0 (- (.length text) 2))]
      (when (seq scope) [scope "/"]))
    (when-let [scope (names/scope-of text)]
      [scope (subs text (inc (.length scope)))])))

(defn- of-scoped
  "What is written under SCOPE in NS, or nil when nothing of that name is.

  A namespace or an alias of one first, and a class second, which is the
  order the compiler reads a name in: what stands before the slash is a
  namespace where the namespace has one of that name, and a class only where
  it does not."
  [ns ^String scope ^String named]
  (or (when-let [found (or (get (ns-aliases ns) (symbol scope))
                           (find-ns (symbol scope)))]
        (when-let [var (get (ns-publics found) (symbol named))]
          (of-var var)))
      (when-let [class (names/class-named ns scope)]
        (of-member class named))))

(defn- of-plain
  "What the name WRITTEN means in NS, or nil when it means nothing there.

  Looked for in the order a completion answers in, which is the order the
  language reads a name in: the locals, then what the namespace maps, then
  its aliases, then the namespaces that have been loaded, then the special
  forms. A local shadows for the same reason it shadows there - it is bound
  where the name is written, and nothing the namespace holds is what that
  name means.

  A class written out in full is last of all. It is not a mapping of the
  namespace and it is not read as one, so it is what the name means only
  where nothing else is."
  [ns msg ^String written]
  (let [named (symbol written)]
    (or (when (some #{written} (names/locals-named msg))
          {:type "local" :name written})
        (let [mapped (get (ns-map ns) named)]
          (cond (var? mapped) (of-var mapped)
                (class? mapped) (of-class mapped)))
        (when-let [aliased (get (ns-aliases ns) named)]
          (of-namespace (str (ns-name aliased))))
        (when (find-ns named) (of-namespace written))
        (of-special-form written)
        (when-let [class (names/class-named ns written)]
          (of-class class)))))

;;; The positions

(defmulti ^:private resolved
  "What the name MSG carries is, at the position MSG says it is written at."
  :position)

(defmethod resolved :default [msg]
  (throw (names/invalid (if (nil? (:position msg))
                          "A symbol needs the :position it is written at"
                          (str "Unknown position: " (pr-str (:position msg)))))))

(defmethod resolved :code [msg]
  (let [ns (names/namespace-named msg)
        written (names/text msg)]
    (cond
      (string/blank? written) nil
      ;; A keyword before anything else, since ::alias/name is written with a
      ;; slash as well as with colons, and it is the colons that say what the
      ;; slash means there
      (string/starts-with? written ":") (of-keyword ns written)
      ;; then a member, since a name written on a thing is read against that
      ;; thing rather than against the namespace
      (string/starts-with? written ".") (of-written-on ns msg written)
      :else (or (of-constructed ns written)
                (when-let [[scope named] (under-a-scope written)]
                  (of-scoped ns scope named))
                (of-plain ns msg written)))))

(defmethod resolved :namespace [msg]
  (let [prefix (names/named-argument msg :prefix)
        written (names/text msg)]
    (when-not (string/blank? written)
      ;; Under a prefix list the prefix is written once and the name under it
      ;; is a piece of a namespace, so the two make the name between them.
      (of-namespace (if (string/blank? prefix) written (str prefix "." written))))))

;; What a :require-macros names is a namespace of this world - see
;; `completion/groups'.
(defmethod resolved :namespace-macros [msg] (resolved (assoc msg :position :namespace)))

(defmethod resolved :var [msg]
  (let [named (:namespace msg)
        found (if (= :refer-clojure named)
                ;; a refer-clojure names no namespace anywhere in itself, and
                ;; the one it refers from is the one every namespace refers
                (find-ns 'clojure.core)
                (find-ns (symbol (names/required-argument msg :namespace))))
        written (names/text msg)]
    (when (and found (not (string/blank? written)))
      (when-let [var (get (ns-publics found) (symbol written))]
        (of-var var)))))

(defmethod resolved :package-or-class [msg]
  (let [written (names/text msg)]
    (when-not (string/blank? written)
      (or (when-let [class (names/class-named (names/namespace-named msg) written)]
            (of-class class))
          ;; A package is a name no file carries: it is where the classes are
          ;; and is not one itself, so what is said about it is that it is
          ;; one.
          (when (some #{written} (:packages (classpath/scan)))
            {:type "package" :name written})))))

(defmethod resolved :class [msg]
  (let [package (names/required-argument msg :package)
        written (names/text msg)]
    (when-not (string/blank? written)
      (when-let [class (names/class-named (names/namespace-named msg)
                                          (str package "." written))]
        (of-class class)))))

(defmethod resolved :load-path [msg]
  (let [written (names/text msg)
        namespace (names/named-argument msg :ns)]
    (when-not (string/blank? written)
      ;; A path that starts with a slash is read from the root of the
      ;; classpath and one that does not is read from the namespace it is
      ;; written in, which is the rule `clojure.core/load' reads one by.
      (let [path (cond
                   (string/starts-with? written "/") (subs written 1)
                   (string/blank? namespace) written
                   :else (str (-> namespace (.replace \- \_) (.replace \. \/))
                              "/" written))]
        (when-let [source (loadable path)]
          (assoc source :type "path" :name path))))))

;; A string written in code. What is being asked is where the thing it names
;; is, which is a question worth asking of any of them: a path on the
;; classpath, a path on disk, a url. The call it is written in is not read
;; here, where a completion reads it - what could be written there depends on
;; what the call takes, and what is written there already is a name that
;; either reaches something or does not.
(defmethod resolved :string [msg]
  (let [written (names/text msg)]
    (when-not (string/blank? written)
      (when-let [source (source-of written)]
        (assoc source :type "path" :name written)))))

;; The keyword positions of a dependency form. What is written at one of them
;; is a keyword the form gives a meaning to - :require says what follows it is
;; a libspec, :as what follows it is an alias - and there is nothing to say
;; about one beyond the form it is written in, which the client already knows
;; because it read the position out of that form.
(defmethod resolved :dependency-type [_] nil)
(defmethod resolved :libspec-option [_] nil)
(defmethod resolved :libspec-option-refer [_] nil)
(defmethod resolved :flag [_] nil)

;;; The answer

(defn named
  "What the name MSG carries is, as the reply frame carries it.

  An absent :symbol is a name that means nothing here, which is an ordinary
  answer rather than a failure: half of what somebody writes is a name they
  have not finished writing."
  [msg]
  {:symbol (resolved msg)})
