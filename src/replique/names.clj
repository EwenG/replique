(ns replique.names
  "What a client wrote, and what it names.

  Two ops read a name out of somebody's buffer. `:completions' is asked what
  could be written where one is being written, and `:symbol' what the one
  written there is - and both of them are handed the same thing: a position, a
  text, and the namespace, the locals and the type the client read around it.
  Reading those keys is one job rather than two, and so is resolving what they
  name.

  Nothing here evaluates anything. What an expression would return is not
  knowable without running it, and running somebody's code is what a keystroke
  must not do."
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [replique.protocol :as protocol]))

;;; What the client wrote

(defn invalid
  "The exception a message written wrongly is refused with."
  [message]
  (ex-info message {:replique/error :invalid-message}))

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
  evaluated yet, which is every file until it is loaded - is answered as
  clojure.core itself. What a namespace refers before it refers anything is
  clojure.core, so the names of core mean there what they mean here, and half
  an answer beats none.

  The :spellings op reads the key the same way and means the same thing by
  it, so what a namespace a client named is, is one rule rather than three."
  [msg]
  (or (when-let [written (named-argument msg :ns)]
        (find-ns (symbol written)))
      (find-ns 'clojure.core)))

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

(def special-forms
  "The forms the compiler reads itself, as they are written.

  The starred ones are left out: let* and fn* and loop* are written let and
  fn and loop, which are macros and are answered as the vars they are. So are
  the dot and the ampersand - one is written on the thing it is called on and
  the other where a parameter vector says the rest of the arguments go, and
  neither is a name written on its own.

  nil, true and false are not here either. They are shorter than asking for
  them would be."
  ["catch" "def" "do" "finally" "if" "monitor-enter" "monitor-exit" "new"
   "quote" "recur" "set!" "throw" "try" "var"])

(defn var-kind
  "What VAR is, as a client annotates it with. A macro before a function
  because a macro has arglists too."
  [var]
  (let [{:keys [arglists macro]} (meta var)]
    (cond macro "macro" arglists "function" :else "var")))

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
  are."
  ^Class [ns ^String scope]
  (or (let [mapped (get (ns-map ns) (symbol scope))]
        (when (class? mapped) mapped))
      (try (Class/forName scope false (clojure.lang.RT/baseLoader))
           (catch Throwable _ nil))))

(defn target-class
  "The class a member is being written on, or nil where nothing says what it
  is.

  The tag first, which is a ^String the client read out of the text - off the
  local the target names, or off the target where it was written at the call
  site. Then the target as it is written: a var declares its class with a
  :tag of its own, and a literal is its own class.

  Nothing is evaluated to find out. What an expression would return is not
  knowable without running it, and running somebody's code is what a
  keystroke must not do - so (.getT (make-thing)) is answered with nothing
  rather than by making one. The target is read rather than evaluated for the
  same reason, and read as edn: what a client sent is text out of somebody's
  buffer, and #= in it is a form the reader would run."
  ^Class [ns msg]
  (or (when-let [tag (named-argument msg :tag)]
        (class-named ns tag))
      (when-let [target (named-argument msg :target)]
        (let [value (try (edn/read-string target) (catch Throwable _ nil))]
          (cond
            (symbol? value)
            (let [found (try (ns-resolve ns value) (catch Throwable _ nil))
                  tag (when (var? found) (:tag (meta found)))]
              (cond (class? tag) tag
                    (symbol? tag) (class-named ns (str tag))))
            (some? value) (class value))))))
