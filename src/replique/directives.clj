(ns replique.directives
  "What a client says to a repl that is not code.

  A repl connection carries plain source, and there are things an editor has to
  say about that source which are not part of it: which namespace to read it
  in, where in a buffer it came from, that a whole file is to be loaded rather
  than this one form. They travel in band, as tagged literals, right before the
  form they are about:

    #replique/src    where the next form comes from
    #replique/ns     the namespace to read in, from here on
    #replique/load   load a file rather than evaluate a form
    #replique/reload load everything that changed

  IN BAND rather than as ops on the control connection, and rather than as a
  process wide atom another connection could set: a directive cannot race with
  another repl, and it stays in order with the code it describes.

  READ HERE and acted on by whichever repl read them - what `#replique/ns'
  means is the same question in both dialects and the answer is not: one moves
  *ns*, the other moves the ClojureScript compiler's cursor. So this namespace
  ends at the record each of them reads, and holds nothing that does either.")

;;; Where the code comes from

;; A repl reads code from a socket, so the line numbers and the file the
;; compiler records are those of the socket - which means nothing to an
;; editor. A client that sends a form taken from a buffer says where it comes
;; from, in band, right before the form:
;;
;;   #replique/src {:file "/home/me/src/foo.clj" :line 42}
;;   (defn foo [] ...)
;;
;; The directive applies to the next form only. In band rather than a process
;; wide atom set by another connection: it cannot race with another repl, and
;; it stays in order with the code it describes.
(defrecord SourceDirective [file line])

(defn source-directive [m]
  (let [bad (fn [message]
              (throw (ex-info message {:replique/error :invalid-source-directive})))]
    (cond
      (not (map? m))
      (bad (str "#replique/src takes a map, got: " (pr-str m)))

      ;; Checked here rather than left to fail later: *file* and the file name
      ;; the compiler records are both strings, and a client sending anything
      ;; else would otherwise get a ClassCastException pointing inside
      ;; replique instead of at the message it sent.
      (not (or (nil? (:file m)) (string? (:file m))))
      (bad (str "#replique/src :file must be a string, got: " (pr-str (:file m))))

      ;; The range a line number really has, rather than integer? alone. It
      ;; ends up in LineNumberingPushbackReader.setLineNumber, which takes an
      ;; int, so a client that counted lines into a long gets an integer
      ;; overflow thrown from inside clojure - which is the cast error pointing
      ;; at replique's own code that this check is here to replace, and it
      ;; arrives as an execution failure rather than the read failure it is.
      ;; The low end is the same rule read the other way: an editor counts
      ;; lines from 1, and a 0 or a negative one would be written into the
      ;; metadata of the var it names as if it were a place in a file.
      (not (or (nil? (:line m))
               (and (integer? (:line m)) (<= 1 (:line m) Integer/MAX_VALUE))))
      (bad (str "#replique/src :line must be an integer between 1 and "
                Integer/MAX_VALUE ", got: " (pr-str (:line m))))

      :else (->SourceDirective (:file m) (:line m)))))

;;; The namespace

;; Code taken from a buffer belongs to the namespace that buffer is in, and
;; the repl is wherever it was left. A client that sends a form says which
;; namespace to read it in, in band, the way it says where it came from:
;;
;;   #replique/ns foo.bar
;;   (defn foo [] ...)
;;
;; Unlike #replique/src this is not about the next form only. It is in-ns
;; without the evaluation: the repl stays there, which is what makes switching
;; to the repl after evaluating something land at a prompt of the namespace
;; that was being worked in. Without the evaluation because an in-ns sent as a
;; form is a form - it has a result, and a prompt after it, and both appear in
;; the transcript as something the developer did not write.
;;
;; It is also how a client moves the repl on its own, with no form after it:
;;
;;   #replique/ns foo.bar
;;   <blank line>
;;
;; and that blank line is answered with a prompt, since nothing else would
;; say where the repl now is.
(defrecord NsDirective [ns])

(defn ns-directive [sym]
  (if (simple-symbol? sym)
    (->NsDirective sym)
    ;; A namespace name is a symbol with no namespace of its own. Qualified
    ;; ones are the mistake worth naming: #replique/ns foo/bar is what comes
    ;; out of a client that took the symbol at point rather than the namespace
    ;; around it
    (throw (ex-info (str "#replique/ns takes an unqualified symbol naming a "
                         "namespace, got: " (pr-str sym))
                    {:replique/error :invalid-ns-directive}))))

;;; Loading

;; Evaluating a buffer form by form is not the same as loading the file those
;; forms are in.  A file is loaded as one unit, its ns form first and its
;; definitions in the order they are written, which is how the compiler will
;; see it and how the application will see it - and that is what somebody
;; means by "load this".  So a client asks for it in band, the way it asks for
;; everything else the repl is to do rather than evaluate:
;;
;;   #replique/load {:file "/home/me/src/foo.clj"}
;;
;; Asked here rather than as an op on the control connection, where the rest
;; of what an editor asks for goes.  What loading a file produces is the
;; developer's own output - the compiler's reflection warnings, a "WARNING:
;; foo already refers to" - and it belongs in the repl they asked from, in
;; order with the result, rather than broadcast to every control connection as
;; something the application happened to print.  Asked here it is also this
;; repl's exception when it throws, with the phase clojure.main triages and a
;; trace pointing into the file; and it is interruptible, because it goes
;; through the same eval step as any other form.
;;
;; A namespace read out of a dependency is not a file: it is an entry inside a
;; jar, and jumping into one and loading what is there is most of the point of
;; being able to jump into one.  So the entry travels beside the jar:
;;
;;   #replique/load {:file "/home/me/.m2/.../clojure-1.12.5.jar"
;;                   :entry "clojure/string.clj"}
;;
;; Which is how a file inside a jar is already written in this protocol - it
;; is what the :symbol op answers where a definition was written, and that
;; answer is exactly what an editor holds when somebody asks to load what they
;; jumped into.  A url spelling the two of them together would be a second way
;; to say the same thing, and one with escaping in it: a jar under a directory
;; with a space in its name is a %20 in a url and is not in a path.
(defrecord LoadDirective [file entry])

(defn load-directive [m]
  (let [bad (fn [message]
              (throw (ex-info message {:replique/error :invalid-load-directive})))]
    ;; Before anything asks what it holds, and not as a branch of the cond
    ;; below: contains? does not answer about a number, it throws about one,
    ;; and the reader failure a client would then see is about the type of a
    ;; hash map rather than about the directive it wrote.
    (when-not (map? m)
      (bad (str "#replique/load takes a map, got: " (pr-str m))))
    (cond
      (not (string? (:file m)))
      (bad (str "#replique/load takes the :file to load, as a string, got: "
                (pr-str (:file m))))

      ;; Absent rather than nil is not distinguished: a client building the
      ;; map out of what :symbol answered has an entry or has nothing, and
      ;; both of those are what nothing means here.
      (not (or (nil? (:entry m)) (string? (:entry m))))
      (bad (str "The :entry of a #replique/load must be a string, got: "
                (pr-str (:entry m))))

      :else (->LoadDirective (:file m) (:entry m)))))

;;; Reloading

;; The other thing an editor asks the repl to load, and the one it cannot
;; name: everything that changed since the process read it.
;;
;;   #replique/reload {}
;;
;; Which is a question about the whole codebase and is still asked here
;; rather than as an op, for everything a load is asked here for.  It
;; compiles files, so what it produces is the compiler's warnings and the
;; code's own output, and both belong in the repl that asked; it throws where
;; a file will not compile, and that is this repl's exception, triaged, with
;; a trace into the file; and it can take a while, so it has to be
;; interruptible, which a form evaluated by the repl is and an op is not.
;;
;; A map with nothing in it, rather than nothing at all, because a tagged
;; literal reads the form after it whatever that form is - and what is asked
;; for here has somewhere to be written down the day there is something to
;; write.  What goes in it today is nothing, and a client that put something
;; there is a client asking for something this does not do, so it is told.
(defrecord ReloadDirective [])

(defn reload-directive [m]
  (let [bad (fn [message]
              (throw (ex-info message {:replique/error :invalid-reload-directive})))]
    (when-not (map? m)
      (bad (str "#replique/reload takes a map, got: " (pr-str m))))
    (when (seq m)
      (bad (str "#replique/reload takes nothing in its map yet, got: " (pr-str m))))
    (->ReloadDirective)))

(def data-readers
  "The tags a repl reads on top of whatever the code it is reading uses.

  A map the caller merges into its own reader's table rather than a binding
  installed here: the two dialects read with two different readers - clojure's
  *data-readers* and clojure.cljs.reader's *host-data-readers* - and what they
  have in common is which tags mean what, which is this."
  {'replique/src    #'source-directive
   'replique/ns     #'ns-directive
   'replique/load   #'load-directive
   'replique/reload #'reload-directive})
