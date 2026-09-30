(ns replique.hooks
  "What to run after an evaluation replaced some of the program.

  A PROGRAM WHOSE TOP LEVEL BUILT SOMETHING has to be told when the code under it
  was replaced, and nothing in a repl protocol is going to know that on its
  behalf. The one this was written for re-renders a React tree:

    (swap! replique.hooks/cljs-hooks assoc 'my-app
           (fn [_] (replique.cljs/eval-form '(my-app.dev/-render))))

  in a `.replique/init.clj', which is where things like this live - see
  `replique.core's init scripts.

  WHAT IS DIFFERENT HERE FROM EVERY OTHER VERSION OF THIS IDEA is where the
  answer comes from. Replique master watched the root of every var of every
  namespace and compared what each namespace mapped before and after each
  evaluation; the first replique-2 fired after a `#replique/load' directive and
  named the namespace by re-reading the file's own `ns' form. Both are guesses
  from outside the compiler, and both are wrong in ways you notice:

    a reload of forty files fired nothing at all, because a reload names files
    and the guess was keyed to one namespace;

    a ClojureScript file recompiled to the same definitions fired nothing,
    because the definitions compared equal - and every function object in the
    module is a new one, so the page did need redrawing;

    `alter-var-root' fired, because a root changed, although no code was
    replaced.

  The compiler knows exactly, and knows it for free: it is already telling a sink
  about every `def' it analyses. So that is what this reads -
  `clojure.analysis/*defined*', which both compilers report through - and the
  question it answers is the one worth asking, which is not `did this namespace's
  value change' but `was this namespace's code replaced'.

  A HOOK FIRES ONCE PER EVALUATION, however much that evaluation turned out to
  do. A reload of forty namespaces is one redraw and not forty, and that is the
  whole reason the collecting is round the evaluation rather than round each file.

  AND ONLY WHERE THE EVALUATION WORKED. What a hook is called after is the
  program having changed; a file that would not compile did not change it."
  (:require [replique.cljs :as cljs]
            [replique.lint :as lint]))

;;; Where a hook is put

(def clj-hooks
  "What to run after a Clojure evaluation defined something, by the namespaces it
  covers: {prefix-symbol (fn [event] ...)}.

  THE KEY IS A PREFIX OF A NAMESPACE NAME, matched with `startsWith' on the name
  as written, so `my-app' catches my-app.views.main and every other one. It is a
  prefix rather than a namespace because what somebody wants a hook for is a
  PROJECT, and a project is a couple of hundred namespaces with a common first
  segment. It is not anchored on a dot, which is master's rule kept rather than
  improved: `my' would catch my-app too, and a key nobody would write is not
  worth a rule."
  (atom {}))

(def cljs-hooks
  "`clj-hooks', for what a ClojureScript evaluation defined.

  TWO REGISTRIES AND NOT ONE, because a namespace name says nothing about which
  compiler it belongs to: my-app.views is a Clojure namespace of macros AND a
  ClojureScript namespace of code, under one name, and a hook that redraws a page
  must not fire because the macros were reloaded on the jvm. Which of the two an
  event belongs to is the compiler's own answer - see `clojure.analysis/*defined*'
  - so one reload that does both fires from both registries, each with what is
  its own."
  (atom {}))

;;; What a hook is handed

(def ^:private dialect-registry
  "Which registry an event of each dialect fires. An event of a dialect nothing
  here names fires nothing, which is how an event shape this does not know yet
  arrives: ignored rather than sent to whichever registry was nearest."
  {:clj clj-hooks :cljs cljs-hooks})

(defn- summary
  "EVENTS, all of one dialect, as the one map a hook is handed:

    :dialect     :clj or :cljs
    :target      the target it happened on - ClojureScript only
    :namespaces  every namespace this evaluation touched, in the order it did
    :vars        the vars it defined, where a var was the unit
    :removed     the vars it took away

  THE UNIT IS WHATEVER WAS ACTUALLY REPLACED, which is why :namespaces is not
  simply the namespaces of :vars. A ClojureScript file that is compiled has its
  module rewritten whole - every function in it is a new object - and the
  compiler reports the namespace, with no var to name. A `def' typed at a prompt
  replaces one var and the compiler reports that var. A Clojure file is the
  second case many times over, because a def is all its compiler ever reports.

  So :namespaces is what a hook that redraws reads, and it is complete; :vars is
  there for a hook that wants to be cleverer than that, and is not."
  [dialect events]
  (let [of (fn [pred k] (into [] (comp (filter pred) (map k) (distinct)) events))]
    (cond-> {:dialect    dialect
             :namespaces (of (constantly true) :ns)
             :vars       (of #(and (= :define (:op %)) (:var %)) :var)
             :removed    (of #(= :remove (:op %)) :var)}
      (= :cljs dialect) (assoc :target cljs/*target*))))

(defn- fire!
  "Call every hook of REGISTRY that covers one of NAMESPACES, once each, with
  EVENT. Answers how many ran.

  A HOOK THAT THROWS DOES NOT TAKE THE EVALUATION WITH IT. What it was called
  after happened - the code was replaced - so turning its failure into the form's
  failure would report the wrong thing about the wrong thing. It goes to `*err*',
  which at a repl is that repl's `err' frames, and the form's own result follows
  it."
  [registry namespaces event]
  (let [hooks (filter (fn [[k _]]
                        (let [p (str k)]
                          (some (fn [n] (.startsWith (str n) p)) namespaces)))
                      @registry)]
    (doseq [[k f] hooks]
      (try (f event)
           (catch Throwable t
             (binding [*out* *err*]
               (println (str "The " k " hook threw: "
                             (or (ex-message t) (.getName (class t)))))))))
    (count hooks)))

;;; Collecting an evaluation

(defn- listening?
  "Whether anything is registered at all. Nothing is collected where nothing is,
  so a process with no hooks in it pays for none of this - which is every process
  that has not been told to do something after a reload."
  []
  (or (seq @clj-hooks) (seq @cljs-hooks)))

(defn- fired!
  "Fire the hooks EVENTS matched, one event per dialect. Answers nil."
  [events]
  (doseq [[dialect es] (group-by :dialect events)
          :let [registry (get dialect-registry dialect)]
          :when registry
          :let [event (summary dialect es)]]
    (fire! registry (:namespaces event) event))
  nil)

(defn removed!
  "Fire the hooks for a var this process has just taken away, outside any
  evaluation.

  WHAT THE `:remove-var' OP DOES, which is an op and not a form: replique unmaps
  the var itself, wherever it is mapped, rather than asking a compiler to - so
  there is no compile for `around*' to collect and nothing would be said about
  the one command whose whole purpose is to take a definition away. The op is
  also the way a person actually does this; `(remove-var ...)' typed into a
  ClojureScript repl is the compiler's own special and reports itself.

  DIALECT is the op's, since a var of that name can exist in both worlds."
  [dialect qsym]
  ;; a use of it anywhere is now a use of nothing
  (lint/changed!)
  (when (listening?)
    (fired! [{:dialect dialect :op :remove
              :ns (symbol (namespace qsym)) :var qsym}]))
  nil)

(defn around*
  "Call F with what it defines collected, and fire the hooks it matched.

  WORKED? is asked of F's value and says whether the evaluation succeeded. The two
  repls answer it differently and both are right: a Clojure evaluation that failed
  threw, and never reaches here at all, so its answer is yes to everything that
  returns; a ClojureScript one comes back as a map saying so, because the failure
  happened in another process and had to be carried over a wire to be reported.

  INSIDE whatever the repl holds while it evaluates, so that a hook may evaluate:
  what it prints comes out beside what the form printed, and the target's output
  is still this connection's."
  ([f] (around* (constantly true) f))
  ([worked? f]
   ;; AND THE CLIENTS SHOWING LINTS ARE TOLD, whether or not anything listens
   ;; here: what they show is read from the model this evaluation may have just
   ;; rewritten - see `replique.lint/watching*'. Which collects what F defines
   ;; itself, and hands it on: a second `replique.analysis/defining*' under it would be a
   ;; binding hiding what F defines from it.
   (if-not (listening?)
     (lint/watching* f)
     (let [seen (volatile! [])
           r    (lint/watching* #(vswap! seen conj %) f)]
       (when (worked? r) (fired! @seen))
       r))))
