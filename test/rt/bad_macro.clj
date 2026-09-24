(ns rt.bad-macro
  "A macro namespace that fails the way a real one does: at expansion time, with
  a message that names nothing.

  THE FIXTURE IS THE BUG ITSELF. sci.impl.copy-vars looks the ClojureScript
  analyzer up once, as it is loaded, with `resolve' - which answers nil for a
  namespace that is not loaded YET rather than loading it - and interns a var
  holding that nil. Nothing has gone wrong yet. What goes wrong is a call, much
  later, from inside a macro the ClojureScript compiler is expanding, and
  clojure compiles a call to a var as a call to its root:

    Cannot invoke \"clojure.lang.IFn.invoke(Object, Object)\" because the
    return value of \"clojure.lang.Var.getRawRoot()\" is null

  That sentence is the whole of what a repl used to say about it. It names
  neither this namespace, nor the namespace being compiled, nor the compiler -
  which is why `replique.cljs-repl/failed-here' carries the frames, and why the
  fixture for it is a var holding nil and not a `throw'. A thrown ex-info would
  make a tidier test and would be a test of a message that says where it came
  from, which is the case that never needed the trace.")

(def ^:private missing
  "What `resolve' answers for a namespace nothing has loaded: nil,
  interned here, called from the macro below."
  (resolve 'no.such.namespace/f))

(defmacro boom
  "Expand by calling what was not there."
  []
  (missing 1 2))
