;; A ClojureScript namespace with a side effect on the JavaScript side, which is
;; what makes it a different fixture from rt/main_program.cljs rather than a
;; second copy of it.
;;
;; NOTHING ELSE CAN TELL WHETHER :main LOADED. Referring to a namespace is
;; enough to make the runtime fetch it - a form mentioning rt.main-program
;; answers whether or not anything required it first - so a test that asks the
;; namespace about itself cannot distinguish "loaded by :main" from "loaded by
;; the question". The marker below is on globalThis and not in this namespace,
;; so the form that reads it mentions no ClojureScript at all and cannot be what
;; put it there.
(ns rt.loaded-program)

(set! (.-__rt_loaded_program js/globalThis) true)

(def marker :yes)
