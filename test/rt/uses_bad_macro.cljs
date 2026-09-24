;; A ClojureScript namespace that will not compile, because the macro it uses
;; throws while it is being expanded. The fixture for what a `:main' that fails
;; inside somebody else's code reports - see rt.bad-macro.
(ns rt.uses-bad-macro
  (:require-macros [rt.bad-macro :refer [boom]]))

(def never (boom))
