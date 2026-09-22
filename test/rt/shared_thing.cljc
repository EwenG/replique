(ns rt.shared-thing)

;; A fixture, and the point of it is the extension: one file that provides a
;; namespace to each of the two worlds ClojureScript compiles with. Nothing
;; here requires it - what is being tested is that the classpath is read two
;; ways, and a namespace nobody loaded is exactly the case that shows it.

(def written-once :in-both-worlds)
