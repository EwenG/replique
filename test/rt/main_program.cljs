;; A ClojureScript namespace with nothing in it but something to find: the
;; fixture for :main, which needs a namespace on the source paths rather than a
;; file loaded by path. It is here and not under test/replique because the
;; source paths a compile sees are the classpath directories, and what is being
;; tested is that a handshake can name a namespace and have it be there.
(ns rt.main-program)

(def program-was-loaded :yes)

(defn twice [x] (* 2 x))
