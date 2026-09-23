;; A ClojureScript namespace whose only point is that it names an npm package:
;; a string require is what makes clojure.cljs.npm run at all, and the :npm
;; option is only visible in what it decides. Beside rt/main_program.cljs and
;; for the same reason - a compile looks for namespaces on the classpath
;; directories, so a fixture has to be one.
(ns rt.npm-program
  (:require ["react" :as react]))

(def uses-a-package react)
