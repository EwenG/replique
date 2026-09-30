(ns replique.cljs-lint-test
  "What is wrong with a ClojureScript file, as its compiler saw it.

  `replique.lint-test' asked of the other compiler, with :dialect :cljs. ONE
  PROCESS FOR THE WHOLE FILE, for `replique.cljs-analysis-test's reason: the
  first ClojureScript question compiles cljs.core, which is seconds. Every test
  works in a source root of its own.

    clojure -M:test:cljs ..."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [replique.classpath :as classpath]
            [replique.cljs :as cljs]
            [replique.core :as core]
            [replique.ops]
            [replique.state :as state]
            [replique.test-client :as client
             :refer [control-client disconnect eval! repl-client request!]]))

(def ^:private the-process (atom nil))

(def ^:private slow 180000)

(defn- with-one-process [f]
  (let [dir (client/temp-dir)
        out *out*
        err *err*]
    (reset! the-process (core/start! {:directory dir :init false}))
    (try
      (binding [*out* out *err* err] (f))
      (finally (core/stop!) (reset! the-process nil)
               (client/delete-recursively dir)))))

(use-fixtures :once with-one-process)

(defn- written-file! [dir name source]
  (let [f (java.io.File. (str dir) (str name))]
    (.mkdirs (.getParentFile f))
    (spit f source)
    (.getPath f)))

(defn- edited-file! [path source]
  (spit path source)
  (.setLastModified (java.io.File. ^String path) (+ 30000 (System/currentTimeMillis)))
  path)

(defn- source-root! []
  (let [dir (client/temp-dir)]
    (.addURL state/class-loader (.toURL (.toURI (java.io.File. (str dir)))))
    (state/adopt-class-loader!)
    (classpath/rescan!)
    dir))

(defn- lints! [path]
  (let [c (control-client @the-process)]
    (try (request! c {:op :lints :id 1 :file path :dialect :cljs :target :node})
         (finally (disconnect c)))))

(defmacro ^:private with-cljs-repl [sym & body]
  `(let [~sym (repl-client @the-process {:dialect :cljs :target :node} slow)]
     (try ~@body (finally (disconnect ~sym)))))

(defn- said [answer]
  (mapv (juxt :type :line :column :message) (:lints answer)))

(deftest what-clj-kondo-would-say-the-clojurescript-compiler-says
  (when (cljs/available?)
    (let [root (source-root!)
          out-dir (binding [cljs/*target* :node] (:out-dir (cljs/environment)))]
      (written-file! out-dir "npm/lintmod.js"
                     "export const $module = { readText(x) { return x; }, other: 1 };\n")
      (written-file! root "lx/util.cljs"
                     (str "(ns lx.util)\n"
                          "(defn two [a b] a)\n"
                          "(defn ^:deprecated old [] 1)\n"))
      (let [core (written-file!
                  root "lx/core.cljs"
                  (str "(ns lx.core (:require [lx.util :as u :refer [two]] [goog.string :as gstr :refer [trim]] [clojure.string :as s]))\n"
                       "(defprotocol P (m [this a]))\n"
                       "(defrecord R [a] P (m [this b] 1))\n"
                       "(defn- rec [n] (rec n))\n"
                       "(defn g [] (two 1) (map) (get {}) (u/two 1 2 3) (u/old) (Date/now))\n"
                       "(defn h [x] (-> x (u/two 2)))\n"))
            mod (written-file! root "lx/mod.cljs"
                               (str "(ns lx.mod (:require [\"lintmod\" :as lm :refer [readText other]]))\n"
                                    "(defn f [] (readText 1))\n"))]
        (with-cljs-repl r
          (eval! r "(require 'lx.core 'lx.mod)")
          (is (= [["unused-namespace" 1 53 "namespace goog.string is required but never used"]
                  ["unused-referred-var" 1 82 "#'goog.string/trim is referred but never used"]
                  ["unused-namespace" 1 90 "namespace clojure.string is required but never used"]
                  ["unused-binding" 3 24 "unused binding this"]
                  ["unused-binding" 3 29 "unused binding b"]
                  ["unused-private-var" 4 8 "Unused private var lx.core/rec"]
                  ["invalid-arity" 5 12 "lx.util/two is called with 1 arg but expects 2"]
                  ["invalid-arity" 5 20 "cljs.core/map is called with 0 args but expects 1, 2, 3, 4 or more"]
                  ["invalid-arity" 5 26 "cljs.core/get is called with 1 arg but expects 2 or 3"]
                  ["invalid-arity" 5 35 "lx.util/two is called with 3 args but expects 2"]
                  ["deprecated-var" 5 50 "#'lx.util/old is deprecated"]
                  ["undeclared-ns" 5 58 "No such namespace: Date"]]
                 (said (lints! core))))
          (testing "a JavaScript module: what the source never writes"
            (is (= [["unused-referred-var" 1 57 "#'lintmod/other is referred but never used"]]
                   (said (lints! mod)))))
          (testing "a var removed by a reload is a use of nothing"
            (edited-file! (str root "/lx/util.cljs") "(ns lx.util)\n(defn two [a b] a)\n")
            (eval! r "#replique/reload {}")
            (is (some #{["unresolved-var" 5 50 "Unresolved var: u/old"]}
                      (said (lints! core))))))))))

(deftest a-variadic-function-of-one-arity-is-called-with-its-arity
  (testing "a single-arity variadic defn leaves its signature in :method-params as a
  seq rather than a vector, and a call of it is judged all the same"
    (when (cljs/available?)
      (let [root (source-root!)]
        (written-file! root "va/util.cljs"
                       (str "(ns va.util)\n"
                            "(defn vary [a & more] a)\n"))
        (let [core (written-file! root "va/core.cljs"
                                  (str "(ns va.core (:require [va.util :as u]))\n"
                                       "(defn g [] (u/vary) (u/vary 1) (u/vary 1 2 3))\n"))]
          (with-cljs-repl r
            (eval! r "(require 'va.core)")
            (is (= [["invalid-arity" 2 12 "va.util/vary is called with 0 args but expects 1 or more"]]
                   (said (lints! core))))))))))

(deftest the-name-a-component-macro-gives-its-fn-is-not-a-binding
  (testing "hx's defnc writes (def C (fn C [props] ...)) out of the one symbol the
  source wrote: the fn's name is there so that it can call itself, and neither
  clj-kondo nor the Clojure compiler's model calls it unused"
    (when (cljs/available?)
      (let [root (source-root!)]
        (written-file! root "comp/macros.clj"
                       (str "(ns comp.macros)\n"
                            "(defmacro defc [n args & body]\n"
                            "  `(def ~(vary-meta n assoc :doc \"c\") (fn ~n ~args ~@body)))\n"))
        (let [core (written-file! root "comp/core.cljs"
                                  (str "(ns comp.core (:require-macros [comp.macros :refer [defc]]))\n"
                                       "(defc Button [props] 1)\n"
                                       "(def f (fn step [x] x))\n"))]
          (with-cljs-repl r
            (eval! r "(require 'comp.core)")
            (is (= [["unused-binding" 2 15 "unused binding props"]]
                   (said (lints! core))))))))))
