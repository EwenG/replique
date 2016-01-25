(ns ewen.replique.cljs-env.macros
  (:refer-clojure :exclude [load-file])
  (:require [cljs.env :as cljs-env]
            [cljs.closure :as closure]
            [cljs.util :as util]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.repl]
            [ewen.replique.server-cljs :refer [compiler-env repl-env
                                               repl-cljs-on-disk
                                               repl-compile-cljs
                                               refresh-cljs-deps
                                               f->src]])
  (:import [java.io File]))

(defn repl-eval-compiled [compiled repl-env f opts]
  (let [src (f->src f)]
    (cljs.repl/-evaluate
     repl-env "<cljs repl>" 1
     (slurp (str (util/output-directory opts)
                 File/separator "cljs_deps.js")))
    (cljs.repl/-evaluate
     repl-env f 1 (closure/add-dep-string opts compiled))
    (cljs.repl/-evaluate
     repl-env f 1
     (closure/src-file->goog-require
      src {:wrap true :reload true :macros-ns (:macros-ns compiled)}))))

(defn load-file*
  ([file-path]
   (load-file* :cljs file-path))
  ([type file-path]
   (cond (= :cljs type)
         (cljs-env/with-compiler-env compiler-env
           (let [opts (:options @compiler-env)
                 compiled (repl-compile-cljs file-path opts)]
             (repl-cljs-on-disk
              compiled (#'cljs.repl/env->opts repl-env) opts)
             (->> (refresh-cljs-deps opts)
                  (closure/output-deps-file
                   (assoc opts :output-to
                          (str (util/output-directory opts)
                               File/separator "cljs_deps.js"))))
             (:value (repl-eval-compiled
                      compiled repl-env file-path opts))))
         (= :clj type)
         (str (clojure.core/load-file file-path))
         :else
         ;; TODO print a warning or error message
         nil)))

(defmacro load-file [& args]
  (apply load-file* args))

;; It seems that calling this macro "in-ns" make the cljs compiler
;; to crash
(defmacro cljs-in-ns [ns-quote]
  (let [[quote ns-name] (vec ns-quote)]
    (when-not (and (= 'quote quote) (symbol? ns-name))
      (throw (IllegalArgumentException.
              "Argument to in-ns must be a symbol.")))
    (when-not (ana/get-namespace ns-name)
      (swap! cljs-env/*compiler*
             assoc-in [::ana/namespaces ns-name]
             {:name ns-name})
      (cljs.repl/-evaluate
       repl-env "<cljs repl>" 1
       (str "goog.provide('" (comp/munge ns-name) "');")))
    (let [in-ns-res (set! ana/*cljs-ns* ns-name)]
      `(quote ~in-ns-res))))
