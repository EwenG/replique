(ns leiningen.replique
  (:require [leiningen.core.eval :as eval]))

(defn ^{:help-arglists '([directory port replique-version])} replique
  [project-map directory port replique-version]
  (let [cljs-compile-path (get-in project-map [:replique :cljs-compile-path] "%s/cljs")
        cljs-compile-path (if (.startsWith ^String cljs-compile-path "%s")
                            (format cljs-compile-path (:target-path project-map))
                            cljs-compile-path)
        opts {:directory directory :port (read-string port) :cljs-compile-path cljs-compile-path}]
    (eval/eval-in-project (-> project-map
                              (assoc-in [:replique :cljs-compile-path] cljs-compile-path)
                              (update-in [:dependencies] conj
                                         ['replique/replique replique-version]))
                          `(replique.main/-main (quote ~project-map) ~opts)
                          '(require 'replique.main))))
