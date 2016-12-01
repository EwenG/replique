(ns leiningen.replique
  (:require [leiningen.core.eval :as eval])
  (:import [java.util UUID]))

(defn ^{:help-arglists '([port]
                         [port process-id])}
  replique
  ([project-map port]
   (replique project-map port (UUID/randomUUID)))
  ([project-map port process-id]
   (let [replique-version (get (->> project-map :plugins (into {})) 'replique/replique)
         cljs-compile-path (get-in project-map [:replique :cljs-compile-path] "%s/cljs")
         cljs-compile-path (if (.startsWith ^String cljs-compile-path "%s")
                             (format cljs-compile-path (:target-path project-map))
                             cljs-compile-path)
         opts {:process-id process-id :port (read-string port)
               :cljs-compile-path cljs-compile-path :version replique-version}]
     (eval/eval-in-project (-> project-map
                               (assoc-in [:replique :cljs-compile-path] cljs-compile-path)
                               (update-in [:dependencies] conj
                                          ['replique/replique replique-version]))
                           `(replique.main/-main (quote ~project-map) ~opts)
                           '(require 'replique.main)))))
