(ns leiningen.replique
  (:require [leiningen.core.eval :as eval])
  (:import [java.util UUID]))

(defn ^{:help-arglists '([host port]
                         [host port process-id])}
  replique
  ([project-map host port]
   (replique project-map host port (UUID/randomUUID)))
  ([project-map host port process-id]
   (let [replique-dep-filter (fn [[k v]] (when (= k 'replique/replique) v))
         replique-version (->> project-map :plugins (keep replique-dep-filter) first)
         cljs-compile-path (get-in project-map [:replique :cljs-compile-path] "%s/cljs")
         cljs-compile-path (if (.startsWith ^String cljs-compile-path "%s")
                             (format cljs-compile-path (:target-path project-map))
                             cljs-compile-path)
         opts {:process-id process-id :host host :port (read-string port)
               :cljs-compile-path cljs-compile-path :version replique-version}]
     (eval/eval-in-project (-> project-map
                               (assoc-in [:replique :cljs-compile-path] cljs-compile-path)
                               (update-in [:dependencies] conj
                                          ['replique/replique replique-version]))
                           `(replique.main/-main (quote ~project-map) ~opts)
                           '(require 'replique.main)))))
