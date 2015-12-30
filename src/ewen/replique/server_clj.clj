(ns ewen.replique.server-clj
  (:require [ewen.replique.server :as server]
            [clojure.core.server]))

(defmethod server/repl :cljs
  [type {:keys [comp-opts repl-env compiler-env]}]
  (println "Clojure" (clojure-version))
  (clojure.main/repl :init clojure.core.server/repl-init
                     :read clojure.core.server/repl-read))

(defmethod server/repl-dispatch [:clj nil]
  [{:keys [port type cljs-env] :as opts}]
  (start-server {:port port :name :replique-tooling-repl
                 :accept 'ewen.replique.server/tooling-repl
                 :server-daemon false})
  (start-server {:port 0 :name :replique-repl
                 :accept 'ewen.replique.server/repl
                 :server-daemon false
                 :args [type]})
  (println "REPL started"))
