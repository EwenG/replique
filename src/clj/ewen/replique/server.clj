(ns ewen.replique.server
  (:require [clojure.main]
            [clojure.core.server :refer [start-server]]
            [ewen.replique.server-cljs :as server-cljs]
            [ewen.replique.server-clj :as server-clj]
            [clojure.java.io :refer [file]]))

(def ^:const init-requires
  '[])

(defmulti tooling-msg-handle :type)

(defn repl []
  (clojure.main/repl :init clojure.core.server/repl-init
                     :read clojure.core.server/repl-read))

(defmulti repl identity)

(defmethod repl :clj [type]
  (println "Clojure" (clojure-version))
  (clojure.main/repl :init clojure.core.server/repl-init
                     :read clojure.core.server/repl-read))

(defmethod repl :cljs [type]
  (println "Clojure" (clojure-version))
  (clojure.main/repl :init clojure.core.server/repl-init
                     :read clojure.core.server/repl-read))

(defn tooling-repl []
  (let [init-fn (fn [] (in-ns 'ewen.replique.server))]
    (clojure.main/repl
     :init init-fn
     :read clojure.core.server/repl-read
     :prompt #())))

(defn init-repl [{:keys [port type cljs-env replique-dir]}]
  (def replique-dir replique-dir)
  (start-server {:port port :name :replique-tooling-repl
                 :accept 'ewen.replique.server/tooling-repl
                 :server-daemon false})
  (start-server {:port 0 :name :replique-repl
                 :accept 'ewen.replique.server/repl
                 :server-daemon false
                 :args [type]})
  (doto (file ".replique-port")
    (spit (str {:repl (-> @#'clojure.core.server/servers
                          (get :replique-tooling-repl)
                          :socket
                          (.getLocalPort))}))
    (.deleteOnExit))


  (println "REPL started"))

(defmulti repl-dispatch (fn [{:keys [type cljs-env]}]
                          [type cljs-env]))

(defmethod repl-dispatch [:clj nil] [opts]
  (server-clj/init-tooling-msg-handle tooling-msg-handle)
  (init-repl opts))

(defmethod repl-dispatch [:cljs :browser] [opts]
  (server-cljs/init-class-loader)
  (server-cljs/init-tooling-msg-handle tooling-msg-handle)
  (init-repl opts))

(defn -main [opts]
  (repl-dispatch (read-string opts)))

(defmethod tooling-msg-handle :repl-infos [msg]
  (let [{:keys [replique-tooling-repl replique-repl]}
        @#'clojure.core.server/servers]
    {:replique-tooling-repl
     {:host (-> (:socket replique-tooling-repl)
                (.getInetAddress) (.getHostAddress))
      :port (-> (:socket replique-tooling-repl) (.getLocalPort))}
     :replique-repl
     {:host (-> (:socket replique-tooling-repl)
                (.getInetAddress) (.getHostAddress))
      :port (-> (:socket replique-repl) (.getLocalPort))}}))
