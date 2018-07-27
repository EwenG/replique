(ns replique.build
  (:require [replique.utils :as utils]
            [badigeon.clean :as clean]
            [badigeon.jar :as jar]
            [badigeon.sign :as sign]
            [badigeon.prompt :as prompt]
            [badigeon.deploy :as deploy]
            [clojure.tools.deps.alpha.util.maven :as maven]))

(defn -main []
  (clean/clean "target")
  (let [jar-path (jar/jar 'replique/replique {:mvn/version utils/version})
        artifacts (sign/sign [{:file-path jar-path} {:file-path "pom.xml"}])
        password (prompt/prompt-password "Password:")]
    (badigeon.deploy/deploy 'replique/replique "0.0.15"
                            {:id "clojars"
                             :url "https://repo.clojars.org/"}
                            {:credentials {:username "ewen"
                                           :password password}})))
