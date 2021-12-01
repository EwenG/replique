(ns replique.build
  (:require [replique.utils :as utils]
            [badigeon.clean :as clean]
            [badigeon.jar :as jar]
            [badigeon.install :as install]
            [badigeon.sign :as sign]
            [badigeon.prompt :as prompt]
            [badigeon.deploy :as deploy]
            [clojure.tools.deps.alpha.util.maven :as maven]))

(defn make-jar []
  (clean/clean "target")
  (jar/jar 'replique/replique {:mvn/version utils/version}))

(defn deploy-local []
  (let [jar-path (make-jar)]
    (install/install 'replique/replique {:mvn/version utils/version}
                     jar-path "pom.xml")))

(defn -main []
  (let [jar-path (make-jar)
        artifacts (sign/sign [{:file-path jar-path} {:file-path "pom.xml"}])
        #_#_password (prompt/prompt-password "Password:")
        deploy-token (slurp "deploy_token.txt")]
    (badigeon.deploy/deploy 'replique/replique utils/version artifacts
                            {:id "clojars"
                             :url "https://repo.clojars.org/"}
                            {:credentials {:username "ewen"
                                           :password deploy-token}})))

(comment
  (deploy-local)
  )
