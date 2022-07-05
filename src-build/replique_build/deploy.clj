(ns replique-build.deploy
  (:require [badigeon2.utils :as utils]
            [badigeon2.deploy :as deploy]
            [badigeon2.sign :as sign]
            [clojure.tools.build.api :as api]))

(defn -main []
  (let [basis (api/create-basis)
        deploy-token (slurp "deploy_token.txt")
        jar-file (format "target/replique-%s.jar" utils/version)
        jar-signature-file (format "target/replique-%s.jar.asc" utils/version)]
    (api/delete {:path "target"})
    (api/copy-dir {:src-dirs ["src"]
                   :target-dir "target/classes"})
    (api/write-pom {:basis basis
                    :class-dir "target/classes"
                    :lib 'replique/replique
                    :version utils/version
                    :src-dirs ["src"]})
    (api/jar {:class-dir "target/classes"
              :jar-file jar-file})
    (sign/sign-gpg {:jar-file jar-file
                    :lib 'replique/replique
                    :class-dir "target/classes"})
    #_(deploy/deploy {:basis basis
                    :class-dir "target/classes"
                    :lib 'replique/replique
                    :version utils/version
                    :jar-file jar-file
                    :jar-signature-file jar-signature-file
                    :repository {:id "clojars"
                                 :url "https://repo.clojars.org/"}
                    :credentials {:username "ewen"
                                  :password deploy-token}})))
