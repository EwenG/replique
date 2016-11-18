(defproject replique/replique "0.0.1-SNAPSHOT"
  :profiles {:provided {:dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                                       [org.clojure/clojurescript "1.9.229"]]}
             :dev {:dependencies [[org.clojure/test.check "0.9.0"]]}}
  :source-paths ["src"])
