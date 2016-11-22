(defproject replique/replique "0.0.1-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :profiles {:dev {:dependencies [[org.clojure/clojurescript "1.9.229"]
                                  [org.clojure/test.check "0.9.0"]]}}
  :source-paths ["src"]
  :eval-in-leiningen true)
