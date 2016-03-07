(defproject ewen.replique/replique "0.0.1-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.7.228"]
                 [ewen/hiccup "1.0.0"]
                 [com.lucasbradstreet/cljs-uuid-utils "1.0.2"]
                 [ewen/ddom "0.0.1-SNAPSHOT"]]
  :source-paths ["src"]
  :aliases {"compile-ui" ["run" "-m" "ewen.replique.main/compile-ui"]})
