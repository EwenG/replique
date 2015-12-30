(defproject ewen.replique/replique "0.0.1-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.170"]
                 [ewen/hiccup "1.0.0"]]
  :source-paths ["src"]
  :plugins [[lein-cljsbuild "1.1.1"]]
  :cljsbuild {:builds [{:source-paths ["src"]
                        :compiler {:output-to "out/main.js"
                                   :optimizations :advanced
                                   :pretty-print false
                                   :externs ["resources/js/externs.js"]}}]})
