(defproject ewen.replique/replique "0.0.1-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.7.228"]
                 [ewen/hiccup "1.0.0"]
                 [com.lucasbradstreet/cljs-uuid-utils "1.0.2"]]
  :source-paths ["src"]
  :plugins [[lein-cljsbuild "1.1.1"]]
  :cljsbuild {:builds [{:id "dev"
                        :source-paths ["src"]
                        :compiler {:output-dir "out/"
                                   :optimizations :none}}
                       {:source-paths ["src"]
                        :compiler {:output-to "out/main.js"
                                   :optimizations :advanced
                                   :pretty-print false
                                   :externs ["resources/js/externs.js"]}}]})
