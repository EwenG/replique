(defproject replique/replique "0.0.12"
  :description "A development environment for Clojure and Clojurescript"
  :url "https://github.com/EwenG/replique"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :profiles {:dev {:resource-paths ["dev-resources"]
                   :dependencies [[org.clojure/clojurescript "1.9.473"]]}}
  :source-paths ["src"]
  :resource-paths ["resources"]
  :eval-in-leiningen true)
