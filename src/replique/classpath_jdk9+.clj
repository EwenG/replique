(ns replique.classpath-jdk9+
  (:import [java.lang.module ModuleFinder ModuleReference]))

;; workaround for https://dev.clojure.org/jira/browse/CLJ-2284
(defmacro interface-static-call
  [sym argtypes]
  `(let [m# (.getMethod ~(symbol (namespace sym))
                        ~(name sym)
                        (into-array Class ~argtypes))]
     (fn [& args#]
       (.invoke m# nil (to-array args#)))))

(defn system-module-resources [^ModuleReference m]
  (-> m (.open) (.list) (.iterator) iterator-seq))

(def ^ModuleFinder system-module ((interface-static-call ModuleFinder/ofSystem [])))

(def system-module-resources (mapcat system-module-resources (.findAll system-module)))
