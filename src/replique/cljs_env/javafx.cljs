(ns replique.cljs-env.javafx)

(defn list-css-urls []
  (when-let [scene (.getScene js/muancefx.core.stage)]
    (pr-str (array-seq (.getStylesheets scene)))))

(defn reload-css [url]
  (when-let [scene (.getScene js/muancefx.core.stage)]
    (.clear (.getStylesheets scene))
    (.add (.getStylesheets scene) url)))
