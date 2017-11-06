(ns replique.source-meta
  (:require [replique.utils :as utils]
            [replique.interactive :as interactive]
            [replique.repl])
  (:import [java.net URL]))

(def ^:private cljs-set-source-meta! (utils/dynaload 'replique.repl-cljs/set-source-meta!))

(defn set-source-meta! [url-str line column]
  (let [url (try (URL. url-str) (catch Exception _ nil))
        path (when url (interactive/url->path url))]
    (if (isa? utils/*repl-env* :replique/cljs)
      (@cljs-set-source-meta! (or path "NO_SOURCE_FILE") (or line 1) (or column 1))
      (replique.repl/set-source-meta! (or path "NO_SOURCE_PATH") (or line 1) (or column 1)))
    :replique/__ignore))
