(ns replique.source-meta)

;; Used to set the source metadata to forms evaluated from a source buffer
(defonce source-meta (atom {:url nil
                            :line nil
                            :column nil}))
