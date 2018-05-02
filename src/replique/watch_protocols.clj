(ns replique.watch-protocols)

(defprotocol IWatchHandler
  (add-watch-handler [this buffer-id]))

(defprotocol IRecordable
  (start-recording [this buffer-id record-size])
  (stop-recording [this buffer-id])
  (record-position [this])
  (most-recent-value [this])
  (value-at-index [this index]))

(defprotocol IGetRef
  (get-ref [this]))
