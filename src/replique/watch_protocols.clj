(ns replique.watch-protocols)

(defprotocol IWatchHandler
  (add-watch-handler [this buffer-id]))

(defprotocol IMostRecentValue
  (most-recent-value [this]))

(defprotocol IRecordable
  (record [this])
  (stop-recording [this]))

(defprotocol IGetRef
  (get-ref [this]))
