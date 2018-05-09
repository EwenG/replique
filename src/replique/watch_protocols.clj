(ns replique.watch-protocols
  (:import [clojure.lang IDeref]))

(defprotocol IWatchHandler
  (add-watch-handler [this buffer-id]))

(defprotocol IRecordable
  (start-recording [this buffer-id record-size])
  (stop-recording [this buffer-id])
  (record-position [this])
  (most-recent-value [this])
  (value-at-index [this index]))

(defprotocol IRecordSize
  (record-size [this]))

(defprotocol IGetRef
  (get-ref [this]))

(deftype WatchedRef [ref values index]
  IDeref
  (deref [this] (get values index)))

(deftype RecordedWatchedRef [ref values index record-size]
  IDeref
  (deref [this] (get values index)))
