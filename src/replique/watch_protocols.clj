(ns replique.watch-protocols
  (:import [clojure.lang IDeref IObj IMeta]))

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

(declare ->WatchedRef)

(deftype WatchedRef [ref values index meta]
  IDeref
  (deref [this] (get values index))
  IObj
  (withMeta [this meta] (->WatchedRef ref values index meta))
  IMeta
  (meta [this] meta))

(declare ->RecordedWatchedRef)

(deftype RecordedWatchedRef [ref values index record-size meta]
  IDeref
  (deref [this] (get values index))
  IObj
  (withMeta [this meta] (->RecordedWatchedRef ref values index record-size meta))
  IMeta
  (meta [this] meta))

(extend-type nil
  IGetRef
  (get-ref [this] nil))
