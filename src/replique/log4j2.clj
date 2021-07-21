(ns replique.log4j2
  (:import [org.apache.logging.log4j LogManager]
           [org.apache.logging.log4j.core LoggerContext]))

(defn log4j2-reload [file-url]
  (.reconfigure ^LoggerContext (LogManager/getContext false)))

