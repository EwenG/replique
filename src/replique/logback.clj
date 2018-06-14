(ns replique.logback
  (:import [org.slf4j LoggerFactory]
           [ch.qos.logback.classic.util ContextInitializer]
           [ch.qos.logback.classic LoggerContext]))

(defn logback-reload []
  (let [^LoggerContext logger-context (LoggerFactory/getILoggerFactory)]
    (.reset logger-context)
    (.autoConfig (ContextInitializer. logger-context))
    logger-context))

