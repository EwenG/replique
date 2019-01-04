(ns replique.logback
  (:import [org.slf4j LoggerFactory]
           [ch.qos.logback.classic.util ContextInitializer]
           [ch.qos.logback.classic LoggerContext]
           [java.net URL]))

(defn logback-reload [file-url]
  (let [file-url (if (string? file-url) (URL. file-url) file-url)
        ^LoggerContext logger-context (LoggerFactory/getILoggerFactory)]
    (.reset logger-context)
    (.configureByResource (ContextInitializer. logger-context) file-url)
    logger-context))

