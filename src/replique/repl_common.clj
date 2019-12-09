(ns replique.repl-common
  (:require [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as readers]
            [replique.source-meta]
            [replique.utils :as utils])
  (:import [java.io File PushbackReader Closeable]
           [java.net URL]))

;; Copied over from cljs.repl
(defn skip-if-eol
  "If the next character on stream s is a newline, skips it, otherwise
  leaves the stream untouched. Returns :line-start, :stream-end, or :body
  to indicate the relative location of the next character on s. The stream
  must either be an instance of LineNumberingPushbackReader or duplicate
  its behavior of both supporting .unread and collapsing all of CR, LF, and
  CRLF to a single \\newline."
  [s]
  (let [c (readers/read-char s)]
    (case c
      \newline :line-start
      nil :stream-end
      (do (readers/unread s c) :body))))

;; Copied over from cljs.repl
(defn skip-whitespace
  "Skips whitespace characters on stream s. Returns :line-start, :stream-end,
  or :body to indicate the relative location of the next character on s.
  Interprets comma as whitespace and semicolon as comment to end of line.
  Does not interpret #! as comment to end of line because only one
  character of lookahead is available. The stream must either be an
  instance of LineNumberingPushbackReader or duplicate its behavior of both
  supporting .unread and collapsing all of CR, LF, and CRLF to a single
  \\newline."
  [s]
  (loop [c (readers/read-char s)]
    (case c
      \newline :line-start
      nil :stream-end
      \; (do (readers/read-line s) :line-start)
      (if (or (Character/isWhitespace c) (identical? c \,))
        (recur (readers/read-char s))
        (do (readers/unread s c) :body)))))

(def request-prompt (Object.))

;; Custom constructor to be able to set :file :line and :column when evaluating
;; code from a source file at the REPL
(def source-logging-push-back-reader-pre-1-3-0-arglists
  '([rdr line column line-start? prev prev-column file-name source-log-frames]))

(def source-logging-push-back-reader-post-1-3-0-arglists
  '([rdr line column line-start? prev prev-column file-name source-log-frames normalize?]))

(defn ^Closeable source-logging-push-back-reader
  "Creates a SourceLoggingPushbackReader from a given string or PushbackReader"
  [s-or-rdr buf-len file-name line column]
  (let [arglists (:arglists (meta #'readers/->SourceLoggingPushbackReader))
        rdr (readers/to-pbr s-or-rdr buf-len)
        source-log-frames (doto (clojure.tools.reader.impl.utils/make-var)
                            (alter-var-root (constantly {:buffer (StringBuilder.)
                                                         :offset 0})))]
    (cond (= arglists source-logging-push-back-reader-pre-1-3-0-arglists)
          (readers/->SourceLoggingPushbackReader
           rdr
           line column
           true
           nil
           0
           file-name
           source-log-frames)
          ;; clojure/tools.reader >= 1.3.0
          (= arglists source-logging-push-back-reader-post-1-3-0-arglists)
          (readers/->SourceLoggingPushbackReader
           rdr
           line column
           true
           nil
           0
           file-name
           source-log-frames
           true)
          :else
          (throw (IllegalStateException. "Wrong clojure/tools.reader version")))))

(defn repl-reader []
  (let [{:keys [url line column]} @replique.source-meta/source-meta
        url (try (URL. url) (catch Exception _ nil))
        path (when url (utils/url->path url))]
    (source-logging-push-back-reader
     *in* 1 (or path "NO_SOURCE_FILE") (or line 1) (or column 1))))

;; Also always reset the reader even is :source-map-inline is false in order to set the metadata on
;; code evaluated from a buffer
;; Delay the creation of the new reader after the source meta has been set by the tooling REPL
(defn repl-read [request-exit]
  ;; Wait for something to come in in order to delay the creation of the new reader
  (.unread ^PushbackReader *in* (.read ^PushbackReader *in*))
  (let [current-in *in*]
    (binding [*in* (repl-reader)]
      (or ({:line-start request-prompt :stream-end request-exit}
           (skip-whitespace *in*))
          (let [input (reader/read {:read-cond :allow :features #{:cljs}} *in*)]
            ;; Transfer 1-char buffer to original *in*
            (readers/unread current-in (readers/read-char *in*))
            (skip-if-eol current-in)
            input)))))

(defn repl-quit-prompt [exit-kw]
  (println "To quit, type:" exit-kw))

(defn repl-prompt [ns]
  (print (str ns "=> ")))

(defn repl-need-prompt []
  (if (readers/indexing-reader? *in*)
    (== (readers/get-column-number *in*) 1)
    (identity true)))
