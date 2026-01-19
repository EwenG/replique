(ns replique.repl-protocols)

(defprotocol ReplLoadFile
  (-load-file
    [repl-env file-path]
    [repl-env file-path opts]))
