(ns replique.control
  "The control connection: the channel the editor uses for everything that is
  not a REPL. It is a message loop, not a REPL with a discarded prompt:
  requests are an explicit, versioned set of ops and they are correlated by
  :id.

  Requests are handled serially, on the thread that reads the connection, so
  that replies come back in request order and no reply can outlive the
  connection. An op that may take long enough to matter answers immediately
  and reports its progress as events, rather than holding the channel; a
  client that wants two requests in flight opens a second control connection."
  (:require [replique.protocol :as protocol]
            [replique.state :as state]
            ;; loads the op implementations
            [replique.ops])
  (:import [java.io Reader]))

(defn- error-frame [msg ^Throwable t]
  (let [kind (:replique/error (ex-data t))]
    (if kind
      (protocol/error msg kind (.getMessage t))
      (protocol/exception-error msg t))))

(defn- handle-request [conn msg]
  (try
    (let [res (protocol/handle conn msg)]
      (cond
        (identical? res protocol/no-reply) nil
        (map? res) (protocol/write-frame! conn (protocol/reply msg res))
        :else (protocol/write-frame!
               conn (protocol/error msg :invalid-handler-result
                                    (str "The handler of op " (:op msg)
                                         " returned a " (type res))))))
    (catch Throwable t
      (protocol/write-frame! conn (error-frame msg t)))))

(defn- dispatch! [conn msg]
  (let [op (protocol/normalize-op (:op msg))
        msg (assoc msg :op op)]
    (cond
      (nil? op)
      (protocol/write-frame!
       conn (protocol/error msg :invalid-message "A request must have an :op"))
      (not (protocol/valid-id? (:id msg)))
      (protocol/write-frame!
       conn (protocol/error (dissoc msg :id) :invalid-message
                            (str "An :id must be a string or a number, got: "
                                 (pr-str (:id msg)))))
      (= :hello op)
      (protocol/write-frame!
       conn (protocol/error msg :already-connected
                            "The connection is already established"))
      :else (handle-request conn msg))))

(defn- skip-line!
  "Drop what is left of the current line. Messages are written one per line by
  convention, thus this is where the next one starts. Returns false at eof.

  Always consumes at least one character, which is what makes the recovery
  loop of control-loop terminate."
  [{:keys [^Reader in]}]
  (loop []
    (let [c (.read in)]
      (cond
        (== c -1) false
        (== c 10) true
        :else (recur)))))

(defn control-loop
  "Read EDN messages until the client disconnects.

  A message that is not a map is rejected but the connection survives: the
  reader is still in sync. Input that is not readable EDN is reported, then
  the rest of the line is dropped and reading resumes. The resynchronization
  is best effort - a malformed message that spans several lines leaves the
  beginning of the next line as garbage, which is reported in turn - but it
  beats closing a connection because of one bad message."
  [conn]
  (loop []
    (let [msg (try (protocol/read-message conn)
                   (catch Throwable t t))]
      (cond
        (protocol/eof? msg) nil

        ;; The client is gone, or the connection is being closed
        (instance? java.io.IOException msg) nil

        (instance? Throwable msg)
        (do (protocol/write-frame!
             conn (protocol/error nil :malformed-message
                                  (str "Could not read an EDN message, skipping "
                                       "to the end of the line: "
                                       (.getMessage ^Throwable msg))))
            (when (skip-line! conn) (recur)))

        (not (map? msg))
        (do (protocol/write-frame!
             conn (protocol/error nil :invalid-message
                                  (str "A request must be a map, got: "
                                       (pr-str (type msg)))))
            (recur))

        :else (do (dispatch! conn msg) (recur))))))

(defmethod protocol/accept-role :control [conn hello]
  (protocol/write-frame! conn (protocol/reply hello (assoc (state/info)
                                                           :role "control"
                                                           :connection (:id conn))))
  (control-loop conn))
