(ns replique.control
  "The control connection: the channel the editor uses for everything that is
  not a REPL. Requests are an explicit, versioned set of ops and they are correlated by
  :id.

  Requests are handled serially, on the thread that reads the connection, so
  that replies come back in request order. An op that may take long enough to matter answers immediately
  and reports its progress as events, rather than holding the channel."

  (:require [replique.protocol :as protocol]
            [replique.server :as server]
            [replique.state :as state]
            ;; loads the op implementations
            [replique.inspect]
            [replique.ops]))

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
        ;; nil is named rather than described: (type nil) is nil, and the
        ;; sentence would stop after "returned a". It is also the likeliest
        ;; way to land here - a handler whose body ends in a when that was
        ;; false - so it is the one case that must read
        :else (protocol/write-frame!
               conn (protocol/error msg :invalid-handler-result
                                    (str "The handler of op " (:op msg) " returned "
                                         (if (nil? res)
                                           "nil"
                                           (str "a " (.getName (class res)))))))))
    (catch Throwable t
      (protocol/write-frame! conn (error-frame msg t)))))

(defn- dispatch! [conn msg]
  (let [raw-op (:op msg)
        op (protocol/as-keyword raw-op)
        msg (assoc msg :op op)]
    (cond
      ;; First, and before the op: this is the check that lets an error frame
      ;; be built at all. A frame carrying an id json cannot write does not
      ;; serialize, and neither does the error frame that would say so, which
      ;; is built around that same id - so a message rejected for its op while
      ;; its id went unlooked at came back as a serialization failure, saying
      ;; nothing about either. The handshake checks the id first for the same
      ;; reason, and one rule cannot be told two ways.
      (not (protocol/valid-id? (:id msg)))
      (protocol/write-frame!
       conn (protocol/error (dissoc msg :id) :invalid-message
                            (protocol/invalid-id-message (:id msg))))
      (nil? op)
      (protocol/write-frame!
       conn (protocol/error msg :invalid-message
                            (if (nil? raw-op)
                              "A request must have an :op"
                              (str "An :op must be a keyword, a string or a symbol, got: "
                                   (pr-str raw-op)))))
      (= :hello op)
      (protocol/write-frame!
       conn (protocol/error msg :already-connected
                            "The connection is already established"))
      :else (handle-request conn msg))))

(defn- handle-message! [conn msg]
  (if (map? msg)
    (dispatch! conn msg)
    (protocol/write-frame!
     conn (protocol/error nil :invalid-message
                          (str "A request must be a map, got: " (pr-str (type msg)))))))

(defn control-loop
  "Read one line at a time until the client disconnects, and handle the
  messages it holds - usually exactly one.

  A message that is not a map is rejected but the connection survives: it was
  read whole. A line that is not readable EDN is reported and abandoned, and
  reading resumes on the next line."
  [conn]
  (loop []
    ;; a frame parked by a busy producer must not wait for the next request
    (protocol/try-flush! conn)
    (let [line (protocol/read-line! conn)]
      (when-not (protocol/eof? line)
        (let [[messages error] (protocol/read-messages line)]
          (run! #(handle-message! conn %) messages)
          (when error
            (protocol/write-frame!
             conn (protocol/error nil :malformed-message
                                  (str "Could not read an EDN message, the rest of "
                                       "the line is ignored: "
                                       (protocol/read-error-message error)))))
          (recur))))))

(defmethod protocol/accept-role :control [conn hello]
  (protocol/write-frame! conn (protocol/reply hello (assoc (state/info)
                                                           :role "control"
                                                           :connection (:id conn))))
  ;; Only now, and not before the reply went out: an event broadcast to a
  ;; connection that is still handshaking would reach a client that is
  ;; waiting for its :hello reply, and reach it first.
  (server/set-role! conn :control)
  (control-loop conn))
