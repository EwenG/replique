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
        :else (protocol/write-frame!
               conn (protocol/error msg :invalid-handler-result
                                    (str "The handler of op " (:op msg)
                                         " returned a " (type res))))))
    (catch Throwable t
      (protocol/write-frame! conn (error-frame msg t)))))

(defn- dispatch! [conn msg]
  (let [raw-op (:op msg)
        op (protocol/normalize-op raw-op)
        msg (assoc msg :op op)]
    (cond
      (nil? op)
      (protocol/write-frame!
       conn (protocol/error msg :invalid-message
                            (if (nil? raw-op)
                              "A request must have an :op"
                              (str "An :op must be a keyword, a string or a symbol, got: "
                                   (pr-str raw-op)))))
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
  (server/set-role! conn :control)
  (protocol/write-frame! conn (protocol/reply hello (assoc (state/info)
                                                           :role "control"
                                                           :connection (:id conn))))
  (control-loop conn))
