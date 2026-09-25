(ns replique.css
  "Reloading a stylesheet in the pages connected to this process.

  ONE OP AND ONE SCRIPT. The script is `reload_css.js', evaluated in every
  connected page as raw JavaScript - `replique.cljs/eval-js' is the seam it goes
  through - and it both finds the stylesheet and swaps it. Everything about
  WHICH stylesheet is decided there, because the page is where the list of them
  is; what is left here is turning a file name into a script and an answer into
  a reply.

  NOTHING HERE COMPILES ANYTHING, AND NOTHING HERE RUNS YOUR BUILD. What a
  stylesheet is built by - sass, gulp, esbuild, a shell script - belongs to the
  project that has one, and replique 1's answer to this was a synchronous
  `sass' invocation that froze Emacs for the length of it and wrote one output
  where the real build wrote three. The client compiles, and then asks for
  this. See doc/protocol.md."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [replique.cljs :as cljs]
            [replique.json :as json]))

(def ^:private script-resource
  "The script, beside this file on the classpath rather than in a string here.

  It is a hundred lines of JavaScript with its own prose, and a Clojure string
  is a bad place for either. `clojure.cljs.browser' keeps runtime_browser.js
  the same way and for the same reason.

  Read on each call, so that editing it and asking again is enough to see the
  change - which is what you want of the one file here that cannot be evaluated
  at a repl."
  "replique/reload_css.js")

(def ^:private within-ms
  "How long a page is given to answer.

  SHORT, BECAUSE THIS IS ASKED WHILE SOMEBODY TYPES. A page that is busy is a
  page in the middle of the `require' you sent it a moment ago, and the honest
  answer then is that nothing was reloaded rather than an editor that stops
  responding until the compile finishes. The script itself does no waiting: it
  puts a node in and returns, and the fetch of the new stylesheet happens after
  the answer is already on its way back.

  `clojure.cljs.repl' says which of the two it was - `busy' where the question
  never got in front of the page, `timed-out' where the page has it - and both
  of those come back as the `note' of a reply that reloaded nothing."
  2000)

(defn- script
  "The script, with the file that changed written into the call at the end.

  As a JSON string literal, which is a JavaScript string literal: what is being
  interpolated is a path, and a path may hold a quote or a backslash as easily
  as anything else."
  [file]
  (str (slurp (io/resource script-resource)) "(" (json/write-str file) ")"))

(defn- read-answer
  "The map the script answered with, read out of what the runtime printed.

  TWICE, because what an evaluation answers with is what the runtime PRINTED:
  a JavaScript string arrives as a printed string, quoting and all, so the
  first read takes that off and the second reads the map that was inside it."
  [value]
  (edn/read-string (edn/read-string value)))

(defn- reply-of
  "The reply for what `eval-js' answered.

  THREE OUTCOMES AND NOT TWO. The script ran, which is the answer. The page
  could not be asked - nobody has it open, it was busy, it never came back -
  which is a FACT ABOUT THE PAGE and not a failure of the request: it comes
  back as a reply that reloaded nothing and says why, because the sentence
  `clojure.cljs.repl' and `clojure.cljs.browser' wrote for it is already the
  one somebody needs, down to the URL to open. And the script itself threw,
  which is none of those: nothing about the page explains it, so it is raised
  rather than reported as though the page had answered."
  [answer]
  (if (= :success (:status answer))
    (read-answer (:value answer))
    (if (:stacktrace answer)
      (throw (ex-info (str "The stylesheet reload failed in the page: "
                           (:value answer))
                      {:replique/error :reload-css
                       :stacktrace (:stacktrace answer)}))
      {:reloaded [] :stylesheets [] :note (:value answer)})))

(defn reload!
  "Reload the stylesheet FILE names, in every page connected to this process.

    {:reloaded [\"http://localhost:8082/css/main.css\"]
     :stylesheets [\"http://localhost:8082/css/main.css\" ...]}

  FILE is a path on this machine - the file the client just built or just saved
  - and what the pages hold are URLs. Nothing here can turn one into the other:
  where a project's assets are served from is the project's arrangement and
  replique has never seen it. So the two are matched by their longest common
  path suffix, in the page, which needs neither side to know the other's half.

  THE BROWSER, WHATEVER THIS THREAD'S TARGET IS. Node has no stylesheets, so
  there is no question here to answer about it and nothing to be gained by
  refusing one: this is a question about pages, and `:browser' is where the
  pages are. `write-main-js!' binds it for the same reason."
  [file]
  ;; The compiler, because the way to a page is the ClojureScript browser
  ;; runtime - the websocket, the asset server and the client the page
  ;; imported. What is missing when this refuses is the classpath, which is
  ;; what the sentence names
  (cljs/refuse-unless-available! "reload a stylesheet in a browser")
  (binding [cljs/*target* :browser]
    (reply-of (cljs/eval-js (script file) within-ms))))
