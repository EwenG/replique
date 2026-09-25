(ns replique.css-test
  "Reloading a stylesheet in a page.

  THE SCRIPT IS TESTED ON NODE, against a document written for it - see
  reload_css_stub.js. It is plain JavaScript evaluated by the runtime, so the
  only thing a browser would add here is a browser: what the tests are about is
  which links the script picks and what it does to them, and a real page would
  have answered and repainted before anything could look. The transport, and
  the fan-out to every page, are the compiler's and are tested in its
  browser-test.

  Written for both processes, as `replique.cljs-test' is. With a compiler:

    clojure -M:test:cljs ..."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [replique.cljs :as cljs]
            [replique.css :as css]
            [replique.json :as json]
            ;; for the defmethod the op is
            [replique.ops]
            [replique.protocol :as protocol]
            [replique.test-client :as client]))

(defn- compiling?
  "Whether the process running this test has a ClojureScript compiler."
  []
  (cljs/available?))

(def ^:private ms 10000)

(def ^:private read-answer
  "The two reads every answer here comes back through - see `replique.css'."
  #'css/read-answer)

(defn- document!
  "Put a document in the runtime for the script to work on.

  SPEC is {:origin :links [{:rel :href}] :sheets [...]}, and `:sheets' is what
  document.styleSheets holds: the @import'ed ones, which have no node and which
  the script must not be reading."
  [spec]
  (let [js (str (slurp (io/resource "replique/reload_css_stub.js"))
                "(" (json/write-str spec) ")")]
    (is (= :success (:status (cljs/eval-js js ms))) "the document was put up")))

(defn- reload
  "Run the script for FILE against the document that is in place."
  [file]
  (let [answer (cljs/eval-js (#'css/script file) ms)]
    (is (= :success (:status answer)) (pr-str answer))
    (read-answer (:value answer))))

(defn- nodes
  "What the document holds now."
  []
  (read-answer (:value (cljs/eval-js "__stub.nodes()" ms))))

(defn- fire!
  "Run the listeners the script registered, as a browser would."
  [kind]
  (cljs/eval-js (str "__stub.fire(" (json/write-str (name kind)) ")") ms))

(def ^:private page
  "A page with the same file name under two paths, which is the case replique 1
  got wrong."
  {:origin "http://localhost:8082"
   :links [{:rel "stylesheet" :href "http://localhost:8082/css/main.css"}
           {:rel "stylesheet" :href "http://localhost:8082/main.css"}]})

;;; Which link is the file that changed

(deftest test-the-longest-path-suffix-wins
  ;; Replique 1 matched the BASENAME, so a project with a main.css per theme
  ;; found several, asked which, and remembered the answer in a defvar that
  ;; died with the Emacs session. A path suffix needs nothing remembered and
  ;; nobody asked: `css/main.css' agrees with the file in two segments where a
  ;; bare `main.css' agrees in one, and two is more than one.
  (when (compiling?)
    (binding [cljs/*target* :node]
      (document! page)
      (let [r (reload "/home/me/app/public/main/css/main.css")]
        (is (= ["http://localhost:8082/css/main.css"] (:reloaded r)))))))

(deftest test-everything-that-ties-for-longest-is-reloaded
  ;; A page that includes one stylesheet twice is a page where reloading one of
  ;; them is a page half reloaded. There is no way to choose between them and
  ;; nothing to be gained by asking, so both go.
  (when (compiling?)
    (binding [cljs/*target* :node]
      (document! {:origin "http://localhost:8082"
                  :links [{:rel "stylesheet" :href "http://localhost:8082/css/main.css"}
                          {:rel "stylesheet" :href "http://localhost:8082/css/main.css"}
                          {:rel "stylesheet" :href "http://localhost:8082/other.css"}]})
      (let [r (reload "/home/me/app/css/main.css")]
        (is (= ["http://localhost:8082/css/main.css"
                "http://localhost:8082/css/main.css"]
               (:reloaded r)))))))

(deftest test-what-the-page-has-comes-back-whether-or-not-anything-matched
  ;; The whole of replique 1's second round trip, and the fix for its worst
  ;; message: "Could not find a css file to reload", with no hint of what the
  ;; page actually had. The list is the hint and it costs nothing.
  (when (compiling?)
    (binding [cljs/*target* :node]
      (document! page)
      (let [r (reload "/home/me/app/css/nothing-like-it.css")]
        (is (= [] (:reloaded r)))
        (is (= ["http://localhost:8082/css/main.css"
                "http://localhost:8082/main.css"]
               (:stylesheets r))
            "and says what the page has, which is what makes one op enough")))))

;;; What is done to it

(deftest test-a-fresh-node-goes-in-beside-the-old-one
  ;; And not the href of the live node rewritten, which is what replique 1 did:
  ;; that unstyles the page for as long as the fetch takes. The clone goes
  ;; directly after the original, so the cascade order is unchanged, and the
  ;; original goes when the clone has loaded.
  (when (compiling?)
    (binding [cljs/*target* :node]
      (document! page)
      (reload "/home/me/app/css/main.css")
      (let [after (nodes)]
        (is (= 3 (count after)) "the old link is still there, and a fresh one beside it")
        (is (= "http://localhost:8082/css/main.css" (:href (first after))))
        (is (re-find #"^http://localhost:8082/css/main\.css\?v=\d+$"
                     (:href (second after)))
            "the clone is next, so nothing moves in the cascade"))
      (fire! :load)
      (let [loaded (nodes)]
        (is (= 2 (count loaded)))
        (is (re-find #"\?v=\d+$" (:href (first loaded)))
            "and the original goes once the fresh one has loaded")))))

(deftest test-a-stylesheet-that-fails-to-load-leaves-the-page-as-it-was
  ;; The reason for the clone. A 404, or a file that no longer parses, would
  ;; otherwise leave the page unstyled until you fixed it and reloaded by hand -
  ;; and what you were doing when it happened was editing that file.
  (when (compiling?)
    (binding [cljs/*target* :node]
      (document! page)
      (reload "/home/me/app/css/main.css")
      (fire! :error)
      (let [after (nodes)]
        (is (= 2 (count after)))
        (is (= ["http://localhost:8082/css/main.css" "http://localhost:8082/main.css"]
               (map :href after))
            "the clone is gone and the stylesheet that was working is still there")))))

(deftest test-the-cache-buster-leaves-the-rest-of-the-query-alone
  ;; Replique 1 split the href on "?" and threw away what was after it, so a
  ;; stylesheet the application asked for with a query came back as a different
  ;; stylesheet. What is set here is one parameter.
  (when (compiling?)
    (binding [cljs/*target* :node]
      (document! {:origin "http://localhost:8082"
                  :links [{:rel "stylesheet"
                           :href "http://localhost:8082/css/main.css?theme=dark&build=7"}]})
      (let [r (reload "/home/me/app/css/main.css")]
        (is (= ["http://localhost:8082/css/main.css?theme=dark&build=7"] (:reloaded r))
            "and the buster is taken back off what is reported")
        (let [fresh (:href (second (nodes)))]
          (is (re-find #"theme=dark" fresh))
          (is (re-find #"build=7" fresh))
          (is (re-find #"v=\d+" fresh)))))))

;;; What is not a stylesheet of this page

(deftest test-only-this-pages-own-origin
  ;; A stylesheet served from somewhere else is not a file you are editing, and
  ;; the port is part of the answer: a page on :8082 and an asset server on
  ;; :3000 are two different origins.
  (when (compiling?)
    (binding [cljs/*target* :node]
      (document! {:origin "http://localhost:8082"
                  :links [{:rel "stylesheet" :href "https://cdn.example.com/css/main.css"}
                          {:rel "stylesheet" :href "http://localhost:3000/css/main.css"}
                          {:rel "stylesheet" :href "http://localhost:8082/css/main.css"}]})
      (let [r (reload "/home/me/app/css/main.css")]
        (is (= ["http://localhost:8082/css/main.css"] (:reloaded r)))
        (is (= ["http://localhost:8082/css/main.css"] (:stylesheets r))
            "and the others are not even listed: they could never be reloaded")))))

(deftest test-only-stylesheet-links-and-only-ones-with-an-href
  ;; Which is the selector doing the work, and the stub reads the selector - a
  ;; preload of the very same file is not a stylesheet, and a <link> with no
  ;; href is not one either. Swapping a preload would put a second copy of the
  ;; file in the page and style nothing.
  (when (compiling?)
    (binding [cljs/*target* :node]
      (document! {:origin "http://localhost:8082"
                  :links [{:rel "preload" :href "http://localhost:8082/css/main.css"}
                          {:rel "stylesheet"}
                          {:rel "stylesheet alternate" :href "http://localhost:8082/css/main.css"}]})
      (let [r (reload "/home/me/app/css/main.css")]
        (is (= ["http://localhost:8082/css/main.css"] (:reloaded r))
            "rel~=stylesheet, so `stylesheet alternate' is one and `preload' is not")
        (is (= 1 (count (:stylesheets r))))))))

(deftest test-an-imported-sheet-is-not-seen-at-all
  ;; It has no node, so there is nothing to swap. Replique 1 read
  ;; document.styleSheets and had to filter it back down to the ones that had an
  ;; ownerNode; reading the DOM starts where that filter ended - and finds the
  ;; one case that list misses, a <link> whose stylesheet 404'd, which is in the
  ;; document and not in document.styleSheets.
  (when (compiling?)
    (binding [cljs/*target* :node]
      (document! {:origin "http://localhost:8082"
                  :links [{:rel "stylesheet" :href "http://localhost:8082/css/main.css"}]
                  :sheets ["http://localhost:8082/css/imported.css"]})
      (let [r (reload "/home/me/app/css/imported.css")]
        (is (= [] (:reloaded r)))
        (is (= ["http://localhost:8082/css/main.css"] (:stylesheets r)))))))

;;; What comes back, and what is raised

(deftest test-a-page-that-could-not-be-asked-is-a-note-and-not-an-error
  ;; It is a FACT ABOUT THE PAGE: nobody has it open, or it is in the middle of
  ;; the require you sent it a moment ago. The sentence the transport wrote for
  ;; each of those is already the one somebody needs - `nobody-connected's names
  ;; the URL to open - so it comes back as written, on a reply that reloaded
  ;; nothing. Raising it would make the client turn an error frame back into the
  ;; sentence it started as.
  (let [note "No browser is connected. Open http://127.0.0.1:59280/ ..."]
    (is (= {:reloaded [] :stylesheets [] :note note}
           (#'css/reply-of {:status :error :value note}))))
  (is (= {:reloaded [] :stylesheets [] :note "The runtime was busy for the whole 2000ms: nothing was evaluated."}
         (#'css/reply-of {:status :error :phase :repl
                          :value "The runtime was busy for the whole 2000ms: nothing was evaluated."}))))

(deftest test-a-script-that-threw-is-raised
  ;; Which nothing about the page explains: the script is replique's, and a page
  ;; that answered by throwing is a bug here. Reported as a note it would read as
  ;; "nothing was reloaded, and here is a JavaScript stack trace" - an answer
  ;; about the page, for something that is not about the page.
  (let [thrown (try (#'css/reply-of {:status :error
                                     :value "TypeError: x is not a function"
                                     :stacktrace "at eval ..."})
                    nil
                    (catch clojure.lang.ExceptionInfo e e))]
    (is (some? thrown))
    (is (= :reload-css (:replique/error (ex-data thrown))))
    (is (re-find #"TypeError" (.getMessage thrown)))))

(deftest test-what-the-page-answered-is-read-twice
  ;; The value of an evaluation is what the runtime PRINTED, so a JavaScript
  ;; string arrives as a printed string: the first read takes the quoting off
  ;; and the second reads the map that was inside it.
  (is (= {:reloaded ["a"] :stylesheets []}
         (#'css/reply-of {:status :success
                          :value (pr-str "{:reloaded [\"a\"] :stylesheets []}")}))))

;;; The op

(deftest test-the-op-needs-a-file
  ;; And says so rather than asking the page to match nothing, which would come
  ;; back as a reload that found nothing - an answer about the page for a
  ;; mistake in the message.
  (let [thrown (try (protocol/handle nil {:op :reload-css :file 42}) nil
                    (catch clojure.lang.ExceptionInfo e e))]
    (is (some? thrown))
    (is (= :invalid-message (:replique/error (ex-data thrown))))))

(deftest test-a-process-without-the-compiler-says-so
  ;; The way to a page is the ClojureScript browser runtime - the websocket, the
  ;; asset server, the client the page imported - so a process without the
  ;; compiler has no way to reach one. What it names is the classpath, which is
  ;; what somebody would have to change.
  (when-not (compiling?)
    (let [thrown (try (protocol/handle nil {:op :reload-css :file "css/main.css"}) nil
                      (catch clojure.lang.ExceptionInfo e e))]
      (is (some? thrown))
      (is (= :no-cljs (:replique/error (ex-data thrown)))))))

(deftest test-with-no-page-open-the-reply-says-where-to-open-one
  ;; The whole path, for real, and the one test here that uses the browser: a
  ;; request frame, the op, the browser runtime starting because the URL it is
  ;; listening on IS the answer, the transport's sentence arriving as a note,
  ;; and a reply frame. It needs no page, which is the point of it - a repl
  ;; started before the browser was opened is the normal way round.
  ;;
  ;; IN A PROCESS OF ITS OWN, which is not ceremony: the asset server's
  ;; dispatcher is not a daemon thread, so a jvm that started one and did not
  ;; stop it never exits. `core/stop!' is what closes it, and a process is what
  ;; has one to close.
  (when (compiling?)
    (client/with-process [info nil]
      (let [c (client/control-client info)]
        (try
          (let [r (client/request! c {:op :reload-css
                                      :file "/home/me/app/css/main.css" :id 1})]
            (is (= "reply" (:tag r)) (pr-str r))
            (is (= [] (:reloaded r)))
            (is (= [] (:stylesheets r)))
            (is (re-find #"http://" (str (:note r))) "and names the URL to open"))
          (finally (client/disconnect c)))))))
