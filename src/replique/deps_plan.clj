(ns replique.deps-plan
  "What the deps files say the classpath should be now, against what the
  process is running with - and whether the difference can be added to a
  running process or needs a new one.

  THE QUESTION IS ASKED OF THE DEPS TOOL AND NOT WORKED OUT HERE. A basis is
  what the clojure cli computes out of deps.edn, the user's deps.edn, -Sdeps
  and the aliases, and computing it again is the only way to know what it
  would be now: an alias pulls in a library that pulls in another, an
  exclusion moves a version two levels down. So the process asks the cli, the
  way clojure.repl.deps does, with the configuration it was started with - the
  one the cli wrote into the basis it handed over - and whatever the client
  says has changed in it since.

  WHAT CAN BE ADDED IS WHAT IS NEW. A library or a source directory the
  process does not have is put on the loader every connection shares, behind
  everything already there, and a process that started with it would have had
  it too. Everything else is a restart:

    a library at another version, or another commit, or another directory -
    the old one is on the application loader, which is asked first, and
    whatever was loaded from it stays loaded;

    a namespace the new entry provides that the classpath already provides -
    behind what is there it would never be the one found, and a process
    started on the new deps might find it first;

    jvm options, which are the jvm's own and are read once;

    a directory the process was started with that is somewhere else now - see
    `replique.classpath/frozen'.

  WHAT IS GONE IS SAID AND NOT ACTED ON. A loader cannot be made to forget a
  url, so a library taken out of deps.edn stays loadable until the process
  restarts - which is usually harmless, and is the one thing here the client
  only warns about."
  (:require [clojure.edn :as edn]
            [clojure.java.basis :as basis]
            [clojure.java.basis.impl :as basis-impl]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.tools.deps.interop :as tool]
            [replique.classpath :as classpath]
            [replique.state :as state])
  (:import [clojure.lang DynamicClassLoader RT]
           [java.net URL]))

;; The source directories added since the process started. A library added is
;; written into the basis, which is where clojure.repl.deps keeps what it has
;; added, and a directory is not a library: a basis has no place to say it was
;; added later, so it is kept here, for the next question to count as had.
(defonce ^:private added-paths (atom #{}))

(defn- configuration
  "The configuration to compute a basis with: the one the process was started
  with, and what MSG says has changed in it since.

  :aliases are the aliases to start with now and :extra the text of the deps
  -Sdeps would be given now. Either one left out is unchanged, which is not
  the same thing as empty: a client that has no reason to think the command
  line changed says nothing about it, and the process was started the way it
  was started."
  [msg]
  (let [started (:basis-config (basis/initial-basis))]
    (cond-> started
      (contains? msg :aliases) (assoc :aliases (vec (:aliases msg)))
      (contains? msg :extra) (assoc :extra (let [text (:extra msg)]
                                             (when-not (or (nil? text) (= "" text))
                                               (edn/read-string text)))))))

(defn- wanted
  "The basis the deps files make of CONFIGURATION today, as the cli computes
  it. Seconds, and the network where something is not downloaded yet."
  [configuration]
  (tool/invoke-tool {:tool-alias :deps
                     :fn 'clojure.tools.deps/create-basis
                     :args configuration}))

(defn- absolute
  "PATH as the process would open it: relative to where it was started."
  ^String [path]
  (let [file (io/file path)]
    (str (.normalize (.toPath (if (.isAbsolute file)
                                file
                                (io/file (System/getProperty "user.dir") (str path))))))))

(defn- source-paths
  "The directories of BASIS that belong to the project rather than to a
  library, made absolute."
  [basis]
  (into #{} (keep (fn [[path {:keys [path-key]}]] (when path-key (absolute path))))
        (:classpath basis)))

(defn- spelt
  "How the library at COORDINATE is told apart from another of itself, in a
  sentence."
  [coordinate]
  (or (:mvn/version coordinate)
      (some-> (:git/sha coordinate) (subs 0 (min 7 (count (:git/sha coordinate)))))
      (:git/tag coordinate)
      (:local/root coordinate)
      (pr-str (dissoc coordinate :paths :parents :dependents :deps/manifest))))

(defn- moved
  "The libraries both have, where WANTED has them somewhere CURRENT does not.

  Somewhere and not at another version: what a library is on a classpath is
  the paths it was resolved to, and a version, a commit and a local directory
  each move them. The paths are also what has to stay put for a library to
  be the same one - a :local/root re-resolved to another directory is the
  same coordinate and another library."
  [current wanted]
  (into [] (keep (fn [[lib was]]
                   (when-let [now (get wanted lib)]
                     (when (not= (:paths was) (:paths now))
                       {:lib (str lib) :was (spelt was) :now (spelt now)}))))
        (sort-by (comp str key) current)))

(def ^:private merged
  "What reads as a namespace and is not one: every data_readers.clj(c) on the
  classpath is read and what they say is merged, so one more of them hides
  nothing - and libraries ship them as often as not."
  #{"data-readers"})

(defn- shadowed
  "What the entries at PATHS would provide that the classpath already does,
  as {:entry :namespaces}, the namespaces a few at most."
  [paths]
  (let [{:keys [namespaces cljs-namespaces]} (classpath/scan)
        had (set/difference (set/union (set namespaces) (set cljs-namespaces)) merged)]
    (into [] (keep (fn [path]
                     (let [{:keys [namespaces cljs-namespaces]} (classpath/entry-names path)
                           both (sort (filter had (distinct (concat namespaces cljs-namespaces))))]
                       (when (seq both)
                         {:entry path :namespaces (vec (take 5 both))}))))
          paths)))

(defn- verdict [{:keys [moved jvm-opts frozen shadowed added-libs added-paths]}]
  (cond
    (or (seq moved) jvm-opts (seq frozen) (seq shadowed)) :restart
    (or (seq added-libs) (seq added-paths)) :additive
    :else :current))

(defn- compare-with
  "The plan for going from what the process has to WANTED, the libraries to
  add carried under ::libs for `apply!'."
  [wanted]
  (let [initial (basis/initial-basis)
        current (:libs (basis/current-basis))
        libs (:libs wanted)
        new-libs (into {} (remove (fn [[lib _]] (contains? current lib))) libs)
        had-paths (into (source-paths initial) @added-paths)
        wanted-paths (source-paths wanted)
        new-paths (vec (sort (remove had-paths wanted-paths)))
        was-opts (:jvm-opts (:argmap initial))
        now-opts (:jvm-opts (:argmap wanted))
        plan {:added-libs (vec (for [[lib coordinate] (sort-by (comp str key) new-libs)]
                                 {:lib (str lib) :now (spelt coordinate)}))
              :added-paths new-paths
              :removed-libs (vec (sort (map str (remove #(contains? libs %) (keys current)))))
              :removed-paths (vec (sort (remove wanted-paths had-paths)))
              :moved (moved current libs)
              :jvm-opts (when (not= (vec was-opts) (vec now-opts))
                          {:was (vec was-opts) :now (vec now-opts)})
              :frozen (classpath/frozen)
              :shadowed (shadowed (concat (mapcat :paths (vals new-libs)) new-paths))}]
    (assoc plan :verdict (verdict plan) ::libs new-libs)))

(defn plan
  "What MSG's configuration makes of the classpath now, against what the
  process has. See the namespace docstring for the verdict, and doc/protocol.md
  for the shape."
  [msg]
  (dissoc (compare-with (wanted (configuration msg))) ::libs))

(defn- url
  "PATH as a url a loader reads it by: a jar where it is a file, and a
  directory everywhere else.

  Everywhere else, and not where it is a directory: a url names a directory
  by ending in a slash, and File.toURI ends it with one only where the
  directory exists by then. A source directory deps.edn names before anybody
  has made it - or one the stage has no link to yet - would go on the loader
  as a jar, which is opened once, found wanting, and never asked again."
  ^URL [path]
  (let [file (io/file path)]
    (if (.isFile file)
      (RT/toUrl file)
      (let [uri (str (.toURI file))]
        (URL. (if (.endsWith uri "/") uri (str uri "/")))))))

(defn apply!
  "Put on the classpath what the plan for MSG adds, when adding is all it
  does, and answer the names of what was added.

  Planned again rather than taken from the client: what is added has to be
  what the deps files say now, and the client is answering a question asked a
  moment ago. A plan that has become a restart since is refused, saying why -
  the client asked to add something, and adding half of what changed would be
  a process that matches neither deps file.

  Expects *data-readers* bound, as clojure.repl.deps does: a library is where
  data_readers.cljc files come from, and what they register is merged in."
  [msg]
  (let [{:keys [verdict added-libs] ::keys [libs] :as plan}
        (compare-with (wanted (configuration msg)))
        paths (:added-paths plan)]
    (case verdict
      :current []
      :restart (throw (ex-info (str "The classpath has changed in a way a running process "
                                    "cannot take - restart it")
                               {:replique/error :restart-needed
                                :plan (dissoc plan ::libs)}))
      :additive
      (let [loader ^DynamicClassLoader state/class-loader]
        (doseq [path (concat (mapcat :paths (vals libs)) paths)]
          (.addURL loader (url path)))
        (basis-impl/update-basis! update :libs merge libs)
        (swap! added-paths into paths)
        (set! *data-readers* (merge (#'clojure.core/load-data-readers) *data-readers*))
        ;; ClojureScript reads its own tags out of the same files, and keeps
        ;; what it read for the life of the process - see
        ;; clojure.cljs.reader/user-data-readers. Asked to read them again if
        ;; it has been loaded at all - a compiler that has read nothing yet
        ;; has nothing to forget, and loading it here to say so would cost
        ;; what starting it costs.
        (when (find-ns 'clojure.cljs.reader)
          (when-let [forget (resolve 'clojure.cljs.reader/forget-data-readers!)]
            (forget)))
        (into (mapv :lib added-libs) paths)))))
