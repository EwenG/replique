(ns replique.analysis-fuzz-test
  "Projects nobody laid out, edited in ways nobody would edit them.

  What replique asks the analysing clojure for is small - load this file,
  load everything that changed, say what would be loaded, say where this name
  is used - and what those answers depend on is not small at all. It is the
  shape of a codebase: which file requires which, which of those requires is
  expanded at compile time and which is called at run time, which files were
  ever loaded and which were only written, and what has been saved since.
  Every test written by hand is one shape out of that, chosen by somebody who
  was thinking of a case - and the cases nobody thinks of are the ones where
  a reload quietly loads one file too few.

  So the shapes are generated. A project here is a handful of files that
  require each other downwards, each defining a macro, a keyword, a class use
  and a function whose value is arithmetic - and each edit rerolls both the
  number the macro expands to and whether a required file is reached through
  its macro, through its function, or not at all. Which is the whole of what
  the reload turns on, written as something that can be counted.

  Counted is the point. There is an expected answer to every one of these,
  and it is not a shape of the answer but the answer: what a file's value
  must be once everything that had to be loaded has been, which files a
  reload must name, in which order, and which files a name is used in. All of
  it is worked out here, from the sources this generated and from what
  `load' and `require' do with them - a file loaded by its path is compiled
  again, a file reached by a require is compiled only the first time - and
  the process is asked the same question and made to agree.

  The strongest of them is the value. A list of stale files that leaves one
  out is a list that still reads plausibly; a function that answers 14 where
  the sources now say 17 is a file holding an expansion of a macro nobody
  reloaded, which is the bug the whole macro cascade exists to prevent. So
  every value of every loaded file is asked for after every reload.

  Seeded and fixed, so a run that fails fails again. What it reports is the
  shape of the project and the number of the run that wrote it - and that
  number is the seed, so the edits behind a failure are the same edits the
  next time, and the whole of it can be walked through and written out by
  hand as a test of its own once it has been found.

  Written for both clojures, like everything else that reads an analysis. A
  process that records nothing cannot be asked what changed, and is asked
  instead to load the whole project in order - which is what somebody without
  a recording compiler does by hand - and must arrive at the same values."
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [replique.classpath :as classpath]
            [replique.ops]
            [replique.state :as state]
            [replique.test-client :as client
             :refer [control-client disconnect eval! repl-client request! with-process]]))

;;; Files on a disk

(def ^:private tick
  "How far forward the next file written is stamped.

  The time a file was last modified is the whole of what says it changed, and
  a filesystem writes it as coarsely as it likes: two writes inside one tick
  of it carry one time, and the second edit would be a file nothing had
  touched. So every write here is stamped further into the future than the
  one before it, and no edit can be lost to how fast the machine is."
  (atom 0))

(defn- written-file!
  "Write TEXT into DIR under NAME, stamped later than anything before it."
  [dir name text]
  (let [f (java.io.File. (str dir) (str name))]
    (.mkdirs (.getParentFile f))
    (spit f text)
    (.setLastModified f (+ (System/currentTimeMillis) (* 10000 (long (swap! tick inc)))))
    (.getPath f)))

(defn- source-root!
  "A directory the process reads names off the classpath, and its path.

  Added to the loader the whole process shares, and the classpath read again
  afterwards, because that reading is what says which entries of it are
  directories - and a file under no directory of the classpath is a file the
  analysis cannot name and does not record."
  []
  (let [dir (client/temp-dir)]
    (.addURL state/class-loader (.toURL (.toURI (java.io.File. (str dir)))))
    (state/adopt-class-loader!)
    (classpath/rescan!)
    dir))

;;; A project nobody wrote

(defn- pick [^java.util.Random random coll]
  (nth coll (.nextInt random (count coll))))

(def ^:private reached
  "What the generated projects turned out to reach.

  A fuzz test says nothing unless its generator is still writing the projects
  it was written to write, and nothing else here would say that it is: a
  generator that drifted into projects where no file is ever made stale by a
  macro somewhere else would go on passing, and would be checking that an
  empty list equals an empty list. So what was reached is counted while the runs
  go, and the counts are asserted like everything else."
  (atom {}))

(defn- reached! [k] (swap! reached update k (fnil inc 0)) nil)

(defn- shape
  "The files of a project and which of them requires which.

  Downwards only - a file requires files written before it - because a
  require cycle is a project that does not load, and what is being fuzzed is
  what happens to projects that do. RUN names it, so that a process running
  several of these keeps them apart: the namespaces of one project must be
  names no other project has used, or a require would find one already
  loaded and compile nothing."
  [^java.util.Random random run]
  (let [n (+ 3 (.nextInt random 5))]
    {:run run
     :prefix (str "probe.r" run)
     :count n
     :requires (vec (for [i (range n)]
                      (vec (for [j (range i) :when (pos? (.nextInt random 3))] j))))}))

(defn- revised
  "What file I says this time round: a number, and what it does with each
  file it requires.

  Three things it can do with one, and they are three different facts about
  the reload. A macro is expanded where it is used, so the number is baked
  into this file and this file is wrong until it is compiled again. A
  function is called through its var, so the number is read when it runs and
  nothing here goes stale. Neither is neither - and a require that was a
  macro use last time and is unused now is an edge the model has to let go
  of, which is the half of the cascade an edit that only ever adds would
  never reach.

  Macros twice over, because they are the case the whole thing is about."
  [^java.util.Random random shape i]
  {:k (.nextInt random 10)
   :roles (into (sorted-map)
                (for [j (nth (:requires shape) i)]
                  [j (pick random [:macro :macro :macro :fn :fn :unused])]))})

(defn- ns-of [shape i] (str (:prefix shape) ".f" i))

(defn- path-of
  "What the classpath calls file I - which is what the model calls it too."
  [shape i]
  (str (string/replace (:prefix shape) "." "/") "/f" i ".clj"))

(defn- text-of
  "File I, as this revision of it says to write it.

  A macro, so that another file can be made stale by editing this one. A
  keyword and a class, so that the other two things the model records are
  recorded and can be asked about. And a function whose value is a sum, which
  is where all of it is made visible: its own number, the number each macro
  it expands stood for when this file was compiled, and whatever the
  functions it calls answer now."
  [shape i {:keys [k roles]}]
  (let [reqs (nth (:requires shape) i)]
    (str "(ns " (ns-of shape i)
         (if (seq reqs)
           (str "\n  (:require "
                (string/join "\n            "
                             (for [j reqs] (str "[" (ns-of shape j) " :as a" j "]")))
                "))\n")
           ")\n")
         "\n(defmacro constant [] " k ")\n"
         "\n(def marker :" (:prefix shape) "/tag)\n"
         "\n(defn made [] (java.util.Date.))\n"
         "\n(defn value []\n  (+ " k
         (apply str (for [[j role] roles :when (not= :unused role)]
                      (str "\n     (a" j "/" (if (= :macro role) "constant" "value") ")")))
         "))\n")))

;;; What the process must be holding

;; The state of a project is two halves that drift apart and are brought back
;; together: what is on the disk, and what this process compiled. A file's
;; :gen counts the times it has been written, and the model's copy of it
;; remembers which of those it read - so a file is changed when the two
;; disagree, which is the same question `changed-files' asks the filesystem.
;;
;; :compiled is what the analysis holds; :required is what `require' holds,
;; and they are not the same set. Loading a file by its path compiles it and
;; tells `require' nothing, so a file loaded that way is compiled again the
;; first time anything requires it. Getting that wrong here would make this
;; test disagree with the process about which files are in the model at all.

(defn- fresh-state [^java.util.Random random shape]
  {:revs (mapv #(revised random shape %) (range (:count shape)))
   :gen (vec (repeat (:count shape) 0))
   :compiled {}
   :required #{}})

(defn- compiled! [state i]
  (assoc-in state [:compiled i] {:gen (nth (:gen state) i) :rev (nth (:revs state) i)}))

(defn- required! [state shape j]
  (if (contains? (:required state) j)
    state
    (-> (reduce #(required! %1 shape %2) state (nth (:requires shape) j))
        (compiled! j)
        (update :required conj j))))

(defn- loaded!
  "State after file I is loaded by its path: what it requires is required
  first, and then it is compiled - whether or not it had been."
  [state shape i]
  (-> (reduce #(required! %1 shape %2) state (nth (:requires shape) i))
      (compiled! i)))

(defn- edited! [state ^java.util.Random random shape i]
  (-> state
      (assoc-in [:revs i] (revised random shape i))
      (update-in [:gen i] inc)))

(defn- changed-of
  "The compiled files the disk has moved on from."
  [state]
  (set (for [[i {:keys [gen]}] (:compiled state)
             :when (not= gen (nth (:gen state) i))]
         i)))

(defn- dependents-of
  "For each file, the compiled files that expand a macro of it.

  Read off what each file was compiled as rather than off what it says now,
  because that is what the model holds: an edit that adds a macro use adds an
  edge nothing knows about until the file has been compiled once."
  [state]
  (reduce (fn [acc [i {:keys [rev]}]]
            (reduce (fn [acc [j role]]
                      (if (= :macro role) (update acc j (fnil conj #{}) i) acc))
                    acc (:roles rev)))
          {} (:compiled state)))

(defn- stale-of
  "The compiled files that did not change and are out of date all the same."
  [state changed]
  (let [dependents (dependents-of state)]
    (loop [dirty (set changed) queue (vec changed)]
      (if-let [x (peek queue)]
        (let [fresh (remove dirty (get dependents x))]
          (recur (into dirty fresh) (into (pop queue) fresh)))
        (set (remove (set changed) dirty))))))

(defn- value-of
  "What file I's function must answer, once nothing is out of date.

  Which is a question about the sources alone: a macro is worth what the file
  that defines it says now, because a file holding an older expansion would
  be a file the reload had to have loaded. Only sound where nothing is stale,
  and that is where it is asked."
  [state i]
  (let [{:keys [k roles]} (nth (:revs state) i)]
    (reduce (fn [total [j role]]
              (case role
                :macro (+ total (:k (nth (:revs state) j)))
                :fn (+ total (value-of state j))
                :unused total))
            k roles)))

;;; Asking the process

(defn- analysing?
  "Whether the process running this test records what the compiler resolved."
  [c]
  (true? (:analysis (request! c {:op :process-info :id 1}))))

(defn- load!
  "Load the file at PATH, and say what went wrong when something did."
  [r path]
  (let [frames (eval! r (str "#replique/load " (pr-str {:file path})))]
    (when-let [thrown (client/frame-tagged frames "exception")]
      (str "loading " path " threw: " (:message thrown)))))

(defn- answered
  "What the repl answered CODE with, or nil where it threw."
  [r code]
  (:value (client/frame-tagged (eval! r code) "ret")))

(defn- reload!
  "Ask for everything that changed to be loaded, as [order problem]."
  [r]
  (let [frames (eval! r "#replique/reload {}")]
    (if-let [thrown (client/frame-tagged frames "exception")]
      [nil (str "the reload threw: " (:message thrown))]
      [(edn/read-string (:value (client/frame-tagged frames "ret"))) nil])))

(defn- indices-of
  "The files of SHAPE that FOUND names, and nil for one it does not.

  A nil is not dropped: the lists are this project's whole answer, and a file
  in one of them that belongs to no file of this project is exactly the kind
  of thing worth failing over."
  [shape found]
  (mapv (fn [path] (some #(when (string/ends-with? (str path) (path-of shape %)) %)
                         (range (:count shape))))
        found))

(defn- said
  "A set of file numbers, written the way a failure reads best."
  [shape files]
  (vec (sort (map #(if % (path-of shape %) %) files))))

;;; What has to be true after a reload

(defn- wrong-values
  "A loaded file whose function does not answer what the sources say."
  [r shape state]
  (some (fn [i]
          (let [expected (str (value-of state i))
                answered (answered r (str "(" (ns-of shape i) "/value)"))]
            (when-not (= expected answered)
              (str (path-of shape i) " answers " (pr-str answered)
                   " where the sources say " expected))))
        (sort (keys (:compiled state)))))

(defn- wrong-stale
  "The two lists said wrongly, or nil when they are said rightly."
  [c shape state]
  (let [found (request! c {:op :stale :id 1})
        changed (changed-of state)
        stale (stale-of state changed)
        of (fn [k] (set (indices-of shape (map :file (get found k)))))]
    (or (when (= "error" (:tag found)) (str "what is stale was refused: " (:message found)))
        (when-not (= changed (of :changed))
          (str "the changed files are " (said shape (of :changed))
               " where the disk says " (said shape changed)))
        (when-not (= stale (of :stale))
          (str "the stale files are " (said shape (of :stale))
               " where the macros say " (said shape stale)))
        ;; a filter rather than an intersection, because a require of
        ;; clojure.set here would be clojure.set loaded into the process
        ;; every test starts its repl in - and one of those tests is about a
        ;; namespace that exists only as a file until something requires it
        (when (seq (filter (of :changed) (of :stale)))
          "a file is in both lists"))))

(defn- wrong-order
  "A file in ORDER loaded before a file whose macro it expands.

  Read off what the files say now, which is what they were compiled as: the
  reload has run, so the graph it ended on is the graph of the new sources -
  and a file that expanded a macro of a file loaded after it expanded the old
  one."
  [shape state order]
  (let [place (zipmap order (range))]
    (some (fn [i]
            (some (fn [[j role]]
                    (when (and (= :macro role) (place j) (> (place j) (place i)))
                      (str (path-of shape i) " was loaded before " (path-of shape j)
                           ", whose macro it expands")))
                  (:roles (nth (:revs state) i))))
          order)))

;;; What has to be true of a usage

(defn- written-at
  "The text the span points at, read off the file it names."
  [{:keys [file line column end-line end-column]}]
  (when (and file line column end-column (= line end-line))
    (let [lines (string/split-lines (slurp file))]
      (when (<= 1 line (count lines))
        (let [^String text (nth lines (dec line))]
          (when (<= (dec end-column) (.length text))
            (subs text (dec column) (dec end-column))))))))

(defn- wrong-usages
  "Where NAME of file J is said to be used, said wrongly.

  Two things at once, and both are answers rather than shapes. The files are
  the files that use it, which this knows because it wrote them: a name
  reached through a macro is used by exactly the files whose require of it is
  a macro use. And each usage is a place in a file, so the text at that place
  is read back off the disk - it has to be the name as it is written there,
  which is the alias and not the namespace, and the name and not the form it
  is written in."
  [c shape state j name role]
  (let [found (request! c {:op :usages :id 1 :position :code
                           :ns (ns-of shape j) :text name})
        expected (set (for [[i {:keys [rev]}] (:compiled state)
                            :when (= role (get (:roles rev) j))]
                        (ns-of shape i)))
        usages (:usages found)]
    (when (seq usages) (reached! (if (= :macro role) :macro-usage :call-usage)))
    (or (when (= "error" (:tag found))
          (str "the usages of " name " were refused: " (:message found)))
        (when-not (= expected (set (map :from-ns usages)))
          (str (ns-of shape j) "/" name " is used from "
               (vec (sort (map :from-ns usages))) " where the sources say "
               (vec (sort expected))))
        (when-not (= (count expected) (count usages))
          (str (ns-of shape j) "/" name " is used " (count usages)
               " times from " (count expected) " files"))
        (some (fn [usage]
                (let [text (written-at usage)]
                  (when-not (= (str "a" j "/" name) text)
                    (str "a usage of " (ns-of shape j) "/" name " in "
                         (:from-ns usage) " points at " (pr-str text)))))
              usages)
        ;; a macro is said to be one, and a function is not said to be one:
        ;; what a client does with the answer is show where the name is used,
        ;; and a usage it cannot see - because the macro that wrote it is
        ;; expanded where nobody typed it - is not the same thing to show
        (some (fn [usage]
                (when-not (= (= :macro role) (true? (:macro usage)))
                  (str "a usage of " (ns-of shape j) "/" name " in "
                       (:from-ns usage) " is " (if (:macro usage) "" "not ")
                       "said to be a macro")))
              usages))))

(defn- wrong-keyword-usages
  "The keyword every file of the project writes, said wrongly.

  One per file that was loaded, because every file writes it once - which is
  the thing a keyword can be asked and a var cannot: what it is is not what
  it is written as anywhere, and there is no var to hang the question on."
  [c shape state]
  (let [text (str ":" (:prefix shape) "/tag")
        found (request! c {:op :usages :id 1 :position :code
                           :ns (ns-of shape 0) :text text})
        expected (set (map #(ns-of shape %) (keys (:compiled state))))
        usages (:usages found)]
    (or (when (= "error" (:tag found))
          (str "the usages of " text " were refused: " (:message found)))
        (when-not (= expected (set (map :from-ns usages)))
          (str text " is used from " (vec (sort (map :from-ns usages)))
               " where the sources say " (vec (sort expected))))
        (some (fn [usage]
                (when-not (= text (written-at usage))
                  (str "a usage of " text " in " (:from-ns usage) " points at "
                       (pr-str (written-at usage)))))
              usages))))

(defn- wrong-class-usages
  "The class every file of the project names, said wrongly.

  Which is the third thing the model records, and the one whose answer is not
  this project's alone: a class is used all over a codebase, and what keeps
  this to the files it wrote is that the projects of the runs before it have
  been deleted off the disk. A usage whose file is gone reaches nothing and
  is left out - which is the same rule a deleted file is answered by
  everywhere else, read here as a way of asking one run at a time."
  [c shape state]
  (let [found (request! c {:op :usages :id 1 :position :code
                           :ns (ns-of shape 0) :text "java.util.Date"})
        expected (set (map #(ns-of shape %) (keys (:compiled state))))
        usages (:usages found)]
    (or (when (= "error" (:tag found))
          (str "the usages of java.util.Date were refused: " (:message found)))
        (when-not (= expected (set (map :from-ns usages)))
          (str "java.util.Date is used from " (vec (sort (map :from-ns usages)))
               " where the sources say " (vec (sort expected))))
        (some (fn [usage]
                (let [text (written-at usage)]
                  (when-not (and text (string/starts-with? text "java.util.Date"))
                    (str "a usage of java.util.Date in " (:from-ns usage)
                         " points at " (pr-str text)))))
              usages))))

(defn- wrong-anywhere
  "Every usage of the project asked about, and the first said wrongly."
  [c shape state]
  (or (some (fn [j]
              (or (wrong-usages c shape state j "constant" :macro)
                  (wrong-usages c shape state j "value" :fn)))
            (sort (keys (:compiled state))))
      (wrong-keyword-usages c shape state)
      (wrong-class-usages c shape state)))

;;; A run

(defn- settled
  "Load what has to be loaded, and answer [state problem].

  Which is two different things to do. A process that recorded what it
  compiled is asked what changed and told to load it, and what it loads is
  checked against what this worked out it would have to. A process that
  recorded nothing cannot be asked - so the whole project is loaded in order,
  by hand, which is what somebody without a recording compiler does, and the
  values it must arrive at are the same ones."
  [r c shape state analysing]
  (if analysing
    (or (when-let [problem (wrong-stale c shape state)] [state problem])
        (let [expected (let [changed (changed-of state)
                             stale (stale-of state changed)]
                         (when (seq changed) (reached! :changed))
                         (when (seq stale) (reached! :cascade))
                         (when (< 1 (count stale)) (reached! :wide-cascade))
                         (into changed stale))
              [order problem] (reload! r)
              loaded (indices-of shape order)]
          (or (when problem [state problem])
              (when-not (= expected (set loaded))
                [state (str "the reload loaded " (said shape loaded)
                            " where what is out of date is " (said shape expected))])
              (when-not (= (count order) (count expected))
                [state (str "the reload loaded " (count order) " files to load "
                            (count expected))])
              (let [state (reduce compiled! state loaded)]
                [state (wrong-order shape state loaded)]))))
    (loop [state state i 0]
      (if (= i (:count shape))
        [state nil]
        (if-let [problem (load! r (str (:root shape) "/" (path-of shape i)))]
          [state problem]
          (recur (loaded! state shape i) (inc i)))))))

(defn- refusals
  "What a process that records nothing must say when it is asked anyway.

  Both of them, because they are two refusals: one is a repl being told to
  load what it cannot know has changed, and the other is a question asked on
  the control connection about a process that cannot answer it. A test that
  only fuzzed the process which can answer would leave the other half of
  every one of these unread."
  [r c]
  (let [thrown (:message (client/frame-tagged (eval! r "#replique/reload {}") "exception"))
        found (request! c {:op :stale :id 1})]
    (or (when-not (string/includes? (or thrown "") "keep track of what it compiled")
          (str "a reload it cannot do was answered with " (pr-str thrown)))
        (when-not (and (= "error" (:tag found))
                       (string/includes? (:message found) "keep track of what it compiled"))
          (str "what is stale was answered with " (pr-str found))))))

(defn- problem-in
  "A project of RANDOM, written, loaded, edited and loaded again - and the
  first thing the process said about it that is not so, or nil.

  The project is deleted afterwards, which is not only tidying: a file the
  model recorded and the disk no longer holds is skipped by everything that
  reads it, so the project of one run is out of the way of the next without
  anything having to be forgotten."
  [r c ^java.util.Random random run analysing]
  (let [root (source-root!)
        shape (assoc (shape random run) :root (str root))]
    (try
      (let [state (fresh-state random shape)]
        (doseq [i (range (:count shape))]
          (written-file! root (path-of shape i) (text-of shape i (nth (:revs state) i))))
        (let [entries (or (seq (for [i (range (:count shape))
                                     :when (zero? (.nextInt random 2))]
                                 i))
                          [(dec (:count shape))])
              problem
              (loop [state state entries entries]
                (if-let [i (first entries)]
                  (or (load! r (str root "/" (path-of shape i)))
                      (recur (loaded! state shape i) (next entries)))
                  ;; loaded, and nothing edited yet: what is out of date is
                  ;; nothing, which is an answer worth being sure of before
                  ;; anything is made out of date on purpose
                  (loop [state state step 0]
                    (let [[state problem] (settled r c shape state analysing)]
                      (or problem
                          (wrong-values r shape state)
                          (when analysing (wrong-anywhere c shape state))
                          (when (< step 4)
                            ;; mostly a file the model holds, because editing
                            ;; one it never read is a thing that has to change
                            ;; nothing and is not a thing to spend a run on
                            (let [held (vec (sort (keys (:compiled state))))
                                  one #(if (and (seq held) (pos? (.nextInt random 4)))
                                         (pick random held)
                                         (.nextInt random (:count shape)))
                                  edited (into #{(one)}
                                               (when (zero? (.nextInt random 2)) [(one)]))
                                  state (reduce #(edited! %1 random shape %2) state edited)]
                              (doseq [i edited]
                                (written-file! root (path-of shape i)
                                               (text-of shape i (nth (:revs state) i))))
                              ;; and now and then a file loaded on its own,
                              ;; which is what an editor does all day: it
                              ;; brings a file the model never held into it,
                              ;; and takes another one out of date on its own
                              (let [pulled (when (zero? (.nextInt random 3))
                                             (.nextInt random (:count shape)))]
                                (or (when pulled
                                      (load! r (str root "/" (path-of shape pulled))))
                                    (recur (if pulled (loaded! state shape pulled) state)
                                           (inc step)))))))))))]
          (when problem
            {:run run :shape (dissoc shape :root) :problem problem})))
      (finally (client/delete-recursively root)))))

(deftest nothing-a-project-can-be-shaped-like-is-loaded-wrongly
  (testing "what a reload has to load is decided by the shape of a codebase -
  what requires what, which of those requires expands a macro, what was ever
  loaded and what has been saved since - and a handful of shapes written by
  hand is a handful out of all of them"
    (with-process [info nil]
      (let [r (repl-client info)
            c (control-client info)
            analysing (analysing? c)]
        (try
          (reset! reached {})
          (let [problem (some (fn [run]
                                (problem-in r c (java.util.Random. (+ 7 run)) run analysing))
                              (range 12))]
            (is (nil? problem))
            ;; and only where nothing failed, since a run that stopped at the
            ;; first project reached little of anything and would say so at
            ;; length, over the top of the one failure worth reading
            (when (and analysing (nil? problem))
              (testing "the projects were the projects this was written to
              write: files made stale by a macro in another file, several of
              them at a time, and names used through a macro and through a
              call - a generator drifted away from those would pass this by
              checking that nothing equals nothing"
                (is (<= 20 (:changed @reached 0)) (str @reached))
                (is (<= 8 (:cascade @reached 0)) (str @reached))
                (is (<= 4 (:wide-cascade @reached 0)) (str @reached))
                (is (<= 40 (:macro-usage @reached 0)) (str @reached))
                (is (<= 20 (:call-usage @reached 0)) (str @reached)))))
          (when-not analysing
            (testing "and a process that recorded nothing says so, whatever is
            asked of it"
              (is (nil? (refusals r c)))))
          (testing "and the repl is still a repl"
            (is (= "42" (answered r "(+ 40 2)"))))
          (finally
            (disconnect r)
            (disconnect c)))))))
