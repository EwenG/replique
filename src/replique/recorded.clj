(ns replique.recorded
  "What was recorded, as text to read - by someone at a repl, or by an agent
  that has one and no editor: what is recorded, and the calls it made. See
  \"Recording\" in replique/debug.clj for what recording is.

  Each function prints, and returns nil, so that what it says reads as it is
  written rather than as a string. Nothing here records, evaluates the code
  recorded, or realizes a value: it reads what is kept, the way the editor's
  views do.

  A UNIT is what was evaluated to be recorded - a definition, mostly - and is
  named by its number. A CALL is named by its unit and its number: \"12/345\",
  which is what `calls' prints, and what it takes to print the calls one call
  made, and its code with what each form evaluated to, `call'. A SITE is a
  form of the code, named by its number, which `call' prints as [n] after its
  value, and `value' takes to print all of it. The numbers are this process's:
  they change when the code is recorded again.

  TO UNDERSTAND WHAT A FEATURE DOES, start wide and go down:

    (summary)                 which functions took part, which called which,
                              and the shapes of the data between them
    (calls)                   the runs, as trees of calls with their values
    (call \"3/4\")              one call in its code, form by form, its loops,
                              and the calls it made
    (value \"3/4\" 12 [1 :k])   one value in full

  WHAT IS KEPT: of each function, the last 10 calls made in no other call kept
  and the calls those made; of each form in a call, its first 10 values and
  its last 10. Values are kept as they are - one mutated since prints as it is
  now. A function recorded by its calls only does not say what it evaluated
  inside. ClojureScript recordings are read from here too, while their
  runtime is connected."
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [replique.debug :as debug]
            [replique.shape :as shape])
  (:import [java.time Instant LocalTime ZoneId]
           [java.time.format DateTimeFormatter]))

(def ^:private printed-up-to
  "How much of a value printed in a line is printed, in chars."
  200)

(defn- cut [^String s]
  (if (> (count s) printed-up-to) (str (subs s 0 (dec printed-up-to)) "…") s))

(defn- named
  "What the unit U, as `debug/units-recorded' says it, is called: its var, or
  where in its namespace it was evaluated."
  [{:keys [ns label calls-only]}]
  (cond
    calls-only label
    (or (= "evaluation" label) (str/starts-with? label "line ")) (str label " of " ns)
    :else (str ns "/" label)))

(defn- where
  "Where the unit U is written: its file, from the working directory where it
  is in it, and line - an entry of a jar after the jar."
  [{:keys [file entry line]}]
  (when file
    (let [here (str (System/getProperty "user.dir") "/")
          file (if (str/starts-with? file here) (subs file (count here)) file)]
      (str file (when entry (str "!" entry)) (when line (str ":" line))))))

(defn- dialect [{:keys [dialect target]}]
  (if (= "cljs" dialect) (str "cljs " target) "clj"))

(defn- columns
  "ROWS, vectors of strings, printed with each column as wide as its widest."
  [rows]
  (let [widths (apply map (fn [& cells] (apply max (map count cells))) rows)]
    (doseq [row rows]
      (println (str/trimr (str/join "  " (map (fn [cell w] (str cell (apply str (repeat (- w (count cell)) \space))))
                                               row widths)))))))

(defn recordings
  "Print what is recorded: each unit's number, what it is, where it is
  written, and how many of its calls are kept - the last ones of those made
  in no other call kept, and the ones those made."
  []
  (let [units (debug/units-recorded)]
    (if (empty? units)
      (println "Nothing is recorded.")
      (columns (for [u units]
                 [(str (:unit u))
                  (named u)
                  (dialect u)
                  (cond (:calls-only u) "calls only" :else (or (where u) ""))
                  (if-let [n (:kept u)]
                    (str n (if (= 1 n) " call" " calls") " kept")
                    "runtime gone")])))))

(defn- time-of [ms]
  (.format (LocalTime/ofInstant (Instant/ofEpochMilli ms) (ZoneId/systemDefault))
           (DateTimeFormatter/ofPattern "HH:mm:ss.SSS")))

(defn- call-line
  "The call C, as `debug/calls-said' says it, on a line - NAMES saying what
  each unit is called. Its thread and when it started are said of a call made
  in no other said, MADE-IN, and its thread where it is not that one's."
  [names made-in {:keys [unit call args value threw index of thread started made-in-gone]}]
  (str unit "/" call "  "
       "(" (str/join " " (cons (get names unit unit) (map cut args))) ")"
       (cond
         threw (str " threw " (cut value))
         value (str " => " (cut value))
         :else " => (no value kept)")
       (when index (str "  · call " index " of " of))
       (when (and thread (not= thread (:thread made-in))) (str " · " thread))
       (when (and started (nil? made-in)) (str " · " (time-of started)))
       (when made-in-gone " · made in a call not kept")))

(defn- the-call [ref]
  (let [[unit call] (if (string? ref)
                      (map parse-long (str/split ref #"/"))
                      ref)]
    (when-not (and (integer? unit) (integer? call))
      (throw (ex-info (str "A call is named \"unit/call\", as `calls' prints it, got: " (pr-str ref))
                      {})))
    [unit call]))

(defn calls
  "Print the calls recorded, as a tree: each with what it was called with and
  what it returned - nothing, where it threw, is still running, or what it
  returned is not kept - the calls it made under it - down to DEPTH levels, and
  LINES lines in all. The calls made in no call kept first, oldest first -
  or, given the call REF, \"unit/call\", the calls it made."
  ([] (calls nil {}))
  ([ref] (calls ref {}))
  ([ref {:keys [depth lines] :or {depth 6 lines 300}}]
   (let [names (into {} (map (juxt :unit named)) (debug/units-recorded))
         printed (volatile! 0)
         stopped (volatile! false)
         more (fn [indent c n]
                (println (str indent "  … " n (if (= 1 n) " call" " calls")
                              " made in it: (calls \"" (:unit c) "/" (:call c) "\")")))]
     (letfn [(walk [made-in cs level]
               (let [indent (apply str (repeat (* 2 level) \space))]
                 (loop [[c & cs] cs]
                   (when c
                     (if (>= @printed lines)
                       (when-not @stopped
                         (vreset! stopped true)
                         (println (str "… and more: (calls " (pr-str ref) " {:lines "
                                       (* 2 lines) "}) prints more")))
                       (do (println (str indent (call-line names made-in c)))
                           (vswap! printed inc)
                           (when (pos? (:made c 0))
                             (if (< (inc level) depth)
                               (walk c (debug/calls-said (:unit c) (:call c)) (inc level))
                               (more indent c (:made c))))
                           (recur cs)))))))]
       (let [cs (if ref (apply debug/calls-said (the-call ref)) (debug/calls-said nil nil))]
         (if (empty? cs)
           (println (if ref "No call made in it is kept." "No call is recorded."))
           (walk nil cs 0)))))))

;;; A call, in its code

(defn- the-unit-or-call
  "The unit and call REF names - \"unit/call\" - or the unit and nil, of a
  REF that names a unit only: its last call."
  [ref]
  (if (or (integer? ref) (and (string? ref) (re-matches #"\d+" ref)))
    [(if (string? ref) (parse-long ref) ref) nil]
    (the-call ref)))

(defn- literal? [form]
  (or (nil? form) (boolean? form) (number? form) (string? form) (keyword? form) (char? form)))

(defn- local-names
  "The names of the params and of the locals of what SAID says was recorded
  in a call - which a form that is just one of them says nothing more than."
  [unit said]
  (into #{}
        (mapcat (fn [{:keys [kind params site]}]
                  (case kind
                    "enter" (remove #{"&"} params)
                    "local" [(debug/site-label unit site)]
                    nil)))
        (:values said)))

(defn- said-lines
  "What the value V, of `debug/recorded-said', is said with - lines, nil
  where it says nothing a reader does not have: a literal, or a local."
  [unit names {:keys [kind site times value not-kept params args]}]
  (let [value (if not-kept "(not kept)" (cut value))
        times (when (and times (> times 1)) (str "  (×" times ", the last)"))]
    (case kind
      "enter" (let [params (remove #{"&"} params)]
                (if (and (seq params) (= (count params) (count args)))
                  (map-indexed (fn [i [p a]] (str p " = " (cut a) "  [" site " " i "]"))
                               (map vector params args))
                  [(str (debug/site-label unit site) " = " value "  [" site "]")]))
      "local" [(str (debug/site-label unit site) " = " value times "  [" site "]")]
      (let [form (debug/site-form site)]
        (when-not (or (literal? form) (and (symbol? form) (contains? names (str form))))
          [(str (debug/site-label unit site) " => " value times "  [" site "]")])))))

(defn- in-span? [{:keys [line column end-line end-column]} v]
  (and (:line v)
       (not (neg? (compare [(:line v) (:column v)] [line column])))
       (not (pos? (compare [(:end-line v) (:end-column v)] [end-line end-column])))))

(def ^:private passing-on
  "The heads of the forms whose value is the value of a form inside them."
  '#{if if-not when when-not let let* loop loop* do cond condp case when-let if-let
     when-some if-some when-first letfn binding try locking})

(defn- passes-on? [v]
  (let [form (debug/site-form (:site v))]
    (and (seq? form) (contains? passing-on (first form)))))

(defn- in-order
  "The values VS, of `debug/recorded-said', in the order they were had: a
  fn's params first, then each form once it ends - without a form that only
  passes on the value of a form inside it, an `if' or a `let', which says
  nothing more."
  [vs]
  (let [sorted (sort-by (fn [v] [(if (= "enter" (:kind v)) 0 1)
                                 (:end-line v 0) (:end-column v 0)
                                 (- (:line v 0)) (- (:column v 0))])
                        vs)]
    (reduce (fn [kept v]
              (if (some #(and (not (:not-kept v)) (passes-on? v)
                              (= (:value v) (:value %)) (= (:times v) (:times %))
                              (not= "enter" (:kind %)) (not= "enter" (:kind v))
                              (in-span? v %))
                        kept)
                kept
                (conj kept v)))
            [] sorted)))

(defn- indices
  "Of the iterations KEPT, [first last] ranges, the first and the last ones,
  UP-TO of them in all - and nil where some in between are left out."
  [kept up-to]
  (let [all (vec (for [[a b] kept i (range a (inc b))] i))]
    (if (<= (count all) up-to)
      all
      (let [head (quot (inc up-to) 2)]
        (concat (subvec all 0 head) [nil] (subvec all (- (count all) (- up-to head))))))))

(defn- print-loop
  "Print what each iteration of the loop L, of `debug/recorded-said', of the
  call C of the unit UNIT, evaluated - UP-TO iterations at most."
  [unit c names l up-to]
  (let [fn? (:fn l)]
    (println (str ";; " (if fn? "fn " "loop of ") (debug/site-label unit (:site l))
                  ", line " (:line l) ", " (:iterations l)
                  (if fn? " calls" " times round")
                  (when (not= (:iterations l) (reduce + (map (fn [[a b]] (inc (- b a))) (:kept l))))
                    (str ", " (str/join " and " (map (fn [[a b]] (if (= a b) (str a) (str a "-" b))) (:kept l)))
                         " kept"))
                  "  [" (:site l) "]"))
    (doseq [i (indices (:kept l) up-to)]
      (if (nil? i)
        (println ";;   …")
        (let [said (debug/recorded-said unit c {:site (:site l) :index i})]
          (println (str ";;   " (if fn? "call " "iteration ") i))
          (doseq [v (in-order (filter #(or (= (:site %) (:site l))
                                           (and (in-span? l %)
                                                (not= (mapv l [:line :column :end-line :end-column])
                                                      (mapv % [:line :column :end-line :end-column]))))
                                      (:values said)))
                  line (said-lines unit names v)]
            (println (str ";;     " line))))))))

(defn call
  "Print the code of the call REF, \"unit/call\" - or of the last call of the
  unit REF - with what each form of it evaluated to under the line it ends
  on: `;; code => value [site]', and its params and locals as `name = value'.
  Not a literal, nor a form that is a local. A form evaluated more than once
  in the call says how many times, and its last value: what each loop of the
  call did each time round follows the code - ITERATIONS times round at most,
  the first ones and the last - and then the calls it made.

  The code is its file's, where the file still has it as it was evaluated -
  or the values are said by line."
  ([ref] (call ref {}))
  ([ref {:keys [iterations] :or {iterations 6}}]
   (let [[unit c] (the-unit-or-call ref)
         said (debug/recorded-said unit c nil)
         c (:call said)
         units (debug/units-recorded)
         u (some #(when (= unit (:unit %)) %) units)
         names (local-names unit said)
         this (some #(when (= c (:call %)) %) (:calls said))
         index (some (fn [[i x]] (when (= c (:call x)) (inc i))) (map-indexed vector (:calls said)))]
     (if-not c
       (println (str "Unit " unit " was not called yet."))
       (let [values (:values said)
             under (group-by (fn [v] (if (= "enter" (:kind v)) (:line u) (:end-line v)))
                             (filter #(or (not= "enter" (:kind %))
                                          (not-any? (fn [l] (= (:site l) (:site %))) (:loops said)))
                                     values))
             said-under (fn [line]
                          (for [v (in-order (get under line))
                                l (said-lines unit names v)]
                            l))
             source (debug/unit-source unit)]
         (println (str ";; " unit "/" c "  " (named u) ", call " index " of " (count (:calls said))
                       (when-let [t (:thread this)] (str " · " t))
                       (when-let [at (:at this)] (str " · " (time-of at)))))
         (when-let [w (where u)] (println (str ";; " w)))
         (if source
           (doseq [[i text] (map-indexed vector (:lines source))
                   :let [line (+ (:line source) i)
                         indent (re-find #"^\s*" text)]]
             (println text)
             (doseq [l (said-under line)]
               (println (str indent ";; " l))))
           (do (println ";; (its file does not have the code it was evaluated from any more:")
               (println ";; what each form evaluated to, by the line it ends on)")
               (doseq [line (sort (keys under))
                       l (said-under line)]
                 (println (str ";; line " line ": " l)))))
         (doseq [l (:loops said)]
           (println)
           (print-loop unit c names l iterations))
         (let [made (debug/calls-said unit c)
               by-name (into {} (map (juxt :unit named)) units)]
           (when (seq made)
             (println)
             (println ";; calls made:")
             (doseq [m made]
               (println (str ";;   " (call-line by-name this m)))))))))))

(defn value
  "Print what the site SITE - [n], as `call' prints it - evaluated to last in
  the call REF, \"unit/call\" - at PATH in it, as `get-in' takes it - LEVEL
  deep and LENGTH long. Of ITERATION, [loop index], in that iteration of the
  loop whose [n] `call' prints as LOOP."
  ([ref site] (value ref site nil {}))
  ([ref site path] (value ref site path {}))
  ([ref site path {:keys [iteration level length] :or {level 8 length 50}}]
   (let [[unit c] (the-call ref)
         s (debug/value-said unit c site
                             (when-let [[l i] iteration] {:site l :index i})
                             (vec path) level length)]
     (if (nil? s)
       (println "Nothing is kept of it in that call.")
       ;; laid out, where it reads back
       (if-let [v (try (edn/read-string {:default tagged-literal} s) (catch Exception _ nil))]
         (binding [*print-length* nil *print-level* nil] (pprint/pprint v))
         (println s))))))

;;; What the calls looked like

(defn- shape-lines
  "The shape S printed under LABEL, INDENT in: on the label's line where it
  fits, laid out under it where it does not - with how many different ones
  it was merged from, N."
  [indent label s n]
  (let [one (pr-str s)
        different (when (> n 1) (str "  (" n " different)"))]
    (if (<= (+ (count indent) (count label) (count one)) 100)
      (println (str indent label " " one different))
      (do (println (str indent label different))
          (doseq [line (str/split-lines (binding [pprint/*print-right-margin* 100]
                                          (with-out-str (pprint/pprint s))))]
            (println (str indent "  " line)))))))

(defn- merged [shapes] (reduce shape/merged 'none shapes))

(defn summary
  "Print, of each function recorded, what its calls kept looked like, merged
  across them: how many there are, which functions made them and which they
  made, and the shapes of what they were called with, returned, and threw -
  see replique.shape: a key not in every map is (optional ...), what was not
  always of one kind is (or ...), a count that varied is [min max]. How many
  different shapes went into one is said after it, where it was more than
  one."
  []
  (let [units (debug/units-recorded)
        names (into {} (map (juxt :unit named)) units)
        calls (debug/calls-shaped)
        by-unit (group-by :unit calls)
        made (frequencies (keep (fn [c] (when-let [by (:made-by c)] [by (:unit c)])) calls))
        counted (fn [freqs]
                  (str/join ", " (for [[k n] (sort-by (fn [[k n]] [(- n) (str (get names k k))]) freqs)]
                                   (str (if (nil? k) "no call recorded" (get names k k)) " ×" n))))
        said (filter #(seq (get by-unit (:unit %))) units)]
    (if (empty? said)
      (println "No call is recorded.")
      (doseq [u said
              :let [cs (get by-unit (:unit u))
                    returned (filter #(and (contains? % :value) (not (:threw %))) cs)
                    threw (filter :threw cs)
                    callees (into {} (keep (fn [[[by unit] n]] (when (= by (:unit u)) [unit n]))) made)]]
        (println (str (named u) "  " (count cs) (if (= 1 (count cs)) " call" " calls") " kept"
                      (when-let [w (where u)] (str " · " w))))
        (println (str "  made in:  " (counted (frequencies (map :made-by cs)))))
        (doseq [[arity group] (sort-by key (group-by #(count (:args %)) cs))]
          (shape-lines "  " (str "called with " arity (if (= 1 arity) " arg" " args")
                                 (when (< (count group) (count cs)) (str " ×" (count group))) ":")
                       (if (zero? arity) [] (vec (apply map (fn [& ss] (merged ss)) (map :args group))))
                       (count (distinct (map :args group)))))
        (when (seq returned)
          (shape-lines "  " (str "returned" (when (< (count returned) (count cs)) (str " ×" (count returned))) ":")
                       (merged (map :value returned)) (count (distinct (map :value returned)))))
        (when (seq threw)
          (shape-lines "  " (str "threw ×" (count threw) ":")
                       (merged (map :value threw)) (count (distinct (map :value threw)))))
        (when-let [none (seq (remove #(contains? % :value) cs))]
          (println (str "  no value kept ×" (count none))))
        (when (seq callees)
          (println (str "  made:     " (counted callees))))
        (println)))))
