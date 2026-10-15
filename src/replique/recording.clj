(ns replique.recording
  "What is recorded, chosen: a function, it with what calls it and what it
  calls, a namespace, the functions a branch changed - and the library
  functions these call.

  THE CODE OF THE PROJECT IS RECORDED FORM BY FORM, evaluated again to be -
  see \"Recording\" in replique.debug. Which the client does, from its files,
  since what it evaluates is what it shows: what is decided here is which
  definitions are around a function, and in which files - `:recording-scope'.

  A LIBRARY FUNCTION IS RECORDED BY ITS CALLS ONLY - what it was called with
  and what it returned or threw - its code being in a jar, not to be read
  through, and not evaluated again: see \"Recording calls only\" in
  replique.debug. The ones the code recorded calls, or any - `:record-calls'.
  Never clojure.core, nor cljs.core, called everywhere, nor replique."
  (:require [replique.cljs :as cljs]
            [replique.debug :as debug]
            [replique.lint :as lint]
            [replique.names :as names]
            [replique.protocol :as protocol])
  (:import [clojure.lang Namespace Var]))

(defn- refuse [kind & message]
  (throw (ex-info (apply str message) {:replique/error kind})))

(defn- as-qsym [x]
  (let [s (some-> x protocol/as-name symbol)]
    (if (qualified-symbol? s)
      s
      (refuse :invalid-message "A var is named by its qualified name, got: " (pr-str x)))))

(defn- as-qsyms [k xs]
  (when-not (or (nil? xs) (sequential? xs))
    (refuse :invalid-message "The " k " of a recording are a list, got: " (pr-str xs)))
  (mapv as-qsym xs))

(defn- not-recorded
  "Why the calls of the var named QSYM are never recorded, or nil."
  [qsym]
  (let [ns (namespace qsym)]
    (when (or (#{"clojure.core" "cljs.core"} ns) (.startsWith ^String ns "replique."))
      "called everywhere, or what recording is made of")))

(defn- var-named
  "The Clojure var QSYM names, or nil."
  ^Var [qsym]
  (when-let [^Namespace n (find-ns (symbol (namespace qsym)))]
    (.findInternedVar n (symbol (name qsym)))))

;; The functions of the project around the var :around - those that call it
;; and those it calls, :depth calls away, 1 without - with it, each with the
;; file it is defined in: what a client records, form by form
(defmethod protocol/handle :recording-scope [_ msg]
  (names/with-dialect msg
    (let [q (as-qsym (:around msg))
          depth (or (:depth msg) 1)]
      (when-not (pos-int? depth)
        (refuse :invalid-message "A :depth is a number of calls, got: " (pr-str depth)))
      (let [{:keys [callers callees]} (lint/around q depth)]
        {:vars (vec (for [v (concat [q] (sort callers) (sort callees))
                          :let [file (lint/defined-in v)]
                          :when file]
                      {:var (str v) :file file}))
         :callers (mapv str (sort callers))
         :callees (mapv str (sort callees))}))))

;; Record the calls of the functions of the vars :vars, and of the library
;; functions the definitions of the vars :callees-of call, by their calls
;; only. Answers the vars `recorded' so, and those `skipped', each with `why'
(defmethod protocol/handle :record-calls [_ msg]
  (names/with-dialect msg
    (let [named (as-qsyms :vars (:vars msg))
          callees (when-let [of (seq (as-qsyms :callees-of (:callees-of msg)))]
                    ;; of which clojure.core's go without saying
                    (remove not-recorded (lint/library-callees of)))
          all (vec (distinct (concat named callees)))
          refused (into {} (keep (fn [q] (when-let [why (not-recorded q)] [q why]))) all)
          asked (remove refused all)
          whys (if (names/cljs?)
                 (do (when-not (cljs/runtime-connected?)
                       (refuse :no-runtime "No " (name cljs/*target*) " runtime is running"))
                     (into {} (debug/cljs-record-calls! asked)))
                 (into {} (for [q asked
                                :let [why (if-let [v (var-named q)]
                                            (debug/record-calls! v)
                                            "no such var")]
                                :when why]
                            [q why])))
          skipped (merge refused whys)]
      {:recorded (vec (for [q all :when (not (contains? skipped q))] (str q)))
       :skipped (vec (for [q all :let [why (get skipped q)] :when why]
                       {:var (str q) :why why}))})))
