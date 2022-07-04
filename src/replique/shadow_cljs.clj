(ns replique.shadow-cljs
  (:require [cljs.analyzer :as ana]
            [clojure.string]
            [cljs.closure]
            [cljs.compiler]))

;; Patch the Clojurescript compiler in order to be compatible
;; with the shadow-cljs syntax

;; This is usually used in combination of the :global-exports option of
;; a deps.cljs file

;; Allow the :default option when requiring javascript dependencies.
;; :default is replaced by :as
(defn- basic-validate-ns-spec-reducer [acc v]
  (if (= :default (peek acc))
    (let [lib (first acc)
          add-default? (not (clojure.string/ends-with? (str lib) "$default"))
          acc (if add-default?
                (assoc acc 0 (str lib "$default"))
                acc)]
      (-> acc pop (conj :as) (conj v)))
    (conj acc v)))

(defonce cljs-basic-validate-ns-spec cljs.analyzer/basic-validate-ns-spec)

(defn basic-validate-ns-spec [env macros? spec]
  (cljs-basic-validate-ns-spec env macros? (reduce basic-validate-ns-spec-reducer [] spec)))

(alter-var-root #'cljs.analyzer/basic-validate-ns-spec (constantly basic-validate-ns-spec))

(def ^{:private true} parse-require-spec-reducer basic-validate-ns-spec-reducer)

(defonce cljs-parse-require-spec cljs.analyzer/parse-require-spec)

(defn parse-require-spec [env macros? deps aliases spec]
  (if (or (symbol? spec) (string? spec))
    (recur env macros? deps aliases [spec])
    (let [spec (reduce parse-require-spec-reducer [] spec)]
      (cljs-parse-require-spec env macros? deps aliases spec))))

(alter-var-root #'cljs.analyzer/parse-require-spec (constantly parse-require-spec))

;; Support for the shadow-cljs (:require "./js-file.js") syntax
;; cljs.closure/add-dep-string calls cljs.compiler/munge on its input but the input is already munged when compiling
;; and double munging sometimes gives a different result than single munging for example:
;; (cljs.compiler/munge "./js-file.js") => "..js_file.js"
;; (cljs.compiler/munge (cljs.compiler/munge "./js-file.js")) => "_DOT__DOT_js_file.js"
;; This results in a cljs_deps.js file which :provides a single munged dependency and requires a double munged dependency
;; Also the "_" characters are replaced by "-" in the provides, but not in the requires.

(def ^:dynamic *munge-hook?* false)

(defonce cljs-munge cljs.compiler/munge)

(defn wrapped-munge
  ([s]
   (if (and *munge-hook?* (.startsWith (str s) ".."))
     (clojure.string/replace s "-" "_")
     (cljs-munge s)))
  ([s reserved]
   (if (and *munge-hook?* (.startsWith (str s) ".."))
     s
     (cljs-munge s reserved))))

(alter-var-root #'cljs.compiler/munge (constantly wrapped-munge))

(defonce cljs-add-dep-string cljs.closure/add-dep-string)

(defn add-dep-string [opts input]
  (binding [*munge-hook?* true]
    (cljs-add-dep-string opts input)))

(alter-var-root #'cljs.closure/add-dep-string (constantly add-dep-string))

