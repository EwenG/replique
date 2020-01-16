(ns replique.shadow-cljs
  (:require [cljs.analyzer :as ana]))

;; Patch the Clojurescript compiler in order to be compatible
;; with the shadow-cljs syntax

;; This is usually used in combination of the :global-exports option of
;; a deps.cljs file

;; Allow the :default option when requiring javascript dependencies.
;; :default is replaced by :as
(defn- basic-validate-ns-spec-reducer [acc v]
  (if (= :default (peek acc))
    (-> acc pop (conj :as) (conj v))
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
