(ns replique.interactive
  "Replique REPL public API"
  (:refer-clojure :exclude [load-file])
  (:require [replique.repl]
            [replique.utils :as utils]
            [replique.server :as server]))

(def ^:private cljs-repl* (utils/dynaload 'replique.repl-cljs/cljs-repl))
(def ^:private cljs-repl-nashorn* (utils/dynaload 'replique.nashorn/cljs-repl))
(def ^:private cljs-load-file (utils/dynaload 'replique.repl-cljs/load-file))
(def ^:private cljs-in-ns* (utils/dynaload 'replique.repl-cljs/in-ns*))
(def ^:private cljs-compiler-env (utils/dynaload 'replique.repl-cljs/compiler-env))
(def ^:private cljs-repl-env (utils/dynaload 'replique.repl-cljs/repl-env))
(def ^:private cljs-set-repl-verbose
  (utils/dynaload 'replique.repl-cljs/set-repl-verbose))

(defn repl [& options]
  (let [options-map (apply hash-map options)
        options-map (cond-> options-map
                      (not (contains? options-map :init)) (assoc :init #())
                      (not (contains? options-map :print)) (assoc :print prn)
                      (not (contains? options-map :caught))
                      (assoc :caught clojure.main/repl-caught)
                      true replique.repl/options-with-ns-change)]
    (apply clojure.main/repl (apply concat options-map))))

(defn cljs-repl
  "Start a Clojurescript REPL"
  []
  (@cljs-repl*))

(defn cljs-repl-nashorn
  "Start a Clojurescript Nashorn REPL"
  []
  (@cljs-repl-nashorn*))

(def repl-port
  "Returns the port the REPL is listening on"
  server/server-port)

;; At the moment, load file does not intern macros in the cljs-env, making dynamically loaded
;; macros unavailable to autocompletion/repliquedoc
(defmacro load-file
  "Sequentially read and evaluate the set of forms contained in the file. Works both for Clojure and Clojurescript"
  [file-path]
  (if (utils/cljs-env? &env)
    (@cljs-load-file @@cljs-repl-env file-path)
    `(clojure.core/load-file ~file-path)))

;; It seems that naming this macro "in-ns" make the cljs compiler to crash
(defmacro cljs-in-ns
  "Change the Clojurescript namespace to the namespace named by the symbol"
  [ns-quote]
  (let [quote (and (seq? ns-quote) (first ns-quote))
        ns-name (and (seq? ns-quote) (second ns-quote))]
    (when-not (and (= 'quote quote) (symbol? ns-name))
      (throw (IllegalArgumentException. "Argument to in-ns must be a symbol.")))
    (list 'quote (@cljs-in-ns* ns-name))))

(defmacro set-cljs-repl-verbose
  "Switch the clojurescript REPL into verbose mode"
  [b]
  (@cljs-set-repl-verbose b)
  b)

(def compiler-opts
  "Clojurescript compiler options that can be set at the REPL"
  #{:verbose :warnings :compiler-stats :language-in :language-out
    :closure-warnings :checked-arrays})

(defmacro set-cljs-compiler-opt
  "Set the value of the Clojurescript compiler option named by the key"
  [opt-key opt-val]
  {:pre [(contains? compiler-opts opt-key)]}
  (swap! @@cljs-compiler-env assoc-in [:options opt-key] opt-val)
  opt-val)

#_(defn remote-repl
    "Start a REPL on a remote machine"
    [host port]
    {:pre [(string? host) (number? port)]}
    (let [s (java.net.Socket. host port)
          s-in (.getInputStream s)
          s-out (java.io.BufferedWriter. (java.io.OutputStreamWriter. (.getOutputStream s)))]
      (future
        (try
          (loop []
            (let [input (.read s-in)]
              (when (not (= -1 input))
                (.write *out* input)
                (.flush *out*)
                (recur))))
          (catch Exception _ nil)
          (finally (.close s))))
      (try
        (loop []
          (let [input (read {:read-cond :allow} *in*)]
            (binding [*out* s-out] (prn input))
            (recur)))
        (finally (.close s)))))

(comment
  (require '[clojure.core.server :as core-s])

  (defn remote-repl-accept []
    (clojure.main/repl :prompt (fn [] (printf "<remote> %s=> " (ns-name *ns*)))
                       :read clojure.core.server/repl-read))

  

  (core-s/start-server {:port 9000 :name :test
                        :accept `remote-repl-accept
                        :server-daemon false})

  (core-s/stop-server :test)

  (remote-repl "localhost" 9000)
  )

