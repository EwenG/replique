(ns replique.deps-plan-test
  (:require [clojure.java.basis :as basis]
            [clojure.java.basis.impl :as basis-impl]
            [clojure.test :refer [deftest is testing]]
            [replique.classpath :as classpath]
            [replique.deps-plan :as deps-plan]
            [replique.ops]
            [replique.state :as state]
            [replique.test-client
             :refer [control-client disconnect request! temp-dir delete-recursively
                     with-process]])
  (:import [java.nio.file Files LinkOption Path Paths]
           [java.nio.file.attribute FileAttribute]))

(defn- path ^Path [& names] (Paths/get (first names) (into-array String (rest names))))

(defn- write-file! [dir & names]
  (let [file (apply path dir names)]
    (Files/createDirectories (.getParent file) (make-array FileAttribute 0))
    (Files/write file (.getBytes "" "UTF-8") (make-array java.nio.file.OpenOption 0))
    file))

(defn- linked! ^Path [^Path link ^Path target]
  (Files/createDirectories (.getParent link) (make-array FileAttribute 0))
  (Files/createSymbolicLink link target (make-array FileAttribute 0)))

(defn- real ^String [^Path p] (str (.toRealPath p (make-array LinkOption 0))))

(defmacro ^:private wanting
  "Run BODY with the deps tool answering the basis F makes of the one the
  process started with - what the deps files would say, without anybody
  having to write any."
  [f & body]
  `(with-redefs [deps-plan/wanted (fn [_#] (~f (basis/initial-basis)))]
     ~@body))

(defn- with-lib [basis lib coordinate]
  (assoc-in basis [:libs lib] coordinate))

(defn- forget-lib!
  "Take LIB back out of the basis of the process, which is all a test can do
  about a library it added: a loader cannot be made to forget a url."
  [lib]
  (basis-impl/update-basis! update :libs dissoc lib))

(defn- plan [c & {:as msg}]
  (request! c (merge {:op :classpath-plan :id 1} msg)))

;;; Asking the deps tool

(deftest the-deps-files-as-they-were-plan-nothing
  (testing "the configuration the process was started with, resolved again by
  the cli, is the classpath it already has - which is the one answer this has
  to get right before any other means anything"
    (with-process [info nil]
      (let [c (control-client info)]
        (try
          (let [reply (plan c)]
            (is (= "reply" (:tag reply)))
            (is (= "current" (:verdict reply)))
            (is (= [] (:added-libs reply) (:moved reply) (:removed-libs reply)
                   (:added-paths reply) (:removed-paths reply) (:shadowed reply)))
            (is (nil? (:jvm-opts reply))))
          (finally (disconnect c)))))))

(deftest the-configuration-asked-is-the-one-resolved
  (with-process [info nil]
    (let [c (control-client info)
          asked (atom nil)]
      (try
        (with-redefs [deps-plan/wanted (fn [configuration]
                                         (reset! asked configuration)
                                         (basis/initial-basis))]
          (plan c)
          (is (= (:basis-config (basis/initial-basis)) @asked)
              "nothing said is the configuration the process was started with")
          (plan c :aliases ["dev" :test])
          (is (= [:dev :test] (:aliases @asked)))
          (plan c :aliases [])
          (is (= [] (:aliases @asked)) "none is none, and not unchanged")
          (plan c :extra "{:aliases {:mine {:extra-paths [\"x\"]}} ; a comment\n}")
          (is (= {:aliases {:mine {:extra-paths ["x"]}}} (:extra @asked))
              "the text -Sdeps would be given, read")
          (plan c :extra "")
          (is (nil? (:extra @asked)) "and nothing given to -Sdeps is no deps at all"))
        (testing "what the client got wrong"
          (is (= "invalid-message" (:error (plan c :extra 42))))
          (is (= "invalid-message" (:error (plan c :aliases [42])))))
        (finally (disconnect c))))))

;;; What can be added

(deftest a-new-library-is-added-and-said
  (with-process [info nil]
    (let [dir (temp-dir)
          c (control-client info)]
      (try
        (write-file! dir "planned" "probe.clj")
        (wanting #(with-lib % 'my/lib {:mvn/version "1.0.0" :paths [dir]})
          (let [reply (plan c)]
            (is (= "additive" (:verdict reply)))
            (is (= [{:lib "my/lib" :now "1.0.0"}] (:added-libs reply))))
          (let [reply (request! c {:op :sync-classpath :id 2})]
            (is (= "reply" (:tag reply)))
            (is (= ["my/lib"] (:added reply))))
          (testing "on the loader every connection shares, and read"
            (is (some? (.getResource state/class-loader "planned/probe.clj")))
            (is (some #{"planned.probe"} (:namespaces (classpath/scan)))))
          (testing "and in the basis, so the next question counts it as had"
            (is (= "current" (:verdict (plan c))))))
        (finally
          (disconnect c)
          (forget-lib! 'my/lib)
          (delete-recursively dir)
          (classpath/rescan!))))))

(deftest a-new-source-directory-is-added-and-kept
  (with-process [info nil]
    (let [dir (temp-dir)
          c (control-client info)]
      (try
        (write-file! dir "sourced" "probe.clj")
        (wanting #(assoc-in % [:classpath dir] {:path-key :extra-paths})
          (is (= [dir] (:added-paths (plan c))))
          (is (= [dir] (:added (request! c {:op :sync-classpath :id 2}))))
          (is (some #{"sourced.probe"} (:namespaces (classpath/scan))))
          (testing "a basis has nowhere to say a directory was added later, so
          the process keeps that itself"
            (is (= "current" (:verdict (plan c))))))
        (finally
          (disconnect c)
          (reset! @#'deps-plan/added-paths #{})
          (delete-recursively dir)
          (classpath/rescan!))))))

(deftest a-source-directory-made-after-it-was-added-is-read
  (with-process [info nil]
    (let [dir (temp-dir)
          later (str (path dir "later"))
          c (control-client info)]
      (try
        (wanting #(assoc-in % [:classpath later] {:path-key :extra-paths})
          (is (= [later] (:added (request! c {:op :sync-classpath :id 2}))))
          (testing "a directory deps.edn names before anybody made it goes on
          the loader as a directory, and is read once it is there"
            (write-file! later "made" "afterwards.clj")
            (is (some? (.getResource ^ClassLoader state/class-loader
                                     "made/afterwards.clj")))))
        (finally
          (disconnect c)
          (reset! @#'deps-plan/added-paths #{})
          (delete-recursively dir)
          (classpath/rescan!))))))

;;; What cannot

(deftest another-version-of-a-library-is-a-restart
  (testing "the old one is on the loader that is asked first, and whatever was
  loaded from it stays loaded - so adding the new one changes nothing but what
  the basis says"
    (with-process [info nil]
      (let [c (control-client info)]
        (try
          (wanting #(with-lib % 'org.clojure/clojure
                      {:mvn/version "9.9.9" :paths ["/nowhere/clojure-9.9.9.jar"]})
            (let [reply (plan c)]
              (is (= "restart" (:verdict reply)))
              (is (= [{:lib "org.clojure/clojure" :was (clojure-version) :now "9.9.9"}]
                     (:moved reply))))
            (testing "and adding is refused, saying so"
              (let [reply (request! c {:op :sync-classpath :id 2})]
                (is (= "error" (:tag reply)))
                (is (= "restart-needed" (:error reply))))))
          (finally (disconnect c)))))))

(deftest a-namespace-the-classpath-already-has-is-a-restart
  (testing "behind what is there the new one is never found, and a process
  started on the new deps might find it first"
    (with-process [info nil]
      (let [dir (temp-dir)
            c (control-client info)]
        (try
          (write-file! dir "replique" "state.clj")
          (wanting #(with-lib % 'my/shadow {:mvn/version "1.0.0" :paths [dir]})
            (let [reply (plan c)]
              (is (= "restart" (:verdict reply)))
              (is (= [{:entry dir :namespaces ["replique.state"]}] (:shadowed reply)))))
          (finally
            (disconnect c)
            (delete-recursively dir)))))))

(deftest a-data-readers-file-shadows-nothing
  (testing "every data_readers file on the classpath is read and merged, so a
  library bringing one more is a library to add"
    (with-process [info nil]
      (let [dir (temp-dir)
            c (control-client info)
            scan classpath/scan]
        (try
          (write-file! dir "data_readers.cljc")
          (with-redefs [classpath/scan #(update (scan) :namespaces conj "data-readers")]
            (wanting #(with-lib % 'my/readers {:mvn/version "1.0.0" :paths [dir]})
              (let [reply (plan c)]
                (is (= "additive" (:verdict reply)))
                (is (= [] (:shadowed reply))))))
          (finally
            (disconnect c)
            (delete-recursively dir)))))))

(deftest other-jvm-options-are-a-restart
  (with-process [info nil]
    (let [c (control-client info)]
      (try
        (wanting #(update-in % [:argmap :jvm-opts] (fnil conj []) "-Xmx1g")
          (let [reply (plan c)]
            (is (= "restart" (:verdict reply)))
            (is (= "-Xmx1g" (last (:now (:jvm-opts reply)))))))
        (finally (disconnect c))))))

(deftest what-is-gone-is-said-and-not-acted-on
  (testing "a loader cannot be made to forget a url, so a library taken out
  stays loadable - which is usually harmless, and is not by itself a restart"
    (with-process [info nil]
      (let [c (control-client info)
            lib (first (keys (:libs (basis/initial-basis))))]
        (try
          (wanting #(update % :libs dissoc lib)
            (let [reply (plan c)]
              (is (= "current" (:verdict reply)))
              (is (= [(str lib)] (:removed-libs reply)))))
          (finally (disconnect c)))))))

;;; What cost no resolving

(deftest a-link-re-pointed-makes-a-reading-due
  (with-process [info nil]
    (let [entry (temp-dir)
          one (temp-dir)
          two (temp-dir)
          c (control-client info)
          status #(request! c {:op :classpath-status :id 1})]
      (try
        (write-file! one "probe" "first.clj")
        (write-file! two "probe" "second.clj")
        (linked! (path entry "probe") (path one "probe"))
        (.addURL ^clojure.lang.DynamicClassLoader state/class-loader
                 (.toURL (.toUri (path entry))))
        (is (true? (:reading-due (status))) "a directory the reading has not seen")
        (request! c {:op :update-classpath :id 2})
        (is (false? (:reading-due (status))))
        (Files/delete (path entry "probe"))
        (linked! (path entry "probe") (path two "probe"))
        (is (true? (:reading-due (status))) "a link that leads somewhere else")
        (request! c {:op :update-classpath :id 3})
        (is (false? (:reading-due (status))))
        (finally
          (disconnect c)
          (delete-recursively entry)
          (delete-recursively one)
          (delete-recursively two)
          (classpath/rescan!))))))

(deftest a-directory-the-process-started-with-that-moved-is-frozen
  (testing "the jvm resolves each directory of java.class.path once, so a link
  that leads somewhere else since goes on being read where it led - and that
  is a restart, whatever else changed"
    (with-process [info nil]
      (let [stage (temp-dir)
            one (temp-dir)
            two (temp-dir)
            c (control-client info)
            link (path stage "src")]
        (try
          (linked! link (path one))
          (with-redefs [classpath/started-directories [[link (.toRealPath link (make-array LinkOption 0))]]]
            (is (= [] (:frozen (request! c {:op :classpath-status :id 1}))))
            (Files/delete link)
            (linked! link (path two))
            (is (= [{:entry (str link) :was (real (path one)) :now (real (path two))}]
                   (:frozen (request! c {:op :classpath-status :id 2}))))
            (wanting identity
              (is (= "restart" (:verdict (plan c))))))
          (finally
            (disconnect c)
            (delete-recursively stage)
            (delete-recursively one)
            (delete-recursively two)))))))
