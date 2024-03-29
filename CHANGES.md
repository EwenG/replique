# Version 1.0.0

- Add the possibility to refresh the clojurescript "deps.cljs" file
- Fix potential races conditions when loading file concurrently from clojure/clojurescript
- Support for Clojurescript 1.11.60
- Update Clojurescript minimum version to 1.10.238

# Version 0.0.17

- Prevents the cljs REPL from hanging when connected to chrome
- Add :global-goog-object&array to the cljs compiler options that can be customized
- More robust "replique.interactive/set-cljs-compiler-opt"
- Default the Clojurescript :language-in option to :ecmascript6
- Add the possibility to reload log4j2 configurations
- Add the replique.files/create-file function
- Fix jump-to-definition for forms evaluated in the REPL without loading a file
- Support for Clojurescript 1.10.891
- The cljs REPL now binds the cljs.analyzer/*fn-invoke-direct* var
- Fix the printing of keywords by the elisp printer (escape special chars)
- Fix several bugs when requiring javascript modules
- The usage of strings in the cljs (:require ...) syntax is now correctly handled

# Version 0.0.16

- Fix replique.watch refresh/browsing errors in some cases
- Logback reloading now requires an explicit url
- Fix private var usage warning with clojurescript 1.10.439
- Add the replique.interactive/eval-js function
- cljs repl params (dynamic bindings) were sometimes unexpectedly reset
- Post hooks API changes + enable post eval hooks for Clojure

# Version 0.0.15

- Improve Clojurescript namespace/var watching
- Replique now depends on clojure tools.deps instead of Leiningen 

# Version 0.0.14

- Add the possibility to reload logback configurations
- Support for Clojurescript 1.10.238

# Version 0.0.13 

- Add the possibility to ensure that a cljs namespace and its dependencies are loaded in the cljs analysis env when starting a cljs REPL 
- Add a find-usage feature
- A .repliquedoc file can be added to directories to exclude from the Replique search based features (find-usage, main js file refreshing, ...)
- Tooling support to visualize watchable data
- Support for Clojurescript 1.10.238
- Autocompletion of system classes for jdk9+
- Fix auto completion in import contexts

# Version 0.0.12 Corrupted release - see 0.0.13

# Version 0.0.11 - Corrupted release - see 0.0.13

# Version 0.0.10

- Tooling messages exception are now returned as data instead of strings
- The cljs REPL no longer tries to automatically reconnect when the connection is closed by the cljs server because of a new connection coming in
- Clojurescript dependencies are now checked for recompilation when analyzed for the first time
- Performance improvement for the replique/reload-all command when used in a cljs context 
- Metadata for namespaces: now returns all the files of a namespace when its definition is split in multiple files 

# Version 0.0.9

- Autocompletion refactoring - fuzzy matching, autocompletion for files and javascript interop calls
- Improve the REPL startup sequence, interactive commands can now be used while the REPL is starting
- Fix Replique startup under jdk9
- Fix the compilation order of Clojurescript files (and their dependencies)
- Add metadata (line, column number, file name) to forms evaluated at the REPL from a clj/cljs buffer
- Clojurescript constants can now be reloaded
- Add support for reloading a clojurescript file an all its dependencies (like the :reload-all option of require in Clojure)

# Version 0.0.8

- Fix undefined var warning for cljs 1.9.854 +
- Fix the cljs repl when using clojurescript version 1.9.655 or 1.9.660
- Add the replique.interactive/load-url macro. clj/cljs/cljc files can now be loaded from files or from files in jar archives
- Fix a "broken link" exception when reloading a web browser page connected to the browser REPL
- The omniscient no longer always use the namespace of the session when resolving symbols 
- Replique now checks the clojurescript version number for compatibility
- The browser REPL now supports query string parameters
- Stringify all Clojure symbols in tooling messages
- Starting a REPL did not work when some dependencies were excluded
- Add the remove-var interactive macro
- Update the minimum cljs version required to 1.9.473
- Improve local bindings analysis
- Fix the omniscient debugger when updating the var being debugged in an already started omniscient repl

# Version 0.0.7

- The omniscient debugger now captures the deftype and defrecord fields
- Omniscient debugger support for defrecord
- Cljs repls did not work when starting a browser repl and a nashorn repl simultaneously
- Cljs error printing and stacktrace handling now follows clojure behavior more closely
- Classpath reloading
- Javascript errors happening during load-file are now printed by a global error handler
- Update the compliment library (autocompletion)

# Version 0.0.6

- Add support for the :checked-arrays cljs compiler option
- Remove the replique.interactive/remote-repl command
- Add an omniscient debugger
- Add the replique.interactive/repl command

# Version 0.0.5

- Add support for javafx css file reloading 
- Add support of Nashorn as a Clojurescript evaluation environment
- Post evaluation hook API for clojurescript vars and namesapces

# Version 0.0.4

- Fix compilation of cljs files with newer version of clojurescript
- Fix the :meta-cljs tooling message (regression introduced in 0.0.3)

# Version 0.0.3

- Clojurescript 1.9.456 compatibility
- When the cursor is on a class and this class was compiled from a var, the meta command returns the metadata of the var
- When the cursor is in a string representing a resource in the classpath, the meta command returns the absolute path of the resource
- The javascript environment will now automatically try to reconnect to the clojurescript server after an unexpected disconnection
- Autocompletion now handles the :renames and :rename-macros keys of clojurescript namespace declarations (available since clojurescript 1.9.183)
- Providing a hostname is now necessary to start the REPL
- Update the cljs-repl connection logic to make connecting to the cljs repl from any environment possible
WARNING: main javascript files must be recreated when updating to the 0.0.3 version

# Version 0.0.2

## Bug fixes

- output-main-js-file now munge namespace names and creates parent directories
- jump to definition always jumps to the cljs definition when editing cljc files
- replique.interactive/load-file sometimes throws an exception

## Changes

- The function used to print data into the tooling REPL can now be customized
- load-css cannot load css as data-uri anymore