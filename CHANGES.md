# Version 0.0.8

- fix the omniscient debugger when updating the var beeing debugged in an already started omniscient repl

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