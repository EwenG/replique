# Version 0.0.5

- The cljs version of the load-file macro does not load files twice anymore

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