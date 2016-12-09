# Version 0.0.3

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