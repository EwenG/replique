# Replique

Replique is a development environment for Clojure and Clojurescript implemented as a [Leiningen](https://github.com/technomancy/leiningen) plugin.
The easiest way to start with Replique is to use a client. The only client currently available is an [emacs mode](https://github.com/EwenG/replique.el).
If you are interested in writing a client for another text editor/ide, please fill an issue to ask for more documentation :)

## Overview

The following is an overview of Replique features, demonstrating the use of Replique with only the help of a terminal.

Create an empty leiningen project. Add Replique to your plugins and Clojurescript to your depenencies. Clojurescript is necessary because we will later start a Clojurescript REPL.

```clojure
(defproject replique-demo "0.0.1"
  :dependencies [[org.clojure/clojurescript "x.x.x"]]
  :plugins [[replique/replique "0.0.2-SNAPSHOT"]])
```

From the project directory, start a REPL server on port 9000:

`lein replique 9000`

In an other terminal, connect to the REPL server:

`telnet localhost 9000`

Enter the 'R' character. You must enter this first character before anything else. The reason is that the REPL server is able to understand several protocols and, in order to recognise the right protocol, the server checks the first character sent by the client.

You are now in a Clojure REPL. You will notice that the current REPL does not print a prompt. The reason is Replique is designed to be easy to integrate into a text editor or an IDE. By not printing the prompt we make the REPL output easier to parse for the machine.
You can enter any Clojure expression to get it evaluated.

`(+ 1 2)`
=> `3`

Now let's start a more familiar Clojure REPL:

`(replique.repl/repl)`

A prompt is now printed. Again, you can enter any Clojure expression.

Let's start a Clojurescipt REPL. Be sure you added a Clojurescript dependency to your project.clj file, as described earlier.

`(replique.interactive/cljs-repl)`

Wait a few seconds for the Clojurescript core libraries to be compiled. Javascript files get compiled to the `target/cljs` folder. As you can see printed in your terminal, the Clojurescript REPL is waiting for a browser to connect on port 900 ...

`Waiting for browser to connect on port 9000 ...`

Open a browser tab at `localhost:9000`

You are now able to evaluate single Clojurescript expressions at the REPL:

`(js/alert "Hello from Replique")`

You can also load entire files. Create a file named `test.cljs` at the root of the current directory. Add the following content to the new file:

```clojure
(ns test)
(js/alert "This file has been compiled to disk and loaded in the browser")
```

Load the file in the browser:

`(replique.interactive/load-file "test.cljs")`

The file has been loaded in the browser AND compiled to disk. It can be linked in some HTML markup to be automaically loaded after a browser refresh.

Quit the Clojurescript REPL to come back to the clojure REPL:

`:cljs/quit`

Replique can be used to enhance your REPL with tooling features. Tooling features are optional. The code supporting them has not been loaded into the Clojure process yet. To leverage this features, your best bet is probably to use a plugin for your favorite editor or IDE. The following demonstrate how Replique can be used to provide autocompletion.

Start a "tooling" REPL.

`(replique.repl/shared-tooling-repl :edn)`

The tooling REPL is designed to be consumed by the machine. This is why no prompt is printed. The first parameter, `:edn`, instructs the REPL to print the results of evaluation to the [EDN](https://github.com/edn-format/edn) format. The tooling REPL currently supports two formats: EDN and [elisp](https://en.wikipedia.org/wiki/Emacs_Lisp).

You can now query the tooling REPL for some code completion:

```clojure
(replique.tooling-msg/tooling-msg-handle {:type :clj-completion
                                          :context nil
                                          :ns 'replique.repl
                                          :prefix "tooli"})
```
=> ```clojure
{:type :clj-completion
 :process-id #uuid "5e1ffe41-797a-4379-a52a-e1d82d1704cd"
 :candidates ({:candidate "tooling-msg" :type :namespace}
              {:candidate "tooling-repl" :type :function :ns "replique.repl"})}
```



---

## License

Copyright 2016 Ewen Grosjean.

The use and distribution terms for this software are covered by the
Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0)
which can be found in the file [epl-v10.html](epl-v10.html) at the root of this distribution.

By using this software in any fashion, you are agreeing to be bound by
the terms of this license.

You must not remove this notice, or any other, from this software.
