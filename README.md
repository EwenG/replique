# Replique

Replique is a development environment for Clojure and Clojurescript.

Replique relies on the Clojure [command line tools](https://clojure.org/guides/deps_and_cli) for starting REPLs.
The Clojure command line tools must be [installed](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools) before using Replique.

If you are interested in using Replique, you should check out its [emacs mode](https://github.com/EwenG/replique.el).

## Overview

The following is an overview of Replique features, demonstrating the use of Replique with only the help of a terminal.

Create an empty directory named "replique-demo". In the replique-demo directory, create a file named "deps.edn" with the following content.

```clojure
{:deps {replique.replique {:git/url "https://github.com/EwenG/replique.git" 
                           :sha "be37cd5d48ed6585bd343a4437fe6bcefd365ec1"
                           :tag "0.0.16"}
        org.clojure/clojurescript {:mvn/version "1.10.516"}}}
```

The dependency on Clojurescript is not strictly needed but is added because we will later start a Clojurescript REPL.

From the project directory, start a REPL server on port 9000:

`clj -J-Dreplique.server.port=9000 -J-Dreplique.http-server.port=9001 -m replique.main`

In an other terminal, connect to the REPL server:

`telnet localhost 9000`

You are now in a Clojure REPL. You will notice that the current REPL does not print a prompt. The reason is Replique is designed to be easy to integrate into a text editor or an IDE. By not printing the prompt we make the REPL output easier to parse for the machine.
You can enter any Clojure expression to get it evaluated.

`(+ 1 2)`
=> `3`

Now let's start a more familiar Clojure REPL:

`(replique.repl/repl)`

A prompt is now printed. Again, you can enter any Clojure expression.

Let's start a Clojurescipt REPL. Ensure that you added a Clojurescript dependency to your deps.edn file, as described earlier.

`(replique.interactive/cljs-repl)`

Wait a few seconds for the Clojurescript core libraries to be compiled. Javascript files get compiled to the `target/cljs` folder. As you can now see, the Clojurescript REPL is waiting for a browser to connect on port 9000 ...

`Waiting for browser to connect on port 9001 ...`

Open a browser tab at `localhost:9001`

You are now able to evaluate Clojurescript expressions at the REPL:

`(js/alert "Hello from Replique")`

You can also load entire files. Create a file named `test.cljs` at the root of the current directory. Add the following content to the new file:

```clojure
(ns test)
(js/alert "This file has been compiled to disk and loaded in the browser")
```

Load the file in the browser:

`(replique.interactive/load-file "test.cljs")`

The file has been loaded in the browser AND compiled to disk. It can be linked in HTML markup to be automatically loaded after a browser refresh.

Quit the Clojurescript REPL to come back to the Clojure REPL:

`:cljs/quit`

Replique can be used to enhance your REPL with tooling features. Tooling features are optional. The code supporting them has not been loaded into the Clojure process yet. To leverage tooling features, it is best to use an editor or IDE.

Let's start a "tooling" REPL.

`(replique.repl/shared-tooling-repl :edn)`

Notice that no prompt is printed. By not pinting a prompt, we make the REPL output easier to parse by the machine. The first parameter, `:edn`, instructs the REPL to print the tooling messages using the [EDN](https://github.com/edn-format/edn) format. The tooling REPL currently supports two formats: EDN and [elisp](https://en.wikipedia.org/wiki/Emacs_Lisp).

Enter the following in the tooling REPL:

```clojure
(replique.tooling-msg/tooling-msg-handle {:type :completion 
                                          :repl-env :replique/clj
                                          :context nil
                                          :ns 'replique.repl
                                          :prefix "tooli"})
```
=> 

```clojure
{:candidates ({:candidate "tooling-msg", :type :namespace, :match-index 5} 
              {:candidate "tooling-repl", :type :function, :ns "replique.repl", :match-index 5} 
              {:candidate "replique.tooling", :type :namespace, :match-index 14} 
              {:candidate "shared-tooling-repl", :type :function, :ns "replique.repl", :match-index 12} 
              {:candidate "replique.tooling-msg", :type :namespace, :match-index 14}),
 :type :completion,
 :repl-env :replique/clj,
 :context nil,
 :ns replique.repl,
 :prefix "tooli"}
```

We are done, kill the process to exit.

---

## License

Copyright 2016 Ewen Grosjean.

The use and distribution terms for this software are covered by the
Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0)
which can be found in the file [epl-v10.html](epl-v10.html) at the root of this distribution.

By using this software in any fashion, you are agreeing to be bound by
the terms of this license.

You must not remove this notice, or any other, from this software.
