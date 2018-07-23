#!/bin/sh

clojure -A:dev -Sdeps "{:deps {org.clojure/clojurescript {:mvn/version \"1.9.473\"} org.clojure/clojure {:mvn/version \"1.10.0-alpha6\"}}}"

## {org.clojure/clojurescript {:mvn/version "1.9.946"}}
## {org.clojure/clojurescript {:mvn/version "1.10.238"}}
## {org.clojure/clojurescript {:mvn/version "1.10.339"}}