(defproject kerbal "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [com.google.protobuf/protobuf-java "3.6.1"]
                 [org.javatuples/javatuples "1.2"]
                 [krpc/krpc-java "0.4.8"]]
  :repl-options {:init-ns kerbal.core})
