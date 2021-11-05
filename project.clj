(defproject intro-to-clojure "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.3"]]
  :repl-options {:init-ns intro-to-clojure.core}
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]]}})
