(defproject lab79/dspec "0.3.9-SNAPSHOT"
  :description "Stronger semantics on top of Datomic, with clojure.spec goodies."
  :url "https://github.com/lab-79/dspec"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src/cljc"]
  :test-paths ["test"]
  :dependencies [
                 [org.clojure/clojure "1.9.0-alpha13"]
                 [org.clojure/clojurescript "1.9.229"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [com.rpl/specter "0.12.0"]
                 [com.stuartsierra/dependency "0.2.0"]
                 [org.clojure/test.check "0.9.0"]
                 [lab79/datomic-spec "0.1.3"]
                 ]
  :profiles {:dev {:dependencies
                    [
                     [bolth "0.1.0"]
                     [org.clojure/tools.nrepl "0.2.11"]
                     [org.clojure/tools.namespace "0.2.11"]
                     [com.datomic/datomic-free "0.9.5390" :exclusions [com.google.guava/guava]]
                     [datascript "0.15.4"]
                     ]}}
  :cljsbuild {:builds [{:id "test"
                        :source-paths ["src/cljc", "test"]
                        :compiler {:output-to "target/testable.js"
                                   :output-dir "target/test-js/"
                                   :main "dspec.test-all"
                                   :target :nodejs
                                   :optimizations :simple
                                   :hashbang false}}]
              :test-commands {"unit-tests"
                              ;["phantomjs" "target/testable.js" "resources/test/test.html"]
                              ["node" "target/testable.js"]
                              }}

  :plugins [[lein-cloverage "1.0.7-SNAPSHOT"]
            [lein-cljsbuild "1.1.4"]])
