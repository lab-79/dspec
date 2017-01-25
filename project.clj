(defproject lab79/dspec "0.3.8"
  :description "Stronger semantics on top of Datomic, with clojure.spec goodies."
  :url "https://github.com/lab-79/dspec"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src/clj"]
  :test-paths ["test"]
  :dependencies [
                 [org.clojure/clojure "1.9.0-alpha12"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [com.rpl/specter "0.13.2"]
                 [com.stuartsierra/dependency "0.2.0"]
                 [org.clojure/test.check "0.9.0"]
                 [lab79/datomic-spec "0.1.2"]
                 ]
  :profiles {:dev {:dependencies
                    [
                     [bolth "0.1.0"]
                     [org.clojure/tools.nrepl "0.2.11"]
                     [org.clojure/tools.namespace "0.2.11"]
                     [com.datomic/datomic-free "0.9.5390"]
                     ]}}
  :plugins [[lein-cloverage "1.0.7-SNAPSHOT"]])
