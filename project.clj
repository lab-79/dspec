(defproject lab79/dspec "0.1.1"
  :description "Stronger semantics on top of Datomic, with clojure.spec goodies."
  :url "https://github.com/lab-79/dspec"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src/clj"]
  :test-paths ["test"]
  :dependencies [
                 [org.clojure/clojure "1.9.0-alpha12"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [com.rpl/specter "0.12.0"]
                 [com.stuartsierra/dependency "0.2.0"]
                 ]
  :profiles {:dev {:dependencies
                    [
                     [org.clojure/test.check "0.9.0"]
                     [bolth "0.1.0"]
                     [org.clojure/tools.nrepl "0.2.11"]
                     [org.clojure/tools.namespace "0.2.11"]
                     [com.datomic/datomic-free "0.9.5390"]
                     ]}})
