(ns dspec.gen-test
  #?(:cljs (:require-macros cljs.spec))
  (:require #?(:clj  [clojure.test.check.properties :refer [for-all]]
               :cljs [clojure.test.check.properties :refer-macros [for-all]])
            #?(:clj  [clojure.test.check.clojure-test :refer [defspec]]
               :cljs [clojure.test.check.clojure-test :refer-macros [defspec]])
            [lab79.dspec.util.gen :refer [only-keys-gen]]
            [dspec.util :refer [instrument-all!]]
            #?(:clj  [clojure.spec :as s]
               :cljs [cljs.spec :as s])))

#?(:cljs (enable-console-print!))

(instrument-all!)

(let [gen-factory (only-keys-gen :x/x :y/y)
      _ (s/def :x/x keyword?)
      _ (s/def :y/y keyword?)
      generator (gen-factory #(s/keys))]
  (defspec test-only-keys-gen
           100
           (for-all [m generator]
                    (= #{:x/x :y/y}
                       (set (keys m))))))