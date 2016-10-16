(ns dspec.util
  #?(:clj (:import (datomic.db DbId))))

(def db-id? #(or (integer? %)
                 #?(:clj (instance? DbId %)
                    :cljs (= :db/current-tx %))))
