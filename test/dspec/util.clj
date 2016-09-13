(ns dspec.util
  (:import (datomic.db DbId)))

(def db-id? #(or (integer? %)
                 (instance? DbId %)))
