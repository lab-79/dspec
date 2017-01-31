(ns lab79.dspec.helpers
  (:require [lab79.dspec :refer [dspec-coll->ast]]
            [lab79.dspec.plugins.datomic :refer [ast->schemas]]
            [lab79.dspec.plugins.clojure-spec :refer [register-specs-for-ast!]]))

(defn specs->datomic
  [specs tempid-factory]
  (-> specs
      dspec-coll->ast
      (ast->schemas tempid-factory)))

(defn create-clojure-specs!
  [specs tempid-factory db-id?]
  (-> specs
      dspec-coll->ast
      (register-specs-for-ast! tempid-factory db-id?)))