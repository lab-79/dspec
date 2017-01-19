(ns lab79.dspec.helpers
  (:require [lab79.dspec :refer [dspec-coll->ast]]
            [lab79.dspec.plugins.datomic :refer [ast->schemas]]
            [lab79.dspec.plugins.clojure-spec :refer [register-specs-for-ast!
                                                      register-specs-for-ast-with-custom-generators!]]))

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

(defn create-clojure-specs-with-custom-generators!
  "Registers all clojure.specs with custom generators. Specs are auto-created
  based off of our interface definitions in resources/data/schemas/ and
  custom generators for these specs and their fields that override the
  defaults are located in src/ccm_om_next/db/gen/all"
  [specs generators tempid-factory db-id?]
  (-> specs
      dspec-coll->ast
      (register-specs-for-ast-with-custom-generators! generators tempid-factory db-id?)))