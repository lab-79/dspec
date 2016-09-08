(ns lab79.dspec.helpers
  (:require [lab79.dspec :refer [semantic-spec-coll->datomic-schemas
                                 semantic-spec-coll->semantic-ast
                                 register-specs-for-ast!]]))

(defn specs->datomic
  [specs]
  (-> specs
      semantic-spec-coll->datomic-schemas))

(defn create-clojure-specs!
  [specs]
  (-> specs
      semantic-spec-coll->semantic-ast
      register-specs-for-ast!))

(defn create-generative-clojure-specs!
  "Registers all clojure.specs with custom generators. Specs are auto-created
  based off of our interface definitions in resources/data/schemas/ and
  custom generators for these specs and their fields that override the
  defaults are located in src/ccm_om_next/db/gen/all"
  [specs generators]
  (-> specs
      semantic-spec-coll->semantic-ast
      (register-specs-for-ast! generators)))
