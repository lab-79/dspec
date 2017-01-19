(ns lab79.dspec.specs.ast
  "clojure.spec's describing the AST into which dev-defined dspecs are transformed."
  (:require [clojure.spec :as s]))

(s/def :interface/ast
  (s/keys :req [:interface.ast/interfaces :interface.ast/enum-map]))

(s/def :interface.ast/interfaces
  (s/map-of keyword? :interface.ast/interface))

(s/def :interface.ast/interface
  (s/keys :req [:interface.ast.interface/name :interface.ast.interface/fields :interface.ast.interface/identify-via]
          :opt [:interface.ast.interface/inherits]))

(s/def :interface.ast.interface/name keyword?)

(s/def :interface.ast.interface/fields (s/map-of keyword? :interface.ast/field))

(s/def :interface.ast.interface/inherits (s/coll-of keyword? :kind set?))

(s/def :interface.ast.interface/identify-via (s/coll-of :datalog/clause :kind vector? :min-count 1))

(s/def :interface.ast/enum-map
  (s/map-of keyword? :interface.ast/enum))

(s/def :interface.ast/enum (s/keys :req [:db/ident]
                                   :opt [:db/doc :db/part]))

(s/def :interface.ast/field
  (s/keys :req [:db/ident
                :interface.ast.field/type
                :db/valueType
                :db/cardinality]
          :opt [:interface.ast.field/possible-enum-vals
                :interface.ast.field/required
                :db/doc
                :db/unique
                :db/index
                :db/isComponent
                :db/noHistory
                :db/fulltext
                :gen/should-generate]))

(s/def :interface.ast.field/type keyword?)

(s/def :interface.ast.field/possible-enum-vals (s/+ keyword?))

(s/def :interface.ast.field/required boolean?)