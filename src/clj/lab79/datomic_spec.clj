(ns lab79.datomic-spec
  "clojure.spec defintions for Datomic"
  (:require [clojure.spec :as s]
            [clojure.test.check.generators :as tcgen]
            [clojure.spec.gen :as gen]))

(def datomic-value-types
  #{:db.type/string :db.type/boolean :db.type/long :db.type/bigint :db.type/float :db.type/double :db.type/bigdec
    :db.type/instant :db.type/uuid :db.type/uri :db.type/keyword :db.type/bytes :db.type/ref})

(def datomic-schema-keys
  #{:db/id :db/ident :db/valueType :db/cardinality :db/doc :db/unique :db/index :db/isComponent :db/noHistory
    :db/fulltext :db.install/_attribute :db.install/_partition})

;
; Special Datomic attributes
;

(s/def :db/ident (s/with-gen keyword? #(tcgen/resize 2 tcgen/keyword-ns)))
(s/def :db/valueType datomic-value-types)
(s/def :db/cardinality #{:db.cardinality/one :db.cardinality/many})
(s/def :db/doc string?)
(s/def :db/unique #{:db.unique/value :db.unique/identity})
(s/def :db/index boolean?)
(s/def :db/isComponent boolean?)
(s/def :db/noHistory boolean?)
(s/def :db/fulltext boolean?)

(s/def :db.install/_attribute #{:db.part/db})

(s/def :datomic/field-schema (s/keys :req [:db/ident :db/valueType :db/cardinality :db/id :db.install/_attribute]
                                     :opt [:db/doc :db/unique :db/index :db/isComponent :db/noHistory :db/fulltext]))

;
; Partitions
;


(s/def :db.install/_partition #{:db.part/db})

(s/def :datomic/partition-schema (s/keys :req [:db/id :db/ident :db.install/_partition]))

(s/def :datomic/enum-schema (s/keys :req [:db/id :db/ident] :opt [:db/doc]))

;
; Datalog values
;

(s/def :datomic.value/any (s/or :kw keyword?
                                :str string?
                                :bool boolean?
                                :num number?
                                :int integer?
                                :float float?
                                :inst inst?
                                :uuid uuid?
                                :uri uri?
                                :bytes bytes?))

;
; Datalog
;

(s/def :datalog/var (s/with-gen symbol? #(gen/return '?e)))
(s/def :datalog/find-vars (s/alt :relation (s/cat :relations (s/+ :datalog/var))
                                 :single-scalar (s/cat :scalar :datalog/var
                                                       :. #{'.})
                                 :collection (s/spec (s/tuple :datalog/var #{'...}))
                                 :single-tuple (s/coll-of :datalog/var :kind vector?)))
(s/def :datalog/find (s/cat :find #{:find}
                            :find-vars :datalog/find-vars))
(s/def :datalog/where (s/cat :where #{:where}
                             :clauses (s/+ :datalog/where-clause)))

(s/def :datalog/tuple-clause (s/tuple #{'?e} :db/ident))
(s/def :datalog/triplet-clause (s/tuple #{'?e} :db/ident :datomic.value/any))
(s/def :datalog/or-clause (s/spec (s/cat :or-sym #{'or}
                                         :clauses (s/+ (s/alt :clause :datalog/where-clause
                                                              :and-clause :datalog/and-clause)))))
(s/def :datalog/and-clause (s/spec (s/cat :and-sym #{'and}
                                          :clauses (s/+ :datalog/where-clause))))
(s/def :datalog/not-clause (s/spec (s/cat :not-sym #{'not}
                                          :clauses (s/+ :datalog/where-clause))))
(s/def :datalog/where-clause
  (s/with-gen
    (s/or  :datalog/tuple-clause :datalog/tuple-clause
                                 :datalog/triplet-clause :datalog/triplet-clause
                                 :datalog/or-clause
                                 :datalog/not-clause)
    #(s/gen :datalog/triplet-clause)))

(s/def :datomic.query.kv/find
  (s/with-gen (s/spec (s/cat :find-rhs :datalog/find-vars))
              #(gen/return ['?e '.])))
(s/def :datomic.query.kv/where (s/coll-of :datalog/where-clause :kind vector? :min-count 1))
(s/def :datomic/map-query (s/keys :req-un [:datomic.query.kv/find :datomic.query.kv/where]))
(s/def :datomic/vec-query (s/spec (s/cat :find :datalog/find
                                         :where :datalog/where)))
(s/def :datomic.api/q
  (s/fspec :args (s/cat :query (s/with-gen
                                 (s/alt :map-query :datomic/map-query
                                        :vector-query :datomic/vec-query)
                                 #(gen/return {:find ['?e '.] :where [['?e :db/id]]})
                                 ;#(s/gen :datomic/map-query)
                                 )
                         :db (s/coll-of (s/tuple :db/id
                                                 :db/ident
                                                 :datomic.value/any))
                         :params (s/with-gen
                                   (s/* any?)
                                   ; Never generate parameters
                                   #(gen/return [])))
            :ret any?))
