(ns datomic-spec.core-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [org.lab79.datomic-spec :refer :all]
            [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [datomic.api :as d]
            ))


(def family-semantic-specs
  [{:interface.def/name :interface/child
    :interface.def/fields {:person/name [:string]}
    :interface.def/inherits [:interface/mother :interface/father]
    :interface.def/identify-via :datomic-spec/interfaces}
   {:interface.def/name :interface/mother
    :interface.def/fields {:person/personality [:enum #{:happy :sad}]}
    :interface.def/identify-via :datomic-spec/interfaces}
   {:interface.def/name :interface/father
    :interface.def/fields {:person/bald? [:boolean]}
    :interface.def/identify-via :datomic-spec/interfaces}])

(deftest tests
  (testing "a semantic spec"
    (testing "identifying its type via :datomic-spec/interfaces"
      (let [spec {:interface.def/name :interface/eponym
                  :interface.def/fields {}
                  :interface.def/identify-via :datomic-spec/interfaces}]
        (testing "generating Datomic schemas"
          (let [datomic-schemas (-> spec
                                    semantic-spec->semantic-ast
                                    (semantic-ast->datomic-schemas d/tempid))]
            (is (= #{{:db/ident :datomic-spec/interfaces
                      :db/valueType :db.type/ref
                      :db/cardinality :db.cardinality/many
                      :db.install/_attribute :db.part/db}
                     {:db/ident :interface/eponym}}
                   (set (map #(dissoc % :db/id) datomic-schemas))))))))
    (testing "with primitively typed attributes"
      (testing "of type :keyword"
        (let [spec {:interface.def/name :interface/entity-with-keyword
                    :interface.def/fields {:obj/keyword-attr [:keyword "A keyword attribute"]}
                    :interface.def/identify-via ['[?e :obj/keyword-attr]]}
              ast (semantic-spec->semantic-ast spec)]
          (testing "semantic-spec->semantic-ast"
            (is (= ast
                   {:interface.ast/interfaces
                                            {:interface/entity-with-keyword
                                             {:interface.ast.interface/name :interface/entity-with-keyword
                                              :interface.ast.interface/fields
                                              {:obj/keyword-attr {:db/ident :obj/keyword-attr
                                                                  :db/valueType :db.type/keyword
                                                                  :interface.ast.field/type :keyword
                                                                  :db/cardinality :db.cardinality/one
                                                                  :db/doc "A keyword attribute"}}
                                              :interface.ast.interface/inherits #{}
                                              :interface.ast.interface/identify-via ['[?e :obj/keyword-attr]]}}
                    :interface.ast/enum-map {}})))
          (testing "generating clojure.spec definitions"
            (register-specs-for-ast! ast)
            (testing "for entity attributes"
              (is (s/valid? :obj/keyword-attr :ok))
              (is (= ::s/invalid (s/conform :obj/keyword-attr "ok"))))
            (testing "for entity interfaces"
              (is (s/valid? :interface/entity-with-keyword {:db/id (d/tempid :db.part/user)
                                                            :obj/keyword-attr :ok}))
              (is (= ::s/invalid (s/conform :interface/entity-with-keyword {:db/id (d/tempid :db.part/user)
                                                                            :obj/keyword-attr "ok"}))))
            (testing "and generating data"
              (let [generator (s/gen :interface/entity-with-keyword)
                    entity (gen/generate generator)]
                (is (contains? entity :db/id))
                (is (or (not (contains? entity :obj/keyword-attr))
                        (keyword? (entity :obj/keyword-attr)))))))))
      (testing "of type :string"
        (let [spec {:interface.def/name :interface/entity-with-string
                    :interface.def/fields {:obj/string-attr [:string "A string attribute"]}
                    :interface.def/identify-via ['[?e :obj/string-attr]]}
              ast (semantic-spec->semantic-ast spec)]
          (testing "semantic-spec->semantic-ast"
            (is (= ast
                   {:interface.ast/interfaces
                                            {:interface/entity-with-string
                                             {:interface.ast.interface/name :interface/entity-with-string
                                              :interface.ast.interface/fields
                                              {:obj/string-attr {:db/ident :obj/string-attr
                                                                 :db/valueType :db.type/string
                                                                 :interface.ast.field/type :string
                                                                 :db/cardinality :db.cardinality/one
                                                                 :db/doc "A string attribute"}}
                                              :interface.ast.interface/inherits #{}
                                              :interface.ast.interface/identify-via ['[?e :obj/string-attr]]}}
                    :interface.ast/enum-map {}})))))
      (testing "of type :boolean"
        (let [spec {:interface.def/name :interface/entity-with-boolean
                    :interface.def/fields {:obj/boolean-attr [:boolean "A boolean attribute" :gen/should-generate]}
                    :interface.def/identify-via ['[?e :obj/boolean-attr]]}
              ast (semantic-spec->semantic-ast spec)]
          (testing "semantic-spec->semantic-ast"
            (is (= ast
                   {:interface.ast/interfaces
                                            {:interface/entity-with-boolean
                                             {:interface.ast.interface/name :interface/entity-with-boolean
                                              :interface.ast.interface/fields
                                              {:obj/boolean-attr {:db/ident :obj/boolean-attr
                                                                  :db/valueType :db.type/boolean
                                                                  :interface.ast.field/type :boolean
                                                                  :db/cardinality :db.cardinality/one
                                                                  :db/doc "A boolean attribute"
                                                                  :gen/should-generate true}}
                                              :interface.ast.interface/inherits #{}
                                              :interface.ast.interface/identify-via ['[?e :obj/boolean-attr]]}}
                    :interface.ast/enum-map {}})))
          (testing "generating clojure.spec definitions"
            (register-specs-for-ast! ast)
            (testing "for entity attributes"
              (is (s/valid? :obj/boolean-attr true))
              (is (= ::s/invalid (s/conform :obj/boolean-attr "true"))))
            (testing "and generating data"
              (let [generator (s/gen :interface/entity-with-boolean)
                    entity (gen/generate generator)]
                (is (boolean? (:obj/boolean-attr entity))))))))
      (testing "of type :long"
        (let [spec {:interface.def/name :interface/entity-with-long
                    :interface.def/fields {:obj/long-attr [:long "A long attribute"]}
                    :interface.def/identify-via ['[?e :obj/long-attr]]}]
          (testing "semantic-spec->semantic-ast"
            (let [ast (semantic-spec->semantic-ast spec)]
              (is (= ast
                     {:interface.ast/interfaces
                                              {:interface/entity-with-long
                                               {:interface.ast.interface/name :interface/entity-with-long
                                                :interface.ast.interface/fields
                                                {:obj/long-attr {:db/ident :obj/long-attr
                                                                 :db/valueType :db.type/long
                                                                 :interface.ast.field/type :long
                                                                 :db/cardinality :db.cardinality/one
                                                                 :db/doc "A long attribute"}}
                                                :interface.ast.interface/inherits #{}
                                                :interface.ast.interface/identify-via ['[?e :obj/long-attr]]}}
                      :interface.ast/enum-map {}}))))))
      (testing "of type :bigint"
        (let [spec {:interface.def/name :interface/entity-with-bigint
                    :interface.def/fields {:obj/bigint-attr [:bigint "A bigint attribute"]}
                    :interface.def/identify-via ['[?e :obj/bigint-attr]]}
              ast (semantic-spec->semantic-ast spec)]
          (testing "semantic-spec->semantic-ast"
            (let [ast (semantic-spec->semantic-ast spec)]
              (is (= ast
                     {:interface.ast/interfaces
                                              {:interface/entity-with-bigint
                                               {:interface.ast.interface/name :interface/entity-with-bigint
                                                :interface.ast.interface/fields
                                                {:obj/bigint-attr {:db/ident :obj/bigint-attr
                                                                 :db/valueType :db.type/bigint
                                                                 :interface.ast.field/type :bigint
                                                                 :db/cardinality :db.cardinality/one
                                                                 :db/doc "A bigint attribute"}}
                                                :interface.ast.interface/inherits #{}
                                                :interface.ast.interface/identify-via ['[?e :obj/bigint-attr]]}}
                      :interface.ast/enum-map {}}))))))

      (testing "of type :float"
        (let [spec {:interface.def/name :interface/entity-with-float
                    :interface.def/fields {:obj/float-attr [:float "A float attribute"]}
                    :interface.def/identify-via ['[?e :obj/float-attr]]}]
          (testing "semantic-spec->semantic-ast"
            (let [ast (semantic-spec->semantic-ast spec)]
              (is (= ast
                     {:interface.ast/interfaces
                                              {:interface/entity-with-float
                                               {:interface.ast.interface/name :interface/entity-with-float
                                                :interface.ast.interface/fields
                                                {:obj/float-attr {:db/ident :obj/float-attr
                                                                   :db/valueType :db.type/float
                                                                   :interface.ast.field/type :float
                                                                   :db/cardinality :db.cardinality/one
                                                                   :db/doc "A float attribute"}}
                                                :interface.ast.interface/inherits #{}
                                                :interface.ast.interface/identify-via ['[?e :obj/float-attr]]}}
                      :interface.ast/enum-map {}}))))))
      (testing "of type :double"
        (let [spec {:interface.def/name :interface/entity-with-double
                    :interface.def/fields {:obj/double-attr [:double "A double attribute"]}
                    :interface.def/identify-via ['[?e :obj/double-attr]]}]
          (testing "semantic-spec->semantic-ast"
            (let [ast (semantic-spec->semantic-ast spec)]
              (is (= ast
                     {:interface.ast/interfaces
                                              {:interface/entity-with-double
                                               {:interface.ast.interface/name :interface/entity-with-double
                                                :interface.ast.interface/fields
                                                {:obj/double-attr {:db/ident :obj/double-attr
                                                                   :db/valueType :db.type/double
                                                                   :interface.ast.field/type :double
                                                                   :db/cardinality :db.cardinality/one
                                                                   :db/doc "A double attribute"}}
                                                :interface.ast.interface/inherits #{}
                                                :interface.ast.interface/identify-via ['[?e :obj/double-attr]]}}
                      :interface.ast/enum-map {}}))))))
      (testing "of type :bigdec"
        (let [spec {:interface.def/name :interface/entity-with-bigdec
                    :interface.def/fields {:obj/bigdec-attr [:bigdec "A bigdec attribute"]}
                    :interface.def/identify-via ['[?e :obj/bigdec-attr]]}]
          (testing "semantic-spec->semantic-ast"
            (let [ast (semantic-spec->semantic-ast spec)]
              (is (= ast
                     {:interface.ast/interfaces
                                              {:interface/entity-with-bigdec
                                               {:interface.ast.interface/name :interface/entity-with-bigdec
                                                :interface.ast.interface/fields
                                                {:obj/bigdec-attr {:db/ident :obj/bigdec-attr
                                                                   :db/valueType :db.type/bigdec
                                                                   :interface.ast.field/type :bigdec
                                                                   :db/cardinality :db.cardinality/one
                                                                   :db/doc "A bigdec attribute"}}
                                                :interface.ast.interface/inherits #{}
                                                :interface.ast.interface/identify-via ['[?e :obj/bigdec-attr]]}}
                      :interface.ast/enum-map {}}))))))
      (testing "of type :instant"
        (let [spec {:interface.def/name :interface/entity-with-instant
                    :interface.def/fields {:obj/instant-attr [:instant "An instant attribute" :gen/should-generate]}
                    :interface.def/identify-via ['[?e :obj/instant-attr]]}
              ast (semantic-spec->semantic-ast spec)]
          (testing "semantic-spec->semantic-ast"
            (is (= ast
                   {:interface.ast/interfaces
                                            {:interface/entity-with-instant
                                             {:interface.ast.interface/name :interface/entity-with-instant
                                              :interface.ast.interface/fields
                                              {:obj/instant-attr {:db/ident :obj/instant-attr
                                                                  :db/valueType :db.type/instant
                                                                  :interface.ast.field/type :instant
                                                                  :db/cardinality :db.cardinality/one
                                                                  :db/doc "An instant attribute"
                                                                  :gen/should-generate true}}
                                              :interface.ast.interface/inherits #{}
                                              :interface.ast.interface/identify-via ['[?e :obj/instant-attr]]}}
                    :interface.ast/enum-map {}})))
          (testing "generating clojure.spec definitions"
            (register-specs-for-ast! ast)
            (testing "for entity attributes"
              (is (s/valid? :obj/instant-attr #inst "2014-05-19T19:12:37.925-00:00"))
              (is (= ::s/invalid (s/conform :obj/instant-attr "not a date"))))
            (testing "and generating data"
              (let [generator (s/gen :interface/entity-with-instant)
                    entity (gen/generate generator)]
                (is (inst? (:obj/instant-attr entity))))))))
      (testing "of type :uuid"
        (let [spec {:interface.def/name :interface/entity-with-uuid
                    :interface.def/fields {:obj/id [:uuid "A uuid" :gen/should-generate]}
                    :interface.def/identify-via ['[?e :obj/id]]}
              ast (semantic-spec->semantic-ast spec)]
          (testing "semantic-spec->semantic-ast"
            (is (= ast
                   {:interface.ast/interfaces
                                            {:interface/entity-with-uuid
                                             {:interface.ast.interface/name :interface/entity-with-uuid
                                              :interface.ast.interface/fields
                                              {:obj/id {:db/ident :obj/id
                                                        :db/valueType :db.type/uuid
                                                        :interface.ast.field/type :uuid
                                                        :db/cardinality :db.cardinality/one
                                                        :db/doc "A uuid"
                                                        :gen/should-generate true}}
                                              :interface.ast.interface/inherits #{}
                                              :interface.ast.interface/identify-via ['[?e :obj/id]]}}
                    :interface.ast/enum-map {}})))
          (testing "generating clojure.spec definitions"
            (register-specs-for-ast! ast)
            (testing "for entity attributes"
              (is (s/valid? :obj/id #uuid "91d7fcc5-d24d-4e33-a111-6ba69d14eb6a"))
              (is (= ::s/invalid (s/conform :obj/id "91d7fcc5-d24d-4e33-a111-6ba69d14eb6a"))))
            (testing "and generating data"
              (let [generator (s/gen :interface/entity-with-uuid)
                    entity (gen/generate generator)]
                (is (uuid? (:obj/id entity))))))))
      (testing "of type :uri"
        (let [spec {:interface.def/name :interface/entity-with-uri
                    :interface.def/fields {:obj/uri-attr [:uri "A uri attribute"]}
                    :interface.def/identify-via ['[?e :obj/uri-attr]]}
              ast (semantic-spec->semantic-ast spec)]
          (testing "semantic-spec->semantic-ast"
            (is (= ast
                   {:interface.ast/interfaces
                                            {:interface/entity-with-uri
                                             {:interface.ast.interface/name :interface/entity-with-uri
                                              :interface.ast.interface/fields
                                              {:obj/uri-attr {:db/ident :obj/uri-attr
                                                              :db/valueType :db.type/uri
                                                              :interface.ast.field/type :uri
                                                              :db/cardinality :db.cardinality/one
                                                              :db/doc "A uri attribute"}}
                                              :interface.ast.interface/inherits #{}
                                              :interface.ast.interface/identify-via ['[?e :obj/uri-attr]]}}
                    :interface.ast/enum-map {}})))
          (testing "generating clojure.spec definitions"
            (register-specs-for-ast! ast)
            (testing "for entity attributes"
              (is (s/valid? :obj/uri-attr (java.net.URI/create "http://google.com/")))
              (is (= ::s/invalid (s/conform :obj/uri-attr "google.com")))))))
      (testing "of type :bytes"
        (let [spec {:interface.def/name :interface/entity-with-bytes
                    :interface.def/fields {:obj/bytes-attr [:bytes "A bytes attribute"]}
                    :interface.def/identify-via ['[?e :obj/bytes-attr]]}
              ast (semantic-spec->semantic-ast spec)]
          (testing "semantic-spec->semantic-ast"
            (is (= ast
                   {:interface.ast/interfaces
                                            {:interface/entity-with-bytes
                                             {:interface.ast.interface/name :interface/entity-with-bytes
                                              :interface.ast.interface/fields
                                              {:obj/bytes-attr {:db/ident :obj/bytes-attr
                                                                :db/valueType :db.type/bytes
                                                                :interface.ast.field/type :bytes
                                                                :db/cardinality :db.cardinality/one
                                                                :db/doc "A bytes attribute"}}
                                              :interface.ast.interface/inherits #{}
                                              :interface.ast.interface/identify-via ['[?e :obj/bytes-attr]]}}
                    :interface.ast/enum-map {}})))
          (testing "generating clojure.spec definitions"
            (register-specs-for-ast! ast)
            (testing "for entity attributes"
              (is (s/valid? :obj/bytes-attr (bytes (byte-array (map (comp byte int) "ascii")))))
              (is (= ::s/invalid (s/conform :obj/bytes-attr "xyz"))))))))
    (testing "with enum attributes"
      (testing "without docstring per enum"
        (let [spec {:interface.def/name :interface/entity-with-enum
                    :interface.def/fields {:obj/enum-attr ["An enum attribute"
                                                           :enum #{:some.enum/a :some.enum/b}]}
                    :interface.def/identify-via ['[?e :obj/enum-attr]]}
              ast (semantic-spec->semantic-ast spec)]
          (testing "semantic-spec->semantic-ast"
            (is (= ast
                   {:interface.ast/interfaces
                                            {:interface/entity-with-enum
                                             {:interface.ast.interface/name :interface/entity-with-enum
                                              :interface.ast.interface/fields
                                              {:obj/enum-attr {:db/ident :obj/enum-attr
                                                               :db/valueType :db.type/ref
                                                               :interface.ast.field/type :enum
                                                               :interface.ast.field/possible-enum-vals #{:some.enum/a :some.enum/b}
                                                               :db/cardinality :db.cardinality/one
                                                               :db/doc "An enum attribute"}}
                                              :interface.ast.interface/inherits #{}
                                              :interface.ast.interface/identify-via ['[?e :obj/enum-attr]]}}
                    :interface.ast/enum-map {:some.enum/a {:db/ident :some.enum/a}
                                             :some.enum/b {:db/ident :some.enum/b}}})))
          (testing "generating clojure.spec definitions"
            (register-specs-for-ast! ast)
            (testing "for entity attributes"
              (is (s/valid? :obj/enum-attr :some.enum/a))
              (is (s/valid? :obj/enum-attr :some.enum/b))
              (is (= ::s/invalid (s/conform :obj/enum-attr :some.enum/c)))))))
      (testing "with docstring per enum"
        (let [spec {:interface.def/name :interface/entity-with-docstring-enum
                    :interface.def/fields {:obj/docstring-enum-attr ["A docstring'ed enum attribute"
                                                                     :enum {:some.doc.enum/a "Enum A"
                                                                            :some.doc.enum/b "Enum B"}
                                                                     :gen/should-generate]}
                    :interface.def/identify-via ['[?e :obj/docstring-enum-attr]]}
              ast (semantic-spec->semantic-ast spec)]
          (testing "semantic-spec->semantic-ast"
            (is (= ast
                   {:interface.ast/interfaces
                                            {:interface/entity-with-docstring-enum
                                             {:interface.ast.interface/name :interface/entity-with-docstring-enum
                                              :interface.ast.interface/fields
                                              {:obj/docstring-enum-attr {:db/ident :obj/docstring-enum-attr
                                                                         :db/valueType :db.type/ref
                                                                         :interface.ast.field/type :enum
                                                                         :interface.ast.field/possible-enum-vals #{:some.doc.enum/a :some.doc.enum/b}
                                                                         :db/cardinality :db.cardinality/one
                                                                         :db/doc "A docstring'ed enum attribute"
                                                                         :gen/should-generate true}}
                                              :interface.ast.interface/inherits #{}
                                              :interface.ast.interface/identify-via ['[?e :obj/docstring-enum-attr]]}}
                    :interface.ast/enum-map {:some.doc.enum/a {:db/ident :some.doc.enum/a
                                                               :db/doc "Enum A"}
                                             :some.doc.enum/b {:db/ident :some.doc.enum/b
                                                               :db/doc "Enum B"}}})))
          (testing "generating clojure.spec definitions"
            (register-specs-for-ast! ast)
            (testing "for entity attributes"
              (is (s/valid? :obj/docstring-enum-attr :some.doc.enum/a))
              (is (s/valid? :obj/docstring-enum-attr :some.doc.enum/b))
              (is (= ::s/invalid (s/conform :obj/docstring-enum-attr :some.enum/a))))
            (testing "and generating data"
              (let [generator (s/gen :interface/entity-with-docstring-enum)
                    entity (gen/generate generator)]
                (is (contains? #{:some.doc.enum/a :some.doc.enum/b} (:obj/docstring-enum-attr entity)))))))))
    (testing "with many-cardinality enum attributes"
      (testing "without docstring per enum"
        (let [spec {:interface.def/name :interface/entity-with-many-enum
                    :interface.def/fields {:obj/many-enum-attr ["A many-cardinality enum attribute"
                                                                [:enum] #{:some.enum/a :some.enum/b}]}
                    :interface.def/identify-via ['[?e :obj/many-enum-attr]]}
              ast (semantic-spec->semantic-ast spec)]
          (testing "semantic-spec->semantic-ast"
            (let [ast (semantic-spec->semantic-ast spec)]
              (is (= ast
                     {:interface.ast/interfaces
                                              {:interface/entity-with-many-enum
                                               {:interface.ast.interface/name :interface/entity-with-many-enum
                                                :interface.ast.interface/fields
                                                {:obj/many-enum-attr {:db/ident :obj/many-enum-attr
                                                                      :db/valueType :db.type/ref
                                                                      :interface.ast.field/type :enum
                                                                      :interface.ast.field/possible-enum-vals #{:some.enum/a :some.enum/b}
                                                                      :db/cardinality :db.cardinality/many
                                                                      :db/doc "A many-cardinality enum attribute"}}
                                                :interface.ast.interface/inherits #{}
                                                :interface.ast.interface/identify-via ['[?e :obj/many-enum-attr]]}}
                      :interface.ast/enum-map {:some.enum/a {:db/ident :some.enum/a}
                                               :some.enum/b {:db/ident :some.enum/b}}}))))))
      (testing "with docstring per enum"
        (let [spec {:interface.def/name :interface/entity-with-many-docstring-enum
                    :interface.def/fields {:obj/many-docstring-enum-attr ["A many-cardinality docstring'ed enum attribute"
                                                                          [:enum] {:some.doc.enum/a "Enum A"
                                                                                   :some.doc.enum/b "Enum B"}]}
                    :interface.def/identify-via ['[?e :obj/many-docstring-enum-attr]]}]
          (testing "semantic-spec->semantic-ast"
            (let [ast (semantic-spec->semantic-ast spec)]
              (is (= ast
                     {:interface.ast/interfaces
                                              {:interface/entity-with-many-docstring-enum
                                               {:interface.ast.interface/name :interface/entity-with-many-docstring-enum
                                                :interface.ast.interface/fields
                                                {:obj/many-docstring-enum-attr {:db/ident :obj/many-docstring-enum-attr
                                                                                :db/valueType :db.type/ref
                                                                                :interface.ast.field/type :enum
                                                                                :interface.ast.field/possible-enum-vals #{:some.doc.enum/a :some.doc.enum/b}
                                                                                :db/cardinality :db.cardinality/many
                                                                                :db/doc "A many-cardinality docstring'ed enum attribute"}}
                                                :interface.ast.interface/inherits #{}
                                                :interface.ast.interface/identify-via ['[?e :obj/many-docstring-enum-attr]]}}
                      :interface.ast/enum-map {:some.doc.enum/a {:db/ident :some.doc.enum/a
                                                                 :db/doc "Enum A"}
                                               :some.doc.enum/b {:db/ident :some.doc.enum/b
                                                                 :db/doc "Enum B"}}})))))))
    (testing "with linked ref attributes"
      (let [spec {:interface.def/name :interface/entity-with-ref
                  :interface.def/fields {:entity/pet [:interface/pet "An entity's pet"
                                                                     :gen/should-generate]}
                  :interface.def/identify-via ['[?e :entity/pet]]}
            pet-spec {:interface.def/name :interface/pet
                      :interface.def/fields {:pet/name [:string "A pet's name"
                                                                :gen/should-generate]}
                      :interface.def/identify-via :datomic-spec/interfaces}
            ast (semantic-spec-coll->semantic-ast [spec pet-spec])]
        (testing "generating data with default spec generators"
          (register-specs-for-ast! ast)
          (let [generator (s/gen :interface/entity-with-ref)
                entity (gen/generate generator)]
            (is (contains? entity :entity/pet))
            (is (string? (get-in entity [:entity/pet :pet/name])))))
        (testing "generating data with overriding generators"
          (let [generators {:pet/name #(s/gen #{"Banana" "Spotty"})}]
            (register-generative-specs-for-ast! ast generators)
            (let [generator (s/gen :interface/entity-with-ref)
                  entity (gen/generate generator)]
              (is (contains? #{"Banana" "Spotty"} (get-in entity [:entity/pet :pet/name]))))))))
    (testing "with an attribute that is a collection"
      (testing "of strings"
        (let [spec {:interface.def/name :interface/entity-with-string-collection
                    :interface.def/fields {:obj/tags [[:string] "A collection of strings"]}
                    :interface.def/identify-via :datomic-spec/interfaces}]
          (testing "semantic-spec->semantic-ast"
            (let [ast (semantic-spec->semantic-ast spec)]
              (is (= ast
                     {:interface.ast/interfaces
                                              {:interface/entity-with-string-collection
                                               {:interface.ast.interface/name :interface/entity-with-string-collection
                                                :interface.ast.interface/fields
                                                {:obj/tags {:db/ident :obj/tags
                                                            :db/valueType :db.type/string
                                                            :interface.ast.field/type :string
                                                            :db/cardinality :db.cardinality/many
                                                            :db/doc "A collection of strings"}}
                                                :interface.ast.interface/inherits #{}
                                                :interface.ast.interface/identify-via ['[?e :datomic-spec/interfaces :interface/entity-with-string-collection]]}}
                      :interface.ast/enum-map {:interface/entity-with-string-collection {:db/ident :interface/entity-with-string-collection}}})))))))
    (testing "with multiple attributes having multiple enums"
      (testing "semantic-spec->semantic-ast"
        (testing "should combine the enums")))
    (testing "with multiple attributes"
      (let [user-spec {:interface.def/name :interface/user
                         :interface.def/fields {:user/username [:string "A user's username"
                                                                :db.unique/identity
                                                                :required]
                                                 :taggable/tags [[:string] "Tags for an entity"]
                                                 :user/registeredAt [:instant "When a user registered"]}
                       :interface.def/identify-via ['[?e :user/username]]}]
        (testing "semantic-spec->semantic-ast"
          (let [ast (semantic-spec->semantic-ast user-spec)]
            (is (= ast
                   {:interface.ast/interfaces
                                            {:interface/user
                                             {:interface.ast.interface/name :interface/user
                                              :interface.ast.interface/fields
                                              {:user/username {:db/ident :user/username
                                                               :db/valueType :db.type/string
                                                               :interface.ast.field/type :string
                                                               :db/cardinality :db.cardinality/one
                                                               :db/doc "A user's username"
                                                               :db/unique :db.unique/identity
                                                               :interface.ast.field/required true}
                                               :taggable/tags {:db/ident :taggable/tags
                                                               :db/valueType :db.type/string
                                                               :interface.ast.field/type :string
                                                               :db/cardinality :db.cardinality/many
                                                               :db/doc "Tags for an entity"}
                                               :user/registeredAt {:db/ident :user/registeredAt
                                                                   :db/valueType :db.type/instant
                                                                   :interface.ast.field/type :instant
                                                                   :db/cardinality :db.cardinality/one
                                                                   :db/doc "When a user registered"}}
                                              :interface.ast.interface/inherits #{}
                                              :interface.ast.interface/identify-via ['[?e :user/username]]}}
                    :interface.ast/enum-map {}}))))
        (testing "generating Datomic schemas"
          (let [datomic-schemas (-> user-spec
                                    semantic-spec->semantic-ast
                                    (semantic-ast->datomic-schemas d/tempid))]
            (is (every? db-id? (map :db/id datomic-schemas)))
            (is (= #{{:db/ident :user/registeredAt
                      :db/valueType :db.type/instant
                      :db/cardinality :db.cardinality/one
                      :db/doc "When a user registered"
                      :db.install/_attribute :db.part/db}
                     {:db/ident :taggable/tags
                      :db/valueType :db.type/string
                      :db/cardinality :db.cardinality/many
                      :db/doc "Tags for an entity"
                      :db.install/_attribute :db.part/db}
                     {:db/ident :user/username
                      :db/valueType :db.type/string
                      :db/cardinality :db.cardinality/one
                      :db/doc "A user's username"
                      :db/unique :db.unique/identity
                      :db.install/_attribute :db.part/db}}
                   (set (map #(dissoc % :db/id) datomic-schemas)))))))))
  (testing "a collection of semantic specs"
    (testing "with internal references between specs"
      (let [specs [{:interface.def/name :interface/entity-with-valid-ref
                    :interface.def/fields {:obj/valid-attr [:interface/refable "Valid ref"]}
                    :interface.def/identify-via ['[?e :obj/valid-attr]]}
                   {:interface.def/name :interface/refable
                    :interface.def/fields {:refable/valid-attr [:string "Refable attr"]}
                    :interface.def/identify-via :datomic-spec/interfaces}]
            ast (semantic-spec-coll->semantic-ast specs)]
        (testing "semantic-spec-coll->semantic-ast"
          (is (= ast
                 {:interface.ast/interfaces
                  {:interface/entity-with-valid-ref {:interface.ast.interface/name :interface/entity-with-valid-ref
                                                     :interface.ast.interface/fields
                                                       {:obj/valid-attr {:db/ident :obj/valid-attr
                                                                         :db/valueType :db.type/ref
                                                                         :interface.ast.field/type :interface/refable
                                                                         :db/cardinality :db.cardinality/one
                                                                         :db/doc "Valid ref"}}
                                                     :interface.ast.interface/inherits #{}
                                                     :interface.ast.interface/identify-via ['[?e :obj/valid-attr]]}
                   :interface/refable {:interface.ast.interface/name :interface/refable
                                       :interface.ast.interface/fields
                                         {:refable/valid-attr {:db/ident :refable/valid-attr
                                                               :db/valueType :db.type/string
                                                               :interface.ast.field/type :string
                                                               :db/cardinality :db.cardinality/one
                                                               :db/doc "Refable attr"}}
                                       :interface.ast.interface/inherits #{}
                                       :interface.ast.interface/identify-via ['[?e :datomic-spec/interfaces :interface/refable]]}}
                  :interface.ast/enum-map {:interface/refable {:db/ident :interface/refable}}}))
          (testing "sets :db/valueType to :db.type/ref"
            (is (= :db.type/ref (get-in ast [:interface.ast/interfaces
                                             :interface/entity-with-valid-ref
                                             :interface.ast.interface/fields
                                             :obj/valid-attr
                                             :db/valueType])))))))
    (testing "with internal references to undefined specs"
      (let [specs [{:interface.def/name :interface/entity-with-invalid-ref
                    :interface.def/fields {:obj/invalid-attr [:interface/some-undefined-type "Is invalid"]}
                    :interface.def/identify-via ['[?e :obj/invalid-attr]]}]]
        (testing "semantic-spec-coll->semantic-ast"
          (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                #"Invalid attribute type :interface/some-undefined-type"
                                (semantic-spec-coll->semantic-ast specs))))))
    (testing "with inheritance"
      (let [specs family-semantic-specs]
        (testing "semantic-spec-coll->semantic-ast"
          (let [ast (semantic-spec-coll->semantic-ast specs)]
            (is (= ast
                   {:interface.ast/interfaces
                    {:interface/child {:interface.ast.interface/name :interface/child
                                       :interface.ast.interface/fields
                                       {:person/name {:db/ident :person/name
                                                      :db/valueType :db.type/string
                                                      :interface.ast.field/type :string
                                                      :db/cardinality :db.cardinality/one}}
                                       :interface.ast.interface/inherits #{:interface/mother :interface/father}
                                       :interface.ast.interface/identify-via ['[?e :datomic-spec/interfaces :interface/child]]}
                     :interface/mother {:interface.ast.interface/name :interface/mother
                                        :interface.ast.interface/fields
                                        {:person/personality {:db/ident :person/personality
                                                              :db/valueType :db.type/ref
                                                              :interface.ast.field/type :enum
                                                              :interface.ast.field/possible-enum-vals #{:happy :sad}
                                                              :db/cardinality :db.cardinality/one}}
                                        :interface.ast.interface/inherits #{}
                                        :interface.ast.interface/identify-via ['[?e :datomic-spec/interfaces :interface/mother]]}
                     :interface/father {:interface.ast.interface/name :interface/father
                                        :interface.ast.interface/fields
                                        {:person/bald? {:db/ident :person/bald?
                                                        :db/valueType :db.type/boolean
                                                        :interface.ast.field/type :boolean
                                                        :db/cardinality :db.cardinality/one}}
                                        :interface.ast.interface/inherits #{}
                                        :interface.ast.interface/identify-via ['[?e :datomic-spec/interfaces :interface/father]]}}
                    :interface.ast/enum-map {:happy {:db/ident :happy}
                                             :sad {:db/ident :sad}
                                             :interface/child {:db/ident :interface/child}
                                             :interface/mother {:db/ident :interface/mother}
                                             :interface/father {:db/ident :interface/father}}}))))
        (testing "generating clojure.spec definitions"
          (let [ast (semantic-spec-coll->semantic-ast specs)]
            (register-specs-for-ast! ast)
            (is (s/valid? :interface/child {:db/id (d/tempid :db.part/user)
                                            :person/name "Brian"
                                            :person/personality :happy
                                            :person/bald? false}))
            ;(let [conformed-mom (s/conform :interface/mother {:db/id (d/tempid :db.part/user)
            ;                                                  :person/name "Gloria"
            ;                                                  :person/personality :happy})]
            ;  (is (= #{:db/id :person/personality} (set (keys conformed-mom)))))
            ))
        (testing "child schemas can generate parent keys"
          (let [generator (s/gen :interface/child)
                data (gen/sample generator 100)]
            (is (= #{:db/id :person/name :person/personality :person/bald?} (set (mapcat keys data))))))
        ))))

(let [ast (semantic-spec-coll->semantic-ast family-semantic-specs)]
  (register-specs-for-ast! ast)
  (defspec parent-schemas-dont-generate-child-keys
           100
           (prop/for-all [entity (s/gen :interface/mother)]
                         (empty? (clojure.set/difference
                                   (set (keys entity))
                                   #{:db/id :person/personality}))))
  (defspec child-should-be-valid-according-to-parent-specs
           100
           (prop/for-all [entity (s/gen :interface/child)]
                         (s/valid? :interface/mother entity))))