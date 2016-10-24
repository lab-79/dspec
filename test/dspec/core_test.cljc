(ns dspec.core-test
  #?(:cljs (:require-macros cljs.spec))
  (:require #?(:clj  [clojure.test :refer [deftest testing is use-fixtures]]
               :cljs [cljs.test :refer-macros [deftest testing is use-fixtures]])
            [clojure.test.check :as tc]
            [clojure.pprint :refer [pprint]]
            #?(:clj  [clojure.test.check.clojure-test :refer [defspec]]
               :cljs [clojure.test.check.clojure-test :refer-macros [defspec]])
            [clojure.test.check.generators :as tcgen :refer [generator?]]
            #?(:clj  [clojure.test.check.properties :refer [for-all]]
               :cljs [clojure.test.check.properties :refer-macros [for-all]])
            [clojure.spec.test :as stest]
            [lab79.dspec :refer [semantic-spec->semantic-ast semantic-spec-coll->semantic-ast register-specs-for-ast!
                                 satisfies-interface? NATIVE-TYPES semantic-ast->datomic-schemas entity->interfaces
                                 eid-satisfies-interface? identify-via-clauses-for
                                 register-specs-for-ast-with-custom-generators!
                                 ast&interface->identifying-datalog-clauses
                                 gen-with-max-depth]]
            [lab79.dspec.gen :refer [ensure-keys-gen]]
            #?(:clj  [clojure.spec :as s]
               :cljs [cljs.spec :as s])
            #?(:clj  [clojure.spec.gen :as gen]
               :cljs [cljs.spec.impl.gen :as gen])
            #?(:clj  [datomic.api :as d]
               :cljs [datascript.core :as d])
            [lab79.datomic-spec.gen-overrides :refer [sized-overrides-for]]
            [dspec.util :refer [db-id?]]))

#?(:cljs (enable-console-print!))

(def ^:dynamic *conn* nil)

(defn- setup&teardown-db
  [f]
  #?(
  :clj
  (let [db-uri (str "datomic:mem://" (gensym))]
    (d/create-database db-uri)
    (binding [*conn* (d/connect db-uri)]
      (f))
    (d/delete-database db-uri))
  :cljs
  (let [db (d/empty-db)]
    (binding [*conn* (d/create-conn db)]
      (f)))))

(use-fixtures :once setup&teardown-db)

; Instrument all our functions in dspec
(let [generator-overrides
      (merge
        (sized-overrides-for 5 :datomic.query.kv/where :datalog/clause)
        {:datomic.api/q #(gen/return d/q)
         :interface.ast.field/type #(s/gen NATIVE-TYPES)

         :datalog/expression-clause #(tcgen/resize 1 (s/gen :datalog/data-pattern))

         :interface/def #(tcgen/resize 1 (s/gen :interface/def))
         :interface.def/fields #(tcgen/resize 1 (s/gen :interface.def/fields))
         :interface.def.field.enum/vals-no-doc #(tcgen/resize 1 (s/gen :interface.def.field.enum/vals-no-doc))
         :interface.def.field.enum/vals-with-doc #(tcgen/resize 1 (s/gen :interface.def.field.enum/vals-with-doc))
         :interface.def/field #(gen/fmap (fn [m] (flatten (vals m))) (s/gen (s/keys :req [:interface.def.field/single-type] :opt [:db/doc])))
         :interface.def/identifying-enum-part #(gen/return :db.part/user)})]
  (-> (stest/enumerate-namespace 'lab79.dspec)
      (stest/instrument generator-overrides)))

(def family-semantic-specs
  [{:interface.def/name :interface/child
    :interface.def/fields {:person/name [:string]}
    :interface.def/inherits [:interface/mother :interface/father]
    :interface.def/identify-via :datomic-spec/interfaces
    :interface.def/identifying-enum-part :db.part/user}
   {:interface.def/name :interface/mother
    :interface.def/fields {:person/personality [:enum #{:happy :sad}]}
    :interface.def/identify-via :datomic-spec/interfaces
    :interface.def/identifying-enum-part :db.part/user}
   {:interface.def/name :interface/father
    :interface.def/fields {:person/bald? [:boolean]}
    :interface.def/identify-via :datomic-spec/interfaces
    :interface.def/identifying-enum-part :db.part/user}])

(defn get-partition-name
  [db partition-eid]
  (d/q '[:find ?ident .
         :in $ ?p
         :where [:db.part/db :db.install/partition ?p]
                [?p :db/ident ?ident]]
       db partition-eid))

(deftest id-via-datomic-spec-interfaces
  (let [spec {:interface.def/name :interface/eponym
              :interface.def/fields {:eponym/required-attr [:keyword :required]}
              :interface.def/identify-via :datomic-spec/interfaces
              :interface.def/identifying-enum-part :db.part/test}
        ast (semantic-spec->semantic-ast spec)]
    (testing "generating Datomic schemas"
      (let [{:datomic/keys [field-schema enum-schema partition-schema]} (semantic-ast->datomic-schemas ast d/tempid)]
        (is (= #{{:db/ident :datomic-spec/interfaces
                  :db/valueType :db.type/ref
                  :db/cardinality :db.cardinality/many
                  :db/index true
                  :db.install/_attribute :db.part/db}
                 {:db/ident :eponym/required-attr
                  :db/valueType :db.type/keyword
                  :db/cardinality :db.cardinality/one
                  :db.install/_attribute :db.part/db}}
               (set (map #(dissoc % :db/id) field-schema))))
        (is (= #{{:db/ident :interface/eponym}}
               (set (map #(dissoc % :db/id) enum-schema))))
        (is (= #{{:db/ident :db.part/test
                  :db.install/_partition :db.part/db}}
               (set (map #(dissoc % :db/id) partition-schema))))
        (testing "writing interface enums to the correct partitions"
          (register-specs-for-ast! ast d/tempid db-id?)
          (let [db (-> (d/db *conn*)
                       (d/with partition-schema)
                       :db-after
                       (d/with (concat enum-schema field-schema))
                       :db-after
                       (d/with [(gen/generate (s/gen :interface/eponym))])
                       :db-after)
                enum-eid (d/q '[:find ?e .
                                :in $ ?enum
                                :where [?e :db/ident ?enum]]
                              db :interface/eponym)
                partition-eid #?(:clj  (d/part enum-eid)
                                 :cljs nil)
                partition-name #?(:clj  (get-partition-name db partition-eid)
                                  :cljs :db.part/test)]
            (is (= partition-name :db.part/test))))))
    (testing "detecting interfaces of entities"
      (register-specs-for-ast! ast d/tempid db-id?)
      (is (= #{:interface/eponym}
             (entity->interfaces ast {:db/id (d/tempid :db.part/user)
                                      :datomic-spec/interfaces #{:interface/eponym}
                                      :eponym/required-attr :k/w} d/q)))
      (is (empty? (entity->interfaces ast {:db/id (d/tempid :db.part/user)} d/q)))
      (is (true? (satisfies-interface? ast
                                :interface/eponym
                                {:db/id (d/tempid :db.part/user)
                                 :datomic-spec/interfaces #{:interface/eponym}
                                 :eponym/required-attr :k/w}
                                d/q)))
      (is (false? (satisfies-interface? ast :interface/eponym {:db/id (d/tempid :db.part/user)
                                                               :eponym/required-attr :k/w} d/q)))
      (testing "can't be invalid"
        (is (false? (satisfies-interface? ast
                                          :interface/eponym
                                          {:db/id (d/tempid :db.part/user)
                                           :datomic-spec/interfaces #{:interface/eponym}}
                                          d/q)))))
    (testing "detecting interfaces of entity ids"
      (register-specs-for-ast! ast d/tempid db-id?)
      (testing "not satisfying"
        (let [{:datomic/keys [field-schema enum-schema partition-schema]} (semantic-ast->datomic-schemas ast d/tempid)
              tempid (d/tempid :db.part/user)
              {tempids :tempids
               db :db-after} (-> (d/db *conn*)
                                 (d/with partition-schema)
                                 :db-after
                                 (d/with enum-schema)
                                 :db-after
                                 (d/with field-schema)
                                 :db-after
                                 (d/with [{:db/id tempid
                                           :eponym/required-attr :k/w}]))
              eid (d/resolve-tempid db tempids tempid)]
          (is (false? (eid-satisfies-interface? ast :interface/eponym eid d/q d/filter db)))))
      (testing "satisfying"
        (let [{:datomic/keys [field-schema enum-schema partition-schema]} (semantic-ast->datomic-schemas ast d/tempid)
              generated-eponym (gen/generate (s/gen :interface/eponym))
              tempid (:db/id generated-eponym)
              {tempids :tempids
               db :db-after} (-> (d/db *conn*)
                                 (d/with partition-schema)
                                 :db-after
                                 (d/with enum-schema)
                                 :db-after
                                 (d/with field-schema)
                                 :db-after
                                 (d/with [generated-eponym]))
              eid (d/resolve-tempid db tempids tempid)]
          (is (eid-satisfies-interface? ast :interface/eponym eid d/q d/filter db))))
      )))


(deftest id-via-attribute
  (let [spec {:interface.def/name :interface/id-by-attr
              :interface.def/fields {:obj/identifying-attr [:keyword]
                                     :obj/required-attr-for-id-by-attr [:keyword :required]}
              :interface.def/identify-via ['[?e :obj/identifying-attr]]}
        ast (semantic-spec->semantic-ast spec)]
    (testing "generating Datomic schemas"
      (let [{:datomic/keys [field-schema]} (semantic-ast->datomic-schemas ast d/tempid)]
        (is (= #{{:db/ident :obj/identifying-attr
                  :db/valueType :db.type/keyword
                  :db/cardinality :db.cardinality/one
                  :db.install/_attribute :db.part/db}
                 {:db/ident :obj/required-attr-for-id-by-attr
                  :db/valueType :db.type/keyword
                  :db/cardinality :db.cardinality/one
                  :db.install/_attribute :db.part/db}}
               (set (map #(dissoc % :db/id) field-schema))))))
    (testing "detecting interfaces of entities"
      (register-specs-for-ast! ast d/tempid db-id?)
      (is (= #{:interface/id-by-attr}
             (entity->interfaces ast {:db/id (d/tempid :db.part/user)
                                      :obj/identifying-attr :k/w
                                      :obj/required-attr-for-id-by-attr :k/w} d/q)))
      (is (empty? (entity->interfaces ast {:db/id (d/tempid :db.part/user)} d/q)))
      (is (true? (satisfies-interface? ast
                                       :interface/id-by-attr
                                       {:db/id (d/tempid :db.part/user)
                                        :obj/identifying-attr :k/w
                                        :obj/required-attr-for-id-by-attr :k/w}
                                       d/q)))
      (is (false? (satisfies-interface? ast :interface/id-by-attr {:db/id (d/tempid :db.part/user)
                                                                   :obj/required-attr-for-id-by-attr :k/w} d/q)))
      (testing "can't be invalid"
        (is (false? (satisfies-interface? ast
                                          :interface/id-by-attr
                                          {:db/id (d/tempid :db.part/user)
                                           :obj/identifying-attr :k/w}
                                          d/q)))))
    (testing "looking up identify via clauses"
      (is (= '[[?xx :obj/identifying-attr]]
             (identify-via-clauses-for ast '?xx :interface/id-by-attr))))))

(deftest semantic-spec-with-keyword-field
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
      (register-specs-for-ast! ast d/tempid db-id?)
      (testing "for entity attributes"
        (is (s/valid? :obj/keyword-attr :ok))
        (is (false? (s/valid? :obj/keyword-attr "ok"))))
      (testing "for entity interfaces"
        (is (s/valid? :interface/entity-with-keyword {:db/id (d/tempid :db.part/user)
                                                      :obj/keyword-attr :ok}))
        (is (false? (s/valid? :interface/entity-with-keyword {:db/id (d/tempid :db.part/user)
                                                              :obj/keyword-attr "ok"}))))
      (testing "and generating data"
        (let [generator (s/gen :interface/entity-with-keyword)
              entity (gen/generate generator)]
          (is (contains? entity :db/id))
          (is (or (not (contains? entity :obj/keyword-attr))
                  (keyword? (entity :obj/keyword-attr)))))))))

(deftest semantic-spec-with-string-field
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
                :interface.ast/enum-map {}}))))))

(deftest semantic-spec-with-boolean-field
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
      (register-specs-for-ast! ast d/tempid db-id?)
      (testing "for entity attributes"
        (is (s/valid? :obj/boolean-attr true))
        (is (false? (s/valid? :obj/boolean-attr "true"))))
      (testing "and generating data"
        (let [generator (s/gen :interface/entity-with-boolean)
              entity (gen/generate generator)]
          (is (boolean? (:obj/boolean-attr entity))))))))

(deftest semantic-spec-with-long-field
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

(deftest semantic-spec-with-bigint-field
  (let [spec {:interface.def/name :interface/entity-with-bigint
              :interface.def/fields {:obj/bigint-attr [:bigint "A bigint attribute"]}
              :interface.def/identify-via ['[?e :obj/bigint-attr]]}]
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

(deftest semantic-spec-with-float-field
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

(deftest semantic-spec-with-double-field
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

(deftest semantic-spec-with-bigdec-field
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

(deftest semantic-spec-with-instant-field
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
      (register-specs-for-ast! ast d/tempid db-id?)
      (testing "for entity attributes"
        (is (s/valid? :obj/instant-attr #inst "2014-05-19T19:12:37.925-00:00"))
        (is (false? (s/valid? :obj/instant-attr "not a date"))))
      (testing "and generating data"
        (let [generator (s/gen :interface/entity-with-instant)
              entity (gen/generate generator)]
          (is (inst? (:obj/instant-attr entity))))))))

(deftest semantic-spec-with-uuid-field
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
      (register-specs-for-ast! ast d/tempid db-id?)
      (testing "for entity attributes"
        (is (s/valid? :obj/id #uuid "91d7fcc5-d24d-4e33-a111-6ba69d14eb6a"))
        (is (false? (s/valid? :obj/id "91d7fcc5-d24d-4e33-a111-6ba69d14eb6a"))))
      (testing "and generating data"
        (let [generator (s/gen :interface/entity-with-uuid)
              entity (gen/generate generator)]
          (is (uuid? (:obj/id entity))))))))

(deftest semantic-spec-with-uri-field
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
      (register-specs-for-ast! ast d/tempid db-id?)
      (testing "for entity attributes"
        (is (s/valid? :obj/uri-attr #?(:clj  (java.net.URI/create "http://google.com/")
                                       :cljs "http://google.com/")))
        (is (false? (s/valid? :obj/uri-attr "google.com")))))))

(deftest semantic-spec-with-bytes-field
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
      (register-specs-for-ast! ast d/tempid db-id?)
      (testing "for entity attributes"
        (is #?(:clj  (s/valid? :obj/bytes-attr (bytes (byte-array (map (comp byte int) "ascii"))))
               :cljs true))
        (is (false? (s/valid? :obj/bytes-attr "xyz")))))))

(deftest semantic-spec-with-enum-field
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
        (register-specs-for-ast! ast d/tempid db-id?)
        (testing "for entity attributes"
          (is (s/valid? :obj/enum-attr :some.enum/a))
          (is (s/valid? :obj/enum-attr :some.enum/b))
          (is (false? (s/valid? :obj/enum-attr :some.enum/c)))))))
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
        (register-specs-for-ast! ast d/tempid db-id?)
        (testing "for entity attributes"
          (is (s/valid? :obj/docstring-enum-attr :some.doc.enum/a))
          (is (s/valid? :obj/docstring-enum-attr :some.doc.enum/b))
          (is (false? (s/valid? :obj/docstring-enum-attr :some.enum/a))))
        (testing "and generating data"
          (let [generator (s/gen :interface/entity-with-docstring-enum)
                entity (gen/generate generator)]
            (is (contains? #{:some.doc.enum/a :some.doc.enum/b} (:obj/docstring-enum-attr entity)))))))))

(deftest semantic-spec-with-many-enum-field
  (testing "without docstring per enum"
    (let [spec {:interface.def/name :interface/entity-with-many-enum
                :interface.def/fields {:obj/many-enum-attr ["A many-cardinality enum attribute"
                                                            [:enum] #{:some.enum/a :some.enum/b}]}
                :interface.def/identify-via ['[?e :obj/many-enum-attr]]}]
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

(deftest semantic-spec-with-linked-ref-field
  (let [spec {:interface.def/name :interface/entity-with-ref
              :interface.def/fields {:entity/pet [:interface/pet "An entity's pet"
                                                  :gen/should-generate]}
              :interface.def/identify-via ['[?e :entity/pet]]}
        pet-spec {:interface.def/name :interface/pet
                  :interface.def/fields {:pet/name [:string "A pet's name"
                                                    :gen/should-generate]}
                  :interface.def/identify-via :datomic-spec/interfaces
                  :interface.def/identifying-enum-part :db.part/user}
        ast (semantic-spec-coll->semantic-ast [spec pet-spec])]
    (testing "generating data with default spec generators"
      (register-specs-for-ast! ast d/tempid db-id?)
      (let [generator (s/gen :interface/entity-with-ref)
            entity (gen/generate generator)]
        (is (contains? entity :entity/pet))
        (is (contains? (:entity/pet entity) :pet/name))
        (is (string? (get-in entity [:entity/pet :pet/name])))
        (is (= #{:interface/pet} (get-in entity [:entity/pet :datomic-spec/interfaces])))))
    (testing "generating data with overriding generators"
      (let [generators {:pet/name #(s/gen #{"Banana" "Spotty"})}]
        (register-specs-for-ast-with-custom-generators! ast generators d/tempid db-id?)
        (let [generator (s/gen :interface/entity-with-ref)
              entity (gen/generate generator)]
          (is (contains? (:entity/pet entity) :pet/name))
          (is (contains? #{"Banana" "Spotty"} (get-in entity [:entity/pet :pet/name])))
          (is (= #{:interface/pet} (get-in entity [:entity/pet :datomic-spec/interfaces]))))))))

(deftest semantic-spec-with-many-string-field
  (let [spec {:interface.def/name :interface/entity-with-string-collection
              :interface.def/fields {:obj/tags [[:string] "A collection of strings"]}
              :interface.def/identify-via :datomic-spec/interfaces
              :interface.def/identifying-enum-part :db.part/other}]
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
                                                      :db/doc "A collection of strings"}
                                           :datomic-spec/interfaces {:db/ident :datomic-spec/interfaces
                                                                     :db/valueType :db.type/ref
                                                                     :db/index true
                                                                     :interface.ast.field/type :enum
                                                                     :interface.ast.field/possible-enum-vals #{:interface/entity-with-string-collection}
                                                                     :interface.ast.field/required true
                                                                     :db/cardinality :db.cardinality/many}}
                                          :interface.ast.interface/inherits #{}
                                          :interface.ast.interface/identify-via ['[?e :datomic-spec/interfaces :interface/entity-with-string-collection]]}}
                :interface.ast/enum-map {:interface/entity-with-string-collection {:db/ident :interface/entity-with-string-collection
                                                                                   :db/part :db.part/other}}}))))))

(deftest spec-with-indexed-field
  (let [specs [#:interface.def{:name :interface/entity-with-index
                               :fields {:obj/indexed-attr [:keyword :db/index]}
                               :identify-via '[[?e :obj/indexed-attr]]}]
        ast (semantic-spec-coll->semantic-ast specs)]
    (testing "generating Datomic schemas"
      (let [{:keys [datomic/field-schema]} (semantic-ast->datomic-schemas ast d/tempid)]
        (is (= #{{:db/ident :obj/indexed-attr
                  :db/valueType :db.type/keyword
                  :db/cardinality :db.cardinality/one
                  :db/index true
                  :db.install/_attribute :db.part/db}}
               (set (map #(dissoc % :db/id) field-schema))))))))

; TODO validate fulltext only works with :string attributes
(deftest spec-with-fulltext-field
  (let [specs [#:interface.def{:name :interface/entity-with-fulltext
                               :fields {:obj/fulltext-attr [:string :db/fulltext]}
                               :identify-via '[[?e :obj/fulltext-attr]]}]
        ast (semantic-spec-coll->semantic-ast specs)]
    (testing "generating Datomic schemas"
      (let [{:keys [datomic/field-schema]} (semantic-ast->datomic-schemas ast d/tempid)]
        (is (= #{{:db/ident :obj/fulltext-attr
                  :db/valueType :db.type/string
                  :db/cardinality :db.cardinality/one
                  :db/fulltext true
                  :db.install/_attribute :db.part/db}}
               (set (map #(dissoc % :db/id) field-schema))))))))

(deftest spec-with-nohistory
  (let [specs [#:interface.def{:name :interface/entity-with-index
                               :fields {:obj/no-history-attr [:keyword :db/noHistory]}
                               :identify-via '[[?e :obj/indexed-attr]]}]
        ast (semantic-spec-coll->semantic-ast specs)]
    (testing "generating Datomic schemas"
      (let [{:keys [datomic/field-schema]} (semantic-ast->datomic-schemas ast d/tempid)]
        (is (= #{{:db/ident :obj/no-history-attr
                  :db/valueType :db.type/keyword
                  :db/cardinality :db.cardinality/one
                  :db/noHistory true
                  :db.install/_attribute :db.part/db}}
               (set (map #(dissoc % :db/id) field-schema))))))))

(deftest spec-with-is-component-field
  (let [specs [#:interface.def{:name :interface/entity-with-component
                               :fields {:obj/component-attr [:interface/component-entity :db/isComponent]}
                               :identify-via :datomic-spec/interfaces
                               :identifying-enum-part :db.part/user}
               #:interface.def{:name :interface/component-entity
                               :fields {:component/key [:keyword]}
                               :identify-via :datomic-spec/interfaces
                               :identifying-enum-part :db.part/user}]
        ast (semantic-spec-coll->semantic-ast specs)]
    (testing "generating Datomic schemas"
      (let [{:datomic/keys [field-schema]} (semantic-ast->datomic-schemas ast d/tempid)]
        (is (= #{{:db/ident :obj/component-attr
                  :db/valueType :db.type/ref
                  :db/cardinality :db.cardinality/one
                  :db/isComponent true
                  :db.install/_attribute :db.part/db}
                 {:db/ident :component/key
                  :db/valueType :db.type/keyword
                  :db/cardinality :db.cardinality/one
                  :db.install/_attribute :db.part/db}
                 {:db/ident :datomic-spec/interfaces
                  :db/valueType :db.type/ref
                  :db/cardinality :db.cardinality/many
                  :db/index true
                  :db.install/_attribute :db.part/db}}
               (set (map #(dissoc % :db/id) field-schema))))))))

(deftest spec-with-multiple-fields
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
      (let [{:keys [datomic/field-schema]} (-> user-spec
                                               semantic-spec->semantic-ast
                                               (semantic-ast->datomic-schemas d/tempid))]
        (is (every? db-id? (map :db/id field-schema)))
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
               (set (map #(dissoc % :db/id) field-schema))))))))

(deftest to-do
  (testing "with multiple attributes having multiple enums"
    (testing "semantic-spec->semantic-ast"
      (testing "should combine the enums"))))

(deftest internal-refs-btwn-specs
  (let [specs [{:interface.def/name :interface/entity-with-valid-ref
                :interface.def/fields {:obj/valid-attr [:interface/refable "Valid ref"]}
                :interface.def/identify-via ['[?e :obj/valid-attr]]}
               {:interface.def/name :interface/refable
                :interface.def/fields {:refable/valid-attr [:string "Refable attr"]}
                :interface.def/identify-via :datomic-spec/interfaces
                :interface.def/identifying-enum-part :db.part/user}]
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
                                                                                 :db/doc "Refable attr"}
                                                            :datomic-spec/interfaces {:db/ident :datomic-spec/interfaces
                                                                                      :db/valueType :db.type/ref
                                                                                      :db/index true
                                                                                      :interface.ast.field/type :enum
                                                                                      :interface.ast.field/possible-enum-vals #{:interface/refable}
                                                                                      :interface.ast.field/required true
                                                                                      :db/cardinality :db.cardinality/many}}
                                                           :interface.ast.interface/inherits #{}
                                                           :interface.ast.interface/identify-via ['[?e :datomic-spec/interfaces :interface/refable]]}}
              :interface.ast/enum-map {:interface/refable {:db/ident :interface/refable
                                                           :db/part :db.part/user}}}))
      (testing "sets :db/valueType to :db.type/ref"
        (is (= :db.type/ref (get-in ast [:interface.ast/interfaces
                                         :interface/entity-with-valid-ref
                                         :interface.ast.interface/fields
                                         :obj/valid-attr
                                         :db/valueType])))))))

(deftest refs-to-undefined-specs
  (let [specs [{:interface.def/name :interface/entity-with-invalid-ref
                :interface.def/fields {:obj/invalid-attr [:interface/some-undefined-type "Is invalid"]}
                :interface.def/identify-via ['[?e :obj/invalid-attr]]}]]
    (testing "semantic-spec-coll->semantic-ast"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Invalid attribute type :interface/some-undefined-type"
                            (semantic-spec-coll->semantic-ast specs))))))

(deftest dspecs-whose-generators-can-violate-their-specs
  (testing "warn against generators that will potentially violate the associated spec"
    (let [specs [#:interface.def{:name :interface/violates-such-that
                                 :fields {:invalid/one [:keyword :required]}
                                 :identify-via [['?e :invalid/one]]}]
          ast (semantic-spec-coll->semantic-ast specs)
          ;generators {:invalid/one #(gen/return nil)}
          generators {:interface/violates-such-that (fn [base-gen-spec-factory]
                                                      (gen/fmap
                                                        (fn [generated-maps]
                                                          (apply merge generated-maps))
                                                        (gen/tuple ;(base-gen-spec-factory)
                                                                   ;(s/gen (base-gen-spec-factory))
                                                                   (gen/return {:invalid/one nil}))))}
          ]
      ; TODO Replace this assertion with the commented assertion under it, when test.check reaches 0.9.1,
      ;      which includes a way to pass the predicate that is not being
      ;      satisfied to an exception builder function
      (is (thrown-with-msg? clojure.lang.ExceptionInfo "nope"
                            (register-specs-for-ast-with-custom-generators! ast generators d/tempid db-id?)))
      (comment
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Spec :interface/violates-such-that and/or its custom generators are defined in such a way that it is possible to generate data such that it does not satisfy the spec."
                            (register-specs-for-ast-with-custom-generators! ast generators d/tempid db-id?))))
      )))

(deftest semantic-spec-coll-with-circular-references
  (testing "interfaces with circular dependencies"
    (let [specs [{:interface.def/name :interface/a-points-to-b
                  :interface.def/fields {:a/b [:interface/b-points-to-a]}
                  :interface.def/identify-via :datomic-spec/interfaces
                  :interface.def/identifying-enum-part :db.part/user}
                 {:interface.def/name :interface/b-points-to-a
                  :interface.def/fields {:b/a [:interface/a-points-to-b]}
                  :interface.def/identify-via :datomic-spec/interfaces
                  :interface.def/identifying-enum-part :db.part/user}]]
      ; Should not throw
      (-> specs
          semantic-spec-coll->semantic-ast
          (register-specs-for-ast! d/tempid db-id?)))))

(let [specs family-semantic-specs]
  (deftest spec-inheritance-ast
    (testing "semantic-spec-coll->semantic-ast"
      (let [ast (semantic-spec-coll->semantic-ast specs)]
        (is (= ast
               {:interface.ast/interfaces
                                        {:interface/child {:interface.ast.interface/name :interface/child
                                                           :interface.ast.interface/fields
                                                           {:person/name {:db/ident :person/name
                                                                          :db/valueType :db.type/string
                                                                          :interface.ast.field/type :string
                                                                          :db/cardinality :db.cardinality/one}
                                                            :datomic-spec/interfaces {:db/ident :datomic-spec/interfaces
                                                                                      :db/valueType :db.type/ref
                                                                                      :db/index true
                                                                                      :interface.ast.field/type :enum
                                                                                      :interface.ast.field/possible-enum-vals #{:interface/child}
                                                                                      :interface.ast.field/required true
                                                                                      :db/cardinality :db.cardinality/many}}
                                                           :interface.ast.interface/inherits #{:interface/mother :interface/father}
                                                           :interface.ast.interface/identify-via ['[?e :datomic-spec/interfaces :interface/child]]}
                                         :interface/mother {:interface.ast.interface/name :interface/mother
                                                            :interface.ast.interface/fields
                                                            {:person/personality {:db/ident :person/personality
                                                                                  :db/valueType :db.type/ref
                                                                                  :interface.ast.field/type :enum
                                                                                  :interface.ast.field/possible-enum-vals #{:happy :sad}
                                                                                  :db/cardinality :db.cardinality/one}
                                                             :datomic-spec/interfaces {:db/ident :datomic-spec/interfaces
                                                                                       :db/valueType :db.type/ref
                                                                                       :db/index true
                                                                                       :interface.ast.field/type :enum
                                                                                       :interface.ast.field/possible-enum-vals #{:interface/mother}
                                                                                       :interface.ast.field/required true
                                                                                       :db/cardinality :db.cardinality/many}}
                                                            :interface.ast.interface/inherits #{}
                                                            :interface.ast.interface/identify-via ['[?e :datomic-spec/interfaces :interface/mother]]}
                                         :interface/father {:interface.ast.interface/name :interface/father
                                                            :interface.ast.interface/fields
                                                            {:person/bald? {:db/ident :person/bald?
                                                                            :db/valueType :db.type/boolean
                                                                            :interface.ast.field/type :boolean
                                                                            :db/cardinality :db.cardinality/one}
                                                             :datomic-spec/interfaces {:db/ident :datomic-spec/interfaces
                                                                                       :db/valueType :db.type/ref
                                                                                       :db/index true
                                                                                       :interface.ast.field/type :enum
                                                                                       :interface.ast.field/possible-enum-vals #{:interface/father}
                                                                                       :interface.ast.field/required true
                                                                                       :db/cardinality :db.cardinality/many}}
                                                            :interface.ast.interface/inherits #{}
                                                            :interface.ast.interface/identify-via ['[?e :datomic-spec/interfaces :interface/father]]}}
                :interface.ast/enum-map {:happy {:db/ident :happy}
                                         :sad {:db/ident :sad}
                                         :interface/child {:db/ident :interface/child
                                                           :db/part :db.part/user}
                                         :interface/mother {:db/ident :interface/mother
                                                            :db/part :db.part/user}
                                         :interface/father {:db/ident :interface/father
                                                            :db/part :db.part/user}}})))))
  (deftest specs-with-inheritance-clojure-spec
    (testing "generating clojure.spec definitions"
      (let [ast (semantic-spec-coll->semantic-ast specs)]
        (register-specs-for-ast! ast d/tempid db-id?)
        (is (false?
              (s/valid? :interface/child {:db/id (d/tempid :db.part/user)
                                          :datomic-spec/interfaces #{:interface/child}
                                          :person/name "Brian"
                                          :person/personality :happy
                                          :person/bald? false})))
        (is (s/valid? :interface/child {:db/id (d/tempid :db.part/user)
                                        :datomic-spec/interfaces #{:interface/child :interface/father :interface/mother}
                                        :person/name "Brian"
                                        :person/personality :happy
                                        :person/bald? false}))
        ;(let [conformed-mom (s/conform :interface/mother {:db/id (d/tempid :db.part/user)
        ;                                                  :person/name "Gloria"
        ;                                                  :person/personality :happy})]
        ;  (is (= #{:db/id :person/personality} (set (keys conformed-mom)))))
        )))

  (deftest specs-with-inheritance-generating-datomic-schemas
    (let [ast (semantic-spec-coll->semantic-ast specs)
          {:datomic/keys [partition-schema enum-schema field-schema]} (semantic-ast->datomic-schemas ast d/tempid)]
      (is (map? (-> (d/db *conn*)
                    (d/with partition-schema)
                    :db-after
                    (d/with enum-schema)
                    :db-after
                    (d/with field-schema))))))
  (deftest specs-with-inheritance--entity-generation
    (testing "schemas generate entities with correct :datomic-spec/interfaces"
      (testing "with an inheritance of interfaces strictly using :datomic-spec/interfaces"
        (let [child (gen/generate (s/gen :interface/child))
              mother (gen/generate (s/gen :interface/mother))
              father (gen/generate (s/gen :interface/father))]
          (is (= (:datomic-spec/interfaces child) #{:interface/child :interface/mother :interface/father}))
          (is (= (:datomic-spec/interfaces mother) #{:interface/mother}))
          (is (= (:datomic-spec/interfaces father) #{:interface/father}))))
      (testing "an interface id'ed via :datomic-spec/interfaces inheriting from an interface id'ed via attributes"
        (let [parent-spec {:interface.def/name :interface/parent-id-via-attr
                           :interface.def/fields {:parent/name [:string :required]}
                           :interface.def/identify-via ['[?e :parent/name]]}
              child-spec {:interface.def/name :interface/self-labeling-child-of-parent-id-via-attr
                          :interface.def/fields {}
                          :interface.def/inherits [:interface/parent-id-via-attr]
                          :interface.def/identify-via :datomic-spec/interfaces
                          :interface.def/identifying-enum-part :db.part/user}
              ast (semantic-spec-coll->semantic-ast [parent-spec child-spec])]
          (register-specs-for-ast! ast d/tempid db-id?)
          (let [child (gen/generate (s/gen :interface/self-labeling-child-of-parent-id-via-attr))]
            (is (= #{:interface/self-labeling-child-of-parent-id-via-attr}
                   (:datomic-spec/interfaces child)))
            (is (contains? child :parent/name)))))
      (testing "an interface id'ed via attributes inheriting from an interface id'ed via :datomic-spec/interfaces"
        (let [parent-spec {:interface.def/name :interface/parent-id-via-datomic-spec-interfaces
                           :interface.def/fields {}
                           :interface.def/identify-via :datomic-spec/interfaces
                           :interface.def/identifying-enum-part :db.part/user}
              child-spec {:interface.def/name :interface/child-id-via-attr-inheriting-parent-id-via-datomic-spec-interfaces
                          :interface.def/fields {:child-id-via-attr-inheriting-parent-id-via-datomic-spec-interfaces/name [:string :required]}
                          :interface.def/inherits [:interface/parent-id-via-datomic-spec-interfaces]
                          :interface.def/identify-via ['[?e :child-id-via-attr-inheriting-parent-id-via-datomic-spec-interfaces/name]]
                          :interface.def/identifying-enum-part :db.part/user}
              ast (semantic-spec-coll->semantic-ast [parent-spec child-spec])]
          (register-specs-for-ast! ast d/tempid db-id?)
          (let [child (gen/generate (s/gen :interface/child-id-via-attr-inheriting-parent-id-via-datomic-spec-interfaces))]
            (is (= #{:interface/parent-id-via-datomic-spec-interfaces}
                   (:datomic-spec/interfaces child)))
            (is (contains? child :child-id-via-attr-inheriting-parent-id-via-datomic-spec-interfaces/name))))))
    (testing "child schemas can generate parent keys"
      (let [generator (s/gen :interface/child)
            data (gen/sample generator 100)]
        (is (= #{:db/id :person/name :person/personality :person/bald? :datomic-spec/interfaces} (set (mapcat keys data))))))))

(deftest invalid-semantic-spec-defs
  (testing "no :interface.def/identify-via"
    (let [specs [{:interface.def/name :interface/non-identifiable-entity
                  :interface.def/fields {}}]]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"did not conform to spec:"
                            (semantic-spec-coll->semantic-ast specs))))))

(deftest toposorted-ordering-of-clojure-spec-declarations
  (let [specs [{:interface.def/name :interface/tx
                :interface.def/fields
                                    {:tx.import/src [:interface.tx/import-src]}
                :interface.def/identify-via ['[?e :tx.import/src]]}
               {:interface.def/name :interface.tx/import-src
                :interface.def/fields {}
                :interface.def/identify-via :datomic-spec/interfaces
                :interface.def/identifying-enum-part :db.part/user}]]
    ; This should not throw
    (-> specs
        semantic-spec-coll->semantic-ast
        (register-specs-for-ast! d/tempid db-id?))))

(let [specs [{:interface.def/name :interface/polyglot
              :interface.def/fields {:polyglot/languages [[:string]]}
              :interface.def/identify-via ['[?e :polyglot/languages]]}
             {:interface.def/name :interface/translator
              :interface.def/fields {:translator/id [:string]}
              :interface.def/inherits [:interface/polyglot]
              :interface.def/identify-via :datomic-spec/interfaces
              :interface.def/identifying-enum-part :db.part/user}]]
  (deftest generating-clojure-spec-defs-with-custom-ensure-keys-generator
    (let [ast (semantic-spec-coll->semantic-ast specs)
          custom-gens {:interface/translator (ensure-keys-gen :db/id :translator/id)}]
      ; Should not throw
      (register-specs-for-ast-with-custom-generators! ast custom-gens d/tempid db-id?)
      (let [{:datomic/keys [partition-schema enum-schema field-schema]} (semantic-ast->datomic-schemas ast d/tempid)
            db (-> (d/db *conn*)
                   (d/with partition-schema)
                   :db-after
                   (d/with enum-schema)
                   :db-after
                   (d/with field-schema)
                   :db-after)]
        (d/with db (gen/sample (s/gen :interface/translator))))))
  (deftest generating-clojure-spec-defs-with-custom-set-generator
    (let [ast (semantic-spec-coll->semantic-ast specs)
          custom-gens {:polyglot/languages #(gen/set (s/gen #{"Cantonese"
                                                              "English"
                                                              "Japanese"
                                                              "Mandarin"
                                                              "Spanish"})
                                                     {:min-elements 1 :max-elements 1})}]
      ; Should not throw
      (register-specs-for-ast-with-custom-generators! ast custom-gens d/tempid db-id?)
      (let [{:datomic/keys [partition-schema enum-schema field-schema]} (semantic-ast->datomic-schemas ast d/tempid)
            db (-> (d/db *conn*)
                   (d/with partition-schema)
                   :db-after
                   (d/with enum-schema)
                   :db-after
                   (d/with field-schema)
                   :db-after)]
        (d/with db (gen/sample (s/gen :interface/translator))))))
  (deftest generating-clojure-spec-defs-with-custom-set-generator-with-member-generator-dependency-injection
    (let [specs [{:interface.def/name :interface/carpenter
                  :interface.def/fields {:carpenter/tools [[:interface/tool] :gen/should-generate]}
                  :interface.def/identify-via :datomic-spec/interfaces
                  :interface.def/identifying-enum-part :db.part/user}
                 {:interface.def/name :interface/tool
                  :interface.def/fields {}
                  :interface.def/identify-via :datomic-spec/interfaces
                  :interface.def/identifying-enum-part :db.part/user}]
          ast (semantic-spec-coll->semantic-ast specs)
          custom-gens {:carpenter/tools (fn [member-gen-factory]
                                          (gen/set (member-gen-factory) {:min-elements 1 :max-elements 1}))}]
      ; Should not throw
      (register-specs-for-ast-with-custom-generators! ast custom-gens d/tempid db-id?)
      (let [carpenter (gen/generate (s/gen :interface/carpenter))]
        (is (= 1 (-> carpenter :carpenter/tools count))))
      (let [{:datomic/keys [partition-schema enum-schema field-schema]} (semantic-ast->datomic-schemas ast d/tempid)
            db (-> (d/db *conn*)
                   (d/with partition-schema)
                   :db-after
                   (d/with enum-schema)
                   :db-after
                   (d/with field-schema)
                   :db-after)
            data (gen/sample (s/gen :interface/carpenter))]
        (d/with db data)))))

(deftest generating-clojure-spec-defs-for-attr-identifying-interfaces-with-custom-gens-from-inherited-inerfaces
  (let [specs [{:interface.def/name :interface/parent-of-child-with-identifying-attr
                :interface.def/fields {:parent-xx/name [:string :required]}
                :interface.def/identify-via [['?e :parent-xx/name]]}
               {:interface.def/name :interface/child-with-identifying-attr
                :interface.def/fields {:child-xx/name [:string :required]}
                :interface.def/identify-via [['?e :child-xx/name]]
                :interface.def/inherits [:interface/parent-of-child-with-identifying-attr]}]
        ast (semantic-spec-coll->semantic-ast specs)
        custom-gens
          {:interface/parent-of-child-with-identifying-attr (fn [base-gen-factory]
                                                              (gen/fmap
                                                                (fn [gen-parent]
                                                                  (merge gen-parent
                                                                         {:parent-xx/name "custom name"}))
                                                                (base-gen-factory)))}]
    (register-specs-for-ast-with-custom-generators! ast custom-gens d/tempid db-id?)
    (let [child (gen/generate (s/gen :interface/child-with-identifying-attr))]
      (is (= "custom name" (:parent-xx/name child)))
      (is (not-empty (:child-xx/name child))))))

(deftest unique-field-with-no-custom-generator-should-throw
  (let [specs [{:interface.def/name :interface/unique-field-no-generator
                :interface.def/fields {:obj/unique-field-no-generator [:keyword :db.unique/identity]
                                       :obj/another-keyword-field [:keyword]}
                :interface.def/identify-via [['?e :obj/unique-field-no-generator]]}]
        custom-gens {:obj/another-keyword-field #(gen/return :some/kw)}
        ast (semantic-spec-coll->semantic-ast specs)]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"You should provide a custom generator to the unique attribute :obj/unique-field-no-generator"
                          (register-specs-for-ast-with-custom-generators! ast custom-gens d/tempid db-id?)))
    (testing "should not throw when no custom generators are provided with clojure.spec registration"
      (register-specs-for-ast! ast d/tempid db-id?))))

(let [ast (semantic-spec-coll->semantic-ast family-semantic-specs)]
  (register-specs-for-ast! ast d/tempid db-id?)
  (defspec parent-schemas-dont-generate-child-keys
           100
           (for-all [entity (s/gen :interface/mother)]
                         (empty? (clojure.set/difference
                                   (set (keys entity))
                                   #{:db/id :person/personality :datomic-spec/interfaces}))))
  (defspec child-should-be-valid-according-to-parent-specs
           100
           (for-all [entity (s/gen :interface/child)]
                         (and (s/valid? :interface/mother entity) (s/valid? :interface/father entity))))
  (defspec child-should-self-label-itself-with-all-inherited-interfaces
           100
           (for-all [entity (s/gen :interface/child)]
                         (= (:datomic-spec/interfaces entity)
                            #{:interface/child :interface/mother :interface/father}))))

(let [specs [{:interface.def/name :interface/string-generator
              :interface.def/fields {:string-generator/str [:string]}
              :interface.def/identify-via [['?e :string-generator/str]]}]
      ast (semantic-spec-coll->semantic-ast specs)]
  (register-specs-for-ast! ast d/tempid db-id?)
  (defspec string-attributes-should-always-generate-non-empty-strings
    100
    (for-all [string (s/gen :string-generator/str)]
                  (is (pos? (count string))))))

(let [specs [{:interface.def/name :interface/string-coll-generator
              :interface.def/fields {:string-coll-generator/strings [[:string]]}
              :interface.def/identify-via [['?e :string-coll-generator/strings]]}]
      ast (semantic-spec-coll->semantic-ast specs)]
  (register-specs-for-ast! ast d/tempid db-id?)
  (defspec string-coll-attribute-should-always-generate-non-empty-strings
           100
           (for-all [strings (s/gen :string-coll-generator/strings)]
                         (is (every? #(pos? (count %)) strings))))
  (let [generators {:string-coll-generator/strings (fn [member-generator-factory]
                                                     (gen/set (member-generator-factory) {:min-elements 1
                                                                                          :max-elements 1}))}]
    (println ast)
    (register-specs-for-ast-with-custom-generators! ast generators d/tempid db-id?)
    (defspec string-coll-attribute-should-always-generate-non-empty-strings-with-custom-generator
             100
             (for-all [strings (s/gen :string-coll-generator/strings)]
                           (is (every? #(pos? (count %)) strings))))))


(deftest determine-identifying-datalog-clauses-from-interface
  (testing "for identify-via :datomic-spec/interfaces"
    (let [specs [{:interface.def/name :interface/obj-id-via-datomic-spec-interfaces
                  :interface.def/fields {}
                  :interface.def/identify-via :datomic-spec/interfaces
                  :interface.def/identifying-enum-part :db.part/user}]
          ast (semantic-spec-coll->semantic-ast specs)]
      (is (= [['?e :datomic-spec/interfaces :interface/obj-id-via-datomic-spec-interfaces]]
             (ast&interface->identifying-datalog-clauses ast :interface/obj-id-via-datomic-spec-interfaces))))
    (testing "with inheritance should only consider a single interface's identify-via parameters"
      (let [specs [{:interface.def/name :interface/parent-id-via-datomic-spec-interfaces
                    :interface.def/fields {}
                    :interface.def/identify-via :datomic-spec/interfaces
                    :interface.def/identifying-enum-part :db.part/user}
                   {:interface.def/name :interface/child-id-via-datomic-spec-interfaces
                    :interface.def/fields {}
                    :interface.def/inherits [:interface/parent-id-via-datomic-spec-interfaces]
                    :interface.def/identify-via :datomic-spec/interfaces
                    :interface.def/identifying-enum-part :db.part/user}]
            ast (semantic-spec-coll->semantic-ast specs)]
        (is (= [['?e :datomic-spec/interfaces :interface/child-id-via-datomic-spec-interfaces]]
               (ast&interface->identifying-datalog-clauses ast :interface/child-id-via-datomic-spec-interfaces)))
        (is (= [['?e :datomic-spec/interfaces :interface/parent-id-via-datomic-spec-interfaces]]
               (ast&interface->identifying-datalog-clauses ast :interface/parent-id-via-datomic-spec-interfaces))))))
  (testing "for identify-via datalog clauses"
    (let [specs [{:interface.def/name :interface/obj-id-via-datalog-query
                  :interface.def/fields {:obj-id-via-datalog-query/name [:string :required]}
                  :interface.def/identify-via [['?e :obj-id-via-datalog-query/name]]}]
          ast (semantic-spec-coll->semantic-ast specs)]
      (is (= [['?e :obj-id-via-datalog-query/name]]
             (ast&interface->identifying-datalog-clauses ast :interface/obj-id-via-datalog-query))))))

(let [gen-3-specs [{:interface.def/name :interface/gen-3-grandparent
                    :interface.def/fields {}
                    :interface.def/identify-via :datomic-spec/interfaces
                    :interface.def/identifying-enum-part :db.part/user}
                   {:interface.def/name :interface/gen-3-parent
                    :interface.def/fields {}
                    :interface.def/inherits [:interface/gen-3-grandparent]
                    :interface.def/identify-via :datomic-spec/interfaces
                    :interface.def/identifying-enum-part :db.part/user}
                   {:interface.def/name :interface/gen-3-grandchild
                    :interface.def/fields {}
                    :interface.def/inherits [:interface/gen-3-parent]
                    :interface.def/identify-via :datomic-spec/interfaces
                    :interface.def/identifying-enum-part :db.part/user}]]
  (-> gen-3-specs
      semantic-spec-coll->semantic-ast
      (register-specs-for-ast! d/tempid db-id?))
  (defspec grandchild-should-self-label-with-all-transitive-inherited-interfaces
           100
           (for-all [entity (s/gen :interface/gen-3-grandchild)]
                    (= (:datomic-spec/interfaces entity)
                       #{:interface/gen-3-grandparent :interface/gen-3-parent :interface/gen-3-grandchild}))))

(defn entity-depth
  ([entity]
   (entity-depth entity 1))
  ([entity curr-depth]
   (->> entity
        (filter (fn [[k _]] (not= k :db/id)))
        (filter (fn [[k v]] (or (map? v)
                                (and (coll? v) (seq v) (map? (first v))))))
        (reduce (fn [curr-depth [k v]]
                  (max curr-depth
                       (cond
                         (map? v)                                 (entity-depth v (inc curr-depth))
                         (and (coll? v) (seq v) (map? (first v))) (entity-depth (first v) (inc curr-depth))
                         :else                                    curr-depth)))
                curr-depth))))
(deftest test-entity-depth
  (testing "no nesting"
    (is (= 1 (entity-depth {:a 1}))))
  (testing "nested maps"
    (is (= 2 (entity-depth {:a {:b 1}})))
    (is (= 3 (entity-depth {:a {:b {:c 1}}}))))
  (testing "nested array of maps"
    (is (= 2 (entity-depth {:a [{:b 1}]})))
    (is (= 3 (entity-depth {:a [{:b [{:c 1}]}]})))))

(let [nested-interface-specs [{:interface.def/name :interface/root-grandparent
                               :interface.def/fields {:grandparent/child [:interface/nested-parent]}
                               :interface.def/identify-via :datomic-spec/interfaces
                               :interface.def/identifying-enum-part :db.part/user}
                              {:interface.def/name :interface/nested-parent
                               :interface.def/fields {:parent/child [:interface/nested-grandchild]}
                               :interface.def/identify-via :datomic-spec/interfaces
                               :interface.def/identifying-enum-part :db.part/user}
                              {:interface.def/name :interface/nested-grandchild
                               :interface.def/fields {:nested-grandchild/name [:string :required]}
                               :interface.def/identify-via :datomic-spec/interfaces
                               :interface.def/identifying-enum-part :db.part/user}]
      ast (semantic-spec-coll->semantic-ast nested-interface-specs)
      custom-generators {:nested-grandchild/name (fn [orig-gen] (gen/return "mike"))}]
  (register-specs-for-ast-with-custom-generators! ast custom-generators d/tempid db-id?)
  (defspec gen-with-max-depth-2-should-not-generate-more-than-max-depth
    100
    (for-all [entity (gen-with-max-depth ast {} 2 :interface/root-grandparent)]
             (>= 2 (entity-depth entity))))
  (defspec gen-with-max-depth-3-should-not-generate-more-than-max-depth
    100
    (for-all [entity (gen-with-max-depth ast {} 3 :interface/root-grandparent)]
             (>= 3 (entity-depth entity))))
  (defspec gen-with-max-depth-should-support-custom-generators
    100
    (for-all [entity (gen-with-max-depth ast {} 3 :interface/root-grandparent)]
             (let [gc-name (get-in entity [:grandparent/child :parent/child :nested-grandchild/name])]
               (or (nil? gc-name) (= "mike" gc-name))))))

(deftest check-semantic-spec->semantic-ast
  (let [gen-size 5
        gen-overrides
          (merge
            (sized-overrides-for 5 :datomic.query.kv/where :datalog/clause)
            {
             ;:interface.def/field #(gen/fmap (fn [m] (flatten (vals m))) (s/gen (s/keys :req [:interface.def.field/single-type] :opt [:db/doc])))
             ;:interface.def/identifying-enum-part #(gen/return :db.part/user)
             :interface.def/fields (fn []
                                     (tcgen/resize
                                       gen-size
                                       (s/gen :interface.def/fields
                                              #:interface.def.field.enum
                                              {:vals-no-doc #(tcgen/resize
                                                               gen-size
                                                               (s/gen :interface.def.field.enum/vals-no-doc))
                                               :vals-with-doc #(tcgen/resize
                                                                 gen-size
                                                                 (s/gen :interface.def.field.enum/vals-with-doc))})))})]
    (doseq [result-map (stest/check `semantic-spec->semantic-ast {:gen gen-overrides
                                                                  :clojure.spec.test.check/opts {:num-tests 100}})]
      (is (not (contains? result-map :failure))))))

(deftest check-entity->interfaces
  (let [gen-size 5
        gen-overrides
          {:interface.ast/field #(gen/fmap
                                   (partial apply merge)
                                   (gen/tuple
                                     (s/gen (s/keys :req [:db/ident :db/cardinality]
                                                    :opt [:interface.ast.field/required
                                                          :db/doc
                                                          :db/unique
                                                          :db/index
                                                          :db/isComponent
                                                          :db/noHistory
                                                          :db/fulltext]))
                                     (gen/bind
                                       (s/gen :interface.ast.field/type
                                              ; TODO I thought I had inherited
                                              ; this from the overrides map, but
                                              ; obviously not. Figure out how to
                                              ; identify the failure more
                                              ; easily.
                                              {:interface.ast.field/type (fn [] (s/gen NATIVE-TYPES))})
                                       (fn [field-type]
                                         (gen/return {:interface.ast.field/type field-type
                                                      :db/valueType (keyword "db.type" (name field-type))})))))
;           :dummy/gen-factory #(s/gen (s/fspec :args (s/cat)
;                                               :ret generator?
;                                               :gen (fn []
;                                                      (gen/return (fn []
;                                                                    (gen/return nil))))))
           }]
    ;(stest/instrument `entity->interfaces {:replace
    ;                                       {'clojure.spec/valid? (fn [_ _] true)}})
    (stest/check `entity->interfaces {:gen gen-overrides
                                      :clojure.spec.test.check/opts {:num-tests 100}})
    ))

(comment
  (let [specs [{:interface.def/name :interface/x->y
                :interface.def/fields {:x->y/y [:interface/y->x :gen/should-generate]}
                :interface.def/identify-via [['?e :x->y/y]]}
               {:interface.def/name :interface/y->x
                :interface.def/fields {:y->x/x [:interface/x->y :gen/should-generate]}
                :interface.def/identify-via [['?e :y->x/x]]}]
        ast (semantic-spec-coll->semantic-ast specs)]
    (register-specs-for-ast! ast d/tempid db-id?)
    (defspec circular-interfaces-data-generation
             10
             (for-all (entity (s/gen :interface/x->y))
                           (= (map? entity))))))
