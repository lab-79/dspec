(ns org.lab79.datomic-spec
  (:require [clojure.core.match :refer [match]]
            [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clojure.spec.test :as stest]
            [com.stuartsierra.dependency :as ssdep]
            [com.rpl.specter :refer [MAP-VALS]]
            [com.rpl.specter.macros :refer [select]]
            [org.lab79.datomic-spec.gen :refer [ensure-keys-gen fn->gen]]
            )
  (:import (java.util Date)))


(s/fdef arity
        :args (s/cat :f fn?)
        :ret boolean?)
(defn- arity [f]
  (-> f class .getDeclaredMethods first .getParameterTypes alength))
(stest/instrument `arity)

;
; Datomic clojure.spec
;

(s/def :db.install/_attribute #{:db.part/db})
(s/def :db.install/_partition #{:db.part/db})

(s/def :db/ident keyword?)
(def datomic-value-types #{:db.type/string :db.type/boolean :db.type/long :db.type/bigint :db.type/float
                           :db.type/double :db.type/bigdec :db.type/instant :db.type/uuid :db.type/uri
                           :db.type/keyword :db.type/bytes :db.type/ref})
(s/def :db/valueType datomic-value-types)
(s/def :db/cardinality #{:db.cardinality/one :db.cardinality/many})
(s/def :db/doc string?)
(s/def :db/unique #{:db.unique/value :db.unique/identity})
(s/def :db/index boolean?)
(s/def :db/isComponent boolean?)
(s/def :db/noHistory boolean?)
(s/def :db/fulltext boolean?)
(s/def :datomic/enum (s/and (s/keys :req [:db/ident])
                            #(not (contains? % :db/valueType))))

;
; clojure.spec describing how devs define data interfaces
;

(s/def :interface/def
  (s/keys :req [:interface.def/name :interface.def/fields :interface.def/identify-via]
          :opt [:interface.def/inherits]))
(s/def :interface.def/name keyword?)
(s/def :interface.def/fields (s/coll-of :interface.def/field :kind set?))
(s/def :interface.def/fields (s/map-of keyword? :interface.def/field))
(s/def :interface.def/field
  (s/cat :field-tags (s/+ :interface.def.field/trait)))
(s/def :interface.def/inherits (s/coll-of keyword? :kind vector?))
(s/def :datalog/clause (s/or :datalog/pair (s/tuple #{'?e} keyword? any?)
                             :datalog/triplet (s/tuple #{'?e} keyword?)))
(s/def :interface.def/identify-via (s/or :identify-via/reserved-attribute #{:datomic-spec/interfaces}
                                         :identify-via/datalog-clause (s/coll-of :datalog/clause :kind vector?)))
(s/def :interface.def.field/trait
  (s/alt :doc           :db/doc
         :unique        :db/unique
         :is-component  #{:db/isComponent}
         :should-index  #{:db/index}
         :should-generate #{:gen/should-generate}
         :no-history    #{:db/noHistory}
         :fulltext      #{:db/fulltext}
         :required      #{:required}
         :many-type     :interface.def.field/many-type
         :single-type   :interface.def.field/single-type))

(s/def :interface.def.field/single-type
  (s/alt :enum (s/cat :flag #{:enum}
                           :vals :interface.def.field.enum/vals)
         :non-enum :interface.def.field/single-non-enum-type))
(s/def :interface.def.field/many-type
  (s/alt :enum (s/cat :flag #{[:enum]}
                      :vals :interface.def.field.enum/vals)
         :non-enum (s/spec (s/cat :member-type :interface.def.field/single-non-enum-type))))
(def semantic-value-types (into #{} (map (comp keyword name)) datomic-value-types))
(s/def :interface.def.field/single-non-enum-type
  (s/alt :db-value-type semantic-value-types
         :interface-type keyword?))
(s/def :interface.def.field.enum/vals
  (s/alt :just-vals     :interface.def.field.enum/vals-no-doc
         :vals-with-doc :interface.def.field.enum/vals-with-doc))
(s/def :interface.def.field.enum/vals-no-doc (s/coll-of keyword? :kind set?))
(s/def :interface.def.field.enum/vals-with-doc (s/map-of keyword? string?))

;
; clojure.spec for defining custom generator maps for our fields
;
(s/def :interface/gen-map (s/map-of keyword? any?))

;
; clojure.spec describing the AST that a dev-defined interface is transformed into
;

(s/def :interface/ast
  (s/keys :req [:interface.ast/interfaces :interface.ast/enum-map]))

(s/def :interface.ast/interfaces
  (s/map-of keyword? :interface.ast/interface))

(s/def :interface.ast/interface
  (s/keys :req [:interface.ast.interface/name :interface.ast.interface/fields]
          :opt [:interface.ast.interface/inherits]))
(s/def :interface.ast.interface/name keyword?)
(s/def :interface.ast.interface/fields (s/map-of keyword? :interface.ast/field))
(s/def :interface.ast.interface/inherits (s/coll-of keyword? :kind set?))

(s/def :interface.ast/enum-map
  (s/map-of keyword? :interface.ast/enum))

(s/def :interface.ast/enum (s/keys :req [:db/ident]
                                   :opt [:db/doc]))

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

;
; clojure.spec describing the parser data used in translating the
; developer-defined interface definitions to the AST form
;

;
; clojure.spec for the resulting Datomic schema maps that can be sent to Datomic transactor
;

(s/def :datomic/schema
  (s/alt :datomic-enum (s/and :datomic/enum
                              :interface.ast/enum
                              (s/keys :req [:db/id]))
         :datomic-field (s/keys :req [:db/ident :db/valueType :db/cardinality
                                      :db/id :db.install/_attribute]
                                :opt [:db/doc :db/unique :db/index :db/isComponent :db/noHistory :db/fulltext])))

(s/def :interface/field-def-parser
  (s/keys :req [:interface.ast/field :interface.ast/enum-map]))
(s/fdef parse-field-def
        :args (s/cat :field-def (s/spec :interface.def/field) :field-name keyword?)
        :ret (s/keys :req [:interface.ast/field :interface.ast/enum-map]))
(defn- parse-field-def
  [field-def field-name]
  (let [{:keys [field-tags]} (s/conform :interface.def/field field-def)
        parsed {:interface.ast/field {:db/ident field-name}
                :interface.ast/enum-map {}}]
    (reduce
      (fn [parsed conform-output]
        (match [conform-output]
               [[:doc doc]] (assoc-in parsed [:interface.ast/field :db/doc] doc)
               [[:single-type
                 [:non-enum
                  [:db-value-type db-value-type]]]] (-> parsed
                                                        (assoc-in [:interface.ast/field :db/cardinality] :db.cardinality/one)
                                                        (assoc-in [:interface.ast/field :db/valueType] (keyword "db.type" (name db-value-type)))
                                                        (assoc-in [:interface.ast/field :interface.ast.field/type] db-value-type))
               [[:single-type
                 [:non-enum
                  [:interface-type interface-type]]]] (-> parsed
                                                          (assoc-in [:interface.ast/field :db/cardinality] :db.cardinality/one)
                                                          (assoc-in [:interface.ast/field :db/valueType] :db.type/ref)
                                                          (assoc-in [:interface.ast/field :interface.ast.field/type] interface-type))
               [[:single-type
                 [:enum
                  {:vals
                   [:just-vals vals]}]]] (-> parsed
                                             (assoc-in [:interface.ast/field :db/cardinality] :db.cardinality/one)
                                             (assoc-in [:interface.ast/field :db/valueType] :db.type/ref)
                                             (assoc-in [:interface.ast/field :interface.ast.field/type] :enum)
                                             (assoc-in [:interface.ast/field :interface.ast.field/possible-enum-vals] (into #{} vals))
                                             (update :interface.ast/enum-map into (map (fn [kw] [kw {:db/ident kw}])) vals))
               [[:single-type
                 [:enum
                  {:vals
                   [:vals-with-doc kw->doc]}]]] (-> parsed
                                                    (assoc-in [:interface.ast/field :db/cardinality] :db.cardinality/one)
                                                    (assoc-in [:interface.ast/field :db/valueType] :db.type/ref)
                                                    (assoc-in [:interface.ast/field :interface.ast.field/type] :enum)
                                                    (assoc-in [:interface.ast/field :interface.ast.field/possible-enum-vals] (into #{} (keys kw->doc)))
                                                    (update :interface.ast/enum-map
                                                            into (map (fn [[kw doc]] [kw {:db/ident kw :db/doc doc}])) kw->doc))
               [[:many-type
                 [:non-enum
                  {:member-type
                   [:db-value-type db-value-type]}]]] (-> parsed
                                                          (assoc-in [:interface.ast/field :db/cardinality] :db.cardinality/many)
                                                          (assoc-in [:interface.ast/field :db/valueType] (keyword "db.type" (name db-value-type)))
                                                          (assoc-in [:interface.ast/field :interface.ast.field/type] db-value-type))
               [[:many-type
                 [:non-enum
                  {:member-type
                   [:interface-type interface-type]}]]] (-> parsed
                                                            (assoc-in [:interface.ast/field :db/cardinality] :db.cardinality/many)
                                                            (assoc-in [:interface.ast/field :db/valueType] :db.type/ref)
                                                            (assoc-in [:interface.ast/field :interface.ast.field/type] interface-type))
               [[:many-type
                 [:enum
                  {:vals
                   [:just-vals vals]}]]] (-> parsed
                                             (assoc-in [:interface.ast/field :db/cardinality] :db.cardinality/many)
                                             (assoc-in [:interface.ast/field :db/valueType] :db.type/ref)
                                             (assoc-in [:interface.ast/field :interface.ast.field/type] :enum)
                                             (assoc-in [:interface.ast/field :interface.ast.field/possible-enum-vals] (into #{} vals))
                                             (update :interface.ast/enum-map into (map (fn [kw] [kw {:db/ident kw}])) vals))

               [[:many-type
                 [:enum
                  {:vals
                   [:vals-with-doc kw->doc]}]]] (-> parsed
                                                    (assoc-in [:interface.ast/field :db/cardinality] :db.cardinality/many)
                                                    (assoc-in [:interface.ast/field :db/valueType] :db.type/ref)
                                                    (assoc-in [:interface.ast/field :interface.ast.field/type] :enum)
                                                    (assoc-in [:interface.ast/field :interface.ast.field/possible-enum-vals] (into #{} (keys kw->doc)))
                                                    (update :interface.ast/enum-map
                                                            into (map (fn [[kw doc]] [kw {:db/ident kw :db/doc doc}])) kw->doc))

               [[:unique val]] (assoc-in parsed [:interface.ast/field :db/unique] val)
               [[:is-component _]] (assoc-in parsed [:interface.ast/field :db/isComponent] true)
               [[:should-index _]] (assoc-in parsed [:interface.ast/field :db/index] true)
               [[:should-generate _]] (assoc-in parsed [:interface.ast/field :gen/should-generate] true)
               [[:no-history _]] (assoc-in parsed [:interface.ast/field :db/noHistory] true)
               [[:fulltext _]] (assoc-in parsed [:interface.ast/field :db/fulltext] true)
               [[:required _]] (assoc-in parsed [:interface.ast/field :interface.ast.field/required] true)))
      parsed
      field-tags)))
(stest/instrument `parse-field-def)

(s/fdef semantic-spec->semantic-ast
        :args (s/cat :spec :interface/def)
        :ret :interface/ast)
(defn semantic-spec->semantic-ast
  "Converts a user-readability-optimized semantic schema spec into a AST that can be used to generate Datomic
  schemas, generate test data, etc."
  [spec]
  (let [{:interface.def/keys [name fields inherits identify-via]} spec
        {:keys [interface-fields enum-map]} (reduce
                                              (fn [parsed [field-name field-spec]]
                                                (let [{:interface.ast/keys [enum-map field]} (parse-field-def field-spec field-name)]
                                                  (-> parsed
                                                      (update :enum-map merge enum-map)
                                                      (assoc-in [:interface-fields field-name] field))))
                                              {:interface-fields {} :enum-map {}}
                                              fields)
        identify-via' (if (= :datomic-spec/interfaces identify-via)
                        [['?e :datomic-spec/interfaces name]]
                        identify-via)]
    {:interface.ast/interfaces
      {name #:interface.ast.interface
       {:name name
        :fields (cond-> interface-fields
                        (= :datomic-spec/interfaces identify-via) (merge {:datomic-spec/interfaces {:db/ident :datomic-spec/interfaces
                                                                                                    :db/valueType :db.type/ref
                                                                                                    :interface.ast.field/type :enum
                                                                                                    :interface.ast.field/possible-enum-vals #{name}
                                                                                                    :interface.ast.field/required true
                                                                                                    :db/cardinality :db.cardinality/many}}))
        :inherits (set inherits)
        :identify-via identify-via'}}
     :interface.ast/enum-map (cond-> enum-map
                                     (= :datomic-spec/interfaces identify-via) (merge {name {:db/ident name}}))}))
(stest/instrument `semantic-spec->semantic-ast)

(s/fdef only-db-keys
        :args (s/cat :map (s/map-of keyword? any?))
        :ret (s/map-of keyword? any?))
(defn- only-db-keys
  [m]
  (into {} (filter
             #(let [key (first %)
                    kw (if (keyword? key)
                         key
                         (:k key))]
               (re-matches #"^db" (namespace kw)))
             m)))
(stest/instrument `only-db-keys)

(def tempid-factory-spec (s/fspec :args (s/cat :partition keyword? :num (s/? number?))
                                  :ret any?))
(s/fdef semantic-ast->datomic-schemas
        ; TODO Added this during oss
        :args (s/cat :ast :interface/ast
                     :tempid-factory tempid-factory-spec)
        :ret (s/coll-of :datomic/schema :kind vector?))
(defn semantic-ast->datomic-schemas
  "Given a semantic AST, generates edn that represents attributes to add to
  Datomic schema. `tempid` will be passed in and will be `datomic.api/tempid`."
  [ast tempid-factory]
  (let [enum-datoms (for [[_ enum] (:interface.ast/enum-map ast)]
                      (-> enum
                          only-db-keys
                          ; TODO Take into account partition for the enums
                          (assoc :db/id (tempid-factory :db.part/user))))
        field-datoms (flatten
                       (for [[_ intfc] (:interface.ast/interfaces ast)]
                         (for [[_ field] (:interface.ast.interface/fields intfc)]
                           (-> field
                               only-db-keys
                               (assoc :db/id (tempid-factory :db.part/db)
                                      :db.install/_attribute :db.part/db)))))]
    (into [] (concat enum-datoms field-datoms))))
(stest/instrument `semantic-ast->datomic-schemas)

(s/fdef semantic-spec->datomic-schemas
        :args (s/cat :spec :interface/def
                     :tempid-factory tempid-factory-spec)
        :ret (s/+ :datomic/schema))
(defn semantic-spec->datomic-schemas
  "Given a semantic spec, generates edn that represents attributes to add to Datomic schema"
  [spec tempid-factory]
  (-> spec
      semantic-spec->semantic-ast
      (semantic-ast->datomic-schemas tempid-factory)))
(stest/instrument `semantic-spec->datomic-schemas)

(defn validate-semantic-ast
  [ast]
  (let [enum-vals (set (select [:interface.ast/enum-map MAP-VALS :db/ident] ast))
        interface-names (set (select [:interface.ast/interfaces MAP-VALS :interface.ast.interface/name] ast))
        primitives #{:keyword :string :boolean :long :bigint :float :double :bigdec :instant :uuid :bytes}
        valid-types (clojure.set/union primitives interface-names #{:enum})]
    (doseq [[interface-name {:keys [interface.ast.interface/fields]}] (:interface.ast/interfaces ast)]
      (doseq [[_ {:keys [interface.ast.field/type] :as field}] fields]
        (if-not (contains? valid-types type)
          (throw (ex-info (str "Invalid attribute type " type)
                          {:interface-name interface-name
                           :field field})))
        (if (= type :enum)
          (let [undeclared-enums (clojure.set/difference (:interface.ast.field/possible-enum-vals field) enum-vals)]
            (if (seq undeclared-enums)
              (throw (ex-info "Undeclared enums"
                              {:undeclared-enums undeclared-enums
                               :interface-name interface-name
                               :field field})))))))))

(s/fdef semantic-spec-coll->semantic-ast
        :args (s/cat :specs (s/spec (s/+ :interface/def)))
        :ret :interface/ast)
(defn semantic-spec-coll->semantic-ast
  "Given a collection of semantic specs, generates a semantic ast"
  [specs]
  (let [asts (map semantic-spec->semantic-ast specs)
        combined-ast (apply merge-with merge asts)]
    (validate-semantic-ast combined-ast)
    combined-ast))
(stest/instrument `semantic-spec-coll->semantic-ast)

; TODO Rename
(s/fdef semantic-spec-coll->datomic-schemas
        :args (s/cat :specs (s/spec (s/+ :interface/def))
                     :tempid-factory tempid-factory-spec)
        :ret (s/+ :datomic/schema))
(defn semantic-spec-coll->datomic-schemas
  "Given a collection of semantic specs, generates edn that represents attributes to add to Datomic schema"
  [specs tempid-factory]
  (-> specs
      semantic-spec-coll->semantic-ast
      (semantic-ast->datomic-schemas tempid-factory)))
(stest/instrument `semantic-spec-coll->datomic-schemas)

(defn validate-semantic-interfaces
  "Validates a collection of semantic interface definitions"
  [specs]
  ; TODO
  (s/valid? (s/+ :interface/def) specs))


;; TODO Use defmulti instead?
(defn- ast-field-type->predicate
  "Given the AST representation of a field, return the appropriate predicate
  testing the validity of the field value. These predicates are intended to
  be used in defining and registering the appropriate clojure.spec spec for
  the field."
  ([type]
   {:pre [(s/valid? :interface.ast.field/type type)]
    :post [(s/valid? (s/or :fn fn? :kw keyword?) %)]
    }
   (case type
     :keyword keyword?
     :string string?
     :boolean boolean?
     :long number?
     :bigint integer?
     :float float?
     :double float?
     :bigdec integer?
     :instant inst?
     :uuid uuid?
     :uri uri?
     :bytes bytes?
     type))
  ([type possible-enum-vals]
   {:pre [(= :enum type)
          (s/valid? :interface.ast.field/possible-enum-vals possible-enum-vals)]}
   (into #{} possible-enum-vals)))

; TODO Get more specific than any?
(s/def :clojure.spec/deps-graph any?)
(s/def :clojure.spec/macros (s/map-of keyword? any?))

(s/fdef ast&interface->ast-fields
        :args (s/cat :ast :interface/ast
                     :ast/interface :interface.ast/interface)
        :ret (s/coll-of :interface.ast/field :kind set?))
(defn- ast&interface->ast-fields
  "Returns the set of all fields that represent an interface."
  [ast interface]
  (let [{:keys [interface.ast/interfaces]} ast
        {:interface.ast.interface/keys [fields inherits]} interface
        immediate-fields (vals fields)
        inherited-fields (->> inherits
                              (map interfaces)
                              (mapcat (partial ast&interface->ast-fields ast)))]
    (into (set inherited-fields) immediate-fields)))
(stest/instrument `ast&interface->ast-fields)

(s/fdef all-inherited-interface-names
        :args (s/cat :ast :interface/ast
                     :interface-name keyword?)
        :ret (s/coll-of keyword? :kind set?))
(defn- all-inherited-interface-names
  "Returns the union of all immediately inherited and recursively inherited interfaces
  for a given interface represented by `interface-name`."
  [ast interface-name]
  (let [interface (-> ast :interface.ast/interfaces interface-name)
        {:keys [interface.ast.interface/inherits]} interface]
    (into inherits (mapcat #(all-inherited-interface-names ast %) inherits))))
(stest/instrument `all-inherited-interface-names)

;(s/def :gen/generator-factory (s/fspec :args (s/cat :member-generator (s/? :gen/member-generator))
;                                       :ret any?))
(s/def :gen/generator-factory fn?)
; TODO Be more specific than any?
(s/def :gen/member-generator any?)

(s/fdef interface->clojure-spec-macros
        :args (s/cat :ast :interface/ast
                     :interface-name keyword?
                     :req-fields (s/coll-of :interface.ast/field :kind vector?)
                     :opt-fields (s/coll-of :interface.ast/field :kind vector?)
                     :gen-map :interface/gen-map
                     :custom-generator-factory (s/? :gen/generator-factory))
        :ret any?)
; TODO Make sure enums include all-inherited for datomic-spec/interfaces
(defn- interface->clojure-spec-macros
  "Returns the entity map clojure.spec for the given interface represented by `interface-name`."
  ([ast interface-name req-fields opt-fields gen-map]
    (let [all-inherited (all-inherited-interface-names ast interface-name)
          all-my-interfaces (conj all-inherited interface-name)
          inherited-custom-generators? (seq (keep gen-map all-inherited))
          identify-via-datomic-spec-interfaces? (some #(= (:db/ident %) :datomic-spec/interfaces) req-fields)

          ; We use distinct here because we could have multiple :datomic-spec/interfaces attributes that we inherit
          req-keys (conj (vec (distinct (map :db/ident req-fields))) :db/id)
          opt-keys (mapv :db/ident opt-fields)
          base-spec `(s/keys :req ~req-keys, :opt ~opt-keys)
          ; We remove :datomic-spec/interfaces, so we don't un-necessarily generate them randomly, before we
          ; over-write them with our own deterministic set of interface keywords.
          base-gen-spec `(s/keys
                           :req ~(vec (remove #(= % :datomic-spec/interfaces) req-keys))
                           :opt ~opt-keys)
          spec (if (and identify-via-datomic-spec-interfaces? (seq all-inherited))
                 `(s/and ~base-spec #(clojure.set/subset? ~all-my-interfaces (:datomic-spec/interfaces %)))
                 base-spec)]
      (if inherited-custom-generators?
        `(s/with-gen ~spec
                     #(gen/fmap
                       (comp (partial apply merge)
                             (fn [~'xs] (conj ~'xs (if identify-via-datomic-spec-interfaces?
                                                     {:datomic-spec/interfaces (conj all-inherited interface-name)}
                                                     {}))))
                       ~(cons 'clojure.spec.gen/tuple (map (fn [x] `(s/gen ~x)) all-inherited))))
        (if-not identify-via-datomic-spec-interfaces?
          spec
          `(s/with-gen ~spec
                       #(gen/fmap
                         (comp (partial apply merge)
                               (fn [~'xs] (conj ~'xs {:datomic-spec/interfaces ~(conj all-inherited interface-name)})))
                         (clojure.spec.gen/tuple (s/gen ~base-gen-spec))))))))

  ([ast interface-name req-fields opt-fields gen-map custom-generator-factory]
   (let [all-inherited (all-inherited-interface-names ast interface-name)
         all-my-interfaces (conj all-inherited interface-name)
         inherited-custom-generators? (seq (keep gen-map all-inherited))
         identify-via-datomic-spec-interfaces? (some #(= (:db/ident %) :datomic-spec/interfaces) req-fields)

         ; We use distinct here because we could have multiple :datomic-spec/interfaces attributes that we inherit
         req-keys (conj (vec (distinct (map :db/ident req-fields))) :db/id)
         opt-keys (mapv :db/ident opt-fields)
         base-spec `(s/keys :req ~req-keys, :opt ~opt-keys)
         ; We remove :datomic-spec/interfaces, so we don't un-necessarily generate them randomly, before we
         ; over-write them with our own deterministic set of interface keywords.
         base-gen-spec `(s/keys
                          :req ~(vec (remove #(= % :datomic-spec/interfaces) req-keys))
                          :opt ~opt-keys)
         spec (if (and identify-via-datomic-spec-interfaces? (seq all-inherited))
                `(s/and ~base-spec #(clojure.set/subset? ~all-my-interfaces (:datomic-spec/interfaces %)))
                base-spec)]
     (if inherited-custom-generators?
       `(s/with-gen ~spec
                    #(gen/fmap
                      (if identify-via-datomic-spec-interfaces?
                        (fn [~'hashes-to-combine]
                          (apply merge (conj ~'hashes-to-combine {:datomic-spec/interfaces ~all-my-interfaces})))
                        (partial apply merge))
                      ~(cons 'clojure.spec.gen/tuple
                             (cons (custom-generator-factory base-gen-spec)
                                   (map (fn [x] `(s/gen ~x)) all-inherited)))))
       `(s/with-gen ~spec
                    ~(if-not identify-via-datomic-spec-interfaces?
                       `(fn []
                          ~(custom-generator-factory spec))
                      `#(gen/fmap
                         (fn [~'obj] (merge ~'obj {:datomic-spec/interfaces ~all-my-interfaces}))
                         ~(custom-generator-factory base-gen-spec))))))))
(stest/instrument `interface->clojure-spec-macros)

(def ^:private NATIVE-TYPES
  #{:keyword :string :boolean :long :bigint :float :double :bigdec :instant :uuid :uri :bytes})

(defn- interface-type?
  [type]
  (not (contains? NATIVE-TYPES type)))

(s/fdef add-interface-to-deps-graph
        :args (s/cat :ast :interface/ast
                     :ast/interface :interface.ast/interface
                     :deps-graph :clojure.spec/deps-graph)
        :ret :clojure.spec/deps-graph)
(defn- add-interface-to-deps-graph
  "Adds the interface and its inherited, field, and pointer dependencies to the dependency graph `deps-graph`"
  [ast interface deps-graph]
  (let [{:interface.ast.interface/keys [name inherits]} interface
        all-fields (ast&interface->ast-fields ast interface)
        field-deps (map :db/ident all-fields)
        ref-deps (->> all-fields
                      (map :interface.ast.field/type)
                      (filter #(and (interface-type? %) (not= name %))))
        dependencies (concat inherits field-deps ref-deps)]
    (reduce #(ssdep/depend %1 name %2) deps-graph dependencies)))
(stest/instrument `add-interface-to-deps-graph)

(s/fdef field->clojure-spec-macro
        :arg (s/cat :field :interface.ast/field
                    ; TODO Better than any?
                    :custom-generator-factory any?)
        :ret any?)
(defn- field->clojure-spec-macro
  "Returns the clojure.spec macro for the given `field`, with an optional `custom-generator-factory`"
  [field custom-generator-factory]
  (let [{:keys [; required
                db/ident
                interface.ast.field/type
                db/cardinality
                ; optional
                interface.ast.field/possible-enum-vals]} field
        pred (if possible-enum-vals
               (ast-field-type->predicate type possible-enum-vals)
               (ast-field-type->predicate type))]
    (case cardinality
      :db.cardinality/many
      (if custom-generator-factory
        `(s/def ~ident (s/with-gen (s/coll-of ~pred :kind set?)
                                   ~#(case (arity custom-generator-factory)
                                      1 (let [member-gen (eval (macroexpand `(s/gen ~pred)))]
                                          (custom-generator-factory member-gen))
                                      (custom-generator-factory))))
        (case type
          :string `(s/def ~ident
                     (s/with-gen (s/coll-of ~pred :kind set?)
                                 #(gen/set (gen/not-empty (gen/string-alphanumeric)))))
          `(s/def ~ident (s/coll-of ~pred :kind set?))))

      :db.cardinality/one
      (if custom-generator-factory
        `(s/def ~ident (s/with-gen ~pred
                                   ~#(custom-generator-factory)))
        (case type
          :string `(s/def ~ident
                     (s/with-gen ~pred
                                 #(gen/not-empty (gen/string-alphanumeric))))
          `(s/def ~ident ~pred))))))
(stest/instrument `field->clojure-spec-macro)

(s/fdef interface->clojure-spec-defs
        :args (s/cat :ast :interface/ast
                     :ast/interface :interface.ast/interface
                     :gen-map :interface/gen-map)
        :ret :clojure.spec/macros)
(defn- interface->clojure-spec-defs
  "Returns the clojure.spec macros that will register this interface and its respective fields as clojure.spec's."
  [ast interface gen-map]
  (let [{:keys [interface.ast.interface/name]} interface
        all-fields (ast&interface->ast-fields ast interface)
        interface-spec-def (let [{req-fields true
                                  opt-fields nil
                                  :or {req-fields []
                                       opt-fields []}} (group-by :interface.ast.field/required all-fields)
                                 {gen-fields true :or {gen-fields []}} (group-by :gen/should-generate all-fields)]
                             (if-let [custom-generator-factory (get gen-map name)]
                               `(s/def ~name ~(interface->clojure-spec-macros ast name req-fields opt-fields gen-map custom-generator-factory))
                               (if (seq gen-fields)
                                 `(s/def ~name ~(interface->clojure-spec-macros ast name req-fields opt-fields gen-map (apply ensure-keys-gen (map :db/ident gen-fields))))
                                 `(s/def ~name ~(interface->clojure-spec-macros ast name req-fields opt-fields gen-map)))))]
    (->> all-fields
         (filter #(not= (:db/ident %) :datomic-spec/interfaces))
         (reduce (fn [macros {:keys [db/ident] :as field}]
                   (merge macros
                          {ident (field->clojure-spec-macro field (get gen-map ident))}))
                 {name interface-spec-def}))))
(stest/instrument `interface->clojure-spec-defs)

(s/fdef deps-graph-for-ast
        :args (s/cat :ast :interface/ast)
        :ret :clojure.spec/deps-graph)
(defn- deps-graph-for-ast
  "Given the intermediate AST, returns the dependency graph that encapsulates the dependencies of an
  interface, including immediate and inherited fields, inherited interfaces, and interfaces to which it points."
  [{:keys [interface.ast/interfaces] :as ast}]
  (reduce #(add-interface-to-deps-graph ast %2 %1)
          (ssdep/graph)
          (vals interfaces)))
(stest/instrument `deps-graph-for-ast)

(s/fdef ast->clojure-spec-macros
        :args (s/cat :ast :interface/ast
                     :gen-map :interface/gen-map)
        :ret :clojure.spec/macros)
(defn- ast->clojure-spec-macros
  "Given the intermediate AST and a mapping of field and interface names to test.check generators, this returns
  the clojure.spec macros that we can expand and evaluate later."
  [{:keys [interface.ast/interfaces] :as ast} gen-map]
  (reduce
    (fn [macros interface] (merge macros (interface->clojure-spec-defs ast interface gen-map)))
    {:datomic-spec/interfaces `(s/def :datomic-spec/interfaces (s/coll-of keyword? :kind set?))}
    (vals interfaces)))
(stest/instrument `ast->clojure-spec-macros)

(s/fdef register-generative-specs-for-ast!
        :args (s/cat :ast :interface/ast
                     :gen-map :interface/gen-map
                     :tempid-factory fn?
                     ;:tempid-factory tempid-factory-spec
                     :db-id? (s/fspec :args (s/cat :x any?)
                                      :ret boolean?)))
(defn register-generative-specs-for-ast!
  "Given an entire interface AST and some custom generators for some fields,
  register all clojure.spec specs that should be associated with the AST."
  [ast gen-map tempid-factory db-id?]
  (let [macros (ast->clojure-spec-macros ast gen-map)
        deps-graph (deps-graph-for-ast ast)]
    (s/def :db/id
      (s/with-gen db-id? (fn->gen #(tempid-factory :db.part/user))))
    (doseq [spec-name (ssdep/topo-sort deps-graph)]
      (eval (macroexpand (macros spec-name))))))
(stest/instrument `register-generative-specs-for-ast!)

(s/fdef register-specs-for-ast!
        :args (s/cat :ast :interface/ast
                     :tempid-factory fn?
                     ;:tempid-factory tempid-factory-spec
                     :db-id? (s/fspec :args (s/cat :x any?)
                                      :ret boolean?)))
(defn register-specs-for-ast!
  "Given an entire interface AST, register all clojure.spec specs that
  should be associated with the AST."
  [ast tempid-factory db-id?]
  (register-generative-specs-for-ast! ast {} tempid-factory db-id?))
(stest/instrument `register-specs-for-ast!)