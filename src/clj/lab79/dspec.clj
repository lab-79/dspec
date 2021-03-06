(ns lab79.dspec
  (:require [clojure.core.match :refer [match]]
            [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clojure.test.check.generators :refer [generator?]]
            [com.stuartsierra.dependency :as ssdep]
            [com.rpl.specter :refer [MAP-VALS collect-one]]
            [com.rpl.specter.macros :refer [select traverse select-first]]
            [lab79.dspec.util :refer [arity filter-kv]]
            [lab79.datomic-spec :refer [datomic-value-types
                                        datomic-schema-keys]]
            [lab79.dspec.gen :refer [ensure-keys-gen fn->gen]]))


;
; clojure.spec describing how devs define data interfaces
;

(s/def :interface/def
  (s/and (s/keys :req [:interface.def/name :interface.def/fields :interface.def/identify-via]
                 :opt [:interface.def/identifying-enum-part])
         (s/or :id-via-dspec-interfaces (s/and #(= (% :interface.def/identify-via) [:identify-via/reserved-attribute :datomic-spec/interfaces])
                                               #(contains? % :interface.def/identifying-enum-part))
               :id-via-attr #(= (-> % :interface.def/identify-via first) :identify-via/datalog-clause))))
(s/def :interface.def/name keyword?)
(s/def :interface.def/fields (s/map-of keyword? :interface.def/field))
(s/def :interface.def/field
  (s/with-gen
    (s/and (s/+ :interface.def.field/trait)
           ; Ensure we have a type
           #(->> % (map first) (some #{:many-type :single-type}))
           ; Ensure each "kind" of trait only appears once per field
           #(->> % (map first) frequencies vals (every? (partial identical? 1))))
    ; TODO Make a richer generator that uses all kinds of traits
    #(gen/fmap
       (fn [[single-type optional-kv]]
         (flatten (conj (vals optional-kv) single-type)))
       (gen/tuple
         (s/gen :interface.def.field/single-type)
         (s/gen (s/keys :opt [:db/doc :db/unique]))))
    ))
(s/def :interface.def/inherits (s/coll-of keyword? :kind vector?))

(s/def :interface.def/identify-via (s/or :identify-via/reserved-attribute #{:datomic-spec/interfaces}
                                         :identify-via/datalog-clause :datomic.query.kv/where))
(s/def :db/part (s/with-gen (s/and keyword? #(= (namespace %) "db.part"))
                            #(gen/fmap (fn [string] (keyword "db.part" string))
                                        (gen/such-that (comp not clojure.string/blank?)
                                                       (gen/string-alphanumeric)))))
(s/def :interface.def/identifying-enum-part :db/part)
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
(def ^:private semantic-value-types (into #{} (map (comp keyword name)) datomic-value-types))
(s/def :interface.def.field/single-non-enum-type
  (s/alt :db-value-type semantic-value-types

         ; Before, we just had keyword? for :interface-type.
         ; We need the additonal predicate added by `(s/and ...)`. Otherwise, we
         ; got the following at the REPL:
         ;
         ; >> (->> (s/conform :interface.def/field [[:enum] #{:A} [:enum] #{:*}]))
         ; => [[:many-type [:enum {:flag [:enum], :vals [:just-vals #{:A}]}]] [[:many-type [:enum {:flag [:enum], :vals [:just-vals #{:*}]}]]]]
         ;
         ; For some reason, two re's that could both match an input would cause this double vector (see second element above)
         :interface-type (s/and keyword?
                                (comp not (conj semantic-value-types :enum)))))
(s/def :interface.def.field.enum/vals
  (s/alt :just-vals     :interface.def.field.enum/vals-no-doc
         :vals-with-doc :interface.def.field.enum/vals-with-doc))
(s/def :interface.def.field.enum/vals-no-doc (s/coll-of keyword? :kind set? :min-count 1))
(s/def :interface.def.field.enum/vals-with-doc (s/map-of keyword? string? :min-count 1))

;
; clojure.spec for defining custom generator maps for our fields
;
(s/def :interface/gen-map (s/map-of keyword? (s/or :no-member-gen-factory :gen/generator-factory
                                                   :with-member-gen-factory :gen/generator-factory-with-member)))

;
; clojure.spec describing the AST that a dev-defined interface is transformed into
;

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

; Given the AST representation of a field, return the appropriate predicate
; testing the validity of the field value. These predicates are intended to
; be used in defining and registering the appropriate clojure.spec spec for
; the field.
(def ^:private ast-field-type->predicate
  {:keyword keyword?, :string string?, :boolean boolean?, :long number?, :bigint integer?, :float float?,
   :double float?, :bigdec integer?, :instant inst?, :uuid uuid?, :uri uri?, :bytes bytes?})

(s/def :interface.ast.field/type keyword?)

(s/def :interface.ast.field/possible-enum-vals (s/+ keyword?))

(s/def :interface.ast.field/required boolean?)
(s/def :gen/should-generate boolean?)

;
; clojure.spec describing the parser data used in translating the
; developer-defined interface definitions to the AST form
;

;
; clojure.spec for the resulting Datomic schema maps that can be sent to Datomic transactor
;

(s/def :interface/field-def-parser
  (s/keys :req [:interface.ast/field :interface.ast/enum-map]))
(s/fdef field-def->ast-field&enum-map
        :args (s/cat :field-def (s/spec :interface.def/field) :field-name keyword?)
        :ret (s/keys :req [:interface.ast/field :interface.ast/enum-map]))
(defn- field-def->ast-field&enum-map
  "Given the field definition portion of an interface defintion, return the ast representation of the field and the
  ast map representation of any enums that may be associated with the field.

  Example:
  Consider the following interface definition:

  ```
  {:interface.def/name :interface/example
   :interface.def/fields {:example/color [:enum #{:green :blue} :required \"A keyword field\"]}
   :interface.def/identify-via [['?e :example/kw]]}
  ```

  The `field-name` is `:example/kw`.
  The `field-def` is `[:enum #{:green :blue} :required \"A keyword field\"]`.
  The return value is:

  ```
  {:interface.ast/field {:db/cardinality :db.cardinality/one
                         :db/valueType :db.type/ref
                         :interface.ast.field/type :enum
                         :interface.ast.field/possible-enum-vals #{:green :blue}
                         :interface.ast.field/required true
                         :db/doc \"A keyword field\"}
   :interface.ast/enum-map {:green {:db/ident :green}
                            :blue {:db/ident :blue}}}
  ```
  "
  [field-def field-name]
  (let [field-tags (s/conform :interface.def/field field-def)
        parsed {:interface.ast/field {:db/ident field-name}
                :interface.ast/enum-map {}}]
    (reduce
      (fn [parsed conform-output]
        (match conform-output
               [:doc doc] (assoc-in parsed [:interface.ast/field :db/doc] doc)
               [:single-type
                [:non-enum
                 [:db-value-type db-value-type]]] (-> parsed
                                                      (assoc-in [:interface.ast/field :db/cardinality] :db.cardinality/one)
                                                      (assoc-in [:interface.ast/field :db/valueType] (keyword "db.type" (name db-value-type)))
                                                      (assoc-in [:interface.ast/field :interface.ast.field/type] db-value-type))
               [:single-type
                [:non-enum
                 [:interface-type interface-type]]] (-> parsed
                                                        (assoc-in [:interface.ast/field :db/cardinality] :db.cardinality/one)
                                                        (assoc-in [:interface.ast/field :db/valueType] :db.type/ref)
                                                        (assoc-in [:interface.ast/field :interface.ast.field/type] interface-type))
               [:single-type
                [:enum
                 {:vals
                  [:just-vals values]}]] (-> parsed
                                             (assoc-in [:interface.ast/field :db/cardinality] :db.cardinality/one)
                                             (assoc-in [:interface.ast/field :db/valueType] :db.type/ref)
                                             (assoc-in [:interface.ast/field :interface.ast.field/type] :enum)
                                             (assoc-in [:interface.ast/field :interface.ast.field/possible-enum-vals] (set values))
                                             (update :interface.ast/enum-map into (map (fn [kw] [kw {:db/ident kw}])) values))
               [:single-type
                [:enum
                 {:vals
                  [:vals-with-doc kw->doc]}]] (-> parsed
                                                  (assoc-in [:interface.ast/field :db/cardinality] :db.cardinality/one)
                                                  (assoc-in [:interface.ast/field :db/valueType] :db.type/ref)
                                                  (assoc-in [:interface.ast/field :interface.ast.field/type] :enum)
                                                  (assoc-in [:interface.ast/field :interface.ast.field/possible-enum-vals] (set (keys kw->doc)))
                                                  (update :interface.ast/enum-map
                                                          into (map (fn [[kw doc]] [kw {:db/ident kw :db/doc doc}])) kw->doc))
               [:many-type
                [:non-enum
                 {:member-type
                  [:db-value-type db-value-type]}]] (-> parsed
                                                        (assoc-in [:interface.ast/field :db/cardinality] :db.cardinality/many)
                                                        (assoc-in [:interface.ast/field :db/valueType] (keyword "db.type" (name db-value-type)))
                                                        (assoc-in [:interface.ast/field :interface.ast.field/type] db-value-type))
               [:many-type
                [:non-enum
                 {:member-type
                  [:interface-type interface-type]}]] (-> parsed
                                                          (assoc-in [:interface.ast/field :db/cardinality] :db.cardinality/many)
                                                          (assoc-in [:interface.ast/field :db/valueType] :db.type/ref)
                                                          (assoc-in [:interface.ast/field :interface.ast.field/type] interface-type))
               [:many-type
                [:enum
                 {:vals
                  [:just-vals values]}]] (-> parsed
                                             (assoc-in [:interface.ast/field :db/cardinality] :db.cardinality/many)
                                             (assoc-in [:interface.ast/field :db/valueType] :db.type/ref)
                                             (assoc-in [:interface.ast/field :interface.ast.field/type] :enum)
                                             (assoc-in [:interface.ast/field :interface.ast.field/possible-enum-vals] (set values))
                                             (update :interface.ast/enum-map into (map (fn [kw] [kw {:db/ident kw}])) values))

               [:many-type
                [:enum
                 {:vals
                  [:vals-with-doc kw->doc]}]] (-> parsed
                                                  (assoc-in [:interface.ast/field :db/cardinality] :db.cardinality/many)
                                                  (assoc-in [:interface.ast/field :db/valueType] :db.type/ref)
                                                  (assoc-in [:interface.ast/field :interface.ast.field/type] :enum)
                                                  (assoc-in [:interface.ast/field :interface.ast.field/possible-enum-vals] (set (keys kw->doc)))
                                                  (update :interface.ast/enum-map
                                                          into (map (fn [[kw doc]] [kw {:db/ident kw :db/doc doc}])) kw->doc))

               [:unique val] (assoc-in parsed [:interface.ast/field :db/unique] val)
               [:is-component _] (assoc-in parsed [:interface.ast/field :db/isComponent] true)
               [:should-index _] (assoc-in parsed [:interface.ast/field :db/index] true)
               [:should-generate _] (assoc-in parsed [:interface.ast/field :gen/should-generate] true)
               [:no-history _] (assoc-in parsed [:interface.ast/field :db/noHistory] true)
               [:fulltext _] (assoc-in parsed [:interface.ast/field :db/fulltext] true)
               [:required _] (assoc-in parsed [:interface.ast/field :interface.ast.field/required] true)))
      parsed
      field-tags)))

(s/fdef semantic-spec->semantic-ast
        :args (s/cat :spec :interface/def)
        :ret :interface/ast)
(defn semantic-spec->semantic-ast
  "Converts a user-readability-optimized semantic schema spec into a AST that can be used to generate Datomic
  schemas, generate test data, etc."
  [spec]
  (let [{:interface.def/keys [name fields inherits identify-via identifying-enum-part]} spec
        {:keys [interface-fields enum-map]} (reduce
                                              (fn [parsed [field-name field-spec]]
                                                (let [{:interface.ast/keys [enum-map field]} (field-def->ast-field&enum-map field-spec field-name)]
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
                 :fields (cond->
                           interface-fields
                           (= :datomic-spec/interfaces identify-via) (assoc :datomic-spec/interfaces
                                                                            {:db/ident :datomic-spec/interfaces
                                                                             :db/valueType :db.type/ref
                                                                             :db/index true
                                                                             :db/cardinality :db.cardinality/many
                                                                             :interface.ast.field/type :enum
                                                                             :interface.ast.field/possible-enum-vals #{name}
                                                                             :interface.ast.field/required true}))
                 :inherits (set inherits)
                 :identify-via identify-via'}}
     :interface.ast/enum-map (cond-> enum-map
                                     (= :datomic-spec/interfaces identify-via) (assoc name {:db/ident name
                                                                                            :db/part identifying-enum-part}))}))


(def ^:private tempid-factory-spec
  (s/fspec :args (s/alt :binary (s/cat :partition keyword? :num integer?)
                        :unary (s/cat :partition keyword?))
           :ret any?))

(s/fdef semantic-ast->datomic-enum-schemas
        :args (s/cat :ast :interface/ast
                     :tempid-factory tempid-factory-spec)
        :ret (s/coll-of :datomic/enum-schema))
(defn- semantic-ast->datomic-enum-schemas
  "Convert all enums into Datomic schema enums that we can transact to Datomic."
  [ast tempid-factory]
  (for [[_ enum] (:interface.ast/enum-map ast)]
    (assoc
      (filter-kv (fn [k _] (contains? datomic-schema-keys k)) enum)
      :db/id (tempid-factory (get enum :db/part :db.part/user)))))

(s/fdef semantic-ast->datomic-field-schemas
        :args (s/cat :ast :interface/ast
                     :tempid-factory tempid-factory-spec)
        :ret (s/coll-of :datomic/field-schema))
(defn- semantic-ast->datomic-field-schemas
  [ast tempid-factory]
  (->> (traverse [:interface.ast/interfaces MAP-VALS :interface.ast.interface/fields MAP-VALS] ast)
       (reduce
         (fn [field-map {:keys [db/ident] :as field-ast}]
           ; distinct because an interface may share the same attribute, as is the case with
           ; :datomic-spec/interfaces
           (if (contains? field-map ident)
             field-map
             (assoc field-map
               ident (as-> field-ast $
                           (filter-kv (fn [k _] (contains? datomic-schema-keys k)) $)
                           (assoc $ :db/id (tempid-factory :db.part/db)
                                    :db.install/_attribute  :db.part/db)))))
         {})
       vals))

(s/fdef semantic-ast->datomic-partition-schemas
        :args (s/cat :ast :interface/ast
                     :tempid-factory tempid-factory-spec)
        :ret (s/coll-of :datomic/partition-schema))
(defn semantic-ast->datomic-partition-schemas
  "Given a semantic AST, generates edn that represents partition datoms to add to the
  Datomic schema. `tempid` will be passed in and will be `datomic.api/tempid`."
  [ast tempid-factory]
  (->> (:interface.ast/enum-map ast)
       vals
       (keep :db/part)
       (filter #(not= :db.part/user %))
       (map (fn [part] {:db/id (tempid-factory :db.part/db)
                        :db/ident part
                        :db.install/_partition :db.part/db}))))

(s/fdef semantic-ast->datomic-schemas
        :args (s/cat :ast :interface/ast
                     :tempid-factory tempid-factory-spec)
        :ret (s/keys :opt [:datomic/field-schema :datomic/partition-schema :datomic/enum-schema]))
(defn semantic-ast->datomic-schemas
  "Given a semantic AST, generates edn that represents attributes to add to
  Datomic schema. `tempid` will be passed in and will be `datomic.api/tempid`."
  [ast tempid-factory]
  {:datomic/field-schema (semantic-ast->datomic-field-schemas ast tempid-factory)
   :datomic/enum-schema (semantic-ast->datomic-enum-schemas ast tempid-factory)
   :datomic/partition-schema (semantic-ast->datomic-partition-schemas ast tempid-factory)})

(defn- validate-semantic-ast
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
                           :field field})))))))

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

(s/def :gen/generator-factory
  (s/fspec :args (s/cat)
           :ret generator?
           :gen #(gen/return
                   (fn [] (gen/return :k/w)))))
(s/def :gen/generator-factory-with-member
  (s/fspec :args (s/cat :member-generator-factory :gen/generator-factory)
           :ret generator?
           :gen #(gen/return (fn [mem-gen-factory]
                   (gen/set (mem-gen-factory) {:min-elements 1
                                               :max-elements 1})))))


(s/def :cljspec/spec s/spec?)
(s/def :dspec/gen-factory (s/fspec :args (s/cat)
                                   :ret generator?))
(s/def :dummy/gen-factory fn?)
(s/fdef interface->clojure-spec&generator
        :args (s/cat :ast :interface/ast
                     :interface-name keyword?
                     :gen-map (s/map-of keyword? :dummy/gen-factory)
                     ;:gen-map :interface/gen-map
                     )
        :ret (s/keys :req-un [:cljspec/spec :dspec/gen-factory]))
(defn- interface->clojure-spec&generator
  "Returns the entity map clojure.spec for the given interface represented by `interface-name`."
  [ast interface-name gen-map]
  (let [interface (get-in ast [:interface.ast/interfaces interface-name])
        all-fields (ast&interface->ast-fields ast interface)
        {req-fields true, opt-fields nil
         :or {req-fields [] opt-fields []}} (group-by :interface.ast.field/required all-fields)
        custom-generator-factory (if-let [custom-generator-factory (get gen-map interface-name)]
                                   custom-generator-factory
                                   (if-let [gen-fields (seq (filter :gen/should-generate all-fields))]
                                     (apply ensure-keys-gen (map :db/ident gen-fields))
                                     nil))

        all-inherited (all-inherited-interface-names ast interface-name)
        all-my-interfaces (conj all-inherited interface-name)

        ; If an ancestor is identified via an existing attribute instead of via the special attribute
        ; :datomic-spec/interfaces, then let's be sure that we do not include the name of that ancestor
        ; in this entity's :datomic-spec/interfaces set of inherited interfaces. This is because:
        ; 1. There will be no Datomic enum in Datomic associated with the name of the ancestor.
        ; 2. We can detect that we share the interface of the ancestor via the same way the ancestor knows
        ;    its interface -- by the existence of an attribute(s) specified by the Datalog query clause in
        ;    , e.g., `:interface.def/identify-via ['[?e :obj/ids-the-ancestral-interface]]`.
        all-my-self-labeling-interfaces (set (filter #(contains? (:interface.ast/enum-map ast) %) all-my-interfaces))
        inherited-custom-generators? (seq (keep gen-map all-inherited))
        identify-via-datomic-spec-interfaces? (some #(= (:db/ident %) :datomic-spec/interfaces) req-fields)

        ; We use distinct here because we could have multiple :datomic-spec/interfaces attributes that we inherit
        req-keys (conj (vec (distinct (map :db/ident req-fields))) :db/id)
        opt-keys (mapv :db/ident opt-fields)

        base-spec (eval `(s/keys :req ~req-keys :opt ~opt-keys))

        ; We remove :datomic-spec/interfaces, so we don't un-necessarily generate them randomly, before we
        ; over-write them with our own deterministic set of interface keywords.
        base-gen-spec (eval `(s/keys
                               :req ~(vec (remove #(= % :datomic-spec/interfaces) req-keys))
                               :opt ~opt-keys))
        base-gen-spec-factory (constantly base-gen-spec)
        spec (if (and identify-via-datomic-spec-interfaces? (seq all-my-self-labeling-interfaces))
               (s/and base-spec #(clojure.set/subset? all-my-self-labeling-interfaces (:datomic-spec/interfaces %)))
               base-spec)
        spec-factory (constantly spec)
        gen-factory (if (and (not inherited-custom-generators?)
                             (not identify-via-datomic-spec-interfaces?)
                             (not custom-generator-factory))
                      #(s/gen spec)
                      (if-not (or identify-via-datomic-spec-interfaces? inherited-custom-generators?)
                        #(custom-generator-factory spec-factory)
                        #(gen/fmap
                           (if identify-via-datomic-spec-interfaces?
                              (fn [objects-to-combine]
                                 (apply merge (conj objects-to-combine
                                                    {:datomic-spec/interfaces all-my-self-labeling-interfaces})))
                              (partial apply merge))
                           (let [generators (cond-> (map (fn [x] (s/gen x)) all-inherited)
                                                    custom-generator-factory (conj (custom-generator-factory base-gen-spec-factory))
                                                    (not custom-generator-factory) (conj (s/gen base-gen-spec)))]
                             (apply gen/tuple generators)))))]
    {:spec spec
     :gen-factory gen-factory}))

(def NATIVE-TYPES
  #{:keyword :string :boolean :long :bigint :float :double :bigdec :instant :uuid :uri :bytes})

(defn- interface-type?
  [type]
  (not (contains? (conj NATIVE-TYPES :enum) type)))

(s/fdef add-field-refs-to-deps-graph
        :args (s/cat :interface-name keyword?
                     :fields (s/coll-of :interface.ast/field :kind set?)
                     :deps-graph :clojure.spec/deps-graph)
        :ret :clojure.spec/deps-graph)
(defn- add-field-refs-to-deps-graph
  [interface-name fields deps-graph]
  (reduce
    (fn [deps-graph {field-name :db/ident, :keys [interface.ast.field/type]}]
      (if (and (interface-type? type)
               (not= interface-name type)
               ; Avoid circular dependency
               (not (ssdep/depends? deps-graph type field-name)))
        (ssdep/depend deps-graph field-name type)
        deps-graph))
    deps-graph
    fields))

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
        dependencies (concat inherits field-deps)
        deps-graph' (add-field-refs-to-deps-graph name all-fields deps-graph)]
    (reduce #(if (ssdep/depends? %1 %2 name)
              %1
              (ssdep/depend %1 name %2))
            deps-graph'
            dependencies)))

(s/fdef field->clojure-specs
        :args (s/cat :field :interface.ast/field
                     :custom-generator-factory (s/alt :nil nil?
                                                      :func (s/alt :arity-0 :gen/generator-factory
                                                                   :arity-1 :gen/generator-factory-with-member)))
        :ret any?)
(defn- field->clojure-specs
  "Returns the clojure.spec for the given `field`, with an optional `custom-generator-factory`"
  [field custom-generator-factory]
  (let [{:db/keys [ident cardinality]
         :interface.ast.field/keys [type possible-enum-vals]} field
        single-predicate (if possible-enum-vals
                           possible-enum-vals
                           (get ast-field-type->predicate type type))
        predicate (if (= cardinality :db.cardinality/many)
                    (s/coll-of single-predicate :kind set?)
                    single-predicate)
        generator-factory (if (and custom-generator-factory (= 1 (arity custom-generator-factory)))
                            (let [member-gen-factory (if (= :string type)
                                                       #(gen/not-empty (gen/string-alphanumeric))
                                                       #(s/gen single-predicate))]
                              #(custom-generator-factory member-gen-factory))
                            (if (and (= :string type) (nil? custom-generator-factory))
                              (case cardinality :db.cardinality/one #(gen/not-empty (gen/string-alphanumeric))
                                                :db.cardinality/many #(gen/set (gen/not-empty (gen/string-alphanumeric))))
                              custom-generator-factory))]
    (if generator-factory
      (s/with-gen predicate generator-factory)
      predicate)))

(s/fdef interface->clojure-specs
        :args (s/cat :ast :interface/ast
                     :ast/interface :interface.ast/interface
                     :gen-map (s/map-of keyword? :dummy/gen-factory)
                     ;:gen-map :interface/gen-map
                     )
        :ret (s/map-of keyword? any?))
(defn- interface->clojure-specs
  "Returns the clojure.spec macros that will register this interface and its respective fields as clojure.spec's."
  [ast interface gen-map]
  (let [{:keys [interface.ast.interface/name]} interface
        all-fields (ast&interface->ast-fields ast interface)
        {:keys [spec gen-factory]} (interface->clojure-spec&generator ast name gen-map)
        specs-by-name {name (s/with-gen spec
                                        (fn []
                                          (gen/such-that
                                            #(< 1 (count (keys %)))
                                            (gen-factory))))}]
    (->> all-fields
         (filter #(not= (:db/ident %) :datomic-spec/interfaces))
         (reduce (fn [specs-by-name {:keys [db/ident] :as field}]
                   (assoc specs-by-name ident (field->clojure-specs field (get gen-map ident))))
                 specs-by-name))))

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

(s/fdef ast->clojure-specs
        :args (s/cat :ast :interface/ast
                     :gen-map (s/map-of keyword? :dummy/gen-factory)
                     ;:gen-map :interface/gen-map
                     )
        :ret :clojure.spec/macros)
(defn ast->clojure-specs
  "Given the intermediate AST and a mapping of field and interface names to test.check generators, this returns
  the clojure.spec macros that we can expand and evaluate later."
  [{:keys [interface.ast/interfaces] :as ast} gen-map]
  (reduce
    (fn [specs interface]
      (merge specs (interface->clojure-specs ast interface gen-map)))
    {:datomic-spec/interfaces (s/coll-of keyword? :kind set?)}
    (vals interfaces)))

(s/fdef validate-generators-for-likely-such-that-violations!
        :args (s/cat :ast :interface/ast
                     :gen-map (s/map-of keyword? :dummy/gen-factory)
                     ;:gen-map :interface/gen-map
                     :sorted-deps (s/coll-of keyword?)
                     :desired-confidence (s/and integer? pos?)))
(defn- validate-generators-for-likely-such-that-violations!
  "Our specs are defined implicitly with gen/such-that. We may end up passing in custom generators
  or defining interfaces that end up generating data that violate the implicit gen/such-that
  predicates. This function makes it easier to understand what might be causing the gen/such-that
  violation. Otherwise, we would have no hints as to what might be causing the violations."
  [ast gen-map sorted-deps desired-confidence]
  (let [interface-name->spec&generator (reduce #(assoc %1 %2 (interface->clojure-spec&generator ast %2 gen-map))
                                               {} (keys (:interface.ast/interfaces ast)))]
    (doseq [spec-name sorted-deps]
      (if-let [{:keys [spec gen-factory]} (interface-name->spec&generator spec-name)]
        (let [data (gen/sample (gen-factory) desired-confidence)]
          (doseq [datum data]
            (when-not (s/valid? spec datum)
              (throw (ex-info (str "Spec " spec-name
                                   " and/or its custom generators are defined in such a way that it is possible to generate data such that it does not satisfy the spec.\n"
                                   (s/explain-str spec datum))
                              {:spec-name spec-name
                               :datum datum
                               :explain-data (s/explain-data spec datum)})))))))))

(s/fdef validate-generators-for-likely-unique-violations!
        :args (s/cat :ast :interface/ast
                     :gen-map (s/map-of keyword? :dummy/gen-factory)
                     ;:gen-map :interface/gen-map
                     ))
(defn- validate-generators-for-likely-unique-violations!
  "When we gen/sample a lot of data, we may run the risk of creating two entities with the same value for a :db.unique/identity
  or :db.unique/value field. In both cases, when we attempt to write the resulting data as datoms to Datomic, we will end up with
  either:
  - A :db.error/datoms-conflict exception in the case of two entities with the same :db.unique/identity value trying to write more than
    one value to a :db.cardinality/one field.
  - A unique value exception because we are trying to write two entities with the same value to a field that is :db.unique/value.

  This validation function throws an exception to suggest that a custom generator should be provided
  (via `(register-specs-for-ast ast custom-generators datomic/tempid db-id?)`) for a field that is either `:db.unique/identity` or
  `:db.unique/value`."
  [ast gen-map]
  (let [unique-fields (->> (select [:interface.ast/interfaces
                                    MAP-VALS
                                    :interface.ast.interface/fields
                                    MAP-VALS
                                    (collect-one :db/ident) (collect-one :interface.ast.field/type)
                                    :db/unique] ast)
                           ; The result of this (select ...) will look like
                           ; `([:some/attribute :uuid :db.unique/ident] [:person/first-name :string nil])`

                           (filter (fn [[_ field-type unique-trait]]
                                     (and (s/valid? :db/unique unique-trait)
                                          (not= :uuid field-type))))
                           ; We only want to consider fields that are flagged
                           ; with `:db.unique/identity` or `:db.unique/value`.
                           ; We don't need to consider `:uuid` fields because
                           ; the default generator likely won't produce
                           ; conflicting pairs of identical uuid values.

                           (map first)
                           ; Let's consider only the names of the fields
                           )]
    (doseq [field-name unique-fields]
      (if-not (contains? gen-map field-name)
        (throw (ex-info (str "You should provide a custom generator to the unique attribute " (str field-name))
                        {:field (select-first
                                  [:interface.ast/interfaces MAP-VALS :interface.ast.interface/fields field-name]
                                  ast)}))))))

(s/fdef validate-generators!
        :args (s/cat :ast :interface/ast
                     :gen-map (s/map-of keyword? :dummy/gen-factory)
                     ;:gen-map :interface/gen-map
                     :sorted-deps (s/coll-of keyword?)))
(defn- validate-generators!
  [ast gen-map sorted-deps]
  (validate-generators-for-likely-such-that-violations! ast gen-map sorted-deps 50)
  (validate-generators-for-likely-unique-violations! ast gen-map))

;(s/fdef register-specs-for-ast-with-custom-generators!
;        :args (s/cat :ast :interface/ast
;                     ;:gen-map :interface/gen-map
;                     :gen-map (s/map-of keyword? fn?)
;                     :tempid-factory fn?
;                     ;:tempid-factory tempid-factory-spec
;;                     :db-id? any?
;                     :db-id? (s/fspec :args (s/cat :x any?)
;                                      :ret boolean?)
;                     )
;        :ret any?)
(defn register-specs-for-ast-with-custom-generators!
  "Given an entire AST and maybe some custom generators for fields and/or interfaces, register all clojure.spec specs
  that should be associated with the AST and possible custom generators."
  [ast gen-map tempid-factory db-id?]
  (let [
        ;{arity1-gen-factories 1
        ; arity0-gen-factories 0} (group-by (fn [[_ generator]] (arity generator)) gen-map)
        ;_ (s/assert :gen/generator-factory-with-member arity1-gen-factories)
        ;_ (s/assert :gen/generator-factory arity0-gen-factories)
        ]
    (let [sorted-deps (-> ast deps-graph-for-ast ssdep/topo-sort)
          specs (ast->clojure-specs ast gen-map)]
       (s/def :db/id
         (s/with-gen db-id? (fn->gen #(tempid-factory :db.part/user))))
       (doseq [spec-name sorted-deps]
         (eval `(s/def ~spec-name ~(get specs spec-name))))
       (if-not (empty? gen-map)
         (validate-generators! ast gen-map sorted-deps))
       ;~@(map
       ;    (fn [spec-name]
       ;      `(s/def ~spec-name (get ~'specs ~spec-name)))
       ;    sorted-deps)
       ;(if-not (empty? ~gen-map)
       ;  (validate-generators! ~ast ~gen-map ~sorted-deps))
       )))

(s/fdef ast&interface->identifying-datalog-clauses
        :args (s/cat :ast :interface/ast
                     :interface-name keyword?)
        :ret (s/coll-of :datalog/clause :kind vector?))
(defn ast&interface->identifying-datalog-clauses
  [ast interface-name]
  (-> ast
      :interface.ast/interfaces
      interface-name
      :interface.ast.interface/identify-via))

(s/fdef register-specs-for-ast!
        :args (s/cat :ast :interface/ast
                     :tempid-factory fn?
                     ;:tempid-factory tempid-factory-spec
                     ;:db-id? fn?
                     :db-id? (s/fspec :args (s/cat :x any?)
                                      :ret boolean?)
                     ))
(defn register-specs-for-ast!
  "Given an entire AST for fields and/or interfaces, register all clojure.spec specs
  that should be associated with the AST"
  [ast tempid-factory db-id?]
  (register-specs-for-ast-with-custom-generators! ast {} tempid-factory db-id?))

(s/fdef entity->datoms
        :args (s/cat :entity (s/keys :req [:db/id]))
        :ret (s/coll-of (s/tuple :db/id keyword? any?)))
(defn- entity->datoms
  "Given an entity represented as a map, convert it to a representation that is a
  sequence of datoms of the form [entity id, attribute, value]"
  [{:keys [db/id] :as entity}]
  (reduce (fn [datoms [k v]]
            (if (coll? v)
              (into datoms (map (fn [member] [id k member]) v))
              (conj datoms [id k v])))
          '()
          (dissoc entity :db/id)))

(s/fdef entity->interfaces
        :args (s/cat :ast :interface/ast
                     :entity (s/keys :req [:db/id])
                     :datomic-q :datomic.api/q)
        :ret (s/coll-of keyword? :kind set?))
(defn entity->interfaces
  "Given an `entity` and our `ast`, return the set of interfaces this entity satisfies."
  [ast entity datomic-q]
  (let [datoms (entity->datoms entity)
        interfaces (-> ast :interface.ast/interfaces vals)]
    (->> interfaces
         (filter
           (fn [{:interface.ast.interface/keys [identify-via name]}]
             (and (not (nil? (datomic-q {:find '[?e .] :where identify-via} datoms)))
                  (s/valid? name entity))))
         (map :interface.ast.interface/name)
         set)))

(s/fdef satisfies-interface?
        :args (s/cat :ast :interface/ast
                     :interface-name keyword?
                     :entity (s/map-of keyword? any?)
                     :datomic-q :datomic.api/q)
        :ret boolean?)
(defn satisfies-interface?
  "Returns true if entity satisfies a data interface named `interface-name` defined in `ast`."
  [ast interface-name entity datomic-q]
  (let [datoms (entity->datoms entity)
        {:keys [interface.ast.interface/identify-via]} (-> ast :interface.ast/interfaces interface-name)]
    (and (some? (datomic-q {:find '[?e .] :where identify-via} datoms))
         (s/valid? interface-name entity))))

(s/fdef eid-satisfies-interface?
        :args (s/cat :ast :interface/ast
                     :interface-name keyword?
                     :eid :db/id
                     :datomic-q :datomic.api/q
                     ; TODO Get more specific than fn?
                     :datomic-filter fn?
                     :db any?)
        :ret boolean?)
(defn eid-satisfies-interface?
  "Like satisfies-interface? but works on an entity id instead of an entity.
  Returns true if the entity represented by the entity id `eid` satisfies
  the interface `interface-name`."
  [ast interface-name eid datomic-q datomic-filter db]
  (let [{:keys [interface.ast.interface/identify-via]} (-> ast :interface.ast/interfaces interface-name)]
    (some? (datomic-q {:find '[?e .]
                       :where identify-via}
                      (datomic-filter db (fn [_ datom]
                                           (= eid (.e datom))))))))

(s/fdef identify-via-clauses-for
        :args (s/cat :ast :interface/ast
                     :eid-symbol symbol?
                     :interface-names (s/+ keyword?))
        :ret (s/coll-of :datalog/clause))
(defn identify-via-clauses-for
  [ast eid-symbol & interface-names]
  (mapcat
    (fn [interface-name]
      (->> ast
           :interface.ast/interfaces
           interface-name
           :interface.ast.interface/identify-via
           (clojure.walk/postwalk #(if (= '?e %) eid-symbol %))))
    interface-names))
