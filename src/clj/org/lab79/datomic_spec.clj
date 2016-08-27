(ns org.lab79.datomic-spec
  (:require [datomic.api :as d]
            [org.lab79.datomic-spec.gen :refer [ensure-keys-gen]]
            [clojure.core.match :refer [match]]
            [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clojure.spec.test :as stest]
            [clojure.test :refer [function?]]
            [com.stuartsierra.dependency :as ssdep]
            [org.lab79.datomic-spec.gen :refer [fn->gen]]
            [com.rpl.specter :as sp]
            [com.rpl.specter.macros
             :refer [paramsfn defprotocolpath defnav extend-protocolpath
                     nav declarepath providepath select select-one select-one!
                     select-first transform setval replace-in defnavconstructor
                     select-any selected-any? collected? traverse
                     multi-transform]]
            [clojure.pprint :refer [pprint]])
  (:import (java.util Date)
           (datomic.db DbId)))

(defn- arity [f]
  {:pre [(instance? clojure.lang.AFunction f)]}
  (-> f class .getDeclaredMethods first .getParameterTypes alength))

;
; Datomic clojure.spec
;

(def db-id? #(or (integer? %)
                 (instance? datomic.db.DbId %)))

(s/def :db/id
  (s/with-gen db-id? (fn->gen #(d/tempid :db.part/user))))
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
  (s/keys :req [:interface.def/name :interface.def/fields]
          :opt [:interface.def/inherits]))
(s/def :interface.def/name keyword?)
(s/def :interface.def/fields (s/coll-of :interface.def/field :kind set?))
(s/def :interface.def/fields (s/map-of keyword? :interface.def/field))
(s/def :interface.def/field
  (s/cat :field-tags (s/+ :interface.def.field/trait)))
(s/def :interface.def/inherits (s/coll-of keyword? :kind vector?))
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
(s/def :interface/field-def-parser
  (s/keys :req [:interface.ast/field :interface.ast/enum-map]))
(s/fdef parse-field-def
        :args (s/cat :field-def (s/spec :interface.def/field) :field-name keyword?)
        :ret (s/keys :req [:interface.ast/field :interface.ast/enum-map]))
(stest/instrument `parse-field-def)

(defn semantic-spec->semantic-ast
  "Converts a user-readability-optimized semantic schema spec into a AST that can be used to generate Datomic
  schemas, generate test data, etc."
  [spec]
  (let [{:interface.def/keys [name fields inherits]} spec
        {:keys [interface-fields enum-map]} (reduce
                                              (fn [parsed [field-name field-spec]]
                                                (let [{:interface.ast/keys [enum-map field]} (parse-field-def field-spec field-name)]
                                                  (-> parsed
                                                      (update :enum-map merge enum-map)
                                                      (assoc-in [:interface-fields field-name] field))))
                                              {:interface-fields {} :enum-map {}}
                                              fields)]
    {:interface.ast/interfaces {name {:interface.ast.interface/name name
                                      :interface.ast.interface/fields interface-fields
                                      :interface.ast.interface/inherits (into #{} inherits)}}
     :interface.ast/enum-map enum-map}))
(s/fdef semantic-spec->semantic-ast
         :args (s/cat :spec :interface/def)
         :ret :interface/ast)
(stest/instrument `semantic-spec->semantic-ast)

(defn- only-db-keys
  [m]
  (into {} (filter
             #(let [key (first %)
                    kw (if (keyword? key)
                         key
                         (:k key))]
               (re-matches #"^db" (namespace kw)))
             m)))
(s/fdef only-db-keys
        :args (s/cat :map (s/map-of keyword? any?))
        :ret (s/map-of keyword? any?))
(stest/instrument `only-db-keys)

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
(def tempid-factory-spec (s/fspec :args (s/cat :partition keyword? :num (s/? number?))
                                  :ret db-id?))
(s/fdef semantic-ast->datomic-schemas
        ; TODO Added this during oss
         :args (s/cat :ast :interface/ast
                      :tempid-factory tempid-factory-spec)
         :ret (s/coll-of :datomic/schema :kind vector?))
(stest/instrument `semantic-ast->datomic-schemas)

(defn semantic-spec->datomic-schemas
  "Given a semantic spec, generates edn that represents attributes to add to Datomic schema"
  [spec tempid-factory]
  (-> spec
      semantic-spec->semantic-ast
      (semantic-ast->datomic-schemas tempid-factory)))
(s/fdef semantic-spec->datomic-schemas
         :args (s/cat :spec :interface/def
                      :tempid-factory tempid-factory-spec)
         :ret (s/+ :datomic/schema))
(stest/instrument `semantic-spec->datomic-schemas)

(defn validate-semantic-ast
  [ast]
  (let [enum-vals (set (select [:interface.ast/enum-map sp/MAP-VALS :db/ident] ast))
        interface-names (set (select [:interface.ast/interfaces sp/MAP-VALS :interface.ast.interface/name] ast))
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

(defn semantic-spec-coll->semantic-ast
  "Given a collection of semantic specs, generates a semantic ast"
  [specs]
  (let [asts (map semantic-spec->semantic-ast specs)
        combined-ast (apply merge-with merge asts)]
    (validate-semantic-ast combined-ast)
    combined-ast))
(s/fdef semantic-spec-coll->semantic-ast
         :args (s/cat :specs (s/spec (s/+ :interface/def)))
         :ret :interface/ast)
(stest/instrument `semantic-spec-coll->semantic-ast)

; TODO Rename
(defn semantic-spec-coll->datomic-schemas
  "Given a collection of semantic specs, generates edn that represents attributes to add to Datomic schema"
  [specs tempid-factory]
  (-> specs
      semantic-spec-coll->semantic-ast
      (semantic-ast->datomic-schemas tempid-factory)))
(s/fdef semantic-spec-coll->datomic-schemas
         :args (s/cat :specs (s/spec (s/+ :interface/def))
                      :tempid-factory tempid-factory-spec)
         :ret (s/+ :datomic/schema))
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
    :post [(s/valid? (s/or :fn function? :kw keyword?) %)]
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

(defn extract-deps-graph&macros-for-field
  "Incorporates any clojure.spec dependencies this field has, in addition the macro to run later
  that will register this field as a clojure.spec."
  [field gen-map deps-graph]
  (let [{:keys [; required
                db/ident
                interface.ast.field/type
                db/cardinality
                ; optional
                interface.ast.field/possible-enum-vals
                ; TODO Anything we can do to test uniqueness in predicates returned by ast-field-type->predicate (?)
                db/unique]} field
        pred (if possible-enum-vals
               (ast-field-type->predicate type possible-enum-vals)
               (ast-field-type->predicate type))
        deps-graph' (if (and (keyword? pred) (not= ident pred))
                      (ssdep/depend deps-graph ident pred)
                      deps-graph)]
    {:clojure.spec/deps-graph deps-graph'
     :clojure.spec/macros
        {ident (let [custom-generator-factory (get gen-map ident)]
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
                       `(s/def ~ident ~pred)))))}}))
(s/fdef extract-deps-graph&macros-for-field
        :args (s/cat :ast/field :interface.ast/field
                     :gen-map :interface/gen-map
                     :deps-graph :clojure.spec/deps-graph)
        :ret (s/keys :clojure.spec/deps-graph
                     :clojure.spec/macros))
(stest/instrument `extract-deps-graph&macros-for-field)

(defn- ast&interface->ast-fields
  "Returns the set of all fields that represent an interface."
  [ast interface]
  (let [{:keys [interface.ast/interfaces]} ast
        {:interface.ast.interface/keys [fields inherits]} interface
        inherited-fields (->> inherits
                              (map interfaces)
                              (reduce (fn [inherited-fields interface]
                                        (into inherited-fields (ast&interface->ast-fields ast interface)))
                                      #{}))
        fields (into #{} (vals fields))
        all-fields (into fields inherited-fields)]
    all-fields))
(s/fdef ast&interface->ast-fields
        :args (s/cat :ast :interface/ast
                     :ast/interface :interface.ast/interface)
        :ret (s/coll-of :interface.ast/field :kind set?))
(stest/instrument `ast&interface->ast-fields)

(defn- all-inherited-interface-names
  "Returns the union of all immediately inherited and recursively inherited interfaces
  for a given interface represented by `interface-name`."
  [ast interface-name]
  (let [interface (-> ast :interface.ast/interfaces interface-name)
        {:keys [interface.ast.interface/inherits]} interface]
    (into inherits (mapcat #(all-inherited-interface-names ast %) inherits))))

(defn self&inherited-interface-gen
  ; TODO Add docstring
  "Returns the entity map spec for the given interface represented by `interface-name`."
  ([ast interface-name req-fields opt-fields gen-map]
    (let [all-inherited (all-inherited-interface-names ast interface-name)
          inherited-custom-generators? (->> all-inherited
                                           (map gen-map)
                                           (filter (comp not nil?))
                                           seq)]
      (if inherited-custom-generators?
        `(s/with-gen (s/keys
                       :req ~(conj (mapv :db/ident req-fields) :db/id)
                       :opt ~(mapv :db/ident opt-fields))
                     (fn []
                       (gen/fmap
                         #(apply merge %)
                         ~(cons 'clojure.spec.gen/tuple (map (fn [x] `(s/gen ~x)) all-inherited)))))
        `(s/keys
           :req ~(conj (mapv :db/ident req-fields) :db/id)
           :opt ~(mapv :db/ident opt-fields)))))
  ([ast interface-name req-fields opt-fields gen-map custom-generator-factory]
   (let [all-inherited (all-inherited-interface-names ast interface-name)
         inherited-custom-generators? (->> all-inherited
                                           (map gen-map)
                                           (filter (comp not nil?))
                                           seq)]
     (if inherited-custom-generators?
       `(s/with-gen (s/keys
                      :req ~(conj (mapv :db/ident req-fields) :db/id)
                      :opt ~(mapv :db/ident opt-fields))
                    (fn []
                      (gen/fmap
                        (fn [~'x] (apply merge ~'x))
                        ~(cons 'clojure.spec.gen/tuple
                               (cons (custom-generator-factory `(s/keys
                                                                  :req ~(conj (mapv :db/ident req-fields) :db/id)
                                                                  :opt ~(mapv :db/ident opt-fields)))
                                     (map (fn [x] `(s/gen ~x)) all-inherited))))))
       `(s/with-gen (s/keys
                      :req ~(conj (mapv :db/ident req-fields) :db/id)
                      :opt ~(mapv :db/ident opt-fields))
                    (fn []
                      ~(custom-generator-factory `(s/keys
                                                    :req ~(conj (mapv :db/ident req-fields) :db/id)
                                                    :opt ~(mapv :db/ident opt-fields)))))))))

(defn extract-deps-graph&macros-for-interface
  "Incorporates any clojure.spec dependencies this interface has, in addition the macros to run later
  that will register this interface and its respective fields as a clojure.spec."
  [ast interface gen-map deps-graph]
  (let [{:interface.ast.interface/keys [name inherits]} interface
        all-fields (ast&interface->ast-fields ast interface)]
    (reduce
      (fn [{:keys [clojure.spec/deps-graph clojure.spec/macros]} field]
        (let [{field-macros :clojure.spec/macros
               :keys [clojure.spec/deps-graph]} (extract-deps-graph&macros-for-field field gen-map deps-graph)]
          {:clojure.spec/deps-graph (ssdep/depend deps-graph name (:db/ident field))
           :clojure.spec/macros (merge macros field-macros)}))
      {:clojure.spec/deps-graph (reduce
                                  (fn [deps-graph inherited-interface-name]
                                    (ssdep/depend deps-graph name inherited-interface-name))
                                  deps-graph
                                  inherits)
       :clojure.spec/macros
          {name (let [{req-fields true
                       opt-fields false
                       :or {req-fields []
                            ; TODO Try without contains? Use nil insteadof false for opt-fields
                            opt-fields []}} (group-by #(contains? % :interface.ast.field/required) all-fields)
                      {gen-fields true} (group-by :gen/should-generate all-fields)]
                  (if-let [custom-generator-factory (get gen-map name)]
                    `(s/def ~name ~(self&inherited-interface-gen ast name req-fields opt-fields gen-map custom-generator-factory))
                    (if (seq gen-fields)
                      `(s/def ~name ~(self&inherited-interface-gen ast name req-fields opt-fields {} (apply ensure-keys-gen (map :db/ident gen-fields))))
                      `(s/def ~name ~(self&inherited-interface-gen ast name req-fields opt-fields gen-map)))))}}
      all-fields)))
(s/fdef extract-deps-graph&macros-for-interface
        :args (s/cat :ast :interface/ast
                     :ast/interface :interface.ast/interface
                     :gen-map :interface/gen-map
                     :deps-graph :clojure.spec/deps-graph)
        :ret (s/keys :clojure.spec/deps-graph
                     :clojure.spec/macros))
(stest/instrument `extract-deps-graph&macros-for-interface)

(defn extract-deps-graph&macros-for-ast
  [ast gen-map]
  (let [{:keys [interface.ast/interfaces]} ast]
    (reduce
      (fn [{:clojure.spec/keys [deps-graph macros]} interface]
        (let [{interface-macros :clojure.spec/macros
               :keys [clojure.spec/deps-graph]} (extract-deps-graph&macros-for-interface
                                                  ast interface gen-map deps-graph)]
          {:clojure.spec/macros (merge macros interface-macros)
           :clojure.spec/deps-graph deps-graph}))
      {:clojure.spec/deps-graph (ssdep/graph)
       :clojure.spec/macros {}}
      (vals interfaces))))
(s/fdef extract-deps-graph&macros-for-ast
        :args (s/cat :ast :interface/ast
                     :gen-map :interface/gen-map)
        :ret (s/keys :clojure.spec/deps-graph
                     :clojure.spec/macros))
(stest/instrument `extract-deps-graph&macros-for-ast)

(defn register-generative-specs-for-ast!
  "Given an entire interface AST and some custom generators for some fields,
  register all clojure.spec specs that should be associated with the AST."
  [ast gen-map]
  (let [{:clojure.spec/keys [macros deps-graph]} (extract-deps-graph&macros-for-ast ast gen-map)]
    (doseq [spec-name (ssdep/topo-sort deps-graph)]
      (eval (macroexpand (macros spec-name))))))
(s/fdef register-generative-specs-for-ast!
        :args (s/cat :ast :interface/ast
                     :gen-map :interface/gen-map))
(stest/instrument `register-generative-specs-for-ast!)

(defn register-specs-for-ast!
  "Given an entire interface AST, register all clojure.spec specs that
  should be associated with the AST."
  [ast]
  (register-generative-specs-for-ast! ast {}))
(s/fdef register-specs-for-ast!
        :args (s/cat :ast :interface/ast))
(stest/instrument `register-specs-for-ast!)
