# dspec

A Clojure library designed to add useful semantics on top of Datomic.

[![Build Status](https://travis-ci.org/lab-79/dspec.svg?branch=master)](https://travis-ci.org/lab-79/dspec)
[![Clojars Project](https://img.shields.io/clojars/v/lab79/dspec.svg)](https://clojars.org/lab79/dspec)
[![codecov](https://codecov.io/gh/lab-79/dspec/branch/master/graph/badge.svg)](https://codecov.io/gh/lab-79/dspec)

## Purpose

Adding additional semantics -- on top of those provided by Datomic --
helps us achieve the following:

1. Documentation for engineers. Document the association of a collection
   of attributes with a particular entity "type" (or "types") -- an association
   that is captured implictly via Datomic queries that do not explicitly by
   themselves make it easy for a developer to look in one place to understand
   the different kinds of entities and data in the project.
2. Eliminate duplication. From just one definition, we can:
   1. Generate the datoms defining the Datomic schema.
   2. Create corresponding `clojure.spec` definitions, that we can leverage for:
      1. Validating data.
      2. Generating sample or test data.

## Goals

This library will help unify the following goals, from one central schema
specification.

1. (Done) Provide a more readable way to read a Datomic schema and understand
   how it is being used in an app.
2. (Done) Generate schema edn for Datomic schema specifications.
3. (Done) Generate `clojure.spec` spec definitions.
4. Provide easy, extensible validation tagging (e.g., `:required`).
5. (Done) Generate fake dev data or test data, by providing additional
   semantics (e.g., what entity types can map to what other entity types via
   Datomic `ref`s).
6. Integrates with a nice way to add or retract from a Datomic db schema when
   these semantic schema definitions change.
7. (Done) Make it easy to extract specific *kinds* of entities.
8. (Done) Conveniently detect what interfaces an entity satisfies.
9. (Done) Support polymorphism.
10. Small code base.
11. Simple design.

## Getting started

Add the following dependency to your `project.clj` file:

[![Clojars Project](https://clojars.org/lab79/dspec/latest-version.svg)](https://clojars.org/lab79/dspec)


## Usage

We adopt a declarative edn specification that we call "interfaces" for defining
stronger semantics on top of Datomic schemas.

As a developer using this library, most of your application of this library
will be to convert some collection of data interfaces to:

1. To Datomic schema attributes that will be added to the Datomic schema.
2. To validation output that specifies if a vector of interfaces is invalid or
   in what ways they are invalid.
3. To `clojure.spec` definitions that we can use to check if a Datomic entity map
   is invalid and in what ways it is invalid.

Architecturally, we convert all data interfaces into a single intermediate AST
representing our entire world of interfaces, enums, and their relationships
with each other (inheritance, references, etc.). Then our library converts this
AST to either Datomic schemas, generated app data, or validation output that we
mentioned above.

### Defining dspec interfaces

In `dspec`, our semantic definitions are defined as data.

```clojure
(def user-spec
  {:interface.def/name :interface/user
   :interface.def/fields
     {:user/username [:string "A user's username"
                             :unique/identity ; can also be :unique/value
                             :required]
      :taggable/tags [[:string] "Tags for an entity"]
      :user/registeredAt [:instant "When a user registered"]}
   :interface.def/identify-via ['[?e :user/username]]})
```

This defines a data interface named `:interface/user` such that an entity that
"satisfies" the interface has the following attributes:

- A string attribute named `:user/username` that is required and uniquely
identifies a given user.
- An attribute named `:taggable/tags` that is a vector of strings. This
corresponds to a Datomic cardinality of `:db.cardinality/many` and is simply
specified by placing the member type inside a vector.
- An attribute named `:user/registeredAt` that is of type instant.

It also specified how we can recognize this entity as implementing the
`:interface/user` interface -- in this case because entities of this kind
of interface have the attribute `:user/username` (indicated by
`:interface.def/identify-via`).

Notice how we can inline docstrings for each attribute. This helps us
understand what each attribute is in a semantic definition. And as we will see
later, it also maps to the `:db/doc` field of a Datomic attribute.

#### Primitive types

The possible types we can specify for an attribute are:

- `:keyword` (or `[:keyword]`)
- `:string` (or `[:string]`)
- `:boolean` (or `[:boolean]`)
- `:long` (or `[:long]`)
- `:bigint` (or `[:bigint]`)
- `:float` (or `[:float]`)
- `:double` (or `[:double]`)
- `:bigdec` (or `[:bigdec]`)
- `:instant` (or `[:instant]`)
- `:uuid` (or `[:uuid]`)
- `:uri` (or `[:uri]`)
- `:bytes` (or `[:bytes]`)

A type signature in vector form (e.g., `[:string]`) implies a "has many"
field -- or in other words, a field that can have multiple members, i.e.,
`:db.cardinality/many`.

#### Enum Types

An attribute can also take on an enumerated set of values.

```clojure
{:interface.def/name :interface/person
 :interface.def/fields
   {:person/gender ["A person's gender"
                    :enum {:person.gender/male "Male"
                           :person.gender/female  "Female"
                           :person.gender/other "Other"}]}
 :interface.def/identify-via :datomic-spec/interfaces
 :interface.def/identifying-enum-part :db.part/user}

; or
{:interface.def/name :person
 :interface.def/fields
   {:person/gender [:enum #{:person.gender/male
                            :person.gender/female
                            :person.gender/other]
                            "A person's gender"}}
 :interface.def/identify-via :datomic-spec/interfaces
 :interface.def/identifying-enum-part :db.part/user}
```

The type is specified by the keyword `:enum` immediately followed by either a
set of enumerated values or a map of enumerated values and their corresponding
docstrings.

#### Special Field Flags

There are a number of special flags that we can include in our field definitions.
For example, if we want to specify a field as one that must be required, we can add
the `:required` flag to the field definition:

```clojure
{:interface.def/name :interface/user
 :interface.def/fields
   {:user/username [:string "A user's username" :required]
    :user/password [:string "A user's hashed password" :required]}
 :interface.def/identify-via ['[?e :user/username]]}
```

The field flags that come pre-packaged with `dspec` include:

- `:required`
  Means the field should be non-nil. It will generate a `clojure.spec` definition
  that requires this particular attribute as a key in the map.
- `:db.unique/value`
  Results in Datomic schema generation of a Datomic attribute that has
  `:db.unique/value` set to `true`.
- `:db.unique/identity`
  Results in Datomic schema generation of a Datomic attribute that has
  `:db.unique/identity` set to `true`.
- `:db/isComponent`
  Results in Datomic schema generation of a Datomic attribute that has
  `:db/isComponent` set to `true`.
- `:db/index`
  Results in Datomic schema generation of a Datomic attribute that has
  `:db/index` set to `true`.
- `:db/noHistory`
  Results in Datomic schema generation of a Datomic attribute that has
  `:db/noHistory` set to `true`.
- `:db/fulltext`
  Results in Datomic schema generation of a Datomic attribute that has
  `:db/fulltext` set to `true`.
- `:gen/should-generate`
  Forces the field to be generated -- by, e.g. `clojure.spec.gen/generate`
  or `clojure.spec.gen/sample` -- even if the field is not `:required`.

#### Relationships between interfaces

In Datomic, a `:db.type/ref` entity can refer to any other entity. Usually, we want
to define more constraints about the kinds of relationships an entity can have with
other "kinds" of entities.

We offer a way to specify how entities map to each other.

Take the example of a person having many names (e.g., maiden, married, etc.).
Here is how we can represent that succinctly:

```clojure
[{:interface.def/name :interface/person
  :interface.def/fields
    {:person.id/ssn [:string "A person's ssn number"
                             :db.unique/value]
     :person/name [[:person/name] "A person's names"]}
     ; The 2nd :person/name refers to the entity type defined below (see [XYZ])
  :interface.def/identify-via ['[?e :person/name]]}


 ; Notice how we can define more than one schema type in a single edn map.
 ; Here we will define a :person/name type
 ; [XYZ] Can define more than one type in a given edn file. For convenience, we
 ; define :person/name here because it is used in the :person type above
 {:interface.def/name :person/name
  :interface.def/fields
    {:person.name/given [:string "A given part of a person's name"]
     :person.name/family [:string "The family part of a person's name"]}
  :interface.def/identify-via ['[?e :person.name/given]]}]
```

The above interface definitions can generate the following Datomic schema:

```clojure
[{:db/id #db/id[:db.part/db]
  :db/doc "A person's names"
  :db/ident :person/name
  :db/valueType :db.type/ref
  :db/cardinality :db.cardinality/many
  :db/isComponent true
  :db.install/_attribute :db.part/db}
 {:db/id #db/id[:db.part/db]
  :db/doc "A given part of a person's name"
  :db/ident :person.name/given
  :db/valueType :db.type/string
  :db/cardinality :db.cardinality/one
  :db.install/_attribute :db.part/db}
 {:db/id #db/id[:db.part/db]
  :db/doc "The family part of a person's name"
  :db/ident :person.name/family
  :db/valueType :db.type/string
  :db/cardinality :db.cardinality/one
  :db.install/_attribute :db.part/db}]
```

Notice how we lose the semantics of the kinds of entities refs can map to.
This is expected, since Datomic does not provide that level of specificity.

The benefit of adding the additional specificity of ref constraints is the following:

1. It self-documents how we intend to use specific `:db.type/ref` attributes.
2. It helps us generate test data that maps to those intentions (see Section farther
   down about data generation).
3. It can be used to do some `clojure.spec` validation to enforce these constraints
   before that information is added to Datomic, where these constraints are not enforced.


#### Polymorphism

We chose the term `interface` because we wanted to signal how flexible we can be
when we interpret data in Datomic. An entity can possess the interfaces of
`:interface/person`, `:interface/mother`, and `:interface/daughter`.

This interface-based polymorphism is demonstrated in the following example, where
a patient and a practitioner are both persons as well.

```clojure
[{:interface.def/name :interface/person
  :interface.def/fields
    {:person/name [:string]}
  :interface.def/identify-via :datomic-spec/interfaces
  :interface.def/identifying-enum-part :db.part/user}

 {:interface.def/name :interface/patient
  :interface.def/inherits [:interface/person]
  :interface.def/fields
    {:patient/physicians [[:interface/physician] "A patient's physicians"]}
  :interface.def/identify-via :datomic-spec/interfaces
  :interface.def/identifying-enum-part :db.part/user}

 {:interface.def/name :interface/physician
  :interface.def/inherits #{:interface/person}
  :interface.def/fields
    {:physician/specialties [[:string] "The physician's medical specialty or specialties."]}
  :interface.def/identify-via :datomic-spec/interfaces
  :interface.def/identifying-enum-part :db.part/user}]
```

The above specifies that the `:patient` entity type and the `:practitioner`
entity type will both inherit the type definition of `:person`.

The benefits of such an approach are that:

1. We keep our interface definitions DRY.

##### Detecting the interfaces an entity can represent

If we read an entity from a Datomic database, how can we determine what interfaces
it implements? We will only be able to rely on the attributes of an entity to figure
this out. We can leverage attributes to identify entities in one of two ways.

1. Via Datomic datalog query clauses that specify a precise way to detect an entity
   via possession of an attribute (or attributes) that is (or are) always present
   (i.e., `:required`).

    ```clojure
    {:interface.def/name :interface/automobile
     :interface.def/fields
       {:automobile/license-plate [:string :required]}
     :interface.def/identify-via ['[?e :automobile/license-plate]]}
    ```

    Since we are using Datomic datalog syntax, we can use datalog itself to specify
    more complex detection queries. In this example, an `:interface/automobile` is one
    that has either a `:automobile/license-plate` or `:automobile/registration-id`:

    ```clojure
    {:interface.def/name :interface/automobile
     :interface.def/fields
       {:automobile/make [:string :required]
        :automobile/model [:string :required]}
     :interface.def/identify-via ['(or [?e :automobile/license-plate]
                                       [?e :automobile/registration-id])]}
    ```

    And in this example, an `:interface/automobile` is one that has both
    `:automobile/make` and `:automobile/model` attribute values:

    ```clojure
    {:interface.def/name :interface/automobile
     :interface.def/fields
       {:automobile/make [:string :required]
        :automobile/model [:string :required]}
     :interface.def/identify-via ['[?e :automobile/make]
                                  '[?e :automobile/model]]}
    ```

2. Via a special attribute `:datomic-spec/interfaces` that is a many-cardinality
   attribute of Datomic enums where an enum value names an interface that the entity
   can be interpreted as.

    ```clojure
    {:interface.def/name :interface/automobile
     :interface.def/fields {:automobile/license-plate [:string]}
     :interface.def/identify-via :datomic-spec/interfaces
     :interface.def/identifying-enum-part :db.part/autos}
    ```

    This comes in handy when we cannot rely on a particular value at an attribute for
    identifying an entity. For example, we may want to specify someone as being an
    `:interface/patient` in addition to an `:interface/person`, but the only attribute
    the entity may have (besides `:db/id`) is `:person/name`. In this case, the only
    way we could also identify a person as also being a patient is if we maintain an
    attribute that the entity can use to self-label itself as an `:interface/patient`.
    That special, reserved attribute is `:datomic-spec/interfaces`. It's Datomic schema
    looks like:

    ```clojure
    {:db/id #db/id[:db.part/db]
     :db/ident :datomic-spec/interfaces
     :db/valueType :db.type/ref
     :db/cardinality :db.cardinality/many
     :db/index true
     :db.install/_attribute :db.part/db}
    ```

    `:datomic-spec/interfaces` takes on a value that is a set of Datomic enums. For example,
    in the `:interface/automobile` example, the Datomic schema will include:

    ```clojure
    {:db/id #db/id[:db.part/autos]
     :db/ident :interface/automobile}
    ```

    The enum value is stored in the partition specified under the interface definition
    key `:interface.def/identifying-enum-part`.

We choose to force you to think about how you will enforce this so that you do not lose
any information about an entity's intended interfaces after you write it to the database.
An interface definition is not valid unless you specify `:interface.def/identify-via`.

We also provide convenience methods to determine the interfaces that a
particular entity or map loaded into memory satisfies:

```clojure
(let [entity {:db/id (datomic/tempid :db.part/user)
              :automobile/make "Toyota"
              :automobile/model "Prius"}]
(lab79.dspec/entity->interfaces ast entity datomic.api/q)
; => #{:interface/automobile}

(let [entity {:db/id (datomic/tempid :db.part/user)}]
(lab79.dspec/entity->interfaces ast entity datomic.api/q)
; => #{}
```

`(lab79.dspec/satisfies-interface? ast interface-name entity datomic-q)`

```clojure
(let [entity {:db/id (datomic/tempid :db.part/user)
              :automobile/make "Toyota"
              :automobile/model "Prius"}]
  (lab79.dspec/satisfies-interface? ast :interface/automobile entity datomic.api/q))
; => true

(let [entity {:db/id (datomic/tempid :db.part/user)}]
  (lab79.dspec/satisfies-interface? ast :interface/automobile entity datomic.api/q))
; => false
```

`(lab79.dspec/eid-satisfies-interface? ast interface-name entity-id datomic-q datomic-filter db)`

```clojure
(let [entity {:db/id (datomic/tempid :db.part/user)
              :automobile/make "Toyota"
              :automobile/model "Prius"}
      tempid (:db/id entity)
      {db :db-after
      tempids :tempids} @(d/transact conn [entity])
      eid (d/resolve-tempid db tempids tempid)]
  (lab79.dspec/eid-satisfies-interface? ast :interface/automobile eid datomic.api/q datomic.api/filter db))
; => true
```

If you want to incorporate the query clauses to limit your Datomic query to a
given set of interfaces, you can use `identify-via-clauses-for`, swapping in
your own custom entity id symbol (e.g., `?xx` in the following example):

```clojure
(datomic.api/q {:find '[?xx ...]
                :where (conj (identify-via-clauses-for ast '?xx :interface/xx)
                             ['?xx :other/attribute :x/y])})
```

The prior example finds the collection of entity ids that satisfy interface
`:interface/xx` and that also have an attribute `:other/attribute` with value
equal to `:x/y`.


### Converting Data Interfaces to Datomic Schemas

Our declarative semantic definitions can be converted to Datomic schema maps.

Consider the data interface we first encountered:

```clojure
(def user-interface
  {:interface.def/name :interface/user
   :interface.def/fields
     {:user/username [:string "A user's username"
                              :unique/identity ; can also be :unique/value
                              :required]
      :taggable/tags [[:string] "Tags for an entity"]
      :user/registeredAt [:instant "When a user registered"]}
   :interface.def/identify-via ['[?e :user/username]]})
```

It will also be able to generate the following Datomic attributes.

```clojure
(require '[datomic.api :as d])
(require [lab79.dspec :refer [dspec->ast]])
(require '[lab79.dspec.plugins.datomic :refer [ast->schemas])

(-> user-interface
    dspec->ast
    (ast->schemas d/tempid))
```

This will generate the following Datomic attributes:

```clojure
[
 {:db/id #db/id[:db.part/db]
  :db/ident :user/username
  :db/doc "A user's username"
  :db/valueType :db.type/string
  :db/cardinality :db.cardinality/one
  :db/unique :db.unique/identity
  :db.install/_attribute :db.part/db}

 {:db/id #db/id[:db.part/db]
  :db/ident :taggable/tags
  :db/doc "Tags for an entity"
  :db/valueType :db.type/string
  :db/cardinality :db.cardinality/many
  :db.install/_attribute :db.part/db}

 {:db/id #db/id[:db.part/db]
  :db/ident :user/registeredAt
  :db/doc "When a user registered"
  :db/valueType :db.type/instant
  :db/cardinality :db.cardinality/one
  :db.install/_attribute :db.part/db}
]
```

These can be sent to a Datomic transactor to register these attributes
as part of the Datomic schema.

You can expect Datomic enum entities to be generated for enum values that you
specify. For example, consider the following

```clojure
(def person-interface
  {:interface.def/name :interface/person
   :interface.def/fields
     {:person/gender ["A person's gender"
                      :enum {:person.gender/male "Male"
                             :person.gender/female  "Female"
                             :person.gender/other "Other"}]}})
(-> person-interface
    dspec->ast
    (ast->schemas d/tempid))
```


This will produce:

```clojure
[{:db/id #db/id[:db.part/db]
  :db/doc "A person's gender"
  :db/ident :person/gender
  :db/valueType :db.type/ref
  :db/cardinality :db.cardinality/one
  :db.install/_attribute :db.part/db}

 ;; Gender enums

 {:db/id #db/id[:db.part/user]
  :db/ident :person.gender/male
  :db/doc "Male"}

 {:db/id #db/id[:db.part/user]
  :db/ident :person.gender/female
  :db/doc "Female"}

 {:db/id #db/id[:db.part/user]
  :db/ident :person.gender/other
  :db/doc "Other"}]
```

### Converting dspec interfaces to clojure.spec definitions

Our declarative semantic definitions can be converted to `clojure.spec` specs.

#### Data Generation

Because we leverage `clojure.spec`, we can also leverage its
[abilities](http://clojure.org/guides/spec#_generators) to
generate test or sample data.

```clojure
(require '[lab79.dspec :refer [dspec-coll->ast]])
(require '[lab79.dspec.plugins.clojure-spec :refer [register-specs-for-ast!
                                                    register-specs-for-ast-with-custom-generators!]])
(require '[clojure.spec.gen :as gen])
(require '[ccm-om-next.db.gen.util :refer [fn->gen ensure-keys-gen]])
(require '[faker.name :as fname])
(require '[clojure.spec.gen :as gen])
(require '[clojure.spec :as s])
(import '[datomic.db DbId])

(def edn-interfaces
  [{:interface.def/name :interface/person
    :interface.def/fields {:person.id/uuid [:uuid "A uuid we assign to identify the person" :db.unique/identity :required]
                           :person.id/ssn [:string "A person's social security number" :db.unique/value]
                           :person/name [[:interface.person/name] "A person's names"]
                           :person/languages [[:string] "Languages a person can speak"]}
    :interface.def/identify-via ['[?e :person.id/uuid]]}

   {:interface.def/name :interface.person/name
    :interface.def/fields {:person.name/given [:string "A given part of a person's name"]
                           :person.name/family [:string "The family part of a person's name"]}
    :interface.def/identify-via ['[?e :person.name/given]]}])

(def generators
  {:person.name/given (fn->gen fname/first-name)
                       ; (fn->gen fn) calls fn to generate data
   :person/name (fn [member-generator]
                  (gen/set member-generator {:min-elements 1 :max-elements 1}))
   :interface.person/name (ensure-keys-gen :person.name/family
                                           :person.name/given)
                           ; (ensure-keys-gen :key1 :key2) generates a map
                           ; with at least :key1 and :key2
   :person/languages #(s/gen #{"English" "Spanish"})})

(def db-id? #(or (integer? %)
                 (instance? datomic.db.DbId %)))

(-> edn-interfaces

    ; Convert the semantic data interfaces to the intermediate semantic ast
    ; representation
    dspec-coll->ast

    ; Register `clojure.spec` specs based off the semantic ast
    ; and based off of custom generators to over-write particular
    ; `clojure.spec` keys.
    (register-specs-for-ast-with-custom-generators! generators datomic.api/tempid db-id?))

; Now that the specs are loaded into our Clojure environment, we
; can generated sample data based off of them.

; First create the generator
(def generator (s/gen :interface/person))

; Then generate data!

(def num-people-to-generate 100)
(gen/sample generator num-people-to-generate)
```

Here is an example of generating and loading sample data into Datomic

```
(require '[datomic.api :as d])
(require '[clojure.core.async :as a :refer [<!!]])

;; Helper fns

(defn vset->vlist
  [m]
  (into {} (map
             (fn [[k v]]
               [k (cond (set? v) (into [] v)
                        (map? v) (vset->vlist v)
                        :else v)])
             m)))

(defn elide-empty-str [m]
  (into {}
        (filter (fn [[_ v]]
                  (or (not (string? v))
                      (not (empty? v))))
                m)))

(defn elide-empty-set [m]
  (into {}
        (filter (fn [[_ v]]
                  (or (not (set? v))
                      (not (empty? v))))
                m)))


;; Data generation

(def num-samples 100)
(def generated-patients (gen/sample (s/gen :interface/patient) num-samples))

;; Write data to Datomic

(def conn (d/connect "datomic:mem//some-db"))
(def concurrency 2)
(def ch (a/to-chan (->> generated-patients
                        (map elide-empty-set)
                        (map elide-empty-str)
                        (map vset->vlist)
                        (map #(merge % {:db/id (d/tempid :db.part/user)}))
                        (map #(vector %)))))
(let [{:keys [result stop]} (tx-pipeline conn conc ch)]
  (<!! result)
  (System/exit 0))
```


### Intermediate AST Representation

As we have seen, our declarative semantic definitions can be converted to one
of two output formats:

1. Datomic schema maps.
2. `clojure.spec` declarations.

Before converting to either of these formats, we convert our human-optimized
semantic definitions into a machine-optimized intermediate representation. 

Consider our semantic definition for a user:

```clojure
(def user-interface
  {:interface.def/name :interface/user
   :interface.def/fields 
     #:user{:username [:string "A user's username"
                               :db.unique/identity ; can also be :unique/value
                               :required]
            :taggable/tags [[:string] "Tags for an entity"]
            :registeredAt [:instant "When a user registered"]
            :taggable/tags [[:string] "Tags for an entity"]
            :user/registeredAt [:instant "When a user registered"]}
   :interface.def/identify-via ['[?e :user/username]]})
```

Then, we can convert this to the intermediate AST.

```clojure
(require '[lab79.dspec :refer [ds]])
(ds/dspec-coll->ast user-interface)
```

This will generate the following AST.

```clojure
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
 :interface.ast/enum-map {}}
```

Let's revisit another example.

```clojure
(def person-interface
  {:interface.def/name :interface/person
   :interface.def/fields
     {:person/gender ["A person's gender"
                      :enum {:person.gender/male "Male"
                             :person.gender/female  "Female"
                             :person.gender/other "Other"}]}
   :interface.def/identify-via :datomic-spec/interfaces
   :interface.def/identifying-enum-part :db.part/user})
```

This semantic data interface generates the following AST.

```clojure
{:interface.ast/interfaces
  {:interface/person
    {:interface.ast.interface/name :interface/person
     :interface.ast.interface/fields
       {:person/gender {:db/ident :person/gender
                        :db/valueType :db.type/ref
                        :db/cardinality :db.cardinality/one
                        :db/doc "A person's gender"
                        :semantic/type :enum
                        :interface.ast.field/enum-seq #{:gender/male
                                                        :gender/female}}

        :datomic-spec/interfaces {:db/ident :datomic-spec/interfaces
                                  :db/valueType :db.type/ref
                                  :db/index true
                                  :interface.ast.field/type :enum
                                  :interface.ast.field/possible-enum-vals #{:interface/refable}
                                  :interface.ast.field/required true
                                  :db/cardinality :db.cardinality/many}}
     :interface.ast.interface/inherits #{}
     :interface.ast.interface/identify-via ['[?e :datomic-spec/interfaces :interface/person]]}}
 :interface.ast/enum-map
   {:gender/male {:db/ident :gender/male
                  :db/doc "Male"}
    :gender/female {:db/ident :gender/female
                    :db/doc "Female"}
    :interface/person {:db/ident :interface/person
                       :db/part :db.part/user}}}
```

Here is how ref constraints are encapsulated in our AST:

```clojure
{:interface.ast/interfaces
  {:interface/person
     {:interface.ast.interface/name :interface/person
      :interface.ast.interface/fields
        {:person.id/ssn {:db/ident :person.id/ssn
                         :db/valueType :db.type/string
                         :interface.ast.field/type :string
                         :db/cardinality :db.cardinality/one
                         :db/doc "A person's ssn number"
                         :db/unique :db.unique/value}
         :person/name {:db/ident :person/name
                       :db/valueType :db.type/ref
                       :interface.ast.field/type :person/name
                       :db/cardinality :db.cardinality/many
                       :db/doc "A person's names"
                       :semantic/type :person/name}}
      :interface.ast.interface/inherits #{}
      :interface.ast.interface/identify-via ['[?e :person/name]]}
   :person/name
     {:interface.ast.interface/name :person/name
      :interface.ast.interface/fields
        {:person.name/given {:db/ident :person.name/given
                             :db/valueType :db.type/string
                             :interface.ast.field/type :string
                             :db/cardinality :db.cardinality/one
                             :db/doc "A given part of a person's name"}
         :person.name/family {:db/ident :person.name/family
                              :db/valueType :db.type/string
                              :interface.ast.field/type :string
                              :db/cardinality :db.cardinality/one
                              :db/doc "The family part of a person's name"}
      :interface.ast.interface/inherits #{}
      :interface.ast.interface/identify-via ['[?e :person.name/given]]}
 :interface.ast/enum-map {}}}}
```

And here is how interface-based polymorphism is encapsulated in our AST:

```clojure
{:interface.ast/interfaces
  {:interface/person
     {:interface.ast.interface/name :interface/person
      :interface.ast/interface/fields
        {:person/name {:db/ident :person/name
                       :db/valueType :db.type/string
                       :interface.ast.field/type :string
                       :db/cardinality :db.cardinality/one}}
      :interface.ast.interface/identify-via ['[?e :person/name]]}
   :interface/patient
     {:interface.ast.interface/name :interface/patient
      :interface.ast.interface/fields
        {:patient/physicians {:db/ident :patient/primary-care-providers
                              :db/valueType :db.type/ref
                              :interface.ast.field/type :practitioner
                              :db/cardinality db.cardinality/many
                              :db/doc "A patient's physicians"
                              :semantic/type :interface/physician}}
      :interface.ast.interface/inherits #{:interface/person}
      :interface.ast.interface/identify-via ['[?e :datomic-spec/interfaces :interface/patient]]
      :interface.ast.interface/identifying-enum-part :db.part/user}
   :interface/physician
     {:interface.ast.interface/name :interface/physician
      :interface.ast.interface/fields
        {:physician/specialties {:db/ident :physician/specialties
                                 :db/valueType :db.type/string
                                 :interface.ast.field/type :string
                                 :db/cardinality :db.cardinality/many
                                 :db/doc "The physician's medical specialty or specialties"}}
      :interface.ast.interface/inherits #{:interface/person}
      :interface.ast.interface/identify-via ['[?e :datomic-spec/interfaces :interface/physician]]
      :interface.ast.interface/identifying-enum-part :db.part/user}
   :interface.ast/enum-map
     {:interface/patient {:db/ident :interface/patient
                          :db/part :db.part/user}
      :interface/physician {:db/ident :interface/physician
                            :db/part :db.part/user}}
```

## Prior Art

There are several other Clojure libraries that add additional semantics on top of Datomic,
make it easier to work with Datomic queries and transactions, or help with .
We've drawn inspiration from these, but decided to work on our own library because none of
them quite fit our needs and desired style.

Stronger Datomic Semantics

- https://github.com/yuppiechef/datomic-schema
- https://github.com/cloojure/tupelo-datomic
- http://docs.caudate.me/adi/
- https://github.com/SparkFund/spec-tacular
- https://github.com/facjure/atomic
- https://github.com/cldwalker/datomico
- https://github.com/ohlo/ginandtomic
- https://github.com/devn/datomic-simple
- https://github.com/eeng/datomic-qb
- https://github.com/moquist/datomic-schematode
- https://github.com/zololabs/demonic
- https://github.com/appcanary/crustacean
- https://github.com/vlacs/hatch
- https://github.com/wkf/crudo

Easier to work with Datomic queries and transactions:

- https://github.com/flyingmachine/datomic-junk
- https://github.com/juxt/datomic-extras
- https://github.com/democracyworks/datomic-toolbox
- https://github.com/magnars/datomic-snippets
- https://github.com/webnf/webnf/tree/master/datomic
- https://github.com/mysema/datomic-tools
- https://github.com/evanspa/pe-datomic-utils
- https://github.com/avodonosov/datomic-helpers
- https://github.com/jonase/datomic-query-helpers
- https://github.com/molst/hazel
- https://github.com/rodnaph/attromic
- https://github.com/totalperspective/fook
- https://github.com/osbert/storable
- https://github.com/mgaare/datomisc
- https://github.com/halgari/fafnir
- https://github.com/CareLogistics/wile
- https://github.com/env/toolbox

Datomic migrations:

- https://github.com/juxt/joplin
- https://github.com/Bijnagte/alchemist
- https://github.com/rkneufeld/conformity
- https://github.com/bitemyapp/brambling
- https://github.com/RallySoftware/datomic-replication
- https://github.com/guilespi/datomic-manage
- https://github.com/ilshad/generations

Storing arbitrary data in Datomic:

- https://github.com/tailrecursion/monocopy
- https://github.com/GoodGuide/datomizer
- https://github.com/stathissideris/datomic-rtree

Datomic and clojure.spec

- https://github.com/alexanderkiel/datomic-spec

## License

The MIT License (MIT)
Copyright © 2016 Lab79, Inc.

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
