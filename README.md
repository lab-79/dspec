# datomic-spec

A Clojure library designed to add useful semantics on top of Datomic.

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
4. Provide easy validation tagging (e.g., `:required`).
5. (Done) Generate fake dev data or test data, by providing additional
   semantics (e.g., what entity types can map to what other entity types via
   Datomic `ref`s).
6. Integrates with a nice way to add or retract from a Datomic db schema when
   these semantic schema definitions change.
7. Make it easy to extract specific *kinds* of entities.
8. Support polymorphism.
9. Small code base.
10. Simple design.


## Usage

We adopt a declarative edn specification for defining stronger semantics on top
of Datomic schemas. This alternative mimics syntax from
[Yuppiechef/datomic-schema](https://github.com/Yuppiechef/datomic-schema).

As an end-user developer using this library, most of your application of this
library will be convert some collection of specs to:

1. To Datomic schema attributes that will be added to the Datomic schema.
2. To app data datoms in `:db.part/user` (or specified partition).
3. To validation output that specifies if a vector of specs is invalid or
   in what ways they are invalid.

Architecturally, we convert all specs into a single intermediate AST
representing our entire world of interfaces, enums, and their relationships
with each other (inheritance, references, etc.). Then our library converts this
AST to either Datomic schemas, generated app data, or validation output that we
mentioned above.

### Defining datomic-spec Specs

In `datomic-spec`, our semantic definitions are defined as data.

```clojure
(def user-spec
  {:interface.def/name :interface/user
   :interface.def/fields {
     :user/username [:string "A user's username"
                             :unique/identity ; can also be :unique/value
                             :required]
     :taggable/tags [[:string] "Tags for an entity"]
     :user/registeredAt [:instant "When a user registered"]}
   :interface.def/identify-via :user/username})
```

This defines a kind of object named `:interface/user` that has the following
attributes:

- A string attribute named `:user/username` that is required and uniquely
identifies a given user.
- An attribute named `:taggable/tags` that is a vector of strings. This
corresponds to a Datomic cardinality of `:cardinality/many` and is simply
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
`:cardinality/many`.

#### Enum Types

An attribute can also take on an enumerated set of values.

```clojure
{:interface.def/name :interface/person
 :interface.def/fields {
   :person/gender ["A person's gender"
                   :enum {:person.gender/male "Male"
                          :person.gender/female  "Female"
                          :person.gender/other "Other"}]}}

; or
{:interface.def/name :person
 :interface.def/fields {
   :person/gender [:enum #{:person.gender/male
                           :person.gender/female
                           :person.gender/other]
                           "A person's gender"}}}
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
 :interface.def/fields {
   :user/username [:string "A user's username" :required]
   :user/password [:string "A user's hashed password" :required]}}
```

The field flags that come pre-packaged with `datomic-spec` include:

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

#### Relationships between interfaces

In Datomic, a `:db.type/ref` entity can refer to any other entity. Usually, we want
to define more constraints about the kinds of relationships an entity can have with
other "kinds" of entities.

We offer a way to specify how entities map to each other.

Take the example of a person having many names (e.g., maiden, married, etc.).
Here is how we can represent that succinctly:

```clojure
[{:interface.def/name :interface/person
  :interface.def/fields {
    :person.id/ssn [:string "A person's ssn number"
                            :db.unique/value]

    :person/name [[:person/name] "A person's names"]}}
    ; The 2nd :person/name refers to the entity type defined below (see [XYZ])


 ; Notice how we can define more than one schema type in a single edn map.
 ; Here we will define a :person/name type
 ; [XYZ] Can define more than one type in a given edn file. For convenience, we
 ; define :person/name here because it is used in the :person type above
 {:interface.def/name :person/name
  :interface.def/fields {
    :person.name/given [:string "A given part of a person's name"]
    :person.name/family [:string "The family part of a person's name"]}}]
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
  :interface.def/fields {
    :person/name [:string]}}

 {:interface.def/name :interface/patient
  :interface.def/inherits [:interface/person]
  :interface.def/fields {
    :patient/physicians [[:interface/physician]
                          "A patient's physicians"]}}

 {:interface.def/name :interface/physician
  :interface.def/inherits #{:interface/person}
  :interface.def/fields {
    :physician/specialties [[:string]
                            "The physician's medical specialty or specialties."]}}]
```

The above specifies that the `:patient` entity type and the `:practitioner`
entity type will both inherit the type definition of `:person`.

The benefits of such an approach are that:

1. We keep our interface definitions DRY.

##### Detecting the interfaces an entity can represent

If we read en entity from a Datomic database, how can we determine what interfaces
it implements. We will only be able to rely on the attributes of an entity to figure
this out. We can leverage attributes to identify entities in one of two ways.

1. Via possession of an attribute (or attributes) that is (or are) always present
   (i.e., `:required`).

   ```clojure
   {:interface.def/name :interface/automobile
    :interface.def/fields {
      :automobile/license-plate [:string :required]
    }
    :interface.def/identify-via :automobile/license-plate}
   ```

   ```clojure
   {:interface.def/name :interface/automobile
    :interface.def/fields {
      :automobile/make [:string :required]
      :automobile/model [:string :required]
    }
    :interface.def/identify-via #{:automobile/make :automobile/model}}
   ```

2. Via a special attribute `:interface.def/interfaces` that is a many-cardinality
   attribute of enums where an enum value names an interface that the entity
   can be interpreted as.

   ```clojure
   {:interface.def/name :interface/automobile
    :interface.def/fields {
      :automobile/license-plate [:string]
    }
    :interface.def/identify-via :interface.def/interfaces}
   ```

We choose to force you to think about how you will enforce this so that you do not lose
any information about an entity's interfaces after you write it to the database. An
interface definition is not valid unless you specify `:interface.def/identify-via`.


### Converting Specs to Datomic Schemas

Our declarative semantic definitions can be converted to Datomic schema maps.

Consider the spec we first encountered:

```clojure
(def user-spec
  {:interface.def/name :interface/user
   :interface.def/fields {
     :user/username [:string "A user's username"
                             :unique/identity ; can also be :unique/value
                             :required]
     :taggable/tags [[:string] "Tags for an entity"]
     :user/registeredAt [:instant "When a user registered"]}})
```

It will also be able to generate the following Datomic attributes.

```clojure
(require '[datomic.api :as d])

(-> user-spec
    semantic-spec->semantic-ast
    (semantic-ast->datomic-schemas d/tempid))
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
(def person-spec
  {:interface.def/name :interface/person
   :interface.def/fields {
     :person/gender ["A person's gender"
                     :enum {:person.gender/male "Male"
                            :person.gender/female  "Female"
                            :person.gender/other "Other"}]}})
(-> person-spec
    semantic-spec->semantic-ast
    (semantic-ast->datomic-schemas d/tempid))
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

### Converting datomic-spec Specs to clojure.spec Specs

Our declarative semantic definitions can be converted to `clojure.spec` specs.

#### Data Generation

Because we leverage `clojure.spec`, we can also leverage its
[abilities](http://clojure.org/guides/spec#_generators) to
generate test or sample data.

```clojure
(require '[datomic-tools.core :refer [semantic-spec-coll->semantic-ast
                                      register-generative-specs-for-ast!]])
(require '[clojure.spec.gen :as gen])
(require '[ccm-om-next.db.gen.util :refer [fn->gen ensure-keys-gen]])
(require '[faker.name :as fname])
(require '[clojure.spec.gen :as gen])
(require '[clojure.spec :as s])

(def edn-specs
    [{:interface.def/name :interface/person
      :interface.def/fields {:person.id/uuid [:uuid "A uuid we assign to identify the person" :db.unique/identity :required]
                             :person.id/ssn [:string "A person's social security number" :db.unique/value]
                             :person/name [[:interface.person/name] "A person's names"]
                             :person/languages [[:string] "Languages a person can speak"]}}

  {:interface.def/name :interface.person/name
   :interface.def/fields {:person.name/given [:string "A given part of a person's name"]
                          :person.name/family [:string "The family part of a person's name"]}}])

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

(-> edn-specs

    ; Convert the semantic specs to the intermediate semantic ast
    ; representation
    semantic-spec-coll->semantic-ast

    ; Register `clojure.spec` specs based off the semantic ast
    ; and based off of custom generators to over-write particular
    ; `clojure.spec` keys.
    (register-generative-specs-for-ast! generators))

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
(def user-spec
  {:interface.def/name :interface/user
  :interface.def/fields #:user{
    :username [:string "A user's username"
                       :db.unique/identity ; can also be :unique/value
                       :required]
    :taggable/tags [[:string] "Tags for an entity"]
    :registeredAt [:instant "When a user registered"]
    [:taggable/tags [:string] "Tags for an entity"]
    [:user/registeredAt :instant "When a user registered"]
  }})

(def user-spec
  {:interface.def/name :interface/user
  :interface.def/fields {
    :user/username [:string "A user's username"
                            :db.unique/identity ; can also be :unique/value
                            :required]
    :taggable/tags [[:string] "Tags for an entity"]
    :user/registeredAt [:instant "When a user registered"]
  }})
```

Then, we can convert this to the intermediate AST.

```clojure
(require '[org.lab79.datomic-spec.core :refer [ds]])
(ds/semantic-spec-coll->semantic-ast user-spec)
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
    }
  }
 :interface.ast/enum-map {}}
```

Let's revisit another example.

```clojure
(def person-spec
  {:interface.def/name :interface/person
   :interface.def/fields {
     :person/gender ["A person's gender"
                     :enum {:person.gender/male "Male"
                            :person.gender/female  "Female"
                            :person.gender/other "Other"}]}})
```

This semantic spec generates the following AST.

```clojure
{:interface.ast/interfaces
  {:person
    {:interface.ast.interface/name :person
     :interface.ast.interface/fields
       {:person/gender {:db/ident :person/gender
                        :db/valueType :db.type/ref
                        :db/cardinality :db.cardinality/one
                        :db/doc "A person's gender"
                        :semantic/type :enum
                        :interface.ast.field/enum-seq #{:gender/male
                                                        :gender/female}}}
     :interface.ast.interface/inherits #{}
    }
  }
 :interface.ast/enum-map
   {:gender/male {:db/ident :gender/male
                  :db/doc "Male"}
    :gender/female {:db/ident :gender/female
                    :db/doc "Female"}}}
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
      :interface.ast.interface/inherits #{}}
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
      :interface.ast.interface/inherits #{}}
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
                      :db/cardinality :db.cardinality/one}}}
   :interface/patient
    {:interface.ast.interface/name :interface/patient
     :interface.ast.interface/fields
       {:patient/physicians {:db/ident :patient/primary-care-providers
                             :db/valueType :db.type/ref
                             :interface.ast.field/type :practitioner
                             :db/cardinality db.cardinality/many
                             :db/doc "A patient's physicians"
                             :semantic/type :interface/physician}}
     :interface.ast.interface/inherits #{:interface/person}}
    :interface/physician
      {:interface.ast.interface/name :interface/physician
       :interface.ast.interface/fields
         {:physician/specialties {:db/ident :physician/specialties
                                  :db/valueType :db.type/string
                                  :interface.ast.field/type :string
                                  :db/cardinality :db.cardinality/many
                                  :db/doc "The physician's medical specialty or specialties"}}
       :interface.ast.interface/inherits #{:interface/person}}
  :interface.ast/enum-map {}}
```

## Prior Art

This library was influenced by reviewing existing, related work. None of the
libraries had everything we wanted, so we worked on a new one.

- https://github.com/yuppiechef/datomic-schema
- https://github.com/cloojure/tupelo-datomic
- http://docs.caudate.me/adi/

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
