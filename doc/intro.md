# Introduction to dspec

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

## Extensibility

Field tags can:

1. Specify what to generate.
2. Specify extra validation.

Examples:

1. `:gen/should-gen`
2. `:string/not-empty`

But do we want to specify validation via tags? Or can we use clojure.spec's preference for
predicates?

We could just use datomic schemas plus clojure.spec's directly, without convenience
functions. The pros would be using only APIs without adding any more complexity.
The cons would be that we would be repeating ourselves in both clojure.spec and in
datomic schemas.

Another alternative would be to try to generate datomic schemas directly from some subset
of clojure.specs plus some additional information not captured by clojure.spec (e.g., `:db/doc`).

The datomic schema attributes that are not captured by clojure.spec include:

1. `:db/doc`
2. `:db/unique`
3. `:db/index`
4. `:db/fulltext`
5. `:db/isComponent`
6. `:db/noHistory`

(def datomic-schema-metadata (ref {}))

(s/def :db/id db-id?)

(s/def :interface/user (s/keys :req [:user/username :user/password]))
(alter datomic-schema-metadata
  assoc :db/doc "Username")

(s/def :user/username string?)
(s/def :user/password string?)


(s/gen (s/merge :interface/person :interface/physician))

```clojure
(defn infer-datomic-schema
  [extras]
  (->> (s/registry)
       (filter #(= (namespace %) ":interface"))
       ((juxt parse-keys is-collection? identity))
       (map
         (fn [ident many? interface-name]
           (merge {:db/ident ident
                   :db/valueType (infer-type ident)
                   :db/cardinality (if many? :db.cardinality/many :db.cardinality/one)}
                   (->> [:db/doc :db/unique :db/index :db/fulltext :db/isComponent :db/noHistory]
                        (map (fn [prop]
                          [prop (get-in extras ident prop)]))
                        (filter (comp (complement nil?) second))
                        (into {})))))))
```