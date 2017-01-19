(ns lab79.dspec.specs.def
  "clojure.spec's describing how devs declare dspec definitions."
  (:require [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [lab79.datomic-spec :refer [datomic-value-types]]))

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
         (s/gen (s/keys :opt [:db/doc :db/unique]))))))

(s/def :interface.def/inherits (s/coll-of keyword? :kind vector?))

(s/def :interface.def/identify-via (s/or :identify-via/reserved-attribute #{:datomic-spec/interfaces}
                                         :identify-via/datalog-clause :datomic.query.kv/where))

(s/def :db/part (s/with-gen (s/and keyword? #(= (namespace %) "db.part"))
                            #(gen/fmap (fn [string] (keyword "db.part" string))
                                       (gen/not-empty (gen/string-alphanumeric)))))

(s/def :interface.def/identifying-enum-part :db/part)

(s/def :gen/should-generate boolean?)

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