(ns lab79.dspec.parser
  "Parse dspec definitions into our special AST."
  #?(:cljs (:require-macros
             cljs.spec
             [cljs.core.match :refer [match]]))
  (:require [clojure.spec :as s]
    #?(:clj  [clojure.core.match :refer [match]]
       :cljs cljs.core.match)
    ;#?(:clj  [clojure.core.match :refer [match]]
    ;   :cljs [cljs.core.match :refer-macros [match]])
            ))

;
; clojure.spec describing the parser data used in translating the
; developer-defined interface definitions to the AST form
;

;
; clojure.spec for the resulting Datomic schema maps that can be sent to Datomic transactor
;

(s/def :interface/field-def-parser
  (s/keys :req [:interface.ast/field :interface.ast/enum-map]))

(s/fdef update-field-ast&enum-map
        :args (s/cat :field-ast&enum-map (s/and #(contains? % :interface.ast/field)
                                                #(contains? % :interface.ast/enum-map))
                     :conformed-field-tag-kv (s/tuple keyword? any?))
        :ret (s/keys :req [:interface.ast/field :interface.ast/enum-map]))
(defmulti update-field-ast&enum-map (fn [field-ast&enum-map [tag-alias conformed-tag-value]]
                                      tag-alias))
(defmethod update-field-ast&enum-map :should-generate
  [field-ast&enum-map _]
  (assoc-in field-ast&enum-map [:interface.ast/field :gen/should-generate] true))

(s/fdef field-def->ast-field&enum-map
        :args (s/cat :field-def (s/spec :interface.def/field)
                     :field-name keyword?
                     :custom-tags (s/? (s/coll-of keyword? :kind set?)))
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
  ([field-def field-name]
   (field-def->ast-field&enum-map field-def field-name #{:should-generate}))
  ([field-def field-name custom-tags]
   (let [field-tags (s/conform :interface.def/field field-def)
         {custom-tag-pairs true
          default-tag-pairs false} (group-by (fn [[tag _]] (contains? custom-tags tag)) field-tags)]
     (letfn [(parse-default-tag-pair
               [parsed conform-output]
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
                         [:just-vals vals]}]] (-> parsed
                                                  (assoc-in [:interface.ast/field :db/cardinality] :db.cardinality/one)
                                                  (assoc-in [:interface.ast/field :db/valueType] :db.type/ref)
                                                  (assoc-in [:interface.ast/field :interface.ast.field/type] :enum)
                                                  (assoc-in [:interface.ast/field :interface.ast.field/possible-enum-vals] (into #{} vals))
                                                  (update :interface.ast/enum-map into (map (fn [kw] [kw {:db/ident kw}])) vals))
                      [:single-type
                       [:enum
                        {:vals
                         [:vals-with-doc kw->doc]}]] (-> parsed
                                                         (assoc-in [:interface.ast/field :db/cardinality] :db.cardinality/one)
                                                         (assoc-in [:interface.ast/field :db/valueType] :db.type/ref)
                                                         (assoc-in [:interface.ast/field :interface.ast.field/type] :enum)
                                                         (assoc-in [:interface.ast/field :interface.ast.field/possible-enum-vals] (into #{} (keys kw->doc)))
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
                         [:just-vals vals]}]] (-> parsed
                                                  (assoc-in [:interface.ast/field :db/cardinality] :db.cardinality/many)
                                                  (assoc-in [:interface.ast/field :db/valueType] :db.type/ref)
                                                  (assoc-in [:interface.ast/field :interface.ast.field/type] :enum)
                                                  (assoc-in [:interface.ast/field :interface.ast.field/possible-enum-vals] (into #{} vals))
                                                  (update :interface.ast/enum-map into (map (fn [kw] [kw {:db/ident kw}])) vals))

                      [:many-type
                       [:enum
                        {:vals
                         [:vals-with-doc kw->doc]}]] (-> parsed
                                                         (assoc-in [:interface.ast/field :db/cardinality] :db.cardinality/many)
                                                         (assoc-in [:interface.ast/field :db/valueType] :db.type/ref)
                                                         (assoc-in [:interface.ast/field :interface.ast.field/type] :enum)
                                                         (assoc-in [:interface.ast/field :interface.ast.field/possible-enum-vals] (into #{} (keys kw->doc)))
                                                         (update :interface.ast/enum-map
                                                                 into (map (fn [[kw doc]] [kw {:db/ident kw :db/doc doc}])) kw->doc))

                      [:unique val] (assoc-in parsed [:interface.ast/field :db/unique] val)
                      [:is-component _] (assoc-in parsed [:interface.ast/field :db/isComponent] true)
                      [:should-index _] (assoc-in parsed [:interface.ast/field :db/index] true)
                      ;[:should-generate _] (assoc-in parsed [:interface.ast/field :gen/should-generate] true)
                      [:no-history _] (assoc-in parsed [:interface.ast/field :db/noHistory] true)
                      [:fulltext _] (assoc-in parsed [:interface.ast/field :db/fulltext] true)
                      [:required _] (assoc-in parsed [:interface.ast/field :interface.ast.field/required] true)))]
       (as-> {:interface.ast/enum-map {}
              :interface.ast/field {:db/ident field-name}} parsed

             (reduce update-field-ast&enum-map parsed custom-tag-pairs)
             (reduce parse-default-tag-pair parsed default-tag-pairs))))))

(s/fdef parse-dspec
        :args (s/cat :dspec :interface/def)
        :ret :interface/ast)
(defn parse-dspec
  "Parses a dspec (optimized for dev readability) into an AST that
  can be used to generate Datomic schemas, generate test data, etc."
  [dspec]
  (let [{:interface.def/keys [name fields inherits identify-via identifying-enum-part]} dspec
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