(ns lab79.dspec.plugins.datomic
  "Leverage the dspec AST to play with Datomic."
  #?(:cljs (:require-macros
             cljs.spec
             [com.rpl.specter.macros :refer [traverse]]))
  (:require [clojure.spec :as s]
            [lab79.datomic-spec :refer [datomic-schema-keys]]
            [com.rpl.specter :refer [MAP-VALS]]
            [lab79.dspec.util :refer [filter-kv]]
    #?(:clj  [com.rpl.specter.macros :refer [traverse]])))

(def ^:private tempid-factory-spec
  (s/fspec :args (s/alt :binary (s/cat :partition keyword? :num integer?)
                        :unary (s/cat :partition keyword?))
           :ret any?))

(s/fdef ast->enum-schemas
        :args (s/cat :ast :interface/ast
                     :tempid-factory tempid-factory-spec)
        :ret (s/coll-of :datomic/enum-schema))
(defn- ast->enum-schemas
  "Given a semantic AST, generates edn that represents Datomic enum schemas
  that we can transact to Datomic to add the enums to the Datomic schema."
  [ast tempid-factory]
  (for [[_ enum] (:interface.ast/enum-map ast)]
    (assoc
      (filter-kv (fn [k _] (contains? datomic-schema-keys k)) enum)
      :db/id (tempid-factory (get enum :db/part :db.part/user)))))

(s/fdef ast->field-schemas
        :args (s/cat :ast :interface/ast
                     :tempid-factory tempid-factory-spec)
        :ret (s/coll-of :datomic/field-schema))
(defn- ast->field-schemas
  "Given a semantic AST, generates edn that represents entity attributes to add to
  Datomic schema. Does not include enum schemas or partition schemas."
  [ast tempid-factory]
  (->> (traverse [:interface.ast/interfaces MAP-VALS :interface.ast.interface/fields MAP-VALS] ast)
       (reduce
         (fn [field-map {:keys [db/ident] :as field-ast}]
           ; distinct because an interface may share the same attribute, as is the case with
           ; :dspec/interfaces
           (if (contains? field-map ident)
             field-map
             (assoc field-map
               ident (as-> field-ast $
                           (filter-kv (fn [k _] (contains? datomic-schema-keys k)) $)
                           (assoc $ :db/id (tempid-factory :db.part/db)
                                    :db.install/_attribute  :db.part/db)))))
         {})
       vals))

(s/fdef ast->partition-schemas
        :args (s/cat :ast :interface/ast
                     :tempid-factory tempid-factory-spec)
        :ret (s/coll-of :datomic/partition-schema))
(defn ast->partition-schemas
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

(s/fdef ast->schemas
        :args (s/cat :ast :interface/ast
                     :tempid-factory tempid-factory-spec)
        :ret (s/keys :opt [:datomic/field-schema :datomic/partition-schema :datomic/enum-schema]))
(defn ast->schemas
  "Given a semantic AST, generates edn that represents attributes to add to
  Datomic schema. `tempid` will be passed in and will be `datomic.api/tempid`."
  [ast tempid-factory]
  {:datomic/field-schema (ast->field-schemas ast tempid-factory)
   :datomic/enum-schema (ast->enum-schemas ast tempid-factory)
   :datomic/partition-schema (ast->partition-schemas ast tempid-factory)})

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
  "Returns the Datomic datalog query clauses that we can use to identify entities that
  satisfy the given interfaces"
  [ast eid-symbol & interface-names]
  (mapcat
    (fn [interface-name]
      (->> ast
           :interface.ast/interfaces
           interface-name
           :interface.ast.interface/identify-via
           (clojure.walk/postwalk #(if (= '?e %) eid-symbol %))))
    interface-names))