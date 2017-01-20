(ns lab79.dspec.plugins.clojure-spec
  "Generate clojure.spec's with the AST."
  #?(:cljs (:require-macros
             cljs.spec
             [com.rpl.specter.macros :refer [select select-first]]))
  (:require [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [lab79.clojure-spec-helpers :refer [is-keys-spec? extract-spec-keys]]
            [clojure.test.check.generators :as tcgen :refer [generator? resize]]
            [com.stuartsierra.dependency :as ssdep]
            [com.rpl.specter :refer [MAP-VALS collect-one]]
            [lab79.dspec.util :refer [arity]]
            [lab79.dspec.util.gen :refer [ensure-keys-gen fn->gen]]
    #?(:cljs [lab79.dspec.util.eval :refer [eval*]])
    #?(:clj  [clojure.spec.gen :as gen]
       :cljs [cljs.spec.impl.gen :as gen])
    #?(:clj  [com.rpl.specter.macros :refer [select select-first]])))

#?(:cljs (defn- eval [form]
           (eval* form 'lab79.dspec)))

; clojure.spec for defining custom generator maps for our fields
(s/def :interface/gen-map (s/map-of keyword? (s/or :no-member-gen-factory :gen/generator-factory
                                                   :with-member-gen-factory :gen/generator-factory-with-member)))

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

(s/fdef get-all-inherited-interface-names
        :args (s/cat :ast :interface/ast
                     :interface-name keyword?)
        :ret (s/coll-of keyword? :kind set?))
(defn- get-all-inherited-interface-names
  "Returns the union of all immediately inherited and recursively inherited interfaces
  for a given interface represented by `interface-name`."
  [ast interface-name]
  (let [interface (-> ast :interface.ast/interfaces interface-name)
        {:keys [interface.ast.interface/inherits]} interface]
    (into inherits (mapcat #(get-all-inherited-interface-names ast %) inherits))))

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
(s/def :dspec/gen-factory-1 (s/fspec :args (s/cat :original-gen-factory :dspec/gen-factory)
                                     :ret generator?))
; TODO Replace dummy with more precise fspec
(s/def :dummy/gen-factory fn?)
(s/def :dummy/gen-factory-1 fn?)

(s/fdef interface->generator-factory
        :args (s/cat :spec s/spec?
                     :inherited-custom-generators (s/coll-of :dummy/gen-factory-1)
                     :custom-generator-factory (s/nilable :dummy/gen-factory-1)
                     :all-my-self-labeling-interfaces (s/coll-of keyword? :kind set?)
                     :all-inherited-interface-names (s/coll-of keyword? :kind set?)
                     :req-keys (s/coll-of keyword?)
                     :opt-keys (s/coll-of keyword?))
        :ret :dspec/gen-factory)
(defn interface->generator-factory
  "Returns a factory function that returns a generator for the interface
  represented by spec, inherited-custom-generators, all-my-self-labeling-interfaces,
  all-inherited-interface-names, req-keys, and opt-keys, while also incorporating
  an optional overriding custom-generatory-factory.

  The simplest case will be just a generator based off just a plain clojure.spec
  `(s/keys :req req :opt opt)`.

  A more advanced case is one in which there are custom generators.

  An even more advanced case is one in which we inherit custom generators via
  interfaces we inherit. Do we automatically merge the results of these generators?
  Is there any sense of applying precedence to the generators? Do we give control
  to the developer, and how do we give that developer control? What are the likely
  concrete examples in which this would occur?

  The only control we have over s/keys is what is required and what is optional.
  The most general way to specify this is as a frequency distribution of generated
  keys across sample entities generated.

  The kind of control we may also want to exercise in generation is association of
  a generated entity with a related entity. For example we may want to generate
  a set of users who belong to a particular access control group and another set
  of users who belong to another access control group. What might this look like?

  We need to respect max-depth when generating an entity. These also need to be
  passed along to children generators that generated interface-type child entities."
  [spec
   ; TODO inherited-custom-generators not used
   inherited-custom-generators
   custom-generator-factory
   all-my-self-labeling-interfaces
   all-inherited-interface-names
   req-keys
   opt-keys]
  (if (and (not (seq inherited-custom-generators))
           (not (seq all-my-self-labeling-interfaces)))
    ; TODO Replace s/gen with max-depth generator factory
    (let [original-generator-factory #(s/gen spec)]
      (if custom-generator-factory
        #(custom-generator-factory original-generator-factory)
        original-generator-factory))
    (let [f (if (seq all-my-self-labeling-interfaces)
              (fn [objects-to-combine]
                (apply merge (conj objects-to-combine
                                   {:dspec/interfaces all-my-self-labeling-interfaces})))
              (partial apply merge))
          ; We remove :dspec/interfaces, so we don't un-necessarily generate them randomly, before we
          ; over-write them with our own deterministic set of interface keywords.
          unevaled-base-gen-spec `(s/keys :req ~(vec (remove #{:dspec/interfaces} req-keys)) :opt ~opt-keys)
          ; TODO Replace s/gen with max-depth generator factory
          base-gen-spec-factory (memoize #(s/gen
                                            ; If the eval happened outside the generator factory,
                                            ; an error would occur because the :req and :opt specs
                                            ; might not be defined yet.
                                            (eval unevaled-base-gen-spec)))
          ; TODO Replace s/gen with max-depth generator factory
          generators-factory (memoize #(cond-> (map (fn [x] (s/gen x)) all-inherited-interface-names)
                                               custom-generator-factory (conj (custom-generator-factory base-gen-spec-factory))
                                               (not custom-generator-factory) (conj (base-gen-spec-factory))))]
      #(let [generators (generators-factory)
             generator (apply gen/tuple generators)]
         (gen/fmap f generator)))))

; TODO Remove or complete.
(defn interface->generator-factory-alt
  [ast interface-name gen-middleware]
  (let []
    (fn [max-depth])))

(s/fdef interface->clojure-spec&generator
        :args (s/cat :ast :interface/ast
                     :interface-name keyword?
                     :gen-overrides (s/map-of keyword? :dummy/gen-factory)
                     ;:gen-overrides :interface/gen-overrides
                     )
        :ret (s/keys :req-un [:cljspec/spec :dspec/gen-factory]))
(defn- interface->clojure-spec&generator
  "For a given interface represented by `interface-name`, this returns:
  1. The entity map clojure.spec for the given interface.
  2. A factory function that returns a generator for generating sample data
     that satisfy the interface."
  [ast interface-name gen-overrides]
  (let [interface (get-in ast [:interface.ast/interfaces interface-name])
        all-fields (ast&interface->ast-fields ast interface)
        {req-fields true, opt-fields nil
         :or {req-fields [] opt-fields []}} (group-by :interface.ast.field/required all-fields)
        custom-generator-factory (if-let [custom-generator-factory (get gen-overrides interface-name)]
                                   custom-generator-factory
                                   (if-let [gen-fields (seq (filter :gen/should-generate all-fields))]
                                     (apply ensure-keys-gen (map :db/ident gen-fields))
                                     nil))

        all-inherited-interface-names (get-all-inherited-interface-names ast interface-name)

        ; If an ancestor is identified via an existing attribute instead of via the special attribute
        ; :dspec/interfaces, then let's be sure that we do not include the name of that ancestor
        ; in this entity's :dspec/interfaces set of inherited interfaces. This is because:
        ; 1. There will be no Datomic enum in Datomic associated with the name of the ancestor.
        ; 2. We can detect that we share the interface of the ancestor via the same way the ancestor knows
        ;    its interface -- by the existence of an attribute(s) specified by the Datalog query clause in
        ;    , e.g., `:interface.def/identify-via ['[?e :obj/ids-the-ancestral-interface]]`.
        all-my-self-labeling-interfaces (set (filter
                                               #(contains? (:interface.ast/enum-map ast) %)
                                               (conj all-inherited-interface-names interface-name)))
        inherited-custom-generators (keep gen-overrides all-inherited-interface-names)

        ; We use distinct here because we could have multiple :dspec/interfaces attributes that we inherit
        req-keys (conj (vec (distinct (map :db/ident req-fields))) :db/id)
        opt-keys (vec (distinct (map :db/ident opt-fields)))

        base-spec (eval `(s/keys :req ~req-keys :opt ~opt-keys))
        spec (if (seq all-my-self-labeling-interfaces)
               (s/and base-spec
                      ; Use `subset?` instead of `=`, because `(:dspec/interfaces %)` may test against
                      ; an entity (i.e., here the entity is `%`) that inherits from this spec. In this case,
                      ; the inheriting entity should pass this inherited spec. Equality would result in it not
                      ; passing the spec. Subset check is what we want.
                      #(clojure.set/subset? all-my-self-labeling-interfaces (:dspec/interfaces %)))
               base-spec)]
    {:spec spec
     :gen-factory (interface->generator-factory spec
                                                inherited-custom-generators
                                                custom-generator-factory
                                                all-my-self-labeling-interfaces
                                                all-inherited-interface-names
                                                req-keys
                                                opt-keys)}))

(def NATIVE-TYPES
  #{:keyword :string :boolean :long :bigint :float :double :bigdec :instant :uuid :uri :bytes})

(s/fdef interface-type?
        :args (s/cat :type keyword?)
        :ret boolean?)
(defn- interface-type?
  "Fields are associated with values that are either scalar values or
  values that are instances of dspec interfaces (i.e., are values that
  are hash maps) or collections of scalar values or interface values.
  This will return true if the field is associated with a value that
  is an interface instance or a collection of interface instances."
  [type]
  (not (contains? (conj NATIVE-TYPES :enum) type)))

(s/fdef interface-field?
        :args (s/cat :field :interface.ast/field)
        :ret boolean?)
(defn- interface-field?
  "Fields are associated with values that are either scalar values or
  values that are instances of dspec interfaces (i.e., are values that
  are hash maps) or collections of scalar values or interface values.
  This will return true if the field is associated with a value that
  is an interface instance or a collection of interface instances."
  [field]
  (interface-type? (:interface.ast.field/type field)))

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
(let [; Given the AST representation of a field, returns the appropriate predicate
      ; testing the validity of the field value. These predicates are intended to
      ; be used in defining and registering the appropriate clojure.spec spec for
      ; the field.
      ast-field-type->predicate
      {:keyword keyword?, :string string?, :boolean boolean?, :long integer?,
       :bigint  integer?, :float #?(:clj float? :cljs number?),
       :double  #?(:clj float? :cljs number?), :bigdec integer?, :instant inst?,
       :uuid    uuid?, :uri #?(:clj uri? :cljs string?), :bytes #?(:clj bytes? :cljs string?)}]
  (defn- field->clojure-specs
    "Returns the clojure.spec for the given `field`, with an optional `custom-generator-factory`"
    [field custom-generator-factory]
    (let [{:db/keys                  [ident cardinality]
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
                                                         ; TODO Replace s/gen with max-depth generator factory
                                                         #(s/gen single-predicate))]
                                #(custom-generator-factory member-gen-factory))
                              (if (and (= :string type) (nil? custom-generator-factory))
                                (case cardinality :db.cardinality/one #(gen/not-empty (gen/string-alphanumeric))
                                                  ; limit max-elements for perf reasons
                                                  :db.cardinality/many #(gen/set (gen/not-empty (gen/string-alphanumeric))
                                                                                 {:max-elements 3}))
                                custom-generator-factory))]
      (if generator-factory
        (s/with-gen predicate generator-factory)
        predicate))))

(s/fdef interface->clojure-specs
        :args (s/cat :ast :interface/ast
                     :ast/interface :interface.ast/interface
                     :gen-overrides (s/map-of keyword? :dummy/gen-factory)
                     ;:gen-overrides :interface/gen-overrides
                     )
        :ret (s/map-of keyword? any?))
(defn- interface->clojure-specs
  "Returns the clojure.spec macros that will register this interface and its respective fields as clojure.spec's."
  [ast interface gen-overrides]
  (let [{:keys [interface.ast.interface/name]} interface
        all-fields (ast&interface->ast-fields ast interface)
        {:keys [spec gen-factory]} (interface->clojure-spec&generator ast name gen-overrides)
        specs-by-name {name (s/with-gen spec
                                        (fn []
                                          (gen/such-that
                                            ; Datomic doesn't accept just
                                            ; interfaces with only :db/id
                                            #(< 1 (count (keys %)))
                                            (resize 1 (gen-factory)))))}]
    (->> all-fields
         (remove #(= (:db/ident %) :dspec/interfaces))
         (reduce (fn [specs-by-name {:keys [db/ident] :as field}]
                   (assoc specs-by-name ident (field->clojure-specs field (get gen-overrides ident))))
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
                     :gen-overrides (s/map-of keyword? :dummy/gen-factory)
                     ;:gen-overrides :interface/gen-overrides
                     )
        :ret :clojure.spec/macros)
(defn ast->clojure-specs
  "Given the intermediate AST and a mapping of field and interface names to test.check generators, this returns
  the clojure.spec macros that we can expand and evaluate later."
  [{:keys [interface.ast/interfaces] :as ast} gen-overrides]
  (reduce
    (fn [specs interface]
      (merge specs (interface->clojure-specs ast interface gen-overrides)))
    {:dspec/interfaces (s/coll-of keyword? :kind set?)}
    (vals interfaces)))

(defn- interface->field->frequency
  [ast interface-name]
  (let [interface (get-in ast [:interface.ast/interfaces interface-name])
        all-fields (ast&interface->ast-fields ast interface)
        {req true, opt true, :or {req [], opt []}} (group-by :interface.ast.field/required all-fields)]
    (into {}
          (concat
            (->> req (map :db/ident) (map (fn [ident] [ident 100])))
            (->> opt (map :db/ident) (map (fn [ident] [ident (/ 100 (count opt))])))))))



; TODO Remove
(defn- xxx
  [field->frequency])

(defn- maybe-throw-depth-violation!
  [required-keys-assoc-with-hash-map-vals max-depth]
  (if (and (pos? (count required-keys-assoc-with-hash-map-vals))
           (<= max-depth 1))
    (throw (ex-info "Max depth incompatible with required keys associated with a hash map value."
                    {:max-depth max-depth
                     :req-hash-map-keys required-keys-assoc-with-hash-map-vals}))))

(s/fdef create-gen-overrides-with-max-depth
        :args (s/cat :spec-name keyword?
                     :assoc-with-hash-map? fn?
                     :max-depth (s/and integer? pos?)
                     :path (s/? (s/coll-of keyword? :kind vector?)))
        :ret (s/map-of vector? :dummy/gen-factory))
; TODO Incorporate custom map generator factories that take into account
;      max depth?
(defn create-gen-overrides-with-max-depth
  "Intended to be used with (clojure.spec/gen spec overrides).
  Creates overrides that result in generated data from spec, such that
  the generated data does not exceed a tree depth of max-depth."
  ([spec assoc-with-hash-map? max-depth]
   (create-gen-overrides-with-max-depth spec assoc-with-hash-map? max-depth []))
  ([spec assoc-with-hash-map? max-depth path]
   (if-not (is-keys-spec? spec)
     {}
     (let [{:keys [req opt]} (extract-spec-keys spec)]
       (if (< 1 max-depth)
         (->> (into req opt)
              (filter (comp assoc-with-hash-map? s/get-spec))
              (map #(create-gen-overrides-with-max-depth (s/get-spec %) assoc-with-hash-map? (dec max-depth) (conj path %)))
              (apply merge))
         (do
           (maybe-throw-depth-violation! (filter assoc-with-hash-map? req) max-depth)
           {path #(eval `(s/keys :req ~(filter (complement assoc-with-hash-map?) req)
                                 :opt ~(filter (complement assoc-with-hash-map?) opt)))}))))))

(s/fdef spec-key-assoc-with-hash-map?
        :args (s/cat :spec-key keyword?)
        :ret boolean?)
(defn- spec-key-assoc-with-hash-map?
  [spec-key]
  (->> spec-key
       s/get-spec
       s/describe
       (s/conform :clojure.spec/keys-expr)
       (not= :clojure.spec/invalid)))

(s/fdef create-top-level-gen-with-max-depth
        :args (s/cat :spec keyword?
                     :assoc-with-hash-map? fn?
                     :max-depth (s/and integer? pos?))
        :ret generator?)
(defn- create-top-level-gen-with-max-depth
  "Used with (clojure.spec/with-gen spec gen). This will adjust the generator
  appropriately so that if max-depth is 1 or 0, then this will generate only
  keys that are associated with shallow values (i.e., not hash map values)."
  [spec assoc-with-hash-map? max-depth]
  (if-not (is-keys-spec? spec)
    (throw (ex-info "Spec does not correspond to a hash map." {:spec spec}))
    (let [{:keys [req opt]} (extract-spec-keys spec)]
      (s/gen (if (< 1 max-depth)
               spec
               (do
                 (maybe-throw-depth-violation! (filter assoc-with-hash-map? req) max-depth)
                 (eval `(s/keys :req ~(filter (complement assoc-with-hash-map?) req)
                                :opt ~(filter (complement assoc-with-hash-map?) opt)))))))))

; TODO Generate proper :dspec/interfaces
(defn- create-dspec-interfaces-field-gen-factory
  [spec])

(s/fdef interface&max-depth->path->interface-name
        :args (s/cat :ast :interface/ast
                     :interface-name keyword?
                     :max-depth (s/and integer? pos?)
                     :prefix-path (s/coll-of keyword? :kind vector?))
        :ret (s/map-of (s/coll-of keyword? :kind vector?)
                       keyword?))
(defn- interface&max-depth->path->interface
  "Given a tree of type interface-name where the tree has max-depth,
  find all possible nested nodes that have an interface type and return
  a hash map that maps the paths to those possible nodes to the interface
  name the node's value is an instant of."
  [ast interface-name max-depth prefix-path]
  (if (>= 1 max-depth)
    {}
    (let [interface-fields
          (->> (get-in ast [:interface.ast/interfaces interface-name :interface.ast.interface/fields])
               vals
               (filter #(interface-type? (:interface.ast.field/type %))))

          path->interface
          (->> interface-fields
               (map #(let [next-prefix-path (conj prefix-path (:db/ident %))
                           interface-name (:interface.ast.field/type %)]
                       [next-prefix-path interface-name]))
               (into {}))]
      (->> path->interface
           (map (fn [[path interface-name]]
                  (interface&max-depth->path->interface ast interface-name (dec max-depth) path)))
           (apply merge path->interface)))))

(s/fdef create-path->dspec-interfaces-gen-factories
        :args (s/cat :ast :interface/ast
                     :interface-name keyword?
                     :max-depth (s/and integer? pos?))
        :ret (s/map-of (s/coll-of keyword? :kind vector?)
                       :dummy/gen-factory))
(defn- create-path->dspec-interfaces-gen-factories
  "For a given dspec interface and desired max-depth that limits
  the depth of the generated tree, return a hash map that associates
  a path (represented as a vector of attributes to get from the root
  to the value) to a generator factory that returns a generator that
  generates a value at that path."
  [ast interface-name max-depth]
  (let [path->interface-name (interface&max-depth->path->interface ast interface-name max-depth [])]
    ))

; TODO Remove
(comment
  (defn- create-max-depth-gen-factory
    [spec assoc-with-hash-map?]
    (fn [max-depth]
      (let [{native-req false, hash-map-req true
             :or {native-req [], hash-map-req []}} (group-by assoc-with-hash-map? req)
            {native-opt false, hash-map-opt true
             :or {native-opt [], hash-map-opt []}} (group-by assoc-with-hash-map? opt)
            native-keys-gen (s/gen `(s/keys :req ~native-req :opt ~native-opt))
            _ (maybe-throw-depth-violation! hash-map-req max-depth)
            hash-map-keys-gen (if (> max-depth 1)
                                (s/gen `(s/keys :req ~hash-map-req :opt ~hash-map-opt))
                                (gen/return {}))]
        {; Used with s/with-gen. Controls what keys to include.
         :generator (gen/fmap (partial apply merge) (gen/tuple native-keys-gen hash-map-keys-gen))
         ; Used with (s/gen spec overrides). Controls depth of child keys.
         :overrides (create-gen-overrides spec assoc-with-hash-map? max-depth [])}))))

(s/def ::req-interface-fields (s/coll-of :interface.ast/field))
(s/def ::req-native-fields (s/coll-of :interface.ast/field))
(s/def ::opt-interface-fields (s/coll-of :interface.ast/field))
(s/def ::opt-native-fields (s/coll-of :interface.ast/field))
(s/fdef ast&interface-name->partitioned-fields
        :args (s/cat :ast :interface/ast
                     :interface-name keyword?)
        :ret (s/keys :req-un [::req-interface-fields ::req-native-fields
                              ::opt-interface-fields ::opt-native-fields]))
(defn- ast&interface-name->partitioned-fields
  "Returns the fields of an interface, partitioned by:
  1. Required native-type fields.
  2. Optional native-type fields.
  3. Required interface-type fields.
  4. Optional interface-type fields."
  [ast interface-name]
  (let [interface (get-in ast [:interface.ast/interfaces interface-name])
        all-fields (ast&interface->ast-fields ast interface)

        {req-fields true, opt-fields nil
         :or {req-fields [] opt-fields []}} (group-by :interface.ast.field/required all-fields)

        {req-interface-fields true, req-native-fields false
         :or {req-interface-fields [], req-native-fields []}} (group-by interface-field? req-fields)

        {opt-interface-fields true, opt-native-fields false
         :or {opt-interface-fields [], opt-native-fields []}} (group-by interface-field? opt-fields)]
    {:req-interface-fields req-interface-fields :req-native-fields req-native-fields
     :opt-interface-fields opt-interface-fields :opt-native-fields opt-native-fields}))

(s/fdef gen-native-only-hash-map
        :args (s/cat :req-native-fields (s/coll-of :interface.ast/field)
                     :opt-native-fields (s/coll-of :interface.ast/field))
        :ret generator?)
(defn- gen-native-only-hash-map
  "Generator for generating native-type fields (e.g., keyword, string, boolean)"
  [req-native-fields opt-native-fields]
  (let [native-only-spec (eval
                           ; Special quoting and evaling is necessary because s/keys is a macro
                           ; and cannot resolve req-native-fields at runtime if not inside a
                           ; quoted clojure.spec expression.
                           `(s/keys
                              :req ~(->> req-native-fields
                                         (map :db/ident)
                                         ; We rm :dspec/interfaces from the spec because
                                         ; we want to hard code the exact values later instead of
                                         ; letting the generator resulting from s/gen generate it
                                         ; for us.
                                         (remove #{:dspec/interfaces})
                                         (into [:db/id]))
                              :opt ~(mapv :db/ident opt-native-fields)))]
    (s/gen native-only-spec)))

(declare gen-with-max-depth)

(s/fdef gen-interface-only-hash-map
        :args (s/cat :req-interface-fields (s/coll-of :interface.ast/field)
                     :opt-interface-fields (s/coll-of :interface.ast/field)
                     :max-depth (s/and integer? pos?)
                     :ast :interface/ast
                     :gen-overrides (s/map-of keyword? :dummy/gen-factory-1))
        :ret generator?)
(defn- gen-interface-only-hash-map
  "Returns a generator that only generates the required and some optional
  keys and values of an interface, limited only to the values that are of
  interface-type (i.e., not native-type)."
  [req-interface-fields opt-interface-fields max-depth ast gen-overrides]
  (let [; Generates a random vector of required and optional fields for the given
        ; interface.
        interface-fields-gen (if (< 1 max-depth)
                               (gen/fmap
                                 (fn [num-opt-interface-fields-to-generate]
                                   (->> (shuffle opt-interface-fields)
                                        (take num-opt-interface-fields-to-generate)
                                        (concat req-interface-fields)))
                                 (gen/choose 0 (count opt-interface-fields)))
                               (gen/return []))]
    (gen/bind
      interface-fields-gen
      (fn [interface-fields]
        (->> interface-fields
             (mapcat (fn [field]
                       (let [key (:db/ident field)
                             interface-name (:interface.ast.field/type field)
                             default-max-depth-gen (gen-with-max-depth ast gen-overrides (dec max-depth) interface-name)
                             val (if-let [gen-factory (get gen-overrides interface-name)]
                                   (gen-factory (constantly default-max-depth-gen))
                                   default-max-depth-gen)]
                         [key val])))
             (apply gen/hash-map))))))

(s/fdef gen-with-max-depth
        :args (s/cat :ast :interface/ast
                     :gen-overrides (s/map-of keyword? :dummy/gen-factory)
                     :max-depth (s/and integer? pos?)
                     :interface-name keyword?)
        :ret generator?)
(defn gen-with-max-depth
  "Creates a generator with a max tree depth. Useful to keep generators from ballooning."
  [ast gen-overrides max-depth interface-name]
  (let [{:keys [req-interface-fields req-native-fields
                opt-interface-fields opt-native-fields]}
        (ast&interface-name->partitioned-fields ast interface-name)
        ; TODO Warn if nested-ness conflicts with max-depth

        native-only-gen (gen-native-only-hash-map req-native-fields opt-native-fields)

        interface-only-gen (gen-interface-only-hash-map req-interface-fields opt-interface-fields max-depth ast gen-overrides)

        self-label-gen (let [all-inherited-interface-names (get-all-inherited-interface-names ast interface-name)
                             all-my-self-labeling-interfaces (set (filter
                                                                    #(contains? (:interface.ast/enum-map ast) %)
                                                                    (conj all-inherited-interface-names interface-name)))]
                         (if (seq all-my-self-labeling-interfaces)
                           (gen/return {:dspec/interfaces all-my-self-labeling-interfaces})
                           (gen/return {})))
        combined-interface-gen (gen/fmap #(apply merge %)
                                         (gen/tuple native-only-gen interface-only-gen self-label-gen))
        ; TODO But we should figure out how max-depth, custom generators, and our built-in combined generator work together.
        generator (if-let [custom-gen-factory (get gen-overrides interface-name)]
                    (custom-gen-factory (constantly combined-interface-gen))
                    combined-interface-gen)]

    ; TODO Remove later. In here for debugging purposes
    (when (= interface-name :interface/violates-such-that)
      (println (keys gen-overrides))
      (println interface-name))

    (tcgen/such-that #(s/valid? interface-name %)
                     generator
                     ; TODO Uncomment this when test.check reaches 0.9.1
                     ;                     {:max-tries 10
                     ;                      :ex-fn (fn [{:keys [gen pred max-tries]}]
                     ;                               (ex-info (str "Spec " interface-name
                     ;                                             " and/or its custom generators are defined in such a way that it is possible to generate data such that it does not satisfy the spec.")
                     ;                                        {:spec-name interface-name
                     ;                                         :pred pred}))}
                     )))

(s/fdef validate-generators-for-likely-such-that-violations!
        :args (s/cat :ast :interface/ast
                     :gen-map (s/map-of keyword? :dummy/gen-factory)
                     ;:gen-map :interface/gen-map
                     :sorted-deps (s/coll-of keyword?)
                     :desired-confidence (s/and integer? pos?)))
(defn- validate-generators-for-likely-such-that-violations!
  "Our spec generators are defined implicitly with gen/such-that such that generated data should
  satisfy the spec. We may end up passing in custom generators via gen-map. This function makes it easier
  to understand what might be causing the gen/such-that violation. Otherwise, we would have no hints
  as to what might be causing the violations."
  [ast gen-map sorted-deps desired-confidence]
  (doseq [spec-name (->> sorted-deps
                         (filter #(get-in ast [:interface.ast/interfaces %]))
                         (filter #(contains? gen-map %)))]
    (println spec-name)
    (let [max-depth 5
          generator (gen-with-max-depth ast gen-map max-depth spec-name)]
      (gen/sample generator desired-confidence))))

(s/fdef validate-generators-for-likely-unique-violations!
        :args (s/cat :ast :interface/ast
                     :gen-overrides (s/map-of keyword? :dummy/gen-factory)
                     ;:gen-overrides :interface/gen-overrides
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
  [ast gen-overrides]
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
      (if-not (contains? gen-overrides field-name)
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
  (println "validate unique")
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
        (s/with-gen db-id? #(fn->gen (fn [] (tempid-factory :db.part/user)))))
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