(ns lab79.dspec.ast
  (:require [clojure.spec :as s]))

(s/fdef ast&interface->ast-fields
        :args (s/cat :ast :interface/ast
                     :ast/interface :interface.ast/interface)
        :ret (s/coll-of :interface.ast/field :kind set?))
(defn ast&interface->ast-fields
  "Returns the set of all fields that correspond to a dspec interface."
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
(defn get-all-inherited-interface-names
  "Returns the union of all immediately inherited and recursively inherited interfaces
  for a given interface represented by `interface-name`."
  [ast interface-name]
  (let [interface (-> ast :interface.ast/interfaces interface-name)
        {:keys [interface.ast.interface/inherits]} interface]
    (into inherits (mapcat #(get-all-inherited-interface-names ast %) inherits))))