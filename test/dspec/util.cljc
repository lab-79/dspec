(ns dspec.util
  (:require [clojure.spec :as s]
            [clojure.test.check.generators :as tcgen :refer [generator?]]
            [lab79.dspec.plugins.clojure-spec :refer [NATIVE-TYPES]]
    #?@(:clj [[datomic.api :as d]
              [clojure.spec.test :as stest]
              [lab79.datomic-spec.gen-overrides :refer [sized-overrides-for]]])
    #?(:clj  [clojure.spec.gen :as gen]
       :cljs [cljs.spec.impl.gen :as gen]))
    #?(:clj (:import (datomic.db DbId))))

(def db-id? #(or (integer? %)
                 #?(:clj (instance? DbId %)
                    :cljs (= :db/current-tx %))))

(defn instrument-all! []
  (let [generator-overrides
        (merge
          (sized-overrides-for 5 :datomic.query.kv/where :datalog/clause)
          {:datomic.api/q #(gen/return d/q)
           :interface.ast.field/type #(s/gen NATIVE-TYPES)

           :datalog/expression-clause #(tcgen/resize 1 (s/gen :datalog/data-pattern))

           :interface/def #(tcgen/resize 1 (s/gen :interface/def))
           :interface.def/fields #(tcgen/resize 1 (s/gen :interface.def/fields))
           :interface.def.field.enum/vals-no-doc #(tcgen/resize 1 (s/gen :interface.def.field.enum/vals-no-doc))
           :interface.def.field.enum/vals-with-doc #(tcgen/resize 1 (s/gen :interface.def.field.enum/vals-with-doc))
           :interface.def/field #(gen/fmap (fn [m] (flatten (vals m))) (s/gen (s/keys :req [:interface.def.field/single-type] :opt [:db/doc])))
           :interface.def/identifying-enum-part #(gen/return :db.part/user)})]
    (-> (mapcat stest/enumerate-namespace
             ['lab79.dspec
              'lab79.dspec.plugins.clojure-spec
              'lab79.dspec.plugins.datomic
              'lab79.dspec.util.gen
              'lab79.dspec.helpers
              'lab79.dspec.parser
              'lab79.dspec.util])
        (stest/instrument generator-overrides))))