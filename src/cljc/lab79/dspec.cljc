; TODO Remove register-specs-for-ast-with-custom-generators!
; TODO Change arity of ast->clojure-specs to 1
; TODO Change arity of interface->clojure-specs to 2
; TODO Change arity of field->clojure-specs to 1

(ns lab79.dspec
  #?(:cljs (:require-macros
             cljs.spec
             [com.rpl.specter.macros :refer [select]]))
  (:require [clojure.pprint :refer [pprint]]
            lab79.dspec.specs.def
            lab79.dspec.specs.ast
            [lab79.dspec.parser :refer [parse-dspec]]
            [clojure.spec :as s]
            [com.rpl.specter :refer [MAP-VALS]]
            #?(:clj  [com.rpl.specter.macros :refer [select]])))

(s/fdef dspec->ast
        :args (s/cat :dspec :interface/def)
        :ret :interface/ast)
(defn dspec->ast
  [dspec]
  (parse-dspec dspec))

(s/fdef validate-semantic-ast!
        :args (s/cat :ast :interface/ast)
        :ret nil?)
(defn- validate-semantic-ast!
  [ast]
  (let [enum-vals (set (select [:interface.ast/enum-map MAP-VALS :db/ident] ast))
        interface-names (set (select [:interface.ast/interfaces MAP-VALS :interface.ast.interface/name] ast))
        primitives #{:keyword :string :boolean :long :bigint :float :double :bigdec :instant :uuid :bytes}
        valid-types (clojure.set/union primitives interface-names #{:enum})]
    (doseq [[interface-name {:keys [interface.ast.interface/fields]}] (:interface.ast/interfaces ast)]
      (doseq [[_ {:keys [interface.ast.field/type] :as field}] fields]
        (if-not (contains? valid-types type)
          (throw (ex-info (str "Invalid attribute type " type)
                          {:interface-name interface-name
                           :field field})))))))

(s/fdef dspec-coll->ast
        :args (s/cat :dspecs (s/spec (s/+ :interface/def)))
        :ret :interface/ast)
(defn dspec-coll->ast
  "Given a collection of semantic specs, generates a semantic ast"
  [dspecs]
  (let [asts (map dspec->ast dspecs)
        combined-ast (apply merge-with merge asts)]
    (validate-semantic-ast! combined-ast)
    combined-ast))