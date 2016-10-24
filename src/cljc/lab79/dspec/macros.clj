(ns lab79.dspec.macros
  (:require [clojure.spec :as s]))

; ast&interface->ast-fields
; interface->clojure-spec&generator

;(defmacro interface->clojure-specs
;  "Returns the clojure.spec macros that will register this interface and its respective fields as clojure.spec's."
;  [ast interface gen-map]
;  `(let [{:keys [interface.ast.interface/name]} ~interface
;         ~'all-fields (~ast&interface->ast-fields ~ast ~interface)
;         {:keys [~'spec ~'gen-factory]} (~interface->clojure-spec&generator ~ast ~'name ~gen-map)
;         ~'specs-by-name {~'name (s/with-gen ~'spec
;                                             (fn []
;                                               (gen/such-that
;                                                 #(< 1 (count (keys %)))
;                                                 (~'gen-factory))))}]
;     (->> ~'all-fields
;          (filter #(not= (:db/ident %) :datomic-spec/interfaces))
;          (reduce (fn [specs-by-name {:keys [db/ident] :as field}]
;                    (assoc specs-by-name ident (field->clojure-specs field (get gen-map ident))))
;                  specs-by-name))))
