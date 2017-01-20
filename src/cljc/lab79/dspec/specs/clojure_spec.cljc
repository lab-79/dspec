(ns lab79.dspec.specs.clojure-spec
  "clojure.spec's related to working with clojure.spec declarations and their associated generators."
  (:require [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clojure.test.check.generators :refer [generator?]]))

; clojure.spec for defining custom generator maps for our fields
(s/def :interface/gen-map (s/map-of keyword? (s/or :no-member-gen-factory :gen/generator-factory
                                                   :with-member-gen-factory :gen/generator-factory-with-member)))

; TODO Get more specific than any?
(s/def :clojure.spec/deps-graph any?)
(s/def :clojure.spec/macros (s/map-of keyword? any?))

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