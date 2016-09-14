(ns lab79.dspec.util
  (:require [clojure.spec :as s]))

; TODO s/fdef commented out until we can figure out how to spec a higher order
; function whose input function can take on any number or args.  Currently,
; clojure.spec does not apparently support specs to handle functions that can
; take 0 or non-0 length args, against a function that has 0 arity.
;(s/fdef arity
;        :args (s/alt :f-0-arity   (s/fspec :args (s/cat)
;                                           :ret any?)
;                     :f-pos-arity (s/fspec :args (s/cat :&args any?)
;                                           :ret any?))
;        :ret boolean?)
(defn arity
  "Returns the arity -- i.e., the number of arguments -- of a function f."
  [f]
  (-> f class .getDeclaredMethods first .getParameterTypes alength))

(s/fdef filter-kv
        :args (s/cat :pred (s/fspec :args (s/cat :key any? :value any?)
                                    :ret boolean?)
                     :hash-map map?)
        :ret map?)
(defn filter-kv
  [pred hash-map]
  (into {} (filter (fn [[k v]] (pred k v)) hash-map)))

