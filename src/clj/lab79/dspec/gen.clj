(ns lab79.dspec.gen
  (:require [clojure.spec :as s]
            [clojure.spec.gen :as gen]))

;; Helpers to use in defining generator definitions

(defn fn->gen [func]
  "Given a zero-arity function, create a generator factory out of it, whose
  generated values are created by func."
  #(gen/fmap (fn [_] (func)) (gen/return nil)))

(defn only-keys-gen
  "Returns a generator factory that ensures every generated map has ONLY
  the keys specified by `kws`."
  [& kws]
  (fn [_]
    (let [kv (flatten (map
                        (fn [kw]
                          [kw (gen/such-that #(not (nil? %)) (s/gen kw))])
                        kws))]
      (apply gen/hash-map kv))))

(defn ensure-keys-gen
  "Returns a generator factory that ensures every generated map has at least
  the keys specified by `field-keys`."
  [& ensure-keys]
  (fn [base-map-spec-factory]
    (s/gen (s/merge (base-map-spec-factory)
                    ; clojure.spec/keys is a macro so it can't take a symbol like `ensure-keys` directly
                    (eval `(s/keys :req ~ensure-keys))))))
