(ns org.lab79.datomic-spec.gen
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
  (fn [keys-spec-macro]
    (let [{req-keys :req optional-keys :opt} (apply hash-map (rest keys-spec-macro))
          optional-keys-to-generate (clojure.set/difference (set optional-keys) (set ensure-keys))
          optional-for-hash-map (mapv #(vec [% `(gen/delay (s/gen ~%))]) optional-keys-to-generate)
          ensured-for-hash-map (mapv #(vec [% `(s/gen ~%)]) (into ensure-keys req-keys))]
      `(gen/bind
         ; Determine how many optional keys to generate
         (gen/choose 0 ~(count optional-keys-to-generate))
         (fn [~'num-optional-keys-to-generate]
           (let [~'args-in-kv-pairs (concat ~ensured-for-hash-map (shuffle ~optional-for-hash-map))]
             (->> ~'args-in-kv-pairs
                  (take (+ ~'num-optional-keys-to-generate ~(count ensured-for-hash-map)))
                  (apply concat)
                  (apply gen/hash-map))))))))