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
  [& field-keys]
  (fn [keys-spec-macro]
    (let [{req-keys :req optional-keys :opt} (apply hash-map (rest keys-spec-macro))
          ensure-keys (set field-keys)
          ensured-keys (into ensure-keys req-keys)
          opt-keys (clojure.set/difference (set optional-keys) ensure-keys)
          egen (fn [k] [k `(s/gen ~k)])
          ogen (fn [k] [k `(gen/delay (s/gen ~k))])]
      `(gen/bind (gen/choose 0 (count ~opt-keys))
                (fn [~'lower]
                  (let [~'ensures ~(into #{} (map egen ensured-keys))
                        ~'opts ~(into #{} (map ogen opt-keys))
                        ~'args (concat (seq ~'ensures) (when (seq ~'opts) (shuffle (seq ~'opts))))]
                    (->> ~'args
                         (take (+ ~'lower (count ~'ensures)))
                         (apply clojure.core/concat)
                         (apply gen/hash-map))))))))
