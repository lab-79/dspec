(ns dspec.util-test
  (:require [clojure.test :refer [deftest is]]
            lab79.dspec.util
            [clojure.spec.test :as stest]))

#?(:cljs (enable-console-print!))

; Instrument all our functions in dspec
(-> 'lab79.dspec.util
    stest/enumerate-namespace
    stest/instrument)

; TODO Turn these tests back on
(deftest util-fn-fuzzing
;  (is (->> (stest/check `filter-kv)
;           (every? #(true? (get-in % [:clojure.spec.test.check/ret :result])))))
  )
