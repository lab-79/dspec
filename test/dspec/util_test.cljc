(ns dspec.util-test
  (:require [clojure.test :refer [deftest is]]
            lab79.dspec.util
            [dspec.util :refer [db-id? instrument-all!]]))

#?(:cljs (enable-console-print!))

; Instrument all our functions in dspec
(instrument-all!)

; TODO Turn these tests back on
(deftest util-fn-fuzzing
;  (is (->> (stest/check `filter-kv)
;           (every? #(true? (get-in % [:clojure.spec.test.check/ret :result])))))
  )
