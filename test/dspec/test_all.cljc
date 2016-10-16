(ns dspec.test-all
  #?(:cljs (:require
             [cljs.test :refer-macros [run-tests]]
             [cljs.nodejs :as nodejs]
             ;dspec.core-test
             dspec.gen-test
             dspec.helpers-test
             dspec.util-test
             )))

#?(:cljs (enable-console-print!))
#?(:cljs (nodejs/enable-util-print!))

#?(:cljs (defn -main [& args]
           ;(run-tests 'dspec.core-test)
           (run-tests 'dspec.gen-test)
           (run-tests 'dspec.helpers-test)
           (run-tests 'dspec.util-test)
           ))

#?(:cljs (set! *main-cli-fn* -main))