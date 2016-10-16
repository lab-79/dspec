(ns lab79.eval
  #?(:cljs (:require [cljs.spec.impl.gen :as gen]
                     [cljs.js])))
; Appropriated from planck.
; https://github.com/mfikes/planck/blob/master/planck-cljs/src/planck/repl.cljs
#?(:cljs (defonce ^:private st (cljs.js/empty-state)))

#?(:cljs (defn eval*
           [form ns]
            (let [result (atom nil)]
               (cljs.js/eval st form
                 {:ns ns
                  :eval cljs.js/js-eval
                  :context       :expr
                  :def-emits-var true}
                 (fn [{:keys [value error]}]
                   (if error
                     (println error)
                     (reset! result value))))
               @result)))
