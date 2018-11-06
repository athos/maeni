(ns maeni.builtins.control
  (:require [maeni.builtins.core :refer [defword]]
            [maeni.builtins.util :as util]
            [maeni.stack :as s]
            [maeni.vm :as vm]))

^:immediate
(defword if
  (let [code (:code @vm/*vm*)]
    (swap! vm/*vm* assoc :code [])
    (swap! vm/*vm* update :cstack conj [code])))

^:immediate
(defword else
  (let [{[c & more] :cstack :keys [code]} @vm/*vm*]
    (swap! vm/*vm* assoc :code [] :cstack (cons (conj c code) more))))

^:immediate
(defword then
  (let [{[c & more] :cstack :keys [code]} @vm/*vm*
        [test then else] (conj c code)
        code `[(do ~(util/emit-combined-code test)
                   (if (not (zero? (s/pop! ~'&dstack)))
                     ~(util/emit-combined-code then)
                     ~@(when else
                         [(util/emit-combined-code else)])))]]
    (swap! vm/*vm* assoc :code code :cstack more)))

^:immediate
(defword recurse
  (let [address (:current-address @vm/*vm*)]
    (swap! vm/*vm* update :code conj
           `(vm/call-compiled-code ~'&cells ~address ~'&dstack))))
