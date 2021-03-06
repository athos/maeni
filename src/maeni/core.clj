(ns maeni.core
  (:require [maeni.builtins :as builtins]
            [maeni.vm :as vm]))

(defn make-vm
  ([] (make-vm nil))
  ([text] (make-vm (builtins/builtin-words) text))
  ([init text]
   (atom (vm/make-vm init text))))

(defn run
  ([text] (run (make-vm) text))
  ([vm text]
   (swap! vm assoc :text text)
   (vm/run vm)
   (into [] (:dstack @vm))))
