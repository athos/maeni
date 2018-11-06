(ns maeni.builtins.stack
  (:require [maeni.builtins.core :refer [defword]]
            maeni.stack))

(defword dup
  (let [x (maeni.stack/peek &dstack)]
    (maeni.stack/push! &dstack x)))

(defword swap
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack x)
    (maeni.stack/push! &dstack y)))

(defword over
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/peek &dstack)]
    (maeni.stack/push! &dstack x)
    (maeni.stack/push! &dstack y)))

(defword rot
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)
        z (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack y)
    (maeni.stack/push! &dstack x)
    (maeni.stack/push! &dstack z)))

(defword drop
  (maeni.stack/pop! &dstack))
