(ns maeni.builtins.math
  (:require [maeni.builtins.core :refer [defword]]
            maeni.stack))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defword +
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (+ y x))))

(defword -
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (- y x))))

(defword *
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (* y x))))

(defword /
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (quot y x))))

(defn bool->int [b]
  (if b 1 0))

(defword =
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (maeni.builtins.math/bool->int (= y x)))))

(defword "~"
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (maeni.builtins.math/bool->int (not= y x)))))

(defword <
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (maeni.builtins.math/bool->int (< y x)))))

(defword >
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (maeni.builtins.math/bool->int (> y x)))))

(defword and
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (maeni.builtins.math/bool->int (and (not (zero? y)) (not (zero? x)))))))

(defword or
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (maeni.builtins.math/bool->int (or (not (zero? y)) (not (zero? x)))))))
