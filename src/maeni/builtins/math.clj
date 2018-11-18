(ns maeni.builtins.math
  (:require [maeni.builtins.core :refer [defword]]
            maeni.stack))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defword +
  (let [x (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (+ (maeni.stack/pop! &dstack) x))))

(defword -
  (let [x (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (- (maeni.stack/pop! &dstack) x))))

(defword *
  (let [x (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (* (maeni.stack/pop! &dstack) x))))

(defword /
  (let [x (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (quot (maeni.stack/pop! &dstack) x))))

(defword "1+"
  (maeni.stack/push! &dstack (inc (maeni.stack/pop! &dstack))))

(defword "1-"
  (maeni.stack/push! &dstack (dec (maeni.stack/pop! &dstack))))

(defword "/mod"
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (rem y x))
    (maeni.stack/push! &dstack (quot y x))))

(defmacro bool->int [b]
  `(if ~b -1 0))

(defword =
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (maeni.builtins.math/bool->int (= y x)))))

(defword "~"
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (bit-not (maeni.builtins.math/bool->int (= y x))))))

(defword <
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (maeni.builtins.math/bool->int (< y x)))))

(defword >
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (maeni.builtins.math/bool->int (> y x)))))

(defword "0="
  (let [x (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (bit-not (bit-shift-right (bit-xor x (- x)) 63)))))

(defword "0<"
  (maeni.stack/push! &dstack (bit-shift-right (maeni.stack/pop! &dstack) 63)))

(defword and
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (bit-and y x))))

(defword or
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (bit-or y x))))
