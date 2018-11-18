(ns maeni.stack
  (:refer-clojure :exclude [pop! peek])
  (:import [java.util ArrayDeque Collection]))

(definterface IStack
  (push [^long val])
  (^long pop [])
  (^long peek []))

(deftype Stack [^longs arr ^:unsynchronized-mutable ^long index]
  IStack
  (push [this val]
    (aset arr index val)
    (set! index (inc index)))
  (pop [this]
    (set! index (dec index))
    (aget arr index))
  (peek [this]
    (aget arr (dec index)))
  clojure.lang.Seqable
  (seq [this] (take index arr)))

(defn ^Stack make-stack
  ([size] (->Stack (long-array size) 0))
  ([size coll]
   (->Stack (long-array size coll) 0)))

(defn with-tag [x]
  (with-meta x {:tag 'maeni.stack.Stack}))

(defmacro push! [s e]
  `(.push ~s ~e))

(defmacro pop! [s]
  `(.pop ~s))

(defmacro peek [s]
  `(.peek ~s))
