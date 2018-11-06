(ns maeni.stack
  (:refer-clojure :exclude [pop! peek])
  (:import [java.util ArrayDeque Collection]))

(defn make-stack
  ([] (ArrayDeque.))
  ([^Collection coll]
   (ArrayDeque. coll)))

(defn- with-tag [x]
  (with-meta x {:tag 'java.util.ArrayDeque}))

(defmacro push! [s e]
  `(.push ~(with-tag s) ~e))

(defmacro pop! [s]
  `(long (.pop ~(with-tag s))))

(defmacro peek [s]
  `(long (.peek ~(with-tag s))))
