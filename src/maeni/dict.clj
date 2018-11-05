(ns maeni.dict
  (:require [clojure.string :as str]))

(defn find-word [dict token]
  (first (get dict (str/upper-case token))))

(defn add-word [dict w]
  (update dict (str/upper-case (:name w)) conj w))
