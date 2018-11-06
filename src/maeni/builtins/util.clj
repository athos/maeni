(ns maeni.builtins.util
  (:require [maeni.dict :as dict]
            [maeni.reader :as reader]
            [maeni.vm :as vm]))

(defn with-next-word [f]
  (let [[token text] (reader/next-token (:text @vm/*vm*))
        w (dict/find-word (:dict @vm/*vm*) token)]
    (f w)
    (swap! vm/*vm* assoc :text text)))

(defn with-next-string [f]
  (let [[s text] (reader/read-string (:text @vm/*vm*))]
    (f s)
    (swap! vm/*vm* assoc :text text)))

(defn emit-combined-code [code]
  `(do ~@code))
