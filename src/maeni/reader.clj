(ns maeni.reader
  (:refer-clojure :exclude [read-string]))

(defn read-until [delimiter delimiter-mandatory? cs]
  (loop [cs cs, ret []]
    (if (empty? cs)
      (if delimiter-mandatory?
        (throw (ex-info (str "missing " delimiter) {}))
        [ret nil])
      (let [c (first cs)]
        (if (= c delimiter)
          [ret (next cs)]
          (recur (next cs) (conj ret c)))))))

(defn read-string [text]
  (let [[ccc text'] (read-until \" true text)]
    [(apply str ccc) text']))

(defn next-token [cs]
  (let [[token cs'] (->> (drop-while #{\space} cs)
                         (read-until \space false))]
    [(apply str token) cs']))
