(ns maeni.core)

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

(defn next-word [cs]
  (let [[word cs'] (->> (drop-while #{\space} cs)
                        (read-until \space false))]
    [(apply str word) cs']))
