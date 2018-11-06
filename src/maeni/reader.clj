(ns maeni.reader
  (:refer-clojure :exclude [read-string]))

(defn read-until [delimiters delimiter-mandatory? cs]
  (let [delims (set delimiters)]
    (loop [cs cs, ret []]
      (if (empty? cs)
        (if delimiter-mandatory?
          (throw (ex-info (str "missing " (first delimiters)) {:delimiters delims}))
          [ret nil])
        (let [c (first cs)]
          (if (contains? delims c)
            [ret (next cs)]
            (recur (next cs) (conj ret c))))))))

(defn read-string [text]
  (let [[ccc text'] (read-until #{\"} true text)]
    [(apply str ccc) text']))

(defn next-token [cs]
  (let [ws #{\space \tab \newline}
        [token cs'] (->> (drop-while ws cs)
                         (read-until ws false))]
    [(apply str token) cs']))
