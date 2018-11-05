(ns maeni.core
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

(defn next-word [cs]
  (let [[word cs'] (->> (drop-while #{\space} cs)
                        (read-until \space false))]
    [(apply str word) cs']))

(def default-dict (atom {}))

(defn make-vm
  ([] (make-vm nil))
  ([text] (make-vm @default-dict text))
  ([init-dict text]
   {:mode :interpret
    :dstack ()
    :dict init-dict
    :text text}))

(defn find-word [dict w]
  (first (get dict w)))

(defn add-word [dict w]
  (update dict (:name w) conj w))

(defn- try-coerce [word]
  (try
    (Long/parseLong word)
    (catch Throwable t nil)))

(defn run1 [vm word]
  (if-let [w (or (find-word (:dict vm) word) (try-coerce word))]
    (if (= (:mode vm) :compile)
      (if (:immediate w)
        ((:compiled-code w) vm)
        (update vm :code conj (:code w)))
      (if (number? w)
        (update vm :dstack conj w)
        ((:compiled-code w) vm)))
    (throw (ex-info (str "No such word: " word) {:word word}))))

(defn run
  ([vm]
   (loop [{:keys [text] :as vm} vm]
     (if (empty? text)
       vm
       (let [[word cs] (next-word text)]
         (recur (run1 (assoc vm :text cs) word))))))
  ([vm text]
   (run (assoc vm :text text))))

(defmacro defword [name maybe-attrs & body]
  (let [{:keys [immediate] :as attrs} (if (map? maybe-attrs)
                                        maybe-attrs
                                        {})
        body (if (map? maybe-attrs)
               body
               (cons maybe-attrs body))]
    `(let [w# (array-map :name ~(str name)
                         :code '(do ~@body)
                         :compiled-code (fn [~'&vm] ~@body)
                         ~@(when immediate
                             [:immediate true]))]
       (swap! default-dict add-word w#)
       '~name)))

(defword +
  (let [{[x y & more] :dstack} &vm]
    (assoc &vm :dstack (cons (+ y x) more))))

(defword -
  (let [{[x y & more] :dstack} &vm]
    (assoc &vm :dstack (cons (- y x) more))))

(defword *
  (let [{[x y & more] :dstack} &vm]
    (assoc &vm :dstack (cons (* y x) more))))

(defword /
  (let [{[x y & more] :dstack} &vm]
    (assoc &vm :dstack (cons (quot y x) more))))

(defword dup
  (let [{[x & more] :dstack} &vm]
    (assoc &vm :dstack (list* x x more))))

(defword over
  (let [{[x y & more] :dstack} &vm]
    (assoc &vm :dstack (list* y x y more))))

(defword rot
  (let [{[x y z & more] :dstack} &vm]
    (assoc &vm :dstack (list* z x y more))))

(defword "s\""
  (let [[s text] (read-string (:text &vm))]
    (assoc &vm :dstack s :text text)))

(defword ".\""
  (let [[s text] (read-string (:text &vm))]
    (print s)
    (assoc &vm :text text)))

(defword "("
  (let [[_ text] (read-until \) true (:text &vm))]
    (assoc &vm :text text)))
