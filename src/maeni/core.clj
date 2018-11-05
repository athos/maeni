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

(defn next-token [cs]
  (let [[token cs'] (->> (drop-while #{\space} cs)
                        (read-until \space false))]
    [(apply str token) cs']))

(def default-dict (atom {}))

(defn make-vm
  ([] (make-vm nil))
  ([text] (make-vm @default-dict text))
  ([init-dict text]
   {:mode :interpret
    :dstack ()
    :dict init-dict
    :text text}))

(defn find-word [dict token]
  (first (get dict token)))

(defn add-word [dict w]
  (update dict (:name w) conj w))

(defn- try-coerce [token]
  (try
    (Long/parseLong token)
    (catch Throwable t nil)))

(defn run1 [vm token]
  (if-let [w (or (find-word (:dict vm) token) (try-coerce token))]
    (if (= (:mode vm) :compile)
      (if (:immediate w)
        ((:compiled-code w) vm)
        (update vm :code conj (:code w)))
      (if (number? w)
        (update vm :dstack conj w)
        ((:compiled-code w) vm)))
    (throw (ex-info (str "No such word: " token) {:token token}))))

(defn run
  ([vm]
   (loop [{:keys [text] :as vm} vm]
     (if (empty? text)
       vm
       (let [[token cs] (next-token text)]
         (recur (run1 (assoc vm :text cs) token))))))
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

(defword ":" {:immediate true}
  (let [[token text] (next-token (:text &vm))]
    (assoc &vm :current-word token :mode :compile :code [] :text text)))

(defword ";" {:immediate true}
  (let [code (:code &vm)
        word {:name (:current-word &vm)
              :compiled-code (eval `(fn [vm#] (as-> vm# ~'&vm ~@(seq code))))}]
    (-> &vm
        (update :dict add-word word)
        (assoc :mode :interpret :current-word nil :code nil))))
