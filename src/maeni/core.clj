(ns maeni.core
  (:refer-clojure :exclude [read-string])
  (:require [clojure.string :as str]))

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
  (first (get dict (str/upper-case token))))

(defn add-word [dict w]
  (update dict (str/upper-case (:name w)) conj w))

(defn- try-coerce [token]
  (try
    (Long/parseLong token)
    (catch Throwable t nil)))

(defn run1 [vm token]
  (if-let [w (or (find-word (:dict vm) token) (try-coerce token))]
    (if (= (:mode vm) :compile)
      (cond (:compile w) ((:compile w) vm)
            (:immediate w) ((:compiled-code w) vm)
            :else
            (let [code (if (number? w)
                         `(update ~'&vm :dstack conj ~w)
                         (:code (meta w)))]
              (update vm :code conj code)))
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

(defmacro defword [name & body]
  (let [name (str/upper-case (str name))
        {:keys [immediate compile] :as attrs} (meta &form)]
    `(let [w# (with-meta
                (array-map :name ~name
                           :compiled-code (fn [~'&vm] ~@body)
                           ~@(when immediate
                               [:immediate true])
                           ~@(when compile
                               [:compile compile]))
                {:code '(do ~@body)})]
       (swap! default-dict add-word w#)
       '~name)))

(defword .
  (let [{[x & more] :dstack} &vm]
    (print x)
    (assoc &vm :dstack more)))

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

(defn- bool->int [b]
  (if b 1 0))

(defword =
  (let [{[x y & more] :dstack} &vm]
    (assoc &vm :dstack (cons (bool->int (= y x)) more))))

(defword "~"
  (let [{[x y & more] :dstack} &vm]
    (assoc &vm :dstack (cons (bool->int (not= y x)) more))))

(defword <
  (let [{[x y & more] :dstack} &vm]
    (assoc &vm :dstack (cons (bool->int (< y x)) more))))

(defword >
  (let [{[x y & more] :dstack} &vm]
    (assoc &vm :dstack (cons (bool->int (> y x)) more))))

(defword and
  (let [{[x y & more] :dstack} &vm]
    (assoc &vm :dstack (cons (bool->int (and (not (zero? y)) (not (zero? x)))) more))))

(defword or
  (let [{[x y & more] :dstack} &vm]
    (assoc &vm :dstack (cons (bool->int (or (not (zero? y)) (not (zero? x)))) more))))

(defword dup
  (let [{[x & more] :dstack} &vm]
    (assoc &vm :dstack (list* x x more))))

(defword swap
  (let [{[x y & more] :dstack} &vm]
    (assoc &vm :dstack (list* y x more))))

(defword over
  (let [{[x y & more] :dstack} &vm]
    (assoc &vm :dstack (list* y x y more))))

(defword rot
  (let [{[x y z & more] :dstack} &vm]
    (assoc &vm :dstack (list* z x y more))))

(defword drop
  (update &vm :dstack rest))

(defn- with-next-string [vm f]
  (let [[s text] (read-string (:text vm))]
    (assoc (f s) :text text)))

^{:compile (fn [vm]
             (with-next-string vm
               (fn [s]
                 (update vm :code conj `(update ~'&vm :dstack conj ~s)))))}
(defword "s\""
  (with-next-string &vm
    #(assoc &vm :dstack %)))

^{:compile (fn [vm]
             (with-next-string vm
               (fn [s]
                 (update vm :code conj `(do (print ~s) ~'&vm)))))}
(defword ".\""
  (with-next-string &vm
    (fn [s]
      (print s)
      &vm)))

^:immediate
(defword "("
  (let [[_ text] (read-until \) true (:text &vm))]
    (assoc &vm :text text)))

^:immediate
(defword ":"
  (let [[token text] (next-token (:text &vm))]
    (assoc &vm :current-word token :mode :compile :code [] :text text)))

^:immediate
(defword ";"
  (let [code (:code &vm)
        word {:name (:current-word &vm)
              :compiled-code (eval `(fn [vm#] (as-> vm# ~'&vm ~@(seq code))))}]
    (-> &vm
        (update :dict add-word word)
        (assoc :mode :interpret :current-word nil :code nil))))
