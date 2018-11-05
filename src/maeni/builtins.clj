(ns maeni.builtins
  (:require [clojure.string :as str]
            [maeni.dict :as dict]
            [maeni.reader :as reader]
            [maeni.stack]))

(def ^:private builtin-words* (atom {}))

(defn builtin-words []
  @builtin-words*)

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
       (swap! builtin-words* dict/add-word w#)
       '~name)))

(defword .
  (let [&dstack (:dstack &vm)
        x (maeni.stack/pop! &dstack)]
    (print x)
    &vm))

(defword +
  (let [&dstack (:dstack &vm)
        x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (+ y x))
    &vm))

(defword -
  (let [&dstack (:dstack &vm)
        x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (- y x))
    &vm))

(defword *
  (let [&dstack (:dstack &vm)
        x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (* y x))
    &vm))

(defword /
  (let [&dstack (:dstack &vm)
        x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (quot y x))
    &vm))

(defn- bool->int [b]
  (if b 1 0))

(defword =
  (let [&dstack (:dstack &vm)
        x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (bool->int (= y x)))
    &vm))

(defword "~"
  (let [&dstack (:dstack &vm)
        x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (bool->int (not= y x)))
    &vm))

(defword <
  (let [&dstack (:dstack &vm)
        x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (bool->int (< y x)))
    &vm))

(defword >
  (let [&dstack (:dstack &vm)
        x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (bool->int (> y x)))
    &vm))

(defword and
  (let [&dstack (:dstack &vm)
        x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (bool->int (and (not (zero? y)) (not (zero? x)))))
    &vm))

(defword or
  (let [&dstack (:dstack &vm)
        x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (bool->int (or (not (zero? y)) (not (zero? x)))))
    &vm))

(defword dup
  (let [&dstack (:dstack &vm)
        x (maeni.stack/peek &dstack)]
    (maeni.stack/push! &dstack x)
    &vm))

(defword swap
  (let [&dstack (:dstack &vm)
        x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack x)
    (maeni.stack/push! &dstack y)
    &vm))

(defword over
  (let [&dstack (:dstack &vm)
        x (maeni.stack/pop! &dstack)
        y (maeni.stack/peek &dstack)]
    (maeni.stack/push! &dstack x)
    (maeni.stack/push! &dstack y)
    &vm))

(defword rot
  (let [&dstack (:dstack &vm)
        x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)
        z (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack y)
    (maeni.stack/push! &dstack x)
    (maeni.stack/push! &dstack z)
    &vm))

(defword drop
  (maeni.stack/pop! (:dstack &vm))
  &vm)

(defn- with-next-string [vm f]
  (let [[s text] (reader/read-string (:text vm))]
    (assoc (f s) :text text)))

^{:compile (fn [vm]
             (with-next-string vm
               (fn [s]
                 (update vm :code conj
                         `(do (maeni.stack/push! (:dstack ~'&vm) ~s) ~'&vm)))))}
(defword "s\""
  (with-next-string &vm
    (fn [s]
      (maeni.stack/push! (:dstack &vm) s)
      &vm)))

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
  (let [[_ text] (reader/read-until \) true (:text &vm))]
    (assoc &vm :text text)))

^:immediate
(defword ":"
  (let [[token text] (reader/next-token (:text &vm))]
    (assoc &vm :current-word token :mode :compile :code [] :text text)))

(defn- emit-combined-code [code]
  `(as-> ~'&vm ~'&vm ~@(seq code)))

^:immediate
(defword ";"
  (let [code (:code &vm)
        compiled-code `(fn [~'&vm] ~(emit-combined-code code))
        word {:name (:current-word &vm)
              :compiled-code (eval compiled-code)}]
    (-> &vm
        (update :dict dict/add-word word)
        (assoc :mode :interpret :current-word nil :code nil))))

^:immediate
(defword if
  (let [code (:code &vm)]
    (-> (assoc &vm :code [])
        (update :cstack conj [code]))))

^:immediate
(defword else
  (let [{[c & more] :cstack :keys [code]} &vm]
    (assoc &vm :code [] :cstack (cons (conj c code) more))))

^:immediate
(defword then
  (let [{[c & more] :cstack :keys [code]} &vm
        [test then else] (conj c code)
        code `[(let [~'&vm ~(emit-combined-code test)
                     v# (maeni.stack/pop! (:dstack ~'&vm))]
                 (if (not (zero? v#))
                   ~(emit-combined-code then)
                   ~@(if else
                       [(emit-combined-code else)]
                       ['&vm])))]]
    (assoc &vm :code code :cstack more)))
