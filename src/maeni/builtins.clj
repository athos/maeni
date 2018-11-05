(ns maeni.builtins
  (:require [clojure.string :as str]
            [maeni.dict :as dict]
            [maeni.reader :as reader]))

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
  (let [[s text] (reader/read-string (:text vm))]
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
        code `[(let [vm# ~(emit-combined-code test)
                     v# (first (:dstack vm#))
                     ~'&vm (update vm# :dstack rest)]
                 (if (not (zero? v#))
                   ~(emit-combined-code then)
                   ~@(if else
                       [(emit-combined-code else)]
                       ['&vm])))]]
    (assoc &vm :code code :cstack more)))
