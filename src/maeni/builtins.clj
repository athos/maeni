(ns maeni.builtins
  (:require [clojure.string :as str]
            [maeni.dict :as dict]
            [maeni.reader :as reader]
            [maeni.stack :as s]
            [maeni.vm :as vm]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private builtin-words*
  (atom {:dict {}, :cells []}))

(defn builtin-words []
  @builtin-words*)

(defmacro defword [name & body]
  (let [name (str/upper-case (str name))
        {:keys [immediate compile] :as attrs} (meta &form)
        address (vm/allocate-cell! builtin-words*)]
    `(let [w# (with-meta
                (array-map :name ~name
                           :compiled-code (fn [~'&dstack] ~@body)
                           ~@(when immediate
                               [:immediate true])
                           ~@(when compile
                               [:compile compile]))
                {:code '(do ~@body)})]
       (vm/add-word! builtin-words* ~address w#)
       '~name)))

(defword .
  (let [x (maeni.stack/pop! &dstack)]
    (print x)))

(defword +
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (+ y x))))

(defword -
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (- y x))))

(defword *
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (* y x))))

(defword /
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (quot y x))))

(defn bool->int [b]
  (if b 1 0))

(defword =
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (maeni.builtins/bool->int (= y x)))))

(defword "~"
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (maeni.builtins/bool->int (not= y x)))))

(defword <
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (maeni.builtins/bool->int (< y x)))))

(defword >
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (maeni.builtins/bool->int (> y x)))))

(defword and
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (maeni.builtins/bool->int (and (not (zero? y)) (not (zero? x)))))))

(defword or
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack (maeni.builtins/bool->int (or (not (zero? y)) (not (zero? x)))))))

(defword dup
  (let [x (maeni.stack/peek &dstack)]
    (maeni.stack/push! &dstack x)))

(defword swap
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack x)
    (maeni.stack/push! &dstack y)))

(defword over
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/peek &dstack)]
    (maeni.stack/push! &dstack x)
    (maeni.stack/push! &dstack y)))

(defword rot
  (let [x (maeni.stack/pop! &dstack)
        y (maeni.stack/pop! &dstack)
        z (maeni.stack/pop! &dstack)]
    (maeni.stack/push! &dstack y)
    (maeni.stack/push! &dstack x)
    (maeni.stack/push! &dstack z)))

(defword drop
  (maeni.stack/pop! &dstack))

(defn- with-next-string [f]
  (let [[s text] (reader/read-string (:text @vm/*vm*))]
    (f s)
    (swap! vm/*vm* assoc :text text)))

^{:compile (fn [vm]
             (with-next-string
               (fn [s]
                 (swap! vm/*vm* update :code conj
                        `(s/push! ~'&dstack ~s)))))}
(defword "s\""
  (with-next-string
    #(s/push! &dstack %)))

^{:compile (fn [vm]
             (with-next-string
               (fn [s]
                 (swap! vm/*vm* update :code conj `(print ~s)))))}
(defword ".\""
  (with-next-string
    (fn [s]
      (print s))))

^:immediate
(defword "("
  (let [[_ text] (reader/read-until #{\)} true (:text @vm/*vm*))]
    (swap! vm/*vm* assoc :text text)))

^:immediate
(defword ":"
  (let [[token text] (reader/next-token (:text @vm/*vm*))
        address (vm/allocate-cell! vm/*vm*)]
    (swap! vm/*vm* assoc
           :current-address address
           :current-word token
           :mode :compile
           :code []
           :text text)))

(defn- emit-combined-code [code]
  `(do ~@code))

^:immediate
(defword ";"
  (let [code (:code @vm/*vm*)
        compiled-code `(fn [~'&dstack] ~(emit-combined-code code))
        address (:current-address @vm/*vm*)
        word {:name (:current-word @vm/*vm*)
              :compiled-code compiled-code}]
    (vm/add-word! vm/*vm* address word)
    (swap! vm/*vm* assoc :mode :interpret :current-word nil :code nil)))

^:immediate
(defword if
  (let [code (:code @vm/*vm*)]
    (swap! vm/*vm* assoc :code [])
    (swap! vm/*vm* update :cstack conj [code])))

^:immediate
(defword else
  (let [{[c & more] :cstack :keys [code]} @vm/*vm*]
    (swap! vm/*vm* assoc :code [] :cstack (cons (conj c code) more))))

^:immediate
(defword then
  (let [{[c & more] :cstack :keys [code]} @vm/*vm*
        [test then else] (conj c code)
        code `[(do ~(emit-combined-code test)
                   (if (not (zero? (s/pop! ~'&dstack)))
                     ~(emit-combined-code then)
                     ~@(when else
                         [(emit-combined-code else)])))]]
    (swap! vm/*vm* assoc :code code :cstack more)))

^:immediate
(defword recurse
  (let [address (:current-address @vm/*vm*)]
    (swap! vm/*vm* update :code conj
           `(vm/call-compiled-code (:cells @vm/*vm*) ~address ~'&dstack))))
