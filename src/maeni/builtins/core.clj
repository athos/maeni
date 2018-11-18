(ns maeni.builtins.core
  (:require [clojure.string :as str]
            [maeni.builtins.util :as util]
            [maeni.reader :as reader]
            [maeni.stack :as s]
            [maeni.vm :as vm]))

(def builtin-words*
  (atom {:dict {}, :cells []}))

(defmacro defword [name & body]
  (let [name (str/upper-case (str name))
        {:keys [immediate compile] :as attrs} (meta &form)
        address (vm/allocate-cell! builtin-words*)]
    `(let [w# (with-meta
                (array-map :name ~name
                           :compiled-code (fn [~'&cells ~(s/with-tag '&dstack)]
                                            ~@body)
                           ~@(when immediate
                               [:immediate true])
                           ~@(when compile
                               [:compile compile]))
                {:code '(do ~@body)})]
       (vm/add-word! builtin-words* ~address w#)
       '~name)))

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

^:immediate
(defword ";"
  (let [code (:code @vm/*vm*)
        compiled-code `(fn [~'&cells ~(s/with-tag '&dstack)] ~(util/emit-combined-code code))
        address (:current-address @vm/*vm*)
        word {:name (:current-word @vm/*vm*)
              :compiled-code (binding [*compiler-options* {:disable-locals-clearing true
                                                           :direct-linking true}]
                               (eval compiled-code))}]
    ;(prn :compiled-code compiled-code)
    (vm/add-word! vm/*vm* address word)
    (swap! vm/*vm* assoc :mode :interpret :current-word nil :code nil)))

^:immediate
(defword "["
  (swap! vm/*vm* assoc :mode :interpret))

(defword "]"
  (swap! vm/*vm* assoc :mode :compile))

^:immediate
(defword "("
  (let [[_ text] (reader/read-until #{\)} true (:text @vm/*vm*))]
    (swap! vm/*vm* assoc :text text)))

^{:compile (fn [vm]
             (util/with-next-string
               (fn [s]
                 (swap! vm/*vm* update :code conj
                        `(s/push! ~'&dstack ~s)))))}
(defword "s\""
  (util/with-next-string
    #(s/push! &dstack %)))

^{:compile (fn [vm]
             (util/with-next-word
               (fn [w]
                 (let [code `(s/push! ~'&dstack ~(:address w))]
                   (swap! vm/*vm* update :code conj code)))))}
(defword "'"
  (util/with-next-word
    (fn [w]
      (maeni.stack/push! &dstack (:address w)))))

(defword execute
  (let [address (maeni.stack/pop! &dstack)]
    (maeni.vm/call-compiled-code &cells address &dstack)))
