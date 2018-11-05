(ns maeni.core
  (:require [maeni.builtins :as builtins]
            [maeni.dict :as dict]
            [maeni.reader :as reader]))

(defn make-vm
  ([] (make-vm nil))
  ([text] (make-vm (builtins/builtin-words) text))
  ([init-dict text]
   {:mode :interpret
    :dstack ()
    :cstack ()
    :dict init-dict
    :text text}))

(defn- try-coerce [token]
  (try
    (Long/parseLong token)
    (catch Throwable t nil)))

(defn run1 [vm token]
  (if-let [w (or (dict/find-word (:dict vm) token) (try-coerce token))]
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
       (let [[token cs] (reader/next-token text)]
         (recur (run1 (assoc vm :text cs) token))))))
  ([vm text]
   (run (assoc vm :text text))))
