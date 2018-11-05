(ns maeni.vm
  (:require [maeni.dict :as dict]
            [maeni.reader :as reader]
            [maeni.stack :as stack]))

(defn make-vm [init-dict text]
  {:mode :interpret
   :dstack (stack/make-stack)
   :cstack ()
   :dict init-dict
   :text text})

(def ^:dynamic *vm*)

(defn- try-coerce [token]
  (try
    (Long/parseLong token)
    (catch Throwable t nil)))

(defn run1 [token]
  (if-let [w (or (dict/find-word (:dict @*vm*) token) (try-coerce token))]
    (if (= (:mode @*vm*) :compile)
      (cond (:compile w) ((:compile w) *vm*)
            (:immediate w) ((:compiled-code w) *vm*)
            :else
            (let [code (if (number? w)
                         `(stack/push! (:dstack @*vm*) ~w)
                         (:code (meta w)))]
              (swap! *vm* update :code conj code)))
      (if (number? w)
        (stack/push! (:dstack @*vm*) w)
        ((:compiled-code w) *vm*)))
    (throw (ex-info (str "No such word: " token) {:token token}))))

(defn run [vm]
  (binding [*vm* (atom vm)]
    (loop []
      (let [text (:text @*vm*)]
        (if (empty? text)
          @*vm*
          (let [[token cs] (reader/next-token text)]
            (swap! *vm* assoc :text cs)
            (run1 token)
            (recur)))))))
