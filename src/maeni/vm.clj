(ns maeni.vm
  (:require [maeni.dict :as dict]
            [maeni.reader :as reader]
            [maeni.stack :as stack]))

(defn make-vm [init text]
  {:mode :interpret
   :dstack (stack/make-stack)
   :cstack ()
   :dict (:dict init)
   :cells (:cells init)
   :text text})

(def ^:dynamic *vm*)

(defn allocate-cell! [vm]
  (dec (count (:cells (swap! vm #(update % :cells conj nil))))))

(defn add-word! [vm address word]
  (let [word' (-> word
                  (assoc :address address)
                  (dissoc :compiled-code))]
    (swap! vm
           #(-> %
                (assoc-in [:cells address] (:compiled-code word))
                (update :dict dict/add-word word')))))

(defmacro call-compiled-code [cells address &dstack]
  `((nth ~cells ~address) ~&dstack))

(defn- try-coerce [token]
  (try
    (Long/parseLong token)
    (catch Throwable t nil)))

(defn run1 [token]
  (if-let [w (or (dict/find-word (:dict @*vm*) token) (try-coerce token))]
    (let [cells (:cells @*vm*)
          &dstack (:dstack @*vm*)]
      (if (= (:mode @*vm*) :compile)
        (cond (:compile w) ((:compile w) &dstack)
              (:immediate w) (call-compiled-code cells (:address w) &dstack)
              :else
              (let [code (if (number? w)
                           `(stack/push! ~'&dstack ~w)
                           (or (:code (meta w))
                               `(call-compiled-code (:cells @*vm*)
                                                    ~(:address w)
                                                    ~'&dstack)))]
                (swap! *vm* update :code conj code)))
        (if (number? w)
          (stack/push! &dstack w)
          (call-compiled-code cells (:address w) &dstack))))
    (throw (ex-info (str "No such word: " token) {:token token}))))

(defn run [vm]
  (binding [*vm* vm]
    (loop []
      (let [text (:text @*vm*)]
        (if (empty? text)
          *vm*
          (let [[token cs] (reader/next-token text)]
            (swap! *vm* assoc :text cs)
            (run1 token)
            (recur)))))))
