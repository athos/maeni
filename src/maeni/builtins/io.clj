(ns maeni.builtins.io
  (:require [maeni.builtins.core :refer [defword]]
            [maeni.builtins.util :as util]
            maeni.stack
            [maeni.vm :as vm]))

(defword .
  (let [x (maeni.stack/pop! &dstack)]
    (print x)))

^{:compile (fn [vm]
             (util/with-next-string
               (fn [s]
                 (swap! vm/*vm* update :code conj `(print ~s)))))}
(defword ".\""
  (util/with-next-string
    (fn [s]
      (print s))))
