(ns maeni.main
  (:require [maeni.core :as maeni]
            [maeni.reader :as reader]))

(defn repl []
  (let [vm (maeni/make-vm)]
    (loop []
      (let [line (read-line)
            [token _] (reader/next-token line)]
        (when-not (= token "bye")
          (maeni/run vm line)
          (flush)
          (println " ok")
          (recur))))))

(defn -main []
  (repl))
