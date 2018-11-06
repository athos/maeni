(ns maeni.builtins
  (:require maeni.builtins.control
            [maeni.builtins.core :as builtins]
            maeni.builtins.io
            maeni.builtins.math
            maeni.builtins.stack))

(defn builtin-words []
  @builtins/builtin-words*)
