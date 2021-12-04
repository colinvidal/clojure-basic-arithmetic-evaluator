(ns basic-arithmetic-evaluator.core
  (:gen-class)
  (:require [basic-arithmetic-evaluator.evaluator :as e]))

(defn -main
  "Asks user for a basic expression to evaluate as input. Evaluates,
  prints the result, and asks again. Exits when EOF reached."
  []
  (loop []
    (print "> ")
    (flush)
    (when-let [expr (read-line)]
      (println (e/calc expr))
      (recur))))
