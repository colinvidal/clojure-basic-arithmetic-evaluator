(ns basic-arithmetic-evaluator.core-test
  (:require [clojure.test :refer :all]
            [basic-arithmetic-evaluator.core :as c]))

(deftest main-repl-test
  (testing "Test entry point is acting as a REPL, evaluating user expression."
    (with-in-str
      "1+2"
      (is (= (with-out-str (c/-main)) "> 3\n> ")))))
