(ns basic-arithmetic-evaluator.core-test
  (:require [clojure.test :refer :all]
            [basic-arithmetic-evaluator.evaluator :as e]))

(deftest tokenize-test
  (testing "Testing tokenize on few basic expressions."
    (is (= (e/tokenize nil) nil))
    (is (= (e/tokenize "") nil))
    (is (= (e/tokenize "a") nil))
    (is (= (e/tokenize "abc") nil))
    (is (= (e/tokenize "0") [{:token 0}]))
    (is (= (e/tokenize  "1") [{:token 1}]))
    (is (= (e/tokenize  "1+2") [{:token 1} {:token :plus} {:token 2}]))
    (is (= (e/tokenize  "1+2*3") [{:token 1}
                                  {:token :plus}
                                  {:token 2}
                                  {:token :mult}
                                  {:token 3}])))
  (testing "Testing tokenize escape white spaces."
    (is (= (e/tokenize "   1+    2 * 3") [{:token 1}
                                          {:token :plus}
                                          {:token 2}
                                          {:token :mult}
                                          {:token 3}]))))

(deftest parse-test
  (testing "Test precedence operator logic."
    (let [literal (fn [x] {:type :literal :value x})
          mult (fn [l r] {:type :mult :left l :right r})
          plus (fn [l r] {:type :plus :left l :right r})]
      (is (= (e/parse 1 [] (e/tokenize "1+2*3")) (plus (literal 1)
                                                       (mult (literal 2)
                                                             (literal 3)))))
      (is (= (e/parse 1 [] (e/tokenize "1*2+3")) (plus (mult (literal 1)
                                                             (literal 2))
                                                       (literal 3)))))))

(deftest calc-test
  (testing "Testing correctness of simple expression."
    (is (= (e/calc "2+2*3") 8))
    (is (= (e/calc "2*2+3") 7))
    (is (= (e/calc "1") 1))
    (is (= (e/calc "5*5*3+2") 77))))

;; Something to try: clojure/test.check for generated properties
;; values.

;; Something else to try: extract logic of -main into a separate
;; function taking the input stream as parameter. Then it would be
;; easy to test this read/eval/print logic, and correctness of EOF
;; handling.
