(ns euler.core
  (:use [clojure.test :as t :only [deftest with-test testing is run-tests run-all-tests]]))

;; CSL original.
(defn euler1
  [s f]
  (let [col (range s f)
        div3 #(zero? (mod % 3))
        div5 #(zero? (mod % 5))
        test (fn [x] (or (div3 x) (div5 x)))
        divs (filter test col)]
    (reduce + divs)))

;; Utility function -- likely to be useful throughout.
(with-test
  (defn- divisible-by? [r n]
    (zero? (mod n r)))
  (is (divisible-by? 5 10))
  (is (not (divisible-by? 6 10)))) 


;; Syntactic sugar.  Useful to have both for partial application.
(with-test
  (defn- divides? [n r]
    (divisible-by? r n))
  (is (divides? 100 20))
  (is (not (divides? 17 3))))

;; Generalized solution to the sum of range divisors problem.
(with-test
  (defn range-divisors-sum 
    ([end divisor] (range-divisors-sum 0 end divisor))   ;;added range                          
    ([start end divisors]  
       (reduce + (filter #(some (partial divides? %) divisors) 
                         (range start end)))))
  ;; 10 + 12 + 14 + 15 + 16 + 18 = 85
  (is (= (range-divisors-sum 10 20 [2 3])) 85))

;; Hard-coded divisors to make the general solution concrete.
(with-test
  (defn euler-1 [n] (range-divisors-sum n [3 5]))
  (is (= (euler-1 10) 23)))

;; Proof that this returns the same result as your (CSL) original.
(deftest test-euler-1 []
  (is (= (euler-1 1000) (euler1 0 1000))))
