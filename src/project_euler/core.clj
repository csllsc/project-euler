(ns project-euler.core
  (:require clojure.stacktrace)
  (:use [clojure.test :as t :only [deftest with-test testing is run-tests run-all-tests]])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))

;; PROBLEM #1 - Multiples of 3 and 5

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

;;PROBLEM #2 - Even Fibonacci numbers

;; CSL original

(defn fib [t0 t1]
  (cons t0 (lazy-seq (fib t1 (+ t0 t1)))))

(defn seq-max [seq max]
  (take-while (fn [x] (< x max)) seq))


(defn euler-2 []
  (reduce + (filter even? (seq-max (fib 0 1) 4000000))))

;;project-euler.core> (euler-2)
;;4613732


;; clwk version:
(with-test
  (defn- fib
    ([] (fib 1 2))
    ([n m] (cons n (lazy-seq (fib m (+ m n))))))
  (is (= (take 10 (fib)) [1 2 3 5 8 13 21 34 55 89])))

(defn euler-2 
  ([] (euler-2 4000000))
  ([n] (reduce + (take-while #(<= % n) (filter even? (fib))))))


;;PROBLEM #3 - Largest Prime Factor
;; csl version


(defn max-prime-factor [n]
  "return largest prime factor"
  (let [num n
        col (range 2 n)
        div (first (filter #(= (rem num %1) 0) col))]
    (if
        (nil? div)
      num
      (recur (/ num div)))))




;;PROBLEM #4 - Largest Palindrome Product
;; csl version

(defn palindrome?
  [n]
  (= (seq (str n)) (reverse (seq (str n)))))

(defn exp 
  [n e]
  (reduce * (repeat e n)))

(defn first-filter
  [f c]
  (first (filter f c)))

(defn max-digits 
  [x]
  (- (exp 10 x) 1))

(defn product-set [x y]
  (for [a (range (max-digits x) 0 -1)
        b (range (max-digits y) 0 -1)]
    (* a b)))

(defn euler-4 [x y]
  (first-filter palindrome? (sort > (product-set x y))))

;; not used
(defn seq-repeat
  "takes a coll and returns a seq repeated n times"
  [c coll n]
  (flatten (lazy-seq (cons c (repeat n coll)))))

(defn seq-repeat-each-item
  "takes a coll and returns a seq of each item repeated n times"
  [c coll n]
  (let [c c
        coll coll
        n n]
    (if
        (empty? coll)
      (flatten c)
      (recur (lazy-seq (cons c (repeat n (first coll))))
             (rest coll)
             n))))

(def misc1 (range (max-digits 3) 0 -1))



;;PROBLEM #6 - Sum Square Difference
;; csl version

(defn sum-squares [s]
  (reduce + (map #(* % %) s)))

(defn square-sums [s]
  (let [x (reduce + s)]
    (* x x)))

(defn pe6 [x]
  (let [r (range 1 (+ x 1))]
    (- (square-sums r) (sum-squares r))))



