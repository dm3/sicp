;; ## Exercises 1.37 - 1.39
(ns e1-37-38-39
 (:use [util.util :only (tolerance close-enough? const phi e square)])
 (:use [clojure.contrib.generic.math-functions :only (tan)])
 (:use [clojure.contrib.test-is :only (deftest is run-tests)]))

;; ### 1.37

(defn cont-frac [n d k]
    (letfn [(iter [ind]
      (if (> ind k)
          0
          (/ (n ind) (+ (d ind) (iter (inc ind))))))]
  (iter 1)))

;; `cont-frac-iter` follows the same pattern as all of the `recursive -> iterating`
;; process transformations:
;;
;; 1. add an accumulating parameter to the physically recursive function
;; 2. return it instead of zero in the end of the computation
;;
(defn cont-frac-iter [n d k]
  (let [iter (fn [ind result]
    (if (> ind k)
        result
        (recur (inc ind) (/ (n ind) (+ (d ind) result)))))]
  (iter 1 0)))

;; Both of the tests should result in the same value \\(\approx 1/phi\\).
(deftest test-cont-frac
  (close-enough?
    (cont-frac (const 1.0) (const 1.0) 100)
    (/ 1 phi)
    0.001)

  (close-enough?
    (cont-frac-iter (const 1.0) (const 1.0) 100)
    (/ 1 phi)
    0.001))

;; ### 1.38

;; Result should be \\(\approx e - 2\\).
(deftest test-cont-frac-non-iter
  (close-enough?
    (cont-frac (const 1.0)
               ;; Series = [1, 2, 1, 1, 4, 1, 1, 6, ..]
               ;; If we index the series beginning with 1, we can determine the value of the
               ;; corresponding element of the series using the index:
               ;; `if` (mod (index - 2) 3) == 0 `=>` series[index] == ((index - 2) * 2) + 2 `else` series[index] == 1
               (fn [ind]
                 (let [base-ind (- ind 2)]
                 (if (= 0 (rem base-ind 3))
                     (+ 2 (* 2 (/ base-ind 3)))
                     1)))
                 100)
    (- e 2)
    0.001))

;; ### 1.39

(defn tan-cf [x k]
  (cont-frac (fn [ind]
               (if (= ind 1) x (- (square x))))
             (fn [ind] (dec (* ind 2)))
             k))

(deftest test-tan-cf
  (close-enough?
    (tan-cf 20 100)
    (tan 20)
    0.001))
