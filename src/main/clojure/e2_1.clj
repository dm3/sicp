;; ## Exercise 2.1
(ns e2-1
 (:use [clojure.contrib.test-is :only (deftest is run-tests)])
 (:use [clojure.contrib.generic.math-functions :only (abs)]))

;; A rational number is just a pair
(defn make-rat [n d]
  (list n d))

;; With numerator being the first element
(defn numer [rat]
  (first rat))

;; And the denominator being the `rest` (the second element)
(defn denom [rat]
  (second rat))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(deftest test-mul
  (is (= (mul-rat (make-rat 2 3) (make-rat 3 4)) (make-rat 6 12)))
  ;; this test case is asserting incorrect results,
  ;; the version of `mul` can't deal with negative numbers
  (is (= (mul-rat (make-rat -2 3) (make-rat 3 -4)) (make-rat -6 -12))))

;; ### 2.1
;;
;; We'll define our own `make-rat` which will properly deal with negative numbers.

;;
(defn stay-negative
  "Keeps the argument negative"
  [x]
  (if (< x 0) x (unchecked-negate x)))

(defn make-rat2
  "Creates a rational number.

   If both arguments are negative, the result will also be negative,
   if the rational number is negative - only the numerator will be negative."
  [n d]
  (if (< n 0)
      (list n (abs d))
      (if (< d 0)
          (list (stay-negative n) (abs d))
          (list n d))))

;; Using the modified version of `make-rat` automatically gives us correct results because
;; the only possibly negative part of the rational number is the numerator which
;; simply obeys multiplication rules. Denominator cannot become negative and mess up our signedness.
(deftest test-mul2
  (is (= (mul-rat (make-rat2 2 3) (make-rat2 3 4)) (make-rat2 6 12)))
  (is (= (mul-rat (make-rat2 -2 3) (make-rat2 3 -4)) (make-rat2 6 12))))

(run-tests 'e2-1)
