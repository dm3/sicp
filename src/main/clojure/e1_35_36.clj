;; ## Exercises 1.35 - 1.36
(ns e1-35-36
 (:use [util.util :only (tolerance close-enough? phi fixed-point)])
 (:use [clojure.contrib.test-is :only (deftest is run-tests)]))

(deftest test-close
  (is (close-enough?
        (fixed-point (fn [x] (+ 1 (/ 1 x))) 1.0)
        phi
        tolerance)))

;; ### 1.36
;; With no damping `(log1000/logx)`
(deftest test-fixed-point-no-damping
   (is (close-enough?
         (fixed-point (fn [x] (/ (Math/log 1000) (Math/log x))) 2)
         4.5555
         tolerance)))

;; With damping `(log1000/logx + x)/2`
(deftest test-fixed-point-damping
   (is (close-enough?
         (fixed-point (fn [x] (/ (+ (/ (Math/log 1000) (Math/log x)) x) 2)) 2)
         4.5555
         tolerance)))

(run-tests 'e1-35-36)
