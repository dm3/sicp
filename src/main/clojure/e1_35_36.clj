;; ## Exercises 1.35 - 1.36
(ns e1-35-36
 (:use [util.util :only (tolerance close-enough? phi)])
 (:use [clojure.contrib.test-is :only (deftest is run-tests)]))

;; ### 1.35
;;
;; Show that the golden ratio (section 1.2.2) is a fixed point of the
;; transformation `x -> 1 + 1/x`, and use this fact to compute by means of the
;; `fixed-point` procedure.

(defn fixed-point [f first-guess]
  (let [trynext (fn [guess]
                  (let [nextval (f guess)]
                    (if (close-enough? guess nextval tolerance)
                      nextval
                      (recur nextval))))]
    (trynext first-guess)))

(deftest test-close
  (is (close-enough?
        (fixed-point (fn [x] (+ 1 (/ 1 x))) 1.0)
        phi
        tolerance)))

;; ### 1.36
;;
;; Find a solution to `x^x = 1000` by finding a fixed point of
;; `x -> log(1000)/log(x)`.

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

;; Compare the number of steps this takes with and without average damping.

(run-tests 'e1-35-36)
