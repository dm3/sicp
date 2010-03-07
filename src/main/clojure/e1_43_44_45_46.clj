(ns e1-43-44-45-46
 (:use [util.util :only (repfun avg dx fixed-point average-damp pow close-enough? tolerance square abs avg)])
 (:use [clojure.contrib.test-is :only (deftest is run-tests)])
 (:use [clojure.contrib.generic.math-functions :only (cos)]))

; 1.43
; repfun is defined in util/util.clj
(deftest repfun-test
  (is (= ((repfun inc 4) 6) 10)))

; 1.44
; smooth should return a function which takes an average of three values: f(x - dx), f(x) and f(x + dx)
(defn smooth [f]
  (fn [x]
    (avg (f (- x dx)) (f x) (f (+ x dx)))))
; repeated smooth is trivial
(defn rep-smooth [f n]
  ((repfun smooth n) f))

; 1.45
; Here we need to find the amount of damping which is needed to average-damp the f(x) = x^n for n > 2.
; for y = sqrt(x) the damping is needed only once:
;   y^2 = x => y = x / y
; now we have a divergence (y0 = y2 = ...):
;   y1 = x / y0, y2 = x / (x / y0) = y1
; which is remedied by an average-damp:
;   y1 = 1/2 * (y0 + x / y0)
;
; in the general case of y = sqrtn(x)
;   (repfun (average-damp y) (dec n))
; seems to produce the desired results.
(defn sqrt-of-n [x n]
  (fixed-point (repfun
                (average-damp (fn [y] (/ x (pow y (dec n))))) (dec n))
                1))

(deftest test-sqrt
  (is (close-enough? (sqrt-of-n 4 2) 2 tolerance))
  (is (close-enough? (sqrt-of-n 8 3) 2 tolerance))
  (is (close-enough? (sqrt-of-n 16 4) 2 tolerance))
  (is (close-enough? (sqrt-of-n 32 5) 2 tolerance))
  (is (close-enough? (sqrt-of-n 64 6) 2 tolerance)))

; 1.46
; the syntax for letfn is strange... Well, not that strange if you think about it...
(defn iterative-improve [good-enough? improve]
  (fn [x]
    (letfn [(iter [x]
              (let [improved (improve x)]
                (if (good-enough? x improved)
                    improved
                    (recur improved))))]
      (iter x))))

; sqrt is fairly straightforward - we use the implementation given in the one of the previous chapters.
(defn iterative-sqrt [x]
  ((iterative-improve
    (fn [guess x]
      (let [ratio (/ guess x)]
        (and (< ratio (+ 1 tolerance)) (> ratio (- 1 tolerance)))))
    (fn [guess]
      (avg guess (/ x guess)))) 1))

(deftest test-iter-sqrt
  (is (close-enough? (iterative-sqrt 4) 2 tolerance))
  (is (close-enough? (iterative-sqrt 9) 3 tolerance)))

; implementation for fixed-point is not as clear as for sqrt
(defn iterative-fixed-point [f first-guess]
  ((iterative-improve
    (fn [guess x]
      (< (abs (- guess x)) tolerance))
    (fn [guess]
      (f guess))) first-guess))

(deftest test-iter-fp
  " see chapter 1.3.3 "
  (is (close-enough? (iterative-fixed-point cos 1.0) 0.739082 tolerance))
  (is (close-enough? (iterative-fixed-point cos 1.0) (fixed-point cos 1.0) tolerance)))

(run-tests 'e1-43-44-45-46)
