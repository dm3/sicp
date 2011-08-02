;; ## Exercise 1.29
(ns e1-29
  (:use clojure.test))

;; Define a procedure that takes as arguments \\(f, a, b\\), and \\(n\\) and
;; returns the value of the integral, computed using Simpson's Rule. Use your
;; procedure to integrate cube between 0 and 1 (with \\(n = 100\\) and \\(n = 1000\\)),
;; and compare the results to those of the integral procedure shown above.

(defn cube [x] (* x x x))

(defn sum
  "Sums up all of the terms."
  [term a nextf b]
  (if (> a b)
      0
      (+ (term a)
         (sum term (nextf a) nextf b))))

(defn integral
  "Takes an integral of the given function `f`.

   * f - function to integrate
   * a, b - points to integrate between
   * n - precision (the more, the better)"
  [f a b n]
  ;h = (b - a) / n
  (let [h (/ (- b a) n)]
    (defn term [ind]
      ;Yind = f(a + ind*h)
      (let [y (f (+ a (* ind h)))]
        (cond (= 0 ind) y
              (odd? ind) (* 4 y)
              (even? ind) (* 2 y))))
    (defn nextf [ind]
      (+ 1 ind))
    (* (sum term 0 nextf n)
       (/ h 3))))

(deftest test-integral
  (is (= (integral cube 0 1 1000) 751/3000)))
