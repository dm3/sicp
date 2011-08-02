;; ## Exercise 1.21
(ns e1-21
  (:use clojure.test))

;; Use the smallest-divisor procedure to find the smallest divisor of each of
;; the following numbers: 199, 1999, 19999.

;; For it we'll need to implement the procedures described earlier in the
;; chapter.

(defn divides? [a b]
  (= (rem b a) 0))

(defn square [a]
  (* a a))

(defn expmod [base exp m]
  (cond (= exp 0) 1
    (even? exp) (rem (square
                           (expmod base (/ exp 2) m)) m)
    :else (rem (* base (expmod base (- exp 1) m))
                     m)))

(defn find-divisor [n test]
  (cond (> (square test) n) n
    (divides? test n) test
    :else (find-divisor n (+ test 1))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(defn prime? [n]
  (= (smallest-divisor n) n))

;; And the answer is...
(deftest test-smallest-divisor
  (are [x y] (= (smallest-divisor x) y)
        199 199
        1999 1999
        19999 7))
