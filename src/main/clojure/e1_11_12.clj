;; ## Exercises 1.11 - 1.12
(ns e1-11-12
  (:use clojure.test))

;; ### 1.11

(defn f [n]
  (if (< n 3) n
    (+ (f (- n 1)) (f (- n 2)) (f (- n 3)))))

(defn f-iter [a b c n]
  (if (< n 3) (+ a b c)
      (f-iter b c (+ a b c) (- n 1))))

(defn fi [n]
  (if (< n 3) n
      (f-iter 0 1 2 (- n 1))))

(deftest fi-test
  (is (= (fi 3) 3))
  (is (= (fi 4) 6))
  (is (= (fi 5) 11)))

;; ### 1.12

(defn pascal [row col]
  (if (or (= row col) (= col 1) (= row 1)) 1
      (+ (pascal (- row 1) col)
         (pascal (- row 1) (- col 1)))))

(deftest test-pascal
  (is (= (pascal 1 1) 1))
  (is (= (pascal 3 2) 2))
  (is (= (pascal 5 3) 6)))

(run-tests 'e1-11-12)
