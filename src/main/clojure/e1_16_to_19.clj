;; ## Exercises 1.16 - 1.19
(ns e1-16-to-19
  (:use clojure.test))

;; ### 1.16

(defn square [a]
  (* a a))

(defn fast-exp-iter [b n a]
  (cond (= n 0) a
        (even? n) (fast-exp-iter (square b) (/ n 2) a)
        :else (fast-exp-iter b (- n 1) (* a b))))

(defn fast-exp [b n]
  (fast-exp-iter b n 1))

(deftest test-fast-exp
  (is (= (fast-exp 2 3) 8))
  (is (= (fast-exp 4 4) 256)))

;; ### 1.17

(defn twice-17 [a]
  (* a 2))

(defn halve-17 [a]
  (/ a 2))

(defn mul-17 [a b]
  (cond (= a 0) 0
        (= b 0) 0
        (= a 1) b
        (even? a) (mul-17 (halve-17 a) (twice-17 b))
        :else (+ b (mul-17 (- a 1) b))))

(deftest test-mul-17
  (is (= (mul-17 1 1) 1))
  (is (= (mul-17 1 2) 2))
  (is (= (mul-17 3 4) 12))
  (is (= (mul-17 0 1) 0))
  (is (= (mul-17 10 10) 100))
  (is (= (mul-17 9 9) 81)))

;; ### 1.18

(defn twice [a]
  (* a 2))

(defn halve [a]
  (/ a 2))

(defn mul-iter [a b n]
  (cond (= a 0) n
        (even? a) (mul-iter (halve a) (twice b) n)
        :else (mul-iter (- a 1) b (+ b n))))

(defn mul [a b]
  (mul-iter a b 0))

(deftest test-mul
  (is (= (mul 1 1) 1))
  (is (= (mul 1 2) 2))
  (is (= (mul 3 4) 12))
  (is (= (mul 0 1) 0))
  (is (= (mul 10 10) 100))
  (is (= (mul 9 9) 81)))

;; ### 1.19

(defn fib-iter [a b p q cnt]
  (cond (= cnt 0) b
    (even? cnt) (fib-iter a
                          b
                          (+ (* q q) (* p p))
                          (+ (* 2 p q) (* q q))
                          (/ cnt 2))
    :else (fib-iter (+ (* b q) (* a q) (* a p))
                    (+ (* b p) (* a q))
                    p
                    q
                    (- cnt 1))))

(defn fib [n]
  (fib-iter 1 0 0 1 n))

(deftest test-fib
  (is (= (fib 0) 0))
  (is (= (fib 1) 1))
  (is (= (fib 2) 1))
  (is (= (fib 3) 2))
  (is (= (fib 10) 55)))

(run-tests 'e1-16-to-19)
