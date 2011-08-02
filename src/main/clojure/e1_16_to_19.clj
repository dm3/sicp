;; ## Exercises 1.16 - 1.19
(ns e1-16-to-19
  (:use clojure.test))

;; ### 1.16
;;
;; Design a procedure that evolves an iterative exponentiation process that
;; uses successive squaring and uses a logarithmic number of steps, as does
;; `fast-expt`.

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
;;
;; The exponentiation algorithms in this section are based on performing
;; exponentiation by means of repeated multiplication. In a similar way, one
;; can perform integer multiplication by means of repeated addition. The
;; following multiplication procedure (in which it is assumed that our language
;; can only add, not multiply) is analogous to the expt procedure:

(defn mul-example [a b]
  (if (= b 0)
    0
    (+ a (* a (- b 1)))))

;; This algorithm takes a number of steps that is linear in \\(b\\).
;; Now suppose we include, together with addition, operations
;;
;; * double, which doubles an integer
;; * halve, which divides an (even) integer by 2
;;
;; Using these, design a multiplication procedure analogous to `fast-expt` that
;; uses a logarithmic number of steps.

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
;;
;; Using the results of exercises 1.16 and 1.17, devise a procedure that
;; generates an iterative process for multiplying two integers in terms of
;; adding, doubling, and halving and uses a logarithmic number of steps

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
;;
;; There is a clever algorithm for computing the Fibonacci numbers in a
;; logarithmic number of steps. Recall the transformation of the state
;; variables \\(a\\) and \\(b\\) in the `fib-iter` process of section 1.2.2:
;;
;; $$a \leftarrow a + b \mbox{ and } b \leftarrow a$$
;;
;; Call this transformation \\(T\\), and observe that applying \\(T\\) over and over again
;; n times, starting with 1 and 0, produces the pair \\(Fib(n + 1)\\) and \\(Fib(n)\\). In
;; other words, the Fibonacci numbers are produced by applying \\(Tn\\), the nth
;; power of the transformation \\(T\\), starting with the pair \\((1, 0)\\).
;; Now consider \\(T\\) to be the special case of \\(p = 0\\) and \\(q = 1\\)
;; in a family of transformations \\(Tpq\\), where \\(Tpq\\) transforms the
;; pair \\((a,b)\\) according to
;;
;; $$a \leftarrow bq + aq + ap$$
;;
;; and
;;
;; $$b \leftarrow bp + aq$$
;;
;; Show that if we apply such a transformation \\(Tpq\\) twice, the effect
;; is the same as using a single transformation \\(Tp'q'\\) of the same form, and
;; compute \\(p'\\) and \\(q'\\) in terms of \\(p\\) and \\(q\\). This gives us
;; an explicit way to square these transformations, and thus we can compute
;; \\(Tn\\) using successive squaring, as in the `fast-expt` procedure. Put
;; this all together to complete the following procedure, which runs in a
;; logarithmic number of steps:

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
