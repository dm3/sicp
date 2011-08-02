;; ## Exercises 1.30 - 1.33
(ns e1-30-to-33
  (:use [util.util :only (id compose square curry)])
  (:use [clojure.contrib.math :only (gcd sqrt)])
  (:use [clojure.contrib.test-is :only (deftest is run-tests)]))

;; ### 1.30
;;
;; The `sum` procedure in the previous chapter generates a linear recursion.
;; The procedure can be rewritten so that the sum is performed iteratively.

(defn sum-iter [term a nextf b]
  (defn iter [a result]
    (if (> a b)
        result
        (iter (nextf a) (+ result (term a)))))
  (iter a 0))

(defn sum-ints [a b]
  (sum-iter id a inc b))

; tests ------------------------- >
(deftest test-sum-ints
  (is (= (sum-ints 1 10) 55)))

;; ### 1.31
;;
;; The sum procedure is only the simplest of a vast number of similar
;; abstractions that can be captured as higher-order procedures. Write an
;; analogous procedure called `product` that returns the product of the values
;; of a function at points over a given range.

(defn product-31 [term a next b]
  (if (> a b)
      1
      (* (term a)
         (product-31 term (next a) next b))))

; tests ------------------------- >
(deftest product-test-31
  (is (= (product-31 id 1 inc 10) 3628800)))

;; If your product procedure generates a recursive process, write one that
;; generates an iterative process. If it generates an iterative process, write
;; one that generates a recursive process.

(defn product-iter [term a nextf b]
  (let [iter (fn [a result]
    (if (> a b)
        result
        (recur (nextf a) (* result (term a)))))]
  (iter a 1)))

; tests ------------------------- >
(deftest product-iter-test
  (is (= (product-iter id 1 inc 10) 3628800)))

;; Show how to define `factorial` in terms of `product`:

(defn fact [x]
  (product-31 id 1 inc x))

(defn twoinc [x]
  ((compose inc inc) x))

;; Also use `product` to compute approximations to `pi`:

(defn pi
  "Calculates pi with the specified accuracy"
  [acc]
  (let [upper (product-31 id 4 twoinc (- acc 1)) ; 4*6*8*...*k, k < acc
        lower (product-31 id 3 twoinc acc) ; 3*5*7*...*l, l <= acc
        factor (if (even? acc)
                    (dec acc)
                    (acc))]
    (* 4 (/
          (* 2 (square upper)) ; 4*4*6*6*8*8*...*k*k, k < acc
          (* factor (square (/ lower factor))))))) ; 3*3*5*5*...*(l-2)*(l-2)*l, l <= acc

; tests ------------------------- >
(deftest pi-test
  (is (= (double (pi 600)) 3.138971384492951)))

;; ### 1.32
;;
;; Show that sum and product (exercise 1.31) are both special cases of a still
;; more general notion called accumulate that combines a collection of terms,
;; using some general accumulation function:
;;
;; `(accumulate combiner null-value term a next b)`
;;
;; Accumulate takes as arguments the same term and range specifications as sum
;; and product, together with a combiner procedure (of two arguments) that
;; specifies how the current term is to be combined with the accumulation of
;; the preceding terms and a null-value that specifies what base value to use
;; when the terms run out. Write `accumulate` and show how `sum` and `product`
;; can both be defined as simple calls to `accumulate.`

(defn accumulate [combiner zero term a nextf b]
  (if (> a b)
      zero
      (combiner (term a)
                (accumulate combiner zero term (nextf a) nextf b))))

;; If your `accumulate` procedure generates a recursive process, write one that
;; generates an iterative process. If it generates an iterative process, write
;; one that generates a recursive process.

(defn accumulate-iter [combiner zero term a nextf b]
    (let [iter (fn [a result]
                 (if (> a b)
                   result
                   (recur (nextf a) (combiner result (term a)))))]
      (iter a zero)))

(defn sum [term a nextf b]
  (accumulate + 0 term a nextf b))

(defn product [term a nextf b]
  (accumulate * 1 term a nextf b))

; ------- tests ---------------
(deftest sum-test
  (is (= (sum square 1 inc 10) 385)))

(deftest product-test
  (is (= (product square 1 inc 10) 13168189440000)))

;; ### 1.33
;;
;; You can obtain an even more general version of accumulate (exercise 1.32) by
;; introducing the notion of a filter on the terms to be combined. That is,
;; combine only those terms derived from values in the range that satisfy a
;; specified condition. The resulting `filtered-accumulate` abstraction takes the
;; same arguments as `accumulate`, together with an additional predicate of one
;; argument that specifies the filter. Write `filtered-accumulate` as a
;; procedure.

(defn filtered-acc [filterf combiner zero term a nextf b]
  (if (> a b)
      zero
      (if (filterf a)
          (combiner (term a)
                    (filtered-acc filterf combiner zero term (nextf a) nextf b))
          (filtered-acc filterf combiner zero term (nextf a) nextf b))))

(defn prime-for? [n x]
  (= (gcd n x) 1))

; ------- tests ---------------
(deftest prime-test
  (is (= (prime-for? 10 4) false)))

(defn prime? [x]
  (let [iter (fn [nextf]
    (if (> nextf (sqrt x)) ; up to the square of x
        true
        (if (> (rem x nextf) 0)
            (recur (inc nextf))
            false)))]
  (if (= x 2) false (iter 2))))

; ------- tests ---------------
(deftest prime-test-2
  (is (= (prime? 3) true))
  (is (= (prime? 37) true))
  (is (= (prime? 254) false)))

;; Show how to express the following using `filtered-accumulate`:

;; *  the sum of the squares of the prime numbers in the interval `a` to `b`:
(defn squares [a b]
  (filtered-acc prime? + 0 square a inc b))

; ------- tests ---------------
(deftest squares-test
  (is (= (squares 1 10) 84)))

;; *  the product of all the positive integers less than `n` that are relatively
;;    prime to `n` (i.e., all positive integers `i < n` such that `GCD(i,n) = 1`).
(defn prod-less-than [n]
  (filtered-acc (curry prime-for? n) + 0 id 1 inc n))

; ------- tests ---------------
(deftest squares-test
  (is (= (prod-less-than 10) 20)))

(run-tests 'e1-30-to-33)
