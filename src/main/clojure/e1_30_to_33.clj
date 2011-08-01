;; ## Exercises 1.30 - 1.33
(ns e1-30-to-33
  (:use [util.util :only (id compose square curry)])
  (:use [clojure.contrib.math :only (gcd sqrt)])
  (:use [clojure.contrib.test-is :only (deftest is run-tests)]))

;; ### 1.30

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

(defn product-31 [term a next b]
  (if (> a b)
      1
      (* (term a)
         (product-31 term (next a) next b))))

; tests ------------------------- >
(deftest product-test-31
  (is (= (product-31 id 1 inc 10) 3628800)))

(defn product-iter [term a nextf b]
  (let [iter (fn [a result]
    (if (> a b)
        result
        (recur (nextf a) (* result (term a)))))]
  (iter a 1)))

; tests ------------------------- >
(deftest product-iter-test
  (is (= (product-iter id 1 inc 10) 3628800)))

(defn fact [x]
  (product-31 id 1 inc x))

(defn twoinc [x]
  ((compose inc inc) x))

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

(defn accumulate [combiner zero term a nextf b]
  (if (> a b)
      zero
      (combiner (term a)
                (accumulate combiner zero term (nextf a) nextf b))))

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

(defn squares [a b]
  (filtered-acc prime? + 0 square a inc b))

; ------- tests ---------------
(deftest squares-test
  (is (= (squares 1 10) 84)))

(defn prod-less-than [n]
  (filtered-acc (curry prime-for? n) + 0 id 1 inc n))

; ------- tests ---------------
(deftest squares-test
  (is (= (prod-less-than 10) 20)))

(run-tests 'e1-30-to-33)
