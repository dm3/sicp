(ns e1-31
  (:use [util.util :only (id compose square)])
  (:use [clojure.contrib.test-is :only (deftest is run-tests)]))

(defn product [term a next b]
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

; tests ------------------------- >
(deftest product-test
  (is (= (product id 1 inc 10) 3628800)))

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
  (product id 1 inc x))

(defn twoinc [x]
  ((compose inc inc) x))

; calculates pi with the specified accuracy
(defn pi [acc]
  (let [upper (product id 4 twoinc (- acc 1)) ; 4*6*8*...*k, k < acc
        lower (product id 3 twoinc acc) ; 3*5*7*...*l, l <= acc
        factor (if (even? acc)
                    (dec acc)
                    (acc))]
    (* 4 (/
          (* 2 (square upper)) ; 4*4*6*6*8*8*...*k*k, k < acc
          (* factor (square (/ lower factor))))))) ; 3*3*5*5*...*(l-2)*(l-2)*l, l <= acc

; tests ------------------------- >
(deftest pi-test
  (is (= (double (pi 600)) 3.138971384492951)))

(run-tests 'e1-31)
