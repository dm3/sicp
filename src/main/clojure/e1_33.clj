(ns e1-33
 (:use [util.util :only (curry square id)])
 (:use [clojure.contrib.test-is :only (deftest is run-tests)])
 (:use [clojure.contrib.math :only (gcd sqrt)]))

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

(run-tests 'e1-33)
