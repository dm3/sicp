(ns e1-30
  (:use [util.util :only (id)])
  (:use [clojure.contrib.test-is :only (deftest is run-tests)]))

; (defn id [x] x)

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
