(ns e1-32
 (:use [util.util :only (id compose square)])
 (:use [clojure.contrib.test-is :only (deftest is run-tests)]))

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

(run-tests 'e1-32)
