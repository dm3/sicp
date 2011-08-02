(ns e2-61-62
  (:use clojure.test)
  (:require [clojurecheck.core :as cc]))

; From the previous exercise
(defn element-of-set? [x xs]
  (cond (empty? xs) false
        (= x (first xs)) true
        :else (element-of-set? x (rest xs))))

; 2.61
; Takes roughly |xs|/2 operations
(defn adjoin-set-ord [x xs]
  (cond (empty? xs) [x]
        (<= x (first xs)) (cons x xs)
        (> x (first xs)) (cons (first xs) (adjoin-set-ord x (rest xs)))))

(deftest test-adjoin-ord
  (cc/property "adjoin-set"
    [i (cc/int)
     s (cc/set (cc/int))]
    (is (element-of-set? i (adjoin-set-ord i s)))

    (is (= (sort s) (reduce #(adjoin-set-ord %2 %) [] s)))))

; 2.62
; At most |xs|+|ys| operations, which is O(n).
; It's (almost) |xs|+|ys| in case xs = {k, ..., m-1, n}, ys = {m, ..., n - 1} where n > m > k.
; In this case we need to scan to the xs(m-1) where we'll have accumulated the
; {k, ..., m-1} segment. Then we'll have to scan to the end of ys which will
; result in {k, ..., m-1, m, ..., n-1}. The last step is appending {n} to the
; end of the set.
(defn union-set-ord [xs ys]
  (cond (empty? xs) ys
        (empty? ys) xs
        (<= (first xs) (first ys))
          (cons (first xs) (union-set-ord (rest xs) ys))
        (> (first xs) (first ys))
          (cons (first ys) (union-set-ord xs (rest ys)))))

(deftest test-union-ord
  (cc/property "union-set"
    [s1 (cc/set (cc/int))
     s2 (cc/set (cc/int))]
    ; pre-sort the arguments to union-set-ord as it expects both sets well-behaved
    (is (= (seq (sort (concat s1 s2))) (seq (union-set-ord (sort s1) (sort s2)))))))
