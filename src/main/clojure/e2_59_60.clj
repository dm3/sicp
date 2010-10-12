(ns e2-59-60
  (:use clojure.test)
  (:require [clojurecheck.core :as cc]))

; 2.59
; I think it's time to use the clojure quickcheck implementation (by Kotarak)
;
; next vs rest: rest returns an empty seq if no more elements exist, next returns nil.

(defn element-of-set? [x xs]
  (cond (empty? xs) false
        (= x (first xs)) true
        :else (element-of-set? x (rest xs))))

(defn adjoin-set [x xs]
  (if (element-of-set? x xs)
      xs
      (cons x xs)))

(defn intersection-set [xs ys]
  (cond (or (empty? xs) (empty? ys)) []
        (element-of-set? (first xs) ys)
          (cons (first xs) (intersection-set (rest xs) ys))
        :else (intersection-set (rest xs) ys)))

; A bit of cheating
(defn union-set [xs ys]
  (distinct (concat xs ys)))

(deftest prop-sets
  (cc/property "element-of-set & adjoin-set"
    [s1 (cc/set (cc/int))
     s2 (cc/set (cc/int))
     i (cc/int)]
    (is (element-of-set? i (adjoin-set i s1)))

    (is (if (or (element-of-set? i s1) (element-of-set? i s2))
            (element-of-set? i (union-set s1 s2))
            true))

    (is (not (element-of-set? i [])))))

; 2.60
; Now sets can contain duplicate elements
; `element-of-set?` remains the same as it was - we're searching for the first matching element.
; I couldn't devise anything smarter than the implementation of `intersection-set` for the non-duplicate sets.

; `adjoin-set` can be simplified - if duplicate elements are allowed we can just cons the element in question.
(defn adjoin-set2 [x xs]
  (cons x xs))

; Just concat without worrying about duplicates
(defn union-set2 [xs ys]
  (concat xs ys))

; The only obvious pro of the above redefinitions is the O(1) complexity (given
; `concat` is O(1)) compared to `adjoin-set` and `union-set` which are O(N).
; I'd use the above representation in applications where inserts and unions
; dominated queries and intersections.  It seems that `element-of-set` would
; take the same amount of operations with both no-duplicates and
; duplicates-allowed representations if we analized the probabilistic
; complexity. `intersection-set`, on the other hand, could potentially take
; much longer as it's O(m*n) and doesn't take into account the duplicates.

(deftest prop-sets2
  (cc/property "element-of-set & adjoin-set"
    [s1 (cc/set (cc/int))
     s2 (cc/set (cc/int))
     i (cc/int)]
    (is (element-of-set? i (adjoin-set2 i s1)))

    (is (if (or (element-of-set? i s1) (element-of-set? i s2))
            (element-of-set? i (union-set2 s1 s2))
            true))))

(run-tests 'e2-59-60)
