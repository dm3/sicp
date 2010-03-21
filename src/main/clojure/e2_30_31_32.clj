(ns e2-30-31-32
  (:use clojure.test)
  (:use [clojure.contrib.generic.math-functions :only (sqr)]))

(defn leaf? [t]
  (not (sequential? t)))

; 2.30
(defn square-tree [t]
  (map (fn [sub-t]
         (if (leaf? sub-t) (* sub-t sub-t)
             (square-tree sub-t)))
       t))

(defn square-tree-look-ma-no-map [t]
  (if (empty? t) nil
      (if (leaf? (first t))
          (cons (* (first t) (first t)) (square-tree-look-ma-no-map (rest t)))
          ; Firstly I wrote (list) instead of (cons). Had to scracth my head about that.
          (cons (square-tree-look-ma-no-map (first t)) (square-tree-look-ma-no-map (rest t))))))

(def *test-list* [1 [2 [3 4] 5] [6 7]])
(def *test-result* [1 [4 [9 16] 25] [36 49]])

(deftest test-square-tree
  (is (= (square-tree *test-list*) *test-result*))
  (is (= (square-tree-look-ma-no-map *test-list*) *test-result*)))

; 2.31
; Here we need to write an fmap for a Tree functor.
(defn tree-map [f t]
  (map (fn [sub-t]
         (if (leaf? sub-t) (f sub-t)
             (tree-map f sub-t)))
       t))

(defn square-tree-with-map [t]
  (tree-map sqr t))

(deftest test-square-tree-with-map
  (is (= (square-tree-with-map *test-list*) *test-result*)))

; 2.32
; Finish the procedure and give a CLEAR explanation on why it works. Clear explanations are much harder
; than making a working program. Everyone knows that (well, except for those verification nerds who prove their
; programs before actually coding them up).
;
; We need to fill the function definition for the function which will map over the tail of the list.
(defn subsets [xs]
  (if (empty? xs)
      ; We can't use [] instead of [[]] as (map f []) exexutes f zero times while (map f [[]]) executes (f []) once
      [[]]
      ; 1. For the xs with length = 1, remaining will hold a set containing an empty set - [[]]
      ;    which is a starting point for the accumulation.
      (let [remaining (subsets (rest xs))]
        ; 2. Then, during each step the 'remaining' set of sets will be joined with another set.
        ;    The other set is the same 'remaining' set of sets where each inner set is prefixed
        ;    by a value from the original set 'xs'.
        ;    Each recursive call to the (subsets) shrinks the original 'xs' set by one, so that
        ;    the prefix is the n-th element of the 'xs' where 'n' is the nesting of the recursion.
        (concat remaining
                (map (fn [x]
                       (if (empty? x)
                           ; We don't want empty lists (acquired at the beginning of the (subsets)) messing up our results
                           (list (first xs))
                           ; here we join a value from the original set and a list of subsets accumulated so far
                           (cons (first xs) x)))
                     remaining)))))

; First subset (empty one) produced by this function will return persisten vector, other ones - lists.

(deftest test-subsets
  (is (= (subsets [1 2 3])
         [[] [3] [2] [2 3] [1] [1 3] [1 2] [1 2 3]])))

(run-tests 'e2-30-31-32)
