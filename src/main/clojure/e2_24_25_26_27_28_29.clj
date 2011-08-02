(ns e2-24-25-26-27-28-29
  (:use clojure.test))

; 2.24
; a) What will (list 1 (list 2 (list 3 4))) print? First guess:
;   (1 (2 (3 4)))
(deftest test-print-list
  (is (= (list 1 (list 2 (list 3 4))) [1 [2 [3 4]]])))

; b,c) No, thanks. I won't draw these diagrams in ASCII.

; 2.25
(deftest test-get-7
  (is (= (first (rest (first (rest (rest [1 3 [5 7] 9])))))
         7))
  (is (= (first (first [[7]]))
         7))
  ; This is MADNESS!
  ; No! THIS! IS! DEEPLY NESTED LIST!
  (is (= (first (rest (first (rest (first (rest (first (rest (first (rest (first (rest [1 [2 [3 [4 [5 [6 7]]]]]]))))))))))))
         7)))

; 2.26
(def x [1 2 3])
(def y [4 5 6])

(deftest test-xy
  ; both arguments of (concat) are seqs
  (is (= (concat x y) [1 2 3 4 5 6]))
  ; (cons) expects the first argument to be a value and the second - a seq
  (is (= (cons x y) [[1 2 3] 4 5 6]))
  ; both arguments of (list) are values
  (is (= (list x y) [[1 2 3] [4 5 6]])))

; 2.27
; The hardest (and most counterintuitive) thing about clojure so far are
; vectors vs seqs.
; See http://stackoverflow.com/questions/1147975/in-clojure-when-should-i-use-a-vector-over-a-list-and-the-other-way-around
; for a vectors vs lists explanation.
(defn deep-reverse [xs]
  (if (empty? xs) nil
      ; sequential? returns true for VECTORS as well as for other seqs
      ; while seq? is false for VECTORS
      (if (sequential? (first xs))
          (concat (deep-reverse (rest xs)) [(deep-reverse (first xs))])
          (concat (deep-reverse (rest xs)) [(first xs)]))))

(deftest test-deep-reverse
  (is (= (deep-reverse [[1 2] [3 4]]) [[4 3] [2 1]])))

; 2.28
; Same as deep-reverse, only doesn't revert.
(defn fringe
  " Flattens the given tree structure. "
  [t]
  (if (empty? t) nil
      (if (sequential? (first t))
          (concat (fringe (first t)) (fringe (rest t)))
          (cons (first t) (fringe (rest t))))))

(deftest test-fringe
  (is (= (fringe [[1 2] [3 4]]) [1 2 3 4]))
  (is (= (fringe [[[1 2] [3 4]][[1 2] [3 4]]]) [1 2 3 4 1 2 3 4])))

; 2.29
; Here we need to build a binary mobile (???). As I understand a mobile is isomorphic to a binary tree with
; additional length info.
(defn make-mobile [l r]
  [l r])

(defn make-branch [len structure]
  [len structure])

; a)
(defn left-branch [m]
  (first m))

(defn right-branch [m]
  (second m))

(defn branch-len [b]
  (first b))

(defn branch-structure
  " Returns either the mobile connected to this branch or the weigth of the branch "
  [b]
  (second b))

(defn weight? [b]
  (number? b))

; b)
; This feels awkward without statically typed ADTs and pattern matching.

; At first I defined branch-weight as an internal function of total-weight.
(defn branch-weight [b]
  (letfn [(weight [b result]
            (let [structure (branch-structure b)]
              (if (weight? structure)
                  (+ result structure)
                  (+ (weight (left-branch structure) result) (weight (right-branch structure) result)))))]
    (weight b 0)))

(defn total-weight [m]
  (+ (branch-weight (left-branch m)) (branch-weight (right-branch m))))

; weight = 15, balanced = false
; *-1-5
;  \
;   1
;    \
;     10
(def *mob1* (make-mobile (make-branch 1 5) (make-branch 1 10)))
; weight = 13, balanced = false
; *-1-5
;  \
;   1
;    \
;     *-2-4
;      \
;       1
;        \
;         4
(def *mob2* (make-mobile (make-branch 1 5)
                         (make-branch 1 (make-mobile (make-branch 2 4)
                                                     (make-branch 1 4)))))
; weight = 15, balanced = true
; *-4-5
;  \
;   1
;    \
;     *-3-5
;      \
;       1
;        \
;         5
(def *mob3* (make-mobile (make-branch 4 5)
                         (make-branch 1 (make-mobile (make-branch 2 10)
                                                     (make-branch 2 10)))))

(deftest test-total-weight
  (is (= (total-weight *mob1*) 15))
  (is (= (total-weight *mob2*) (+ 5 4 4))))

; c) Mobile is balanced when the (len left * weight left) = (len right * weight right) and all of the
; sub-mobiles are balanced. This can be expressed as a conjunction of the results of 'branch-balanced?'
; on the left and right branches of the mobile.
; However, we also need to compare the torques of the branches if they do not contain any sub-mobiles.
;
; I bumped into a difficulty while solving this problem, as I didn't really understand what it meant
; for a binary mobile to be balanced. You need to calculate a torque not only for a branch which contains
; the weight, but for the branches which contain sub-mobiles too. It may be obvious for you, but it wasn't
; obvious for me.

(defn torque [b]
  (* (branch-weight b) (branch-len b)))

; Clojure supports mutually recursive functions, however, you need an additional definition
; so that the compiler wouldn't give you an 'Unable to resolve symbol' exception.
(def balanced?)

(defn branch-balanced? [b]
  (let [structure (branch-structure b)]
    (if (weight? structure) true
        (balanced? structure))))

(defn balanced? [m]
  (and (= (torque (left-branch m)) (torque (right-branch m)))
       (branch-balanced? (left-branch m))
       (branch-balanced? (right-branch m))))

(deftest test-balanced?
  (is (not (balanced? *mob1*)))
  (is (balanced? *mob3*)))

; d) If we change the representation, only the accessor functions will change as all other functions
; are defined in terms of the accessors.
