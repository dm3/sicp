(ns e2-7-8-9-10-11
  (:use [clojure.contrib.test-is :only (deftest is run-tests)])
  (:use [util.util :only (avg)])
  (:use [clojure.contrib.generic.math-functions :only ()]))

; 2.7
; Same old intervals composed of cons. Invariant: lower bound always comes before the upper bound.
(defn make-interval [a b] [a b])

(defn lower-bound [z]
  (get z 0))

(defn upper-bound [z]
  (get z 1))

; 2.8
; To write like Lisa, you gotta think like Lisa. E-Valuator.
; Having (a0, b0) and (a1, b1) where a0 <= b0 and a1 <= b1
; we need to perform (a0, b0) - (a1, b1). One solutions that jumps right at us is
; the (a0, b0) - (a1, b1) = (a0 - a1, b0 - b1). But is that really so?
(defn sub-interval [a b]
  (let [l (- (lower-bound a) (lower-bound b))
        u (- (upper-bound a) (upper-bound b))]
    (make-interval (min l u) (max l u))))

; There exist several possible combinations of two intervals (I didn't examine the combinations
; where intervals overlap as my ASCII drawing skills are already pushed to the maximum):
(deftest sub-interval-test
  ; 1. ----|---\----|---\-------
  ;        a0  a1  b0   b1
  (is (= (sub-interval (make-interval 1 3) (make-interval 2 5)) (make-interval -2 -1)))
  ; 2. ---\--------\-|--------|-
  ;      a1       b1 a0       b0
  (is (= (sub-interval (make-interval 4 6) (make-interval 1 3)) (make-interval 3 3)))
  ; 3. ---|--------|-\--------\-
  ;      a0       b0 a1       b1
  (is (= (sub-interval (make-interval 1 3) (make-interval 4 6)) (make-interval -3 -3)))
  ; 4. ---\----|---\----|-------
  ;      a1   a0   b1   b0
  (is (= (sub-interval (make-interval 2 5) (make-interval 1 3)) (make-interval 1 2))))

; 2.9
; We need to show that the radius of the resulting interval depends on the radiuses of the source material
; in cases of addition/subtraction. Results of division/multiplication, on the other hand, should not depend
; on the radiuses of original intervals.
; I'll give some examples, no proofs attached.
(defn radius-interval [z]
  (avg (upper-bound z) (lower-bound z)))

(defn add-interval [a b]
  (make-interval (+ (lower-bound a) (lower-bound b))
                 (+ (upper-bound a) (upper-bound b))))

(defn mul-interval [a b]
  (let [p1 (* (lower-bound a) (lower-bound b))
        p2 (* (lower-bound a) (upper-bound b))
        p3 (* (upper-bound a) (lower-bound b))
        p4 (* (upper-bound a) (upper-bound b))]
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(defn div-interval [a b]
  (mul-interval a
    (make-interval (/ 1 (upper-bound b))
                   (/ 1 (lower-bound b)))))

(defn proportional?
  " Takes an operation on intervals, an op on numbers and two intervals. Returns true if the result is proportional
    to the input values, false otherwise. "
  [fint f a b]
  (let [result (fint a b)]
    (= (radius-interval result) (f (radius-interval a) (radius-interval b)))))

(deftest addition-subtraction-test
  (is (proportional? sub-interval - (make-interval 1 6) (make-interval 3 15)))
  (is (proportional? sub-interval - (make-interval -1 6) (make-interval -6 -5)))
  (is (proportional? add-interval + (make-interval 1 6) (make-interval 3 15)))
  (is (proportional? add-interval + (make-interval -1 6) (make-interval -6 -5)))
  (is (not (proportional? mul-interval * (make-interval 1 6) (make-interval 3 15))))
  (is (not (proportional? mul-interval * (make-interval -1 6) (make-interval -6 -5))))
  (is (not (proportional? div-interval / (make-interval 1 6) (make-interval 3 15))))
  (is (not (proportional? div-interval / (make-interval -1 6) (make-interval -6 -5)))))

; 2.10
; It's Ben Bitwrestler, an expert system programmer, who watches Lisa P. Hacker over her shoulder.
; Lets hope Lisa doesn't wear anything too distracting.
(defn contains-interval [a n]
  (if (or (> (lower-bound a) n) (< (upper-bound a) n))
      false
      true))

(defn div-interval-mod [a b]
  (if (contains-interval b 0) (println "Cannot divide by interval containing a zero!" b)
  (mul-interval a
    (make-interval (/ 1 (upper-bound b))
                   (/ 1 (lower-bound b))))))

; 2.11
; Oh come on, Ben. We know that you manually unroll your loops and inline your functions. Don't make Lisa
; get down to this lowly-zero-abstraction stuff. How do you know the compiler isn't doing the same thing
; already, perhaps even more efficiently? Do you run with -server and how do you know what the JIT is gonna
; do after a couple thousand passes?
; What? We run a dialect of lisp that has not been invented yet at the time the book you are a fictional character
; of had been written? We run it on a platform that hadn't existed at that time? Ok, ok, I see your point.
; If the management ever plans to migrate our codebase to scheme, we'll have our loops already unrolled and functions inlined. Checked.
; P.S. and if we won't have any loops to unroll, we'll unroll recursion.

(run-tests 'e2-7-8-9-10-11)
