(ns e2-46-to-51
  (:use clojure.test))

; 2.46
;
; data abstraction definitions
(defn make-vect [x y]
  [x y])

(defn xcor-vect [v]
  (first v))

(defn ycor-vect [v]
  (second v))

(deftest test-make-vect
  (let [zo (make-vect 0 1)]
    (is (= 0 (xcor-vect zo)))
    (is (= 1 (ycor-vect zo)))))

; test data - should be using quickcheck for testing operations
(def a {:data [(make-vect 0 0) (make-vect 1 1)] :add (make-vect 1 1) :sub (make-vect -1 -1)})
(def b {:data [(make-vect 2 3) (make-vect 5 4)] :add (make-vect 7 7) :sub (make-vect -3 -1)})

; operations
(defn add-vect [v1 v2]
  (make-vect
    (+ (xcor-vect v1) (xcor-vect v2))
    (+ (ycor-vect v1) (ycor-vect v2))))

(defn sub-vect [v1 v2]
  (make-vect
    (- (xcor-vect v1) (xcor-vect v2))
    (- (ycor-vect v1) (ycor-vect v2))))

(defn scale-vect [v x]
  (make-vect (* (xcor-vect v) x) (* (ycor-vect v) x)))

(deftest test-vect-ops
  (is (= (add-vect (first (get a :data)) (second (get a :data))) (get a :add)))
  (is (= (add-vect (first (get b :data)) (second (get b :data))) (get b :add)))
  (is (= (sub-vect (first (get a :data)) (second (get a :data))) (get a :sub)))
  (is (= (sub-vect (first (get b :data)) (second (get b :data))) (get b :sub))))

; 2.47

; test-data
(def va (make-vect 1 1))

;
(defn make-frame [o e1 e2]
  (list o e1 e2))

(defn origin-frame [f]
  (first f))

(defn edge1-frame [f]
  (second f))

(defn edge2-frame [f]
  (second (next f)))

(deftest test-frame
  (is (= (origin-frame (make-frame va va va)) va))
  (is (= (edge1-frame (make-frame va va va)) va))
  (is (= (edge2-frame (make-frame va va va)) va)))

; second
(defn make-frame2 [o e1 e2]
  (cons o (cons e1 e2)))
;
; This is a degenerate abstraction for frames - the `e2` part becomes the tail
; of a list created by `(cons e1 e2)`.
;
;  user=> (cons [1 1] (cons [2 2] [3 3]))
;  ([1 1] [2 2] 3 3)

(defn origin-frame2 [f]
  (first f))

(defn edge1-frame2 [f]
  (second f))

(defn edge2-frame2 [f]
  (let [remaining (next (next f))]
    (list (first remaining) (second remaining))))

(deftest test-frame2
  (is (= (origin-frame2 (make-frame2 va va va)) va))
  (is (= (edge1-frame2 (make-frame2 va va va)) va))
  (is (= (edge2-frame2 (make-frame2 va va va)) va)))

; 2.48
;
; Another exercise in abstraction and concept layering...

; This was hard
(defn make-segment [a b]
  (list a b))

(defn start-segment [s]
  (first s))

(defn end-segment [s]
  (second s))

; 2.49
;

(defn segments->painter [segs] nil)

; a) Paints the border around the frame
(defn paint-frame [f]
  ((segments->painter [(make-segment (make-vect 0 0) (make-vect 0 1))
                       (make-segment (make-vect 0 1) (make-vect 1 1))
                       (make-segment (make-vect 0 0) (make-vect 1 0))
                       (make-segment (make-vect 1 0) (make-vect 1 1))]) f))

; b) Paints the X inside the frame
(defn paint-frame [f]
  ((segments->painter [(make-segment (make-vect 0 0) (make-vect 1 1))
                       (make-segment (make-vect 0 1) (make-vect 1 0))]) f))

; c)/d) I think I'll just skip those...
