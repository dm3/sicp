(ns e2-2-3
  (:use [util.util :only (close-enough? tolerance)])
  (:use [clojure.contrib.test-is :only (deftest is run-tests)])
  (:use [clojure.contrib.generic.math-functions :only (abs acos sqr sqrt)]))

; 2.2
; Points are just pairs (same as rational numbers in exercise 2.1)
(defn make-point [x y]
  (list x y))

(defn x-point [a]
  (first a))

(defn y-point [a]
  (second a))

(defn print-point [a]
  (println "(" (x-point a) "," (y-point a) ")"))

; Surprise! Segments are also pairs. Could probably use multimethods here, but why bother?
(defn make-segment
  " 'a' and 'b' are points on the XY coordinate axis "
  [a b]
  (list a b))

(defn start-segment [a]
  (first a))

(defn end-segment [a]
  (second a))

; The meat of the exercise which is fairly obvious if you are able to imagine two-dimensional coordinate axis.
; If you are not, please draw it and put it before your eyes.
(defn basepoint-segment
  " Subtracts the start of the segment from its end and returns the resulting point
    I don't know how this operation is named correctly, hence the basepoint part. "
  [a]
  (make-point (- (x-point (end-segment a)) (x-point (start-segment a)))
              (- (y-point (end-segment a)) (y-point (start-segment a)))))

(defn midpoint-segment [a]
  (let [base-point (basepoint-segment a)]
    (make-point (+ (/ (x-point base-point) 2) (x-point (start-segment a)))
                (+ (/ (y-point base-point) 2) (y-point (start-segment a))))))

(deftest midpoint-test
  (is (= (midpoint-segment
           (make-segment (make-point 0 0) (make-point 1 1)))
         (make-point 0.5 0.5)))
  (is (= (midpoint-segment
           (make-segment (make-point 2 2) (make-point 3 3)))
         (make-point 2.5 2.5)))
  (is (= (midpoint-segment
           (make-segment (make-point -2 -2) (make-point 3 3)))
         (make-point 0.5 0.5))))

; 2.3
; This one got messy as I tried to stuff more logic into segment/vector manipulations.

(defn length-segment
  " length of the given segment as a sqrt(x^2 + y^2) "
  [s]
  (let [base-point (basepoint-segment s)]
    (sqrt (+ (sqr (x-point base-point))
             (sqr (y-point base-point))))))

(defn dot-product
  " the dot product of two vectors "
  [a b]
  (let [base-point-a (basepoint-segment a)
        base-point-b (basepoint-segment b)]
  (+ (* (x-point base-point-a) (x-point base-point-b))
     (* (y-point base-point-a) (y-point base-point-b)))))

(defn normalize
  " normalizes the given segment (treating it as a 2d vector) "
  [v]
  (let [base-point (basepoint-segment v)
        length (length-segment v)]
    (make-segment (make-point 0 0)
                  (make-point (/ (x-point base-point) length)
                              (/ (y-point base-point) length)))))

(defn orthogonal-segments?
  " dot product of orthogonal normalized vectors is equal to 0
    as dot product is the cosine of the angle between those vectors
    and cos(90) = 0 "
  [a b]
  (= (dot-product (normalize a) (normalize b)) 0))

(defn make-rect
  " Makes rectangle out of a point and two segments satisfying both conditions:
    1. both segments must have equal x-points
    2. the angle between the segments must equal 90 degrees "
  [width height]
  (if (not (orthogonal-segments? width height))
      (println "Width and Height segments aren't orthogonal!")
      (if (not (and (= (x-point (start-segment height)) (x-point (start-segment width)))
                    (= (y-point (start-segment height)) (y-point (start-segment width)))))
          (println "Segments do not start at the same point!")
          ; valid data received
          (list width height))))

(defn width-rect [r]
  (length-segment (first r)))

(defn height-rect [r]
  (length-segment (second r)))

(defn area-rect [r]
  (* (width-rect r) (height-rect r)))

(deftest area-test
  (is (= (area-rect
           (make-rect (make-segment (make-point 0 0) (make-point 4 0))
                      (make-segment (make-point 0 0) (make-point 0 4))))
         16))
  (is (close-enough? (area-rect
                       (make-rect (make-segment (make-point 5 5) (make-point 6 6))
                                  (make-segment (make-point 5 5) (make-point 6 4))))
         2 tolerance)))

(defn perimeter-rect [r]
  (+ (* 2 (width-rect r)) (* 2 (height-rect r))))

(deftest perimeter-test
  (is (= (perimeter-rect
           (make-rect (make-segment (make-point 0 0) (make-point 4 0))
                      (make-segment (make-point 0 0) (make-point 0 4))))
         16))
  (is (close-enough? (perimeter-rect
                       (make-rect (make-segment (make-point 5 5) (make-point 6 6))
                                  (make-segment (make-point 5 5) (make-point 6 4))))
         (* (sqrt 2) 4)
         tolerance)))

(run-tests 'e2-2-3)
