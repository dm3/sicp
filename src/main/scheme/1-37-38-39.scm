#lang scheme
(require "sicp-util.scm")

;1.37
(define (cont-frac n d k)
  (define (iter ind)
    (if (> ind k)
        0
        (/ (n ind) (+ (d ind) (iter (inc ind))))))
  (iter 1))

; Follows the same pattern as all of the recursive -> iterating process transformations.
; Add an accumulating parameter to the physically recursive function 
; and return it instead of zero in the end of computation.
(define (cont-frac-iter n d k)
  (define (iter ind result)
    (if (> ind k)
        result
        (iter (inc ind) (/ (n ind) (+ (d ind) result)))))
  (iter 1 0))

; ~= 1/phi
(close-enough? 
 (cont-frac (const 1.0) (const 1.0) 100) 
 (/ 1 phi) 
 0.001)

(close-enough? 
 (cont-frac-iter (const 1.0) (const 1.0) 100) 
 (/ 1 phi) 
 0.001)

;1.38
; ~= e - 2
(close-enough? 
 (cont-frac (const 1.0) 
            ; Series = [1, 2, 1, 1, 4, 1, 1, 6, ..] (wonder if haskell can infer the algorithm)
            ; If we index the series beginning with 1, we can determine the value of the 
            ; corresponding element of the series using the index:
            ; `if` (mod (index - 2) 3) == 0 `=>` series[index] == ((index - 2) * 2) + 2 `else` series[index] == 1
            (lambda (ind) 
              (let ((base-ind (- ind 2)))
                (if (= 0 (remainder base-ind 3))
                    (+ 2 (* 2 (/ base-ind 3)))
                    1)))
            100)
 (- e 2)
 0.001)

;1.39
(define (tan-cf x k)
  (cont-frac (lambda (ind)
               (if (= ind 1) x (- (square x))))
             (lambda (ind) (dec (* ind 2)))
             k))

(close-enough?
 (tan-cf 20 100)
 (tan 20)
 0.001)
  


