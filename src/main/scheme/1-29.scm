#lang scheme
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; f - function to integrate
; a, b - points to integrate between
; n - precision (the more, the better)
(define (integral f a b n)
  ;h = (b - a) / n
  (let ((h (/ (- b a) n)))
    (define (term ind)
      ;Yind = f(a + ind*h)
      (let ((y (f (+ a (* ind h)))))
        (cond ((= 0 ind) y) 
              ((odd? ind) (* 4 y))
              ((even? ind) (* 2 y)))))
    (define (next ind)
      (+ 1 ind))
    (* (sum term 0 next n) 
       (/ h 3))))

(define (cube x) (* x x x))

;(integral cube 0 1 1000)