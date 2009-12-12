#lang scheme
(provide phi e dx tolerance                                     ;constants
         id const compose and-then repeat
         inc dec
         square cube avg
         deriv fixed-point
         close-enough?) 

(define tolerance 0.0001)
(define dx tolerance)
(define phi 1.618)
(define e 2.718)

(define (inc x) (+ x 1))
(define (dec x) (- x 1))
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (avg x y) (/ (+ x y) 2))
; returns a function which calculates the derivative
; of the given function `g`.
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (close-enough? x expected tolerance) 
  (< (abs (- x expected)) tolerance)) 

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next tolerance)
          next
          (try next))))
  (try first-guess))

(define (id x) x)
(define (curry f arg1)  
  (lambda (arg) (apply f (cons arg1 (list arg)))))
(define (const x)
  (lambda (arg) x))
(define (compose f g)
  (lambda (x) (f (g x))))
(define (and-then f g)
  (lambda (x) (g (f x))))
; (repeat f 3) == (compose (compose f f) f)
(define (repeat f n)
  (if (= n 1)
      f
      (compose f (repeat f (dec n)))))
