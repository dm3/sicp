#lang scheme
(require "sicp-util.scm")

(define (average-damp f)
  (lambda (x) (avg x (f x))))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;1.40
(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) x)))

(newtons-method (cubic 1 2 3) 1.0)

;1.41
(define (double f)
  (compose f f))

(eqv? ((double inc) 1) 3)
;(+1)x16 (+1)x4  (+1)x2
(eqv? (((double (double double)) inc) 5) 21)
;(((compose (compose double double) (compose double double)) inc) 5)

;1.42
;done in sicp-util.scm

;1.43
;done in sicp-util.scm
(eqv? ((repeat inc 3) 1)
      ((compose (compose inc inc) inc) 1))
(eqv? ((repeat square 2) 5) 625)

;1.44
(define (smooth f)
  (define (avg a b c)
    (/ (+ a b c) 3))
  (lambda (x)
    (avg (f (- x dx))
         (f x)
         (f (+ x dx)))))

(close-enough?
 ((smooth square) 9)
 (square 9)
 tolerance)

; repeats smooth function n times and applies it to the given function
(define (n-smooth f n)
  ((repeat smooth n) f))

(close-enough?
 ((n-smooth square 10) 9)
 (square 9)
 tolerance)

;1.45
(define (square2 x)
  (average-damp (lambda (y) (/ x y))))
(define (square4 x)
  (average-damp (average-damp (lambda (y) (/ x (* y y y))))))
(define (square8 x)
  (average-damp (average-damp (average-damp (lambda (y) (/ x (* y y y y y y y)))))))
(define (square16 x)
  (average-damp (average-damp (average-damp (average-damp (lambda (y) (/ x (* y y y y y y y y y y y y y y y y))))))))

(fixed-point (square2 4) 1.0)
(fixed-point (square4 16) 1.0)
(fixed-point (square8 (* 16 16)) 1.0)
(fixed-point (square16 (* 256 256)) 1.0)
; => we need to repeat average-damp log2(n) times

(define (fixed-point-sqrt n x)
  (define (multi-mul n x)
    (if (= n 1)
        x
        (* x (multi-mul (dec n) x))))
  (let ((times (round (sqrt n)))) ;how many times do we need to repeat average-damp?
    (fixed-point ((repeat average-damp times)
                  (lambda (y) (/ x (multi-mul n y))))
                 1.0)))

(close-enough?
 (fixed-point-sqrt 16 (* 256 256))
 (fixed-point (square16 (* 256 256)) 1.0)
 tolerance)

;1.46
(define (iter-improve pred f)
  (define (iter guess)
    (let ((next (f guess)
                (if (pred next guess)
                    guess
                    (iter next))))))
  iter)

(define (fixed-point1 f)
  (iter-improve
   (lambda (guess last) (close-enough? guess next tolerance))
   f))

(define (sqrt1 x)
  (iter-improve
    (
