#lang scheme
(require "sicp-util.scm")

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (sum-ints a b)
  (sum-iter id a inc b))

(eqv? (sum-ints 1 10) 55)