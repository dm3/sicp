#lang scheme
(require "sicp-util.scm")

(define (accumulate combiner zero term a next b)
  (if (> a b)
      zero
      (combiner (term a)
                (accumulate combiner zero term (next a) b))))

(define (accumulate-iter combiner zero term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a zero))

(define (sum term a next b)
  (accumulate (+) 0 term a next b))

(define (product term a next b)
  (accumulate (*) 1 term a next b))