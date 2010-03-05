#lang scheme
(require "sicp-util.scm")

(define (filtered-acc filter combiner zero term a next b)
  (if (> a b)
      zero
      (if (filter a)
          (combiner (term a) 
                    (filtered-acc filter combiner zero term (next a) next b))
          (filtered-acc filter combiner zero term (next a) next b))))

(define (prime-for? n x)
  (= (gcd n x) 1))

(eqv? (prime-for? 10 4) #f)

(define (prime? x)
  (define (iter next)
    (if (> next (sqrt x)) ; up to the square of x
        true
        (if (> (remainder x next) 0)
            (iter (inc next))
            false)))
  (if (= x 2) false (iter 2)))

(eqv? (prime? 3) #t)
(eqv? (prime? 37) #t)
(eqv? (prime? 254) #f)

(define (squares a b)
  (filtered-acc prime? + 0 square a inc b))

(eqv? (squares 1 10) 84)

(define (prod-less-than n)
  (filtered-acc (curry prime-for? n) + 0 id 1 inc n))

(eqv? (prod-less-than 10) 20)