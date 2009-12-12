#lang scheme
;f(n) = n if n < 3
;f(n) = f(n-1) + f(n-2) + f(n-3) if n >= 3

(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1)) (f (- n 2)) (f (- n 3))))))

(define (fi n)
  (if (< n 3) n
      (f-iter 0 1 2 (- n 1))))

(define (f-iter a b c n)
  (if (< n 3) (+ a b c)
      (f-iter b c (+ a b c) (- n 1))))


(fi 3)
" 3?"
(fi 4)
" 6?"
(fi 5)
" 11?"
