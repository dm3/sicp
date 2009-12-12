(define (double a)
  (* a 2))

(define (halve a)
  (/ a 2))

(define (mul-iter a b n)
  (cond ((= a 0) n)
        ((even? a) (mul-iter (halve a) (double b) n))
        (else (mul-iter (- a 1) b (+ b n)))))

(define (mul a b)
  (mul-iter a b 0))

(mul 1 1)
" 1?"
(mul 1 2)
" 2?"
(mul 2 1)
" 2?"
(mul 3 4)
" 12?"
(mul 4 4)
" 16?"
(mul 10 10)
" 100?"
(mul 9 9)
" 81?"
