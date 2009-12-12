(define (fast-exp-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-exp-iter (square b) (/ n 2) a))
        (else (fast-exp-iter b (- n 1) (* a b)))))

(define (fast-exp b n)
  (fast-exp-iter b n 1))

(define (square a)
  (* a a))

(fast-exp 2 3)
" 8?"
(fast-exp 4 4)
" 256?"
