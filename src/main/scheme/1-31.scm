#lang scheme
(require "sicp-util.scm")

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

;3628800
(product id 1 inc 10)

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;3628800
(product-iter id 1 inc 10)

(define (fact x)
  (product id 1 inc x))

(define (2inc x)
  ((compose inc inc) x))

; calculates pi with the specified accuracy
(define (pi acc)
  (let ((upper (product id 4 2inc (- acc 1))) ; 4*6*8*...*k, k < acc
        (lower (product id 3 2inc acc)) ; 3*5*7*...*l, l <= acc
        (factor (if (even? acc)
                    (dec acc)
                    (acc)))) 
    (* 4 (/ 
          (* 2 (square upper)) ; 4*4*6*6*8*8*...*k*k, k < acc 
          (* factor (square (/ lower factor))))))) ; 3*3*5*5*...*(l-2)*(l-2)*l, l <= acc

;~3.139
(pi 600)