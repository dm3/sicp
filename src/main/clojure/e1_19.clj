(ns e1-19)

(defn fib-iter [a b p q cnt]
  (cond (= cnt 0) b
    (even? cnt) (fib-iter a
                          b
                          (+ (* q q) (* p p))
                          (+ (* 2 p q) (* q q))
                          (/ cnt 2))
    :else (fib-iter (+ (* b q) (* a q) (* a p))
                    (+ (* b p) (* a q))
                    p
                    q
                    (- cnt 1))))

(defn fib [n]
  (fib-iter 1 0 0 1 n))

(fib 0)
" 0?"
(fib 1)
" 1?"
(fib 2)
" 1?"
(fib 3)
" 2?"
(fib 10)
" 55?"
