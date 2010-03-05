(ns e1-17)

(defn twice [a]
  (* a 2))

(defn halve [a]
  (/ a 2))

(defn mul [a b]
  (cond (= a 0) 0
        (= b 0) 0
        (= a 1) b
        (even? a) (mul (halve a) (twice b))
        :else (+ b (mul (- a 1) b))))

(mul 1 1)
" 1?"
(mul 1 2)
" 2?"
(mul 2 1)
" 2?"
(mul 3 4)
" 12?"
(mul 0 1)
" 0?"
(mul 10 10)
" 100?"
(mul 9 9)
" 81?"
