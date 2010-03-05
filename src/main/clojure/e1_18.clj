(ns e1-18)

(defn twice [a]
  (* a 2))

(defn halve [a]
  (/ a 2))

(defn mul-iter [a b n]
  (cond (= a 0) n
        (even? a) (mul-iter (halve a) (twice b) n)
        :else (mul-iter (- a 1) b (+ b n))))

(defn mul [a b]
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
