(ns e1-16)

(defn square [a]
  (* a a))

(defn fast-exp-iter [b n a]
  (cond (= n 0) a
        (even? n) (fast-exp-iter (square b) (/ n 2) a)
        :else (fast-exp-iter b (- n 1) (* a b))))

(defn fast-exp [b n]
  (fast-exp-iter b n 1))

(fast-exp 2 3)
" 8?"
(fast-exp 4 4)
" 256?"
