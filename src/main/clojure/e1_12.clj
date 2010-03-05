(ns e1-12)

(defn pascal [row col]
  (if (or (= row col) (= col 1) (= row 1)) 1
      (+ (pascal (- row 1) col)
         (pascal (- row 1) (- col 1)))))

(pascal 1 1)
" 1?"
(pascal 3 2)
" 2?"
(pascal 5 3)
" 6?"
