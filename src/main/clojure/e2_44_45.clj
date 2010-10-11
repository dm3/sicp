(ns e2-44-45
  (:use clojure.test))

; 2.44
; First some stubs so that the definition would compile

(defn beside [a b] nil)
(defn below [a b] nil)

; Essentially the same as `right-split` with the order of below/beside flipped
(defn up-split [painter n]
  (if (= n 0)
    painter
    (let [smaller (up-split painter (dec n))]
      (below painter (beside smaller smaller)))))

; 2.45

; The following definitions should result in a function which produces the same
; results as the definition for `up-split` above and the definition for
; `right-split` in the book.
;
;(defn right-split
;  (split beside below))
;(defn up-split
;  (split below beside))
;
; Lets note that the only thing different between the `right` and `up` splits is the last line:
;
; * right-split:  (beside (below m m) painter))))
; * up-split:     (below (beside m m) painter))))
;
; Which means that we need to return a function which takes a `painter` and an
; `n`. Inside of that function we need to use the arguments (in lexical scope)
; to the `split` function.

(defn split [f g]
  (fn [painter n]
    (if (= n 0)
      painter
      (let [m ((split f g) painter (dec n))]
        (f (g m m) painter)))))
