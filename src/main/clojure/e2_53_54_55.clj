(ns e2-53-54-55
  (:use clojure.test))

; 2.53

(defn memq [item x]
  (cond (empty? x) false
        (= item (first x)) true
        :else (memq item (next x))))

(deftest test-2-53
  (is (= (list 'a 'b 'c)
         '(a b c)))

  (is (= (list (list 'george))
         '((george))))

  ; cdr = next
  ;(cdr '((x1 x2) (y1 y2)))
  (is (= (next '((x1 x2) (y1 y2)))
         '((y1 y2))))

  ; cadr = second
  ;(cadr '((x1 x2) (y1 y2)))
  (is (= (second '((x1 x2) (y1 y2)))
         '(y1 y2)))

  ; the closest analog of `pair?` I could find was `sequential?`
  ;(pair? (car '(a short list)))
  (is (= (sequential? (first '(a short list)))
         false))

  (is (= (memq 'red '((red shoes) (blue socks)))
         false))

  (is (= (memq 'apple '(x (apple sauce) y apple pear))
         true)))

; 2.54

; `=` already does equalities on lists, however we'll write our own which will
; delegate to `=` only for the non-seq arguments.
(defn equal? [a b]
  (cond (and (not (sequential? a)) (not (sequential? b))) (= a b)
        (and (sequential? a) (sequential? b)) (and (equal? (first a) (first b)) (equal? (next a) (next b)))
        :else false))

(deftest test-equal?
  (is (= (equal? 'a 'a) true))
  (is (= (equal? 'a 'b) false))
  (is (= (equal? '(a b) '(a b)) true))
  (is (= (equal? '(a b) '(a a)) false))
  (is (= (equal? '(this is a list) '(this is a list)) true))
  (is (= (equal? '(this is a list) '(this (is a) list)) false)))

; 2.55

; The output is 'quote, because the ''abracadabra gets expanded to (quote
; (quote abracadabra)). As we remember, quote doesn't allow its arguments to be
; evaluated, that's why the result of the first `quote` is a list containing
; two elements: 'quote and 'abracadabra.
(deftest test-abracadabra
  (is (= (first ''abracadabra) 'quote)))

(run-tests 'e2-53-54-55)
