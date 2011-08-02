(ns e2-77-to-80
  (:use clojure.test))

; 2.77
; So, why does `magnitude` work when we add the below snippet to the
; `install-complex-package` function?
;
;    (put 'real-part '(complex) real-part)
;    (put 'imag-part '(complex) imag-part)
;    (put 'magnitude '(complex) magnitude)
;    (put 'angle '(complex) angle)
;
; Well, let's remember the structure of the number which works with our
; enhanced datatype-dispatch mechanism.
;
;     -> [*|*]   -> [*|*]     -> [*|*]
;         |          |            | |
;     [complex] [rectangular]   [3] [4]
;
; The first row of the above represents the structure of the cons cells, while
; the third represents the contents of those cells. In other words, a `complex`
; number with a `rectangular` representation is represented by a
;
;     (list 'complex 'rectangular x y)
;
; Now, what's going to happen when we call `magnitude` on a record having the above
; structure?
;
; 1) magnitude calls `apply-generic 'magnitude`
; 2) apply-generic gets the procedure registered with `put` (which is `magnitude`)
; 3) magnitude is called one more time with arguments stripped from the `'complex` tag (so that only
;    the 'rectangular tag remains)
; 4) apply-generic gets called with 'rectangular arguments which we already know how to process

; 2.78

(defn attach-tag [type-tag contents]
  (cond (or (symbol? contents) (number? contents)) contents
        :else (cons type-tag contents)))

(defn type-tag [datum]
  (cond (symbol? datum) 'scheme-symbol
        (number? datum) 'scheme-number
        (seq datum) (first datum)
        :else (throw (IllegalArgumentException. (format "Datum %s doesn't contain any type tags" datum)))))

(defn contents [datum]
  (cond (or (symbol? datum) (number? datum)) datum
        :else (second datum)))

(deftest test-278
  (is (= (attach-tag nil 1) 1))
  (is (= (attach-tag 'any 'a) 'a))
  (is (= (attach-tag 'any ['a]) (cons 'any ['a])))

  (is (= (type-tag 1) 'scheme-number))
  (is (= (type-tag 'a) 'scheme-symbol))
  (is (= (type-tag ['lol 1]) 'lol))

  (is (= (contents 1) 1))
  (is (= (contents 'a) 'a))
  (is (= (contents ['lol 1]) 1)))

; 2.79

; noop operations
(defn apply-generic [op & args] nil)
(defn real-part [x] nil)
(defn imag-part [x] nil)
(defn put [op tag proc])

(defn eq-contents [a b]
  (= (contents a) (contents b)))

(defn eq-rect-polar [a b]
  (and (= (real-part a) (real-part b))
       (= (imag-part a) (imag-part b))))

(put 'equ? '(rational rational) eq-contents)
(put 'equ? '(rectangular rectangular) eq-contents)
(put 'equ? '(polar polar) eq-contents)

(put 'equ? '(rectangular polar) eq-rect-polar)
(put 'equ? '(polar rectangular) eq-rect-polar)

; equality for complex numbers dispatches on the contents
; which can be either 'rectangular or 'polar
(put 'equ? '(complex complex)
  (fn [a b]
    (apply-generic 'equ? (contents a) (contents b))))

; equality for native clojure symbols and numbers is just '=
(put 'equ? '(scheme-symbol scheme-symbol) =)
(put 'equ? '(scheme-number scheme-number) =)

; 2.80
; This definition is actually hidden inside of the 'ration installation package
(defn numer [rat]
  (first rat))

(defn zero-number? [x] (= x 0))
; denominator cannot be 0
(defn zero-rational? [x] (= (numer x) 0))
; complex number is considered zero when both its real and imaginary parts are zero.
; I hope I got this right...
(defn zero-rect-polar? [x]
  (and (= (real-part x) 0) (= (imag-part x) 0)))

; register functions with tags
(put 'is-zero? 'rational zero-rational?)
(put 'is-zero? 'rectangular zero-rect-polar?)
(put 'is-zero? 'polar zero-rect-polar?)

(put 'is-zero? 'complex
  (fn [x]
    (apply-generic 'is-zero? (contents x))))

(put 'is-zero? 'scheme-symbol zero-number?)
(put 'is-zero? 'scheme-number zero-number?)
