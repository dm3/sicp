(ns e2-4-5-6
  (:use [clojure.contrib.test-is :only (deftest is run-tests)])
  (:use [util.util :only (pow id)])
  (:use [clojure.contrib.generic.math-functions :only ()]))

; 2.4
; Trivial! cons returns a function which itself accepts a function accepting two arguments:
; the first is car, the second - cdr

(defn cons! [x y]
  (fn [m] (m x y)))

(defn car [z]
  (z (fn [p q] p)))

(defn cdr [z]
  (z (fn [p q] q)))

(deftest test-car-cdr
  (is (= (car (cons! 1 2)) 1))
  (is (= (cdr (cons! 1 2)) 2)))

; 2.5
; I suck at these exercises :/
; We need to decompose the result of 2^a * 3^b into 'a' for 'car' and 'b' for 'cdr'.
; 2^a * 3^b = x
; a*log2 + b*log3 = logx => a = (logx - b*log3) / log2
; This obviously won't work as we need to know b. I would be a disgrace for a CS speciality. It's fortunate I'm a SE.
(defn cons-n [x y]
  (* (pow 2 x) (pow 3 y)))

; But there is a simple solution:
; car-n = the number of times we need to divide the result of cons by 2 before we reach a power of 3,
; cdr-n is symmetric to car-n.
;
; Can this be written as a fold . take-while? Should think about implementing this with a fold.
(defn- iter [z n x]
  (if (= (rem z x) 0)
      (recur (/ z x) (inc n) x)
      n))

(defn car-n [z]
  (iter z 0 2))

(defn cdr-n [z]
  (iter z 0 3))

(deftest test-car-cdr-n
  (is (= (car-n (cons-n 3 2)) 3))
  (is (= (cdr-n (cons-n 3 2)) 2)))

; 2.6
; Ahh.. The Church numerals. I can't imagine the wickedness of a man able to invent stuff like this.
; Representing numbers in terms of functions.

; Zero is easy: it returns a function which disregards its argument and returns an id function.
; Put it other way, *zero* must be applied once
(def *zero*
  (fn [f] id))

; Add-1 isn't easy... Lets expand add-1 with the *zero* as an argument
; 1. (add-1 *zero*) ->
; 2. (fn [f] (fn [x] (f ((*zero* f) x)))) ->
; 3. (fn [f] (fn [x] (f (((fn [z] id) f) x)))) ->
; 4. (fn [f] (fn [x] (f (id x)))) ->
; 5. (fn [f] (fn [x] (f x)))
;
; At step 5 we have a function which returns a closure over its argument (which itself must be a function).
; The closure, when applied, applies the argument of the function it closes over to the argument of its own (the argument of the closure).
; In simple terms - (add-1 *zero*) returns a function which must be applied two times to yield a result.
;
; For the sake of fun and profit, lets expand the add-1 with the result of the previous expansion
; 1. (add-1 (add-1 *zero*)) ->
; 2. (fn [f] (fn [x] (f (((fn [g] (fn [h] (g h))) f) x)))) ->
; 3. (fn [f] (fn [x] (f ((fn [h] (f h)) x)))) ->
; 4. (fn [f] (fn [x] (f (f x))))
;
; Here we see that 'f' is applied 2 times (which represents the 'number 2' in the church encoding).
(defn add-1 [n]
  (fn [f]
    (fn [x]
      (f ((n f) x)))))

; Given the expansions we did in the previous comment block, defining *one* and *two* is trivial.
(def *one*
  (fn [f] (fn [x] (f x))))

(def *two*
  (fn [f] (fn [x] (f (f x)))))

; #(+ 1 %) is a macro for fn [x] (+ 1 x)
; same as (_ + 1) in scala
(deftest one-two-test
  (is (= (((add-1 *one*) #(+ 1 %)) 0) 2))
  (is (= (((add-1 *two*) #(+ 1 %)) 0) 3)))

; + is trickier. Even more so if we can't use (add-1).
; first try
; (defn plus [a b]
;   (fn [f] (fn [x] (a (b x))))
; failed.
; Note that 'a' and 'b' themselves are church numerals. This means that we need to apply them
; to the first argument of the function returned as the result of a (plus), which leads us
; to the working version of (plus):
(defn plus [a b]
  (fn [f] (fn [x] ((a f) ((b f) x)))))

(deftest plus-test
  ; should be equal to (*two*)
  (is (= (((plus (add-1 *zero*) (add-1 *zero*)) #(+ 1 %)) 0) 2)))
