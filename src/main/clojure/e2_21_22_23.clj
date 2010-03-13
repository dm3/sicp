(ns e2-21-22-23
  (:use clojure.test)
  (:use [clojure.contrib.generic.math-functions :only (sqr)]))

; 2.21
; This one is easy. The point of this exercise is to show how higher order functions and
; their combinators reduce the boilerplate (explicit recursion in case of scheme/clojure, for loops in scala/java).
(defn square-list [xs]
  (if (empty? xs)
      nil
      (cons (sqr (first xs))
      ; Argh.. I can't recur from a non-TC optimizable position!
      ;     (recur (rest xs)))))
      ; However, that's what expected in this exercise, so I'll leave this function unfinished.
            nil)))

; Ta-da.
(defn square-list-map [xs]
  (map sqr xs))

(deftest test-square-list-map
  (is (= (square-list-map [1 2 3 4]) [1 4 9 16])))

; 2.22
; Louis Reasoner (russian translation named this character 'Hugo Doom', which sounds like 'tugodum' = 'slowcoach/slow-witted' person).
; I guess his name should be interpreted as 'lousy reasoner' in english.
;
; His first try produces the reverse list. Why?
; Well, he wrote an internal recurring function which takes the unprocessed (unsquared) list as its first argument
; and accumulates the result in the second argument. The accumulation process takes the square of the first argument
; of the unprocessed list and conses it with the already-processed result.
; Now lets examine a simple case of list-square'ing a [1 2] vector:
; 1. things = [1 2], answer = nil
; 2. things = [2], answer = [1] : nil
; 3. things = nil, answer = [4] : [1] : nil
;
; Second try isn't much better. Lets see what happens:
; 1. things = [1 2], answer = nil
; 2. things = [2], answer = nil : [1]
; This shouldn't even work, as (cons nil x) doesn't make any sense.
(defn square-list-2 [xs]
  (letfn [(iter [things answer]
      (if (empty? things)
          answer
          ; FIRST version
          ; (recur (rest things)
          ;        (cons (sqr (first things)) answer))))]
          ; SECOND version
          (recur (rest things)
                 (cons answer (sqr (first things))))))]
    (iter xs nil)))

(deftest test-square-list-2
  ; NOTE:
  ; you can't (ns :use clojure.test :only (thrown?)) as it's not public, but macro-generated.
  (is (thrown? java.lang.IllegalArgumentException (square-list-2 [1 2 3 4]))))

; 2.23
; Write a for-each. Side effecting functions in clojure always start with a do. Lets honour the tradition.
(defn do-for-each
  " Performs an f for every x in xs and returns a nil "
  [f xs]
  (if (empty? xs) nil
      (do (f (first xs))
          (recur f (rest xs)))))

(run-tests 'e2-21-22-23)
