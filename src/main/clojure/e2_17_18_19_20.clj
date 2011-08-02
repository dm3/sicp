(ns e2-17-18-19-20
  (:use [clojure.contrib.test-is :only (deftest is run-tests)])
  (:use [clojure.contrib.math :only (round)]))

; 2.17
(defn last-pair [xs]
  ; (seq xs) is a clojure idiom for checking if the seq is non-empty.
  ; I would have used (not (empty? xs)) if I wouldn't have read the docstring for (empty?).
  (if (seq (next xs))
      (last-pair (next xs))
      xs))

(deftest test-last-pair
  ; we can use either vectors which have syntactic sugar support (square brackets), or (list).
  ; The difference is the same (at least that's what I picked up from the internets) as that of
  ; the ArrayList (vector) and the LinkedList in java.
  (is (= (last-pair [1 2 3]) [3]))
  (is (= (last-pair []) [])))

; 2.18
; See the clean version of reverse in clojure.core source
(defn my-reverse [xs]
  (if (empty? xs) xs
      ; scheme append = clojure concat
      (concat (my-reverse (next xs)) [(first xs)])))

(deftest test-reverse
  (is (= (my-reverse [1 4 9 16 25]) [25 16 9 4 1])))

; 2.19
(def us-coins [50 25 10 5 1])
(def uk-coins [100 50 20 10 5 2 1 0.5])

(defn no-more? [coin-values]
  (empty? coin-values))

(defn except-first-denomination [coin-values]
  (next coin-values))

(defn first-denomination [coin-values]
  (first coin-values))

(defn cc [amount coin-values]
  (cond (= amount 0) 1
        (or (< amount 0) (no-more? coin-values)) 0
        :else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values))))

(deftest test-cc
  (is (= (cc 100 us-coins) 292))
  ; does the order of coins matter?
  (is (= (cc 100 (conj (next us-coins) (first us-coins))) 292))
  (is (= (cc 100 (reverse us-coins)) 292)))

; As cc counts the number of combinations, the initial order of coins shouldn't matter. In fact, it doesn't!
; The result of cc is always the same. After all, all we do is try out all of the possible combinations
; of coins.

; 2.20
; Clojure doesn't have dot-notation, instead it has the and-notation which is essentially the same.
; The purpose of this exercise is the introduction of the dot-notation. Otherwise it's quite simple.
(defn same-parity
  " Returns the list of arguments having the same parity as the first argument "
  [x & xs]
  ; had to make iter accumulating (iterative process) as clojure doesn't recur from non-TC position.
  (letfn [(iter [result ys even]
                (if (empty? ys) result
                    (if (or (and even (even? (first ys)))
                            (not (or even (even? (first ys)))))
                        (recur (conj result (first ys)) (rest ys) even)
                        (recur result (rest ys) even))))]
    (cons x (iter [] xs (even? x)))))

(deftest test-same-parity
  (is (= (same-parity 1 2 3 4 5 6 7) [1 3 5 7]))
  (is (= (same-parity 2 3 4 5 6 7) [2 4 6])))
