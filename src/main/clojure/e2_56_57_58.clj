(ns e2-56-57-58
  (:use clojure.test))

; Time to remember differentiation

(defn variable? [x]
  (symbol? x))

(defn same-variable? [x y]
  (and (variable? x) (variable? y) (= x y)))

(defn make-sum [a b]
  ['+ a b])

(defn sum? [a]
  (= (first a) '+))

(defn addend [a]
  (second a))

(defn augend [a]
  (second (next a)))

(defn make-product [a b]
  ['* a b])

(defn product? [a]
  (= (first a) '*))

(defn multiplier [a]
  (second a))

(defn multiplicand [a]
  (second (next a)))

(defn is-number? [x]
  (and (not (variable? x)) (number? x)))

(defn deriv [exp v]
  (cond (is-number? exp) 0
        (variable? exp) (if (same-variable? exp v) 1 0)
        (sum? exp) (make-sum (deriv (addend exp) v)
                             (deriv (augend exp) v))
        (product? exp) (make-sum (make-product (multiplier exp)
                                               (deriv (multiplicand exp) v))
                                 (make-product (multiplicand exp)
                                               (deriv (multiplier exp) v)))
        :else nil))

(deftest test-deriv-as-in-book
  (is (= (deriv '(+ x 3) 'x) ['+ 1 0]))
  (is (= (deriv '(* x y) 'x) ['+ ['* 'x 0] ['* 'y 1]])))

; 2.56
; Here we need to implement the differentiation rule for exponentiation

(defn exponentiation? [a]
  (= (first a) '**))

(defn base [a]
  (second a))

(defn exponent [a]
  (second (next a)))

(defn make-exponentiation [base exp]
  (cond (= exp 0) 1
        (= exp 1) base
        :else ['** base exp]))

; And an optimizing `make-sum` as described in the book
(defn make-sum-opt [a b]
  (cond (and (is-number? a) (= a 0)) b
        (and (is-number? b) (= b 0)) a
        (and (is-number? a) (is-number? b)) (+ a b)
        :else ['+ a b]))

(defn deriv-exp [exp v]
  (cond (is-number? exp) 0
        (variable? exp) (if (same-variable? exp v) 1 0)
        (sum? exp) (make-sum-opt (deriv-exp (addend exp) v)
                                 (deriv-exp (augend exp) v))
        (product? exp) (make-sum-opt (make-product (multiplier exp)
                                                   (deriv-exp (multiplicand exp) v))
                                     (make-product (multiplicand exp)
                                                   (deriv-exp (multiplier exp) v)))
        (exponentiation? exp) (make-product
                                (make-product (exponent exp)
                                              (make-exponentiation (base exp)
                                                                   (make-sum-opt (exponent exp) -1)))
                                (deriv-exp (base exp) v))
        :else nil))

(deftest test-deriv-modified
  (is (= (deriv-exp '(+ x 3) 'x) 1))
  (is (= (deriv-exp '(* x y) 'x) ['+ ['* 'x 0] ['* 'y 1]]))
  (is (= (deriv-exp '(** x 3) 'x) ['* ['* 3 ['** 'x 2]] 1])))

; 2.57
; Now we should extend the `make-product/sum` so that the functions would
; accept more than two arguments

(defn make-product-2-57 [multiplier & m]
  (concat (list '* multiplier) m))

(defn multiplier-2-57 [a]
  (second a))

(defn multiplicand-2-57 [a]
  (let [m (next (next a))]
    (cond (> (count m) 1) (make-product-2-57 (first m) (rest m))
          :else (first m))))

(defn make-sum-2-57 [addend & a]
  (concat (list '+ addend) a))

(defn addend-2-57 [a]
  (second a))

(defn augend-2-57 [a]
  (let [m (next (next a))]
    (cond (> (count m) 1) (make-sum-2-57 (first m) (rest m))
          :else (first m))))

; Exactly the same as the `deriv`, only the names of the functions were changed
(defn deriv-2-57 [exp v]
  (cond (is-number? exp) 0
        (variable? exp) (if (same-variable? exp v) 1 0)
        (sum? exp) (make-sum-2-57 (deriv-2-57 (addend-2-57 exp) v)
                                  (deriv-2-57 (augend-2-57 exp) v))
        (product? exp) (make-sum-2-57 (make-product-2-57 (multiplier-2-57 exp)
                                                         (deriv-2-57 (multiplicand-2-57 exp) v))
                                      (make-product-2-57 (multiplicand-2-57 exp)
                                                         (deriv-2-57 (multiplier-2-57 exp) v)))
        :else nil))
(defn make-product-2-57 [multiplier & m]
  (concat (list '* multiplier) m))

(defn multiplier-2-57 [a]
  (second a))

(defn multiplicand-2-57 [a]
  (let [m (next (next a))]
    (cond (> (count m) 1) (make-product-2-57 (first m) (rest m))
          :else (first m))))

(defn make-sum-2-57 [addend & a]
  (concat (list '+ addend) a))

(defn addend-2-57 [a]
  (second a))

(defn augend-2-57 [a]
  (let [m (next (next a))]
    (cond (> (count m) 1) (make-sum-2-57 (first m) (rest m))
          :else (first m))))
(defn make-product-2-57 [multiplier & m]
  (concat (list '* multiplier) m))

(defn multiplier-2-57 [a]
  (second a))

(defn multiplicand-2-57 [a]
  (let [m (next (next a))]
    (cond (> (count m) 1) (make-product-2-57 (first m) (rest m))
          :else (first m))))

(defn make-sum-2-57 [addend & a]
  (concat (list '+ addend) a))

(defn addend-2-57 [a]
  (second a))

(defn augend-2-57 [a]
  (let [m (next (next a))]
    (cond (> (count m) 1) (make-sum-2-57 (first m) (rest m))
          :else (first m))))

; 2.58
; a) (x + (3 * (x + (y + 2))))
; This one is easy, just make the operator infix instead of prefix.

(defn product-2-58a? [a]
  (= (second a) '*))

(defn make-product-2-58a [a b]
  (list a '* b))

(defn multiplier-2-58a [a]
  (first a))

(defn multiplicand-2-58a [a]
  (second (next a)))

(defn sum-2-58a? [a]
  (= (second a) '+))

(defn make-sum-2-58a [a b]
  (list a '+ b))

(defn addend-2-58a [a]
  (first a))

(defn augend-2-58a [a]
  (second (next a)))

(defn deriv-2-58a [exp v]
  (cond (is-number? exp) 0
        (variable? exp) (if (same-variable? exp v) 1 0)
        (sum-2-58a? exp) (make-sum-2-58a (deriv-2-58a (addend-2-58a exp) v)
                                  (deriv-2-58a (augend-2-58a exp) v))
        (product-2-58a? exp) (make-sum-2-58a (make-product-2-58a (multiplier-2-58a exp)
                                                         (deriv-2-58a (multiplicand-2-58a exp) v))
                                      (make-product-2-58a (multiplicand-2-58a exp)
                                                         (deriv-2-58a (multiplier-2-58a exp) v)))
        :else nil))

(deftest test-258a
  (is (= (deriv-2-58a '((x * y) * (x + 3)) 'x) '(((x * y) * (1 + 0)) + ((x + 3) * ((x * 0) + (y * 1))))))
  (is (= (deriv-2-58a '(x + (3 * (x + (y + 2)))) 'x) '(1 + ((3 * (1 + (0 + 0))) + ((x + (y + 2)) * 0))))))

; b)
; (x + 3 * (x + y + 2))
; This one is significantly harder. Each predicate/selector/constructor must
; account for all of the possibilities of sign placement. For example, (2 * x + 1)
; is a `sum`, but not a `product`.

(defn supported-op? [x]
  (or (= x '+) (= x '*)))

; an expression is a product when it contains a '* and doesn't contain a '+.
; We do the querying in three passes, for example, take (2 * x + 1)
; 1. (2 * x + 1) -> (* +)
; 2. (* +) -> (true false)
; 3. (true false) -> (and true false) -> false
(defn product-2-58b? [a]
  (reduce #(and % %2) true
  (map (fn [x]
         (cond (= x '+) false
               (= x '*) true))
    (filter #(supported-op? %) a))))

(defn make-product-2-58b [a b]
  (list a '* b))

; At first there was duplication between multiplier and addend, the only
; difference between them being the symbol ('* vs '+).
(defn first-part [a sym]
  (let [m (take-while #(not (= % sym)) a)]
    (cond (> (count m) 1) m
          :else (first m))))

(defn second-part [a sym]
  (let [m (drop 1 (drop-while #(not (= % sym)) a))]
    (cond (> (count m) 1) m
          :else (first m))))

(defn multiplier-2-58b [a]
  (first-part a '*))

(defn multiplicand-2-58b [a]
  (second-part a '*))

; an expression is a sum when it contains at least one '+
(defn sum-2-58b? [a]
  (reduce #(or % %2) false
  (map #(= % '+)
       (filter #(supported-op? %) a))))

(defn make-sum-2-58b [a b]
  (list a '+ b))

(defn addend-2-58b [a]
  (first-part a '+))

(defn augend-2-58b [a]
  (second-part a '+))

(defn deriv-2-58b [exp v]
  (cond (is-number? exp) 0
        (variable? exp) (if (same-variable? exp v) 1 0)
        (sum-2-58b? exp) (make-sum-2-58b (deriv-2-58b (addend-2-58b exp) v)
                                  (deriv-2-58b (augend-2-58b exp) v))
        (product-2-58b? exp) (make-sum-2-58b (make-product-2-58b (multiplier-2-58b exp)
                                                         (deriv-2-58b (multiplicand-2-58b exp) v))
                                      (make-product-2-58b (multiplicand-2-58b exp)
                                                         (deriv-2-58b (multiplier-2-58b exp) v)))
        :else nil))

(deftest test-258b
  (is (= (deriv-2-58b '((x * y) * (x + 3)) 'x) '(((x * y) * (1 + 0)) + ((x + 3) * ((x * 0) + (y * 1))))))
  (is (= (deriv-2-58b '(x + 3 * (x + y + 2)) 'x) '(1 + ((3 * (1 + (0 + 0))) + ((x + y + 2) * 0))))))
