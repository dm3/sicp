;; ## Exercises 2.87 to 2.91
;;
(ns e2-87-to-91
  (:use clojure.test))

;; We'll not be lazy this time and will put out the whole `apply-generic`
;; framework before us.
(defn err
  "Throws an IllegalArgumentException with the given `msg`. If arguments other
  than the message string are present, message is expected to contain
  placeholders for formatting."
  [msg & vars]
  (throw (IllegalArgumentException.
           (if (seq? vars) (apply format msg vars)
               msg))))

(def *table* (ref {}))
(defn get-binding [k] (get (deref *table*) k))
(defn put-binding [k v] (dosync (alter *table* assoc k v)))

(defn get-op [op tag] (get-binding [op tag]))
(defn put-op [op tag f] (put-binding [op tag] f))

(defn attach-tag [type-tag contents]
  (cons type-tag (if (seq? contents) contents [contents])))

(defn type-tag [datum]
  (if (seq datum) (first datum)
      (err "Datum %s doesn't contain any type tags " datum)))

(defn contents [datum]
  (if (seq datum)
    (let [value (rest datum)]
      (if (> (count value) 1)
        value
        ; in case of a number we want to return a single value
        (first value)))
      (err "Datum %s doesn't contain any contents " datum)))

(defn same-variable? [exp v] (and (symbol? exp) (= exp v)))
(defn variable? [exp] (symbol? exp))

; coercions
(defn get-coercion [t1 t2] nil)
(defn put-coercion [t f])

(defn apply-generic
  [op & args]
  (let [type-tags (map type-tag args)
        proc (get-op op type-tags)]
    (if proc
      (apply proc (map contents args))
      (if (= (count args) 2)
        (let [type1 (first type-tags)
              type2 (second type-tags)
              a1 (first args)
              a2 (second args)
              t1->t2 (get-coercion type1 type2)
              t2->t1 (get-coercion type2 type1)]
          (cond (t1->t2
                  (apply-generic op (t1->t2 a1) a2))
                (t2->t1
                  (apply-generic op a1 (t2->t1 a2)))
                :else
                  (err "No method %s for type %s." op args)))))))

; generic procedures
(defn add [x y]
  (apply-generic 'add x y))
(defn sub [x y]
  (apply-generic 'sub x y))
(defn mul [x y]
  (apply-generic 'div x y))
(defn div [x y]
  (apply-generic 'div x y))
(defn is-zero? [x]
  (apply-generic 'is-zero x))
(defn gt [x y]
  (apply-generic 'gt x y))
(defn lt [x y]
  (apply-generic 'lt x y))

; packages
(defn install-number-package []
  "Installs 'number' encompassing both integer, rational and real"
  (let [tag (fn [x] (attach-tag 'number x))]
    (put-op 'add '(number number) #(tag (+ %1 %2)))
    (put-op 'sub '(number number) #(tag (- %1 %2)))
    (put-op 'mul '(number number) #(tag (* %1 %2)))
    (put-op 'div '(number number) #(tag (/ %1 %2)))
    (put-op 'is-zero '(number) #(= %1 0))
    (put-op 'gt '(number number) #(> %1 %2))
    (put-op 'lt '(number number) #(< %1 %2))
    (put-op 'make 'number #(tag %1))))

(defn mk-number [x]
  ((get-op 'make 'number) x))

; polynomials
(defn install-poly-package []
  (def ^:private *the-empty-termlist* [])
  (let [make-poly (fn [vari terms]
        ; If `make-poly` only consisted of `(cons vari terms)` the following test:
        ; `(is-zero? (mk-poly 'a []))`
        ; wouldn't work as `apply-generic` uses `apply` to call the registered `is-zero` procedure.
        ; When a polynomial doesn't have any coefficients, it's representation degenerates to `(variable)` which,
        ; when passed through apply, gets unwrapped and `is-zero` receives a symbol ('a) instead of a polynomial.
          (if (empty? terms)
            (cons vari [[0 (mk-number 0)]])
            (cons vari terms)))

        variable (fn [x] (first x))
        term-list (fn [x] (rest x))
        first-term (fn [xs] (first xs))
        rest-terms (fn [xs] (rest xs))
        empty-termlist? (fn [xs] (empty? xs))
        make-term (fn [order coeff] [order coeff])
        order (fn [term] (first term))
        coeff (fn [term] (second term))
        adjoin-term (fn [x ys]
            (if (is-zero? (coeff x)) ys
                (cons x ys)))

        ; we give an additional name to the function as there is no way to
        ; reference the let-binding from inside of the function being bound.
        add-terms (fn a-t [as bs]
          (cond (empty-termlist? as) bs
                (empty-termlist? bs) as
                :else (let [a (first-term as)
                            b (first-term bs)]
                        (cond (> (order a) (order b))
                              (adjoin-term a (a-t (rest-terms as) bs))
                              (< (order a) (order b))
                              (adjoin-term b (a-t as (rest-terms bs)))
                              :else
                              (adjoin-term
                                (make-term (order a)
                                           (add (coeff a) (coeff b)))
                                (a-t (rest-terms as) (rest-terms bs)))))))

        add-poly (fn [a b]
          (if (same-variable? (variable a) (variable b))
              (make-poly (variable a)
                         (add-terms (term-list a)
                                    (term-list b)))
              (err "Polynomials of different variables (%s, %s)!" a b)))

        mul-term-by-all-terms (fn m-t-b-a-t [a bs]
          (if (empty-termlist? bs) *the-empty-termlist*
              (let [b (first-term bs)]
                (adjoin-term
                  (make-term (+ (order a) (order b))
                             (mul (coeff a) (coeff b)))
                  (m-t-b-a-t a (rest-terms b))))))

        mul-terms (fn m-t [as bs]
          (if (empty-termlist? as) *the-empty-termlist*
              (add-terms (mul-term-by-all-terms (first-term as) bs)
                         (m-t (rest-terms as) bs))))

        mul-poly (fn [a b]
          (if (same-variable? (variable a) (variable b))
              (make-poly (variable a)
                         (mul-terms (term-list a)
                                    (term-list b)))
              (err "Polynomials of different variables (%s, %s)!" a b)))

        tag (fn [x] (attach-tag 'poly x))

; 2.87
;
; Define `zero?` for polynomials. Polynomials are represented as lists where
; the first element (head) represents the variable and the rest is the list
; of pairs containing orders of the term and the coefficients, e.g. ['a [3 2] [2 1] [1 2]].

        zero-poly? (fn [x]
          (empty? (filter #(not (is-zero? (coeff %1))) (term-list x))))

; 2.88
;
; Define generic polynomial subtraction. This is just copy-paste from `add-terms`.
; In order to avoid copy paste we could extract a generic `change-sign` procedure
; which would accept either a `sub` or an `add`.

        sub-terms (fn s-t [as bs]
          (cond (empty-termlist? as) bs
                (empty-termlist? bs) as
                :else (let [a (first-term as)
                            b (first-term bs)]
                        (cond (> (order a) (order b))
                              (adjoin-term a (s-t (rest-terms as) bs))
                              (< (order a) (order b))
                              (adjoin-term b (s-t as (rest-terms bs)))
                              :else
                              (adjoin-term
                                (make-term (order a)
                                           (sub (coeff a) (coeff b)))
                                (s-t (rest-terms as) (rest-terms bs)))))))

        sub-poly (fn [a b]
          (if (same-variable? (variable a) (variable b))
              (make-poly (variable a)
                         (sub-terms (term-list a)
                                    (term-list b)))
              (err "Polynomials of different variables (%s, %s)!" a b)))]

    (put-op 'add '(poly poly) #(tag (add-poly %1 %2)))
    (put-op 'sub '(poly poly) #(tag (sub-poly %1 %2)))
    (put-op 'mul '(poly poly) #(tag (mul-poly %1 %2)))
    (put-op 'is-zero '(poly) #(zero-poly? %1))
    (put-op 'make 'poly #(tag (make-poly %1 %2)))))

(defn mk-poly [vari terms]
  ((get-op 'make 'poly) vari terms))

(install-number-package)
(install-poly-package)

(deftest test-number-package
  (let [n0 (mk-number 0) n1 (mk-number 1) n2 (mk-number 2)
        n3 (mk-number 3) n4 (mk-number 4) n6 (mk-number 6)]
    (is (= (add n1 n2) n3))
    (is (= (add (mk-poly 'a [[1 n2] [2 n3]])
                (mk-poly 'a [[1 n2] [2 n3]]))
           (mk-poly 'a [[1 n4] [2 n6]])))

    (is (is-zero? (mk-poly 'a [])))
    (is (is-zero? (mk-poly 'a [[1 n0]])))
    (is (is-zero? (mk-poly 'a [[1 (mk-poly 'a [])]])))
    (is (not (is-zero? (mk-poly 'a [[1 n1]]))))

    (is (= (sub n2 n1) n1))
    (is (= (sub (mk-poly 'a [[1 n3] [2 n4]])
                (mk-poly 'a [[1 n2] [2 n3]]))
           (mk-poly 'a [[1 n1] [2 n1]])))))

; 2.89
;
; Define a non-sparse representation of polynomials. In this exercise
; polynomials will be represented as lists containing the variable name as
; their head element and the list of coefficients as the rest, e.g.
;
;     ['a 0 1 4 2]
;
; We will only modify the accessors so that they would return data compatible
; with the operations defined in the original `install-poly-package`.
;
; To make the reuse of the operations possible, we need to maintain the
; contract of `term-list`, `first-term`, `rest-terms`, `empty-termlist?`,
; `make-term`, `coeff` and `order`. As we don't store the order in our compact
; reprsentation, we need to be able to figure out the order of a term when
; given access to any sub-list of terms (result of `rest-terms`), which means
; that we need to store the total number of terms in the polynomial somewhere.
;
; The easiest way to do that is to append the number of terms to the end of the
; term list. We can either use a number, which is very risky as there's no way
; to determine whether the number is a valid coefficient or a piece of
; metadata, or as a tagged structure containing the number, say `{:size x}`.

(defn install-poly-package-2 []
  (let [make-poly (fn [vari terms]
          (if (empty? terms)
            (cons vari [0])
            (cons vari terms)))

        variable (fn [x] (first x))

        ; here we'll cheat by returning the original number of coefficients as
        ; the last element.
        term-list (fn [x] (reverse (cons {:size (count (rest x))}
                                         (reverse (rest x)))))

        ; return the same representation as in the original package (a pair of
        ; (order, coeff)).
        first-term (fn [xs] (let [order (- (:size (last xs)) (dec (count xs)))]
                              [order (first xs)]))
        rest-terms (fn [xs] (rest xs))

        ; only the length of the list remains
        empty-termlist? (fn [xs] (= 1 (count xs)))

        ; we don't store the order in our compact representation, so this function
        ; depends on us calling it at the right place, in the right order.
        make-term (fn [order coeff] coeff)

        ; order and coeff should only be called on the result of the `first-term`.
        order (fn [term] (first term))
        coeff (fn [term] (second term))

        ; produces the list of terms without metadata
        adjoin-term (fn [x ys] (cons x (filter #(not (:size %1)) ys)))

        ; Two functions below (add-terms and add-poly) are unchanged.  They
        ; should work with the compact representation as well as they did with
        ; the sparse one.
        add-terms (fn a-t [as bs]
          (cond (empty-termlist? as) bs
                (empty-termlist? bs) as
                :else (let [a (first-term as)
                            b (first-term bs)]
                        (cond (> (order a) (order b))
                              (adjoin-term a (a-t (rest-terms as) bs))
                              (< (order a) (order b))
                              (adjoin-term b (a-t as (rest-terms bs)))
                              :else
                              (adjoin-term
                                (make-term (order a)
                                           (add (coeff a) (coeff b)))
                                (a-t (rest-terms as) (rest-terms bs)))))))

        add-poly (fn [a b]
          (if (same-variable? (variable a) (variable b))
              (make-poly (variable a)
                         (add-terms (term-list a)
                                    (term-list b)))
              (err "Polynomials of different variables (%s, %s)!" a b)))

        tag (fn [x] (attach-tag 'poly2 x))]

    (put-op 'add '(poly2 poly2) #(tag (add-poly %1 %2)))
    (put-op 'make 'poly2 #(tag (make-poly %1 %2)))))

(defn mk-poly2 [vari terms]
  ((get-op 'make 'poly2) vari terms))

(install-poly-package-2)

(deftest test-289
  (let [n0 (mk-number 0) n1 (mk-number 1) n2 (mk-number 2)
        n3 (mk-number 3) n4 (mk-number 4) n6 (mk-number 6)]
    (is (= (add (mk-poly2 'a [n2 n3])
                (mk-poly2 'a [n2 n3]))
           (mk-poly2 'a [n4 n6])))))

; 2.90
;
; Reengineer the polynomial packages so that there would remain only one
; package which could handle both sparse and compact representations. It will
; be analogous to the complex package which can handle 'rectangular and 'polar
; representations of complex numbers.
;
; We will have to work with two representations:
; * ['a [0 1] [2 3]] - sparse
; * ['a 0 1 4 2] - compact
;
; Usually the cause of such a change in a system is either:
;
; * the need to support a different data representation (integration with a
;   separate system)
; * the need for better performance characteristics (different representations
;   for different use cases)
;
; Both reasons are valid and require similar functionality (at least from the
; user's perspective), but the approaches to their implementation most often are
; completely dissimilar. Changes for the sake of integration with a different
; data representation prioritize:
;
; * Stability of the API
; * Ease of use
; * Minimization of risk (as little as possible changes to the existing codebase)
;
; on the other hand, changes which address performance issues have one priority
; which overshadows all of the other concerns - maximum positive performance
; impact.
;
; With regards to the following exercise, performance related changes would
; require us to implement all of the operations (add/sub/mul/...) in the most
; efficient way possible. This would probably mean one implementation for each
; data representation, together with selectors and related stuff. We could make
; all of the reusable procedures dispatch through `apply-generic` and neatly
; separate packages for 'sparse and 'compact representations.
;
; The 'integration' scenario, on the other hand, can be implemented with much
; less effort, by reusing most of the original implementation for the 'sparse
; data representation. We could convert the input representation into the
; 'sparse one on every input and tag it with whatever type it was represented
; originally. This would lead to larger memory footprint in some of the cases
; (really compact polynomials) but performance isn't what we're aiming for, so
; the solution would be acceptable.

;(defn install-poly-package-3 []
;  (let [tag (fn [x] x)]
;    (put-op 'add '(poly poly) #(tag (add-poly %1 %2)))
;    (put-op 'make 'poly #(tag (make-poly %1 %2)))))

(run-tests 'e2-87-to-91)
