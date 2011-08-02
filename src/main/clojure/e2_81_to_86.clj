(ns e2-81-to-86
  (:use [clojure.contrib.math :only (round)])
  (:use clojure.test))

; noop
(defn type-tag [datum])
(defn get-op [a b])
(defn get-coercion [a b])
(defn contents [datum] (second datum))

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
                  (throw (Exception. (str "No method for types " (cons op args))))))
        (throw (Exception. (str "No method for types " (cons op args))))))))


; 2.81
; Hugo defines additional 'noop' procedures which coerce the type to itself.
;
; a) What is going to happen if the `apply-generic` is called for arguments of
; the same type for a procedure which is not registered for this type?
;
; The `apply-generic` procedure will diverge as no amount of coercions will
; make the requested operation appear.

; b) Was Hugo sane when he decided to add these coercions?
;
; Yes. And I'm a certified psychiatrist.
; In order for `apply-generic` to work with the arguments of the same type,
; there should be a check for type equality before coercions are applied.

; c)

(defn apply-generic
  [op & args]
  (let [type-tags (map type-tag args)
        proc (get op type-tags)]
    (if proc
      (apply proc (map contents args))
      (if (= (count args) 2)
        (let [type1 (first type-tags)
              type2 (second type-tags)
              a1 (first args)
              a2 (second args)
              t1->t2 (get-coercion type1 type2)
              t2->t1 (get-coercion type2 type1)]
              ; CHANGED HERE
          (cond (= type1 type2)
                  (throw (Exception. (str "No method for types " (cons op args))))
                (not (nil? t1->t2))
                  (apply-generic op (t1->t2 a1) a2)
                (not (nil? t2->t1))
                  (apply-generic op a1 (t2->t1 a2))
                :else
                  (throw (Exception. (str "No method for types " (cons op args))))))
        (throw (Exception. (str "No method for types " (cons op args))))))))

; 2.82
;
; Create a generic version of `apply-generic` which tries to type-coerce any
; number of arguments (not just 2).
;
; One possible solution is to try to coerce every argument to the type of the
; first argument, then, if the operation wasn't found, to the type of the
; second argument, e.t.c.
; However, this strategy will fail when operation table contains operations for
; mixed types which will never get called when all of the arguments will get
; coerced to one type (even the original `apply-generic` procedure will fail in
; this case, as it always coerces arguments to the same type).

; 2.83
;
; Create a raise procedure which will raise values up the hierarchy (left to
; right):
;
;   integer -> rational -> real -> complex.
;
(defn err [msg]
  (throw (IllegalArgumentException. msg)))

; there is no way to determine a type-tag of a vanilla `raised`
; clojure number. E.g., a raised integer `1` is still an integer.
; That's why we need to attach explicit tags to everything.
(defn get-type-tag [x] (first x))

(defn integer->rational [a]
  (if (= (get-type-tag a) 'integer)
    ['rational (rationalize (contents a))]
    (err (str "Not an integer " a))))

(defn rational->real [a]
  (if (= (get-type-tag a) 'rational)
    ['real (double (contents a))]
    (err (str "Not a rational " a))))

(defn real->complex [a]
  (if (= (get-type-tag a) 'real)
    ['complex 'rectangular (contents a) 0]
    (err (str "Not a real " a))))

; now, for the generic `raise` procedure:

; this one would be implemented as a registry accessor
(defn get-raise-op [arg-type]
  (cond (= arg-type 'integer) integer->rational
        (= arg-type 'rational) rational->real
        (= arg-type 'real) real->complex
        :else (err (str "Cannot raise " arg-type))))

(defn raise [a]
  ((get-raise-op (get-type-tag a)) a))

(deftest test-283
  (is (= (get-type-tag (raise ['integer 1])) 'rational))
  (is (= (get-type-tag (raise (raise ['integer 1]))) 'real))
  (is (= (get-type-tag (raise (raise (raise ['integer 1])))) 'complex)))

; 2.84

; to make the chain of conversions (type tower) extensible we would need a set
; of procedures which would modify and ensure the integrity of the tower.
(def *raise-chain* ['integer 'rational 'real 'complex])

(defn level [x]
  (inc (.indexOf *raise-chain* x)))

(defn valid-type? [x]
  (<= (level x)
     (count *raise-chain*)))

(defn can-convert [from to]
  "returns true if `from` type can be converted to `to` type (false if `from = to`)"
  (if (and (valid-type? from) (valid-type? to))
      (and (not (= from to))
           (< (level from) (level to)))
      false))

(defn raise-chain [from to]
  "returns a list of types between and including `from` and `to`"
  (drop-while #(not (= %1 from))
              (reverse (drop-while #(not (= %1 to))
                                   (reverse *raise-chain*)))))

(defn raise-to
  "returns a function which raises the first argument to the type of the second
  or nil if it's impossible"
  [from to]
  (if (= from to) identity
      (fn [x]
        (if (can-convert from to)
            (last (take (count (raise-chain from to))
                        (iterate raise x)))
            nil))))

(defn apply-generic
  [op & args]
  (let [type-tags (map type-tag args)
        proc (get op type-tags)]
    (if proc
      (apply proc (map contents args))
      (if (= (count args) 2)
        (let [type1 (first type-tags)
              type2 (second type-tags)
              a1 (first args)
              a2 (second args)
              ; CHANGES BELOW
              t1->t2 (raise-to type1 type2)
              t2->t1 (raise-to type2 type1)]
          (cond (not (nil? t1->t2))
                  (apply-generic op (t1->t2 a1) a2)
                (not (nil? t2->t1))
                  (apply-generic op a1 (t2->t1 a2))
                :else
                  (err (str "No method for types " (cons op args)))))
        (err (str "No method for types " (cons op args)))))))

; we won't test apply-generic as the generic op registry isn't implemented
; in this scope.
(deftest test-284
  (is (= ((raise-to 'integer 'real) ['integer 1]) ['real 1.0]))
  (is (= ((raise-to 'rational 'complex) ['rational 1/2]) ['complex 'rectangular 0.5 0]))
  (is (nil? ((raise-to 'real 'integer) ['real 1.0]))))

; 2.85
;
; We will reuse `*raise-chain*`, `valid-type?' and `level` from the last exercise.
; Other procedures (such as `raise-to`) will be generalized to make `drop-to` possible.

(defn complex->real [x]
  (assert (= (first x) 'complex))
  ['real (second (rest x))])

(defn real->rational [x]
  (assert (= (first x) 'real))
  ['rational (rationalize (second x))])

(defn rational->integer [x]
  (assert (= (first x) 'rational))
  ['integer (round (second x))])

; this one would be implemented as a registry accessor
(defn get-lower-op [arg-type]
  (cond (= arg-type 'complex) complex->real
        (= arg-type 'real) real->rational
        (= arg-type 'rational) rational->integer
        :else (err (str "Cannot drop " arg-type))))

; use lower instead of `drop` as `drop` is in clojure.core
; Theoretically, we could exclude it from imports... Nah.
(defn lower [a]
  ((get-lower-op (get-type-tag a)) a))

(defn can-project [from to]
  (and (valid-type? from) (valid-type? to)
       (not (= from to))))

(defn project-chain [chain from to]
  "returns a list of types between and including `from` and `to`"
  (drop-while #(not (= %1 from))
              (reverse (drop-while #(not (= %1 to))
                                   (reverse chain)))))

(defn project-to [chain f from to]
  (if (= from to) identity
      (fn [x]
        (if (can-project from to)
            (last (take (count (project-chain chain from to))
                        (iterate f x)))
            nil))))

; An example of how `raise-to` could be refactored in the presence of a generic
; `project-to`
(defn raise-to-2 [from to] (project-to *raise-chain* raise from to))
(defn drop-to [from to] (project-to (reverse *raise-chain*) lower from to))

; Won't copy the `apply-generic` once again, as the only change is the change
; of coercion procedures from `raise-to` as defined in 2.84 to `drop-to`.

(deftest test-285
  (is (= ((raise-to-2 'integer 'real) ['integer 1]) ['real 1.0]))
  (is (= ((raise-to-2 'rational 'complex) ['rational 1/2]) ['complex 'rectangular 0.5 0]))
  (is (nil? ((raise-to-2 'real 'integer) ['real 1.0])))
  (is (= ((drop-to 'real 'integer) ['real 1.4]) ['integer 1]))
  (is (= ((drop-to 'complex 'rational) ['complex 'rectangular 0.5 0]) ['rational 1/2]))
  (is (nil? ((drop-to 'integer 'real) ['real 1.0]))))

; 2.86
;
; We'll cheat and skip the actual implementation changes in this exercise as it
; would require completely writing out the whole dispatch mechanism. Instead,
; we will list all of the required modifications.
;
; The essence of the modification is in making it possible to use type
; conversions on the components of types, such as the real and imaginary parts
; of complex numbers. To support this functionality we will have to change the
; way complex numbers are represented and significantly change all of the
; procedures which are currently used to work with complex numbers.
;
; # Representation
;
; Currently a complex is represented by a list containing two tags ('complex
; and 'rectangular/'polar) and two numbers (real and imaginary parts),
; e.g. ['complex 'rectangular 1 0].
;
; After the change a complex number is going to look like this:
; ['complex 'rectangular ['integer 1] ['integer 0]].
;
; # Functions
;
; All of the operations which are currently registered for the 'complex tag will
; have to be reimplemented to use the `apply-generic` procedure themselves.
