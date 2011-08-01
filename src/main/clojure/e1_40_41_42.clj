;; ## Exercises 1.40 - 1.42
(ns e1-40-41-42
 (:use [util.util :only (repfun compose square)])
 (:use [clojure.contrib.test-is :only (deftest is run-tests)]))

;; ### 1.40
;;
;; We must define a function which will use newtons-method to find the
;; approximate 0 (zero) of the cubic equation of the form
;;
;;     `x^3 + ax^2 + bx + c`
;;
;; I.e. a function which approximates to a `sqrt(x)` is defined as
;;
;;     `y -> y^2 - x, y0 = 1`
;;
;; Newtons method idea is to start with a reasonable guess of the result and
;; iterate further by using:
;;
;;     X_n+1_ = X_n_ - f(Xn)/f'(Xn)
;;
;; This means we have to express given differentiable function in a way that's
;; iteratable by the (newtons-method) function. In our case:
;;
;; * g(x) = x^3 + ax^2 + bx + c = y
;; * Dg(x) = 3x^2 + x^2 + b
;;
;; Actually, this exercise doesn't need any of this stuff. Disregard it and carry on.
(defn cubic [a b c]
  (fn [x] (+ (* x x x)
             (* x x a)
             (* x b)
             c)))

;; `2^3 + 2^3 + 2^2 + 2 = 22`
(deftest cubic-test
  (is (= ((cubic 2 2 2) 2) 22)))

;; ### 1.41

(defn doubl
  "Essentially a `(repfun f 2)`"
  [f]
  (fn [x] (f (f x))))

(deftest doubl-test-1
  (is (= ((doubl inc) 1) 3)))

;; This one is tricky. It's obvious, that `((double inc) x)` will perform an `(inc (inc x))`.
;; Let's see what is going to happen with `(((doubl doubl) inc) x)`.
;;
;; Lets expand doubles one by one:
;;
;; 1. `(((doubl doubl) inc) x)`
;; 2. `(fn [x] (doubl (doubl inc)) x)`
;; 3. `(fn [x] (doubl (inc (inc x))))`
;; 4. `(fn [x] (inc (inc (inc (inc x)))))`
;;
;; So we can say that nesting `doubl`s once will result in the argument function being applied 4 times.
(deftest doubl-test-2
  (is (= (((doubl doubl) inc) 1) 5)))

;; And what about `(((doubl (doubl doubl)) inc) x)` ? Oh, it will clearly apply the inc function 8 times, will it?
;; Actually, no. For the sake of clarity we'll call `(doubl doubl)` a `(quad)`. Lets see what happens here:
;;
;;     `(((doubl quad) inc) x)` -> `((quad (quad inc)) x)`
;;
;; That's not an 8x application, but a 16x! We apply `(doubl doubl)` to the `inc` once and then apply
;; `(doubl doubl)` to the result of the previous application.
(deftest doubl-test-2
  (is (= (((doubl (doubl doubl)) inc) 5) 21)))

;; ### 1.42
;;
;; `compose` is defined in util/util.clj
(deftest compose-test
  (is (= ((compose square inc) 6) 49)))

(run-tests 'e1-40-41-42)
