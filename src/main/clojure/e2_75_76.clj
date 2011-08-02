(ns e2-75-76
  (:use clojure.test))

; The chapter preceding ex. 2.75 is really a revelation. We are exposed to the
; object oriented approach to abstraction - abstraction of columns in the
; datatype->operations table in the fig. 2.22 (as opposed to abstraction of
; rows enabled by the data-centric programming). The choice between the two
; forms of abstraction is essentialy what's called an
; [expression problem](http://www.daimi.au.dk/~madst/tool/papers/expression.txt).
;
; An interesting discussion of the expression problem can be found
; [here](http://lambda-the-ultimate.org/node/2232).

; 2.75

(defn make-from-mag-ang [r a]
  (fn [op]
    (cond (= op 'real-part) (* r (-> a Math/toRadians Math/cos))
          (= op 'imag-part) (* r (-> a Math/toRadians Math/sin))
          (= op 'magnitude) r
          (= op 'angle) a
          :else (assert false))))

(deftest test-make-from-mag-ang
  (is (= ((make-from-mag-ang 10 90) 'imag-part) 10))
  (is (= ((make-from-mag-ang 10 90) 'magnitude) 10))
  (is (= ((make-from-mag-ang 10 90) 'angle) 90)))

; 2.76
; Which strategy suits the situation where new datatypes are added more often than new functions better?
; Of course, the strategy presented in this chapter - message passing. In this
; case we will only need to create a new datatype constructor which will
; encapsulate the dispatches for different messages.
;
; Which strategy suits best when new functions are added more often?
; The data-centric programming suits better in this case as we don't need to
; modify datatypes when new operations are added. If we were to choose message
; passing we would need to modify each datatype present in the system when new
; function (operation) got added.
