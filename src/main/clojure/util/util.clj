(ns util.util)

(def tolerance 0.0001)
(def dx tolerance)
(def phi 1.618)
(def e 2.718)

(defn abs [x] (if (> x 0) x (- x)))
(defn square [x] (* x x))
(defn cube [x] (* x x x))
(defn avg [x y] (/ (+ x y) 2))

; returns a function which calculates the derivative
; of the given function `g`.
(defn deriv [g]
  (fn [x]
    (/ (- (g (+ x dx)) (g x))
       dx)))

(defn close-enough? [x expected tolerance]
  (< (abs (- x expected)) tolerance))

(defn fixed-point [f first-guess]
  (let [trynext (fn [guess]
                  (let [nextval (f guess)]
                    (if (close-enough? guess nextval tolerance)
                      nextval
                      (recur nextval))))]
    (trynext first-guess)))

(defn newton-transform [g]
  (fn [x]
    (- x (/ (g x) ((deriv g) x)))))

(defn newtons-method [g guess]
  (fixed-point (newton-transform g) guess))

(defn id [x] x)
(defn curry [f arg1]
  (fn [arg] (apply f (cons arg1 (list arg)))))
(defn const [x]
  (fn [arg] x))
(defn compose [f g]
  (fn [x] (f (g x))))
(defn and-then [f g]
  (fn [x] (g (f x))))

; (repfun f 3) == (compose (compose f f) f)
(defn repfun [f n]
  (if (= n 1)
      f
      (compose f (repfun f (dec n)))))
