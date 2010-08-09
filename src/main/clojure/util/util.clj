(ns util.util)

(def tolerance 0.0001)
(def dx tolerance)
(def phi 1.618)
(def e 2.718)

(defn abs [x] (if (> x 0) x (- x)))
(defn square [x] (* x x))
(defn cube [x] (* x x x))
(defn avg [x & more]
  (/ (+ x (apply + more)) (+ 1 (count more))))
(defn pow [x n]
  (. (. java.math.BigInteger (valueOf x)) (pow n)))

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

(defn average-damp [f]
  (fn [x] (avg x (f x))))

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

; Really clean implementation of zip. I wasn't aware of the (vector) function, so I tried to write
; the haskellish recursive (accumulate-n - like) solution. However, I couldn't properly handle varargs, so I looked it up.
; The first link google returned for 'clojure varargs' was the www.mail-archive.com/clojure@googlegroups.com/msg24733.html
; which (surprise!) had the implementation for zip.
(defn zip [& lists]
  (apply map vector lists))

; (repfun f 3) == (compose (compose f f) f)
(defn repfun [f n]
  (if (= n 1)
      f
      (compose f (repfun f (dec n)))))

; I had so much trouble writing this function... The scary thing is, this
; wasn't the first time I had to write it.
(defn permutations [xs]
  (let [len (count xs)]
    ; xs can never be [], only if the initial argument is []
    (cond (= len 0) []
          (= len 1) [xs]
          :else (mapcat (fn [x] (map #(cons x %) (permutations (remove #(= x %) xs))))
                        xs))))

(defn forall
  " @param f - (A -> Bool)
    @param xs - [A]
    @return true if f holds for all x in xs, false otherwise "
  [f xs]
  (reduce #(and %1 %2) true (map f xs)))

(defn exists
  " @param f - (A -> Bool)
    @param xs - [A]
    @return true if f holds for any x in xs, false otherwise "
  [f xs]
  (reduce #(or %1 %2) false (map f xs)))

; http://groups.google.com/group/clojure/browse_thread/thread/d0ecd17cdeb740f7
(defn drop-nth [n coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (concat (take n s) (drop (inc n) s)))))

; ------------- validation --------
; http://stackoverflow.com/questions/1640311/should-i-use-a-function-or-a-macro-to-validate-arguments-in-clojure
(defmacro assert* [val test]
  `(let [result# ~test]
     (when (not result#)
       (throw (Exception.
                (str "Test failed: " (quote ~test)
                     " for " (quote ~val) " = " ~val))))))

(defmulti validate* (fn [val test] test))

(defn validate [& tests]
  (doseq [test tests] (apply validate* test)))
