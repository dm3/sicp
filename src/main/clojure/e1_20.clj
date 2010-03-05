(ns e1-20)

(defn divides? [a b]
  (= (rem b a) 0))

(defn square [a]
  (* a a))

(defn expmod [base exp m]
  (cond (= exp 0) 1
    (even? exp) (rem (square
                           (expmod base (/ exp 2) m)) m)
    :else (rem (* base (expmod base (- exp 1) m))
                     m)))

(defn find-divisor [n test]
  (cond (> (square test) n) n
    (divides? test n) test
    :else (find-divisor n (+ test 1))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(defn prime? [n]
  (= (smallest-divisor n) n))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)
