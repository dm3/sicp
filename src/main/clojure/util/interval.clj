(ns util.interval)

(defn make-interval [a b] [a b])

(defn lower-bound [z]
  (get z 0))

(defn upper-bound [z]
  (get z 1))

(defn mul-interval [a b]
  (let [p1 (* (lower-bound a) (lower-bound b))
        p2 (* (lower-bound a) (upper-bound b))
        p3 (* (upper-bound a) (lower-bound b))
        p4 (* (upper-bound a) (upper-bound b))]
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(defn add-interval [a b]
  (make-interval (+ (lower-bound a) (lower-bound b))
                 (+ (upper-bound a) (upper-bound b))))

(defn div-interval [a b]
  (mul-interval a
    (make-interval (/ 1 (upper-bound b))
                   (/ 1 (lower-bound b)))))

