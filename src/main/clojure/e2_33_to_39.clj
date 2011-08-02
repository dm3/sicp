(ns e2-33-to-39
  (:use [util.util :only (zip)])
  (:use clojure.test)
  (:use [clojure.contrib.generic.math-functions :only (sqr)]))

; 2.33
; accumulate = foldr. Clojure reduce = foldl
; You can easily distinguish foldl from foldr because the arguments order for the reducing function differs:
; foldl has the accumulator argument in the first position while foldr has it in the second.
; Consider how the (foldl f 0 [1 2 3]) is computed:
;   (f 0 1) 2 3
;   (f (f 0 1) 2) 3
;   (f (f (f 0 1) 2) 3)
; Same case for foldr holds:
;   1 2 (f 3 0)
;   1 (f 2 (f 3 0))
;   (f 1 (f 2 (f 3 0)))
(defn accumulate [f zero xs]
  (if (empty? xs) zero
    (f (first xs)
       (accumulate f zero (rest xs)))))

(defn my-map [f xs]
  (accumulate (fn [el acc]
                (cons (f el) acc)) [] xs))

(deftest test-my-map
  (is (= (my-map #(+ 1 %) [1 2 3]) [2 3 4])))

; This one was surprisingly hard. I still think I'm cheating by using conj.
; Then there is this drudgery involving nil? check...
(defn append [xs ys]
  (accumulate cons (rest ys) (if (nil? (first ys)) xs
                                 (conj xs (first ys)))))

(deftest test-append
  (is (= (append [1 2 3] [4 5 6]) [1 2 3 4 5 6]))
  (is (= (append [] [1 2]) [1 2]))
  (is (= (append [1 2] []) [1 2])))

(defn length [xs]
  (accumulate #(+ 1 %2) 0 xs))

(deftest test-length
  (is (= (length [1 2 3]) 3))
  (is (= (length []) 0)))

; 2.34
; Horners rule makes polynoms lispy.
(defn horner-eval [x coeff-seq]
  (accumulate (fn [el acc]
                (+ el (* (horner-eval x (rest coeff-seq)) x)))
              0 coeff-seq))

(deftest test-horner-eval
  (is (= (horner-eval 2 [1 3 0 5 0 1]) 79))
  (is (= (horner-eval 2 []) 0)))

; 2.35
; t is a leaf tree (all information is in the leaves) composed of lists
; (1 (2 (3 4)) 7)
;      -- *
;     /  / \
;    1  *   7
;      / \
;     2   *
;        / \
;       3   4
(defn count-leaves [t]
  (accumulate (fn [el acc]
                (if (sequential? el)
                    (+ (count-leaves el) acc)
                    (inc acc))) 0
              t))

(deftest test-count-leaves
  (is (= (count-leaves [1 [2 [3 4]] 7]) 5)))
; Seems that I'm doing well without a map. I suppose a call to map should
; flatten the tree structure (after a second thought, is it even possible to
; flatten a nested list with a map? It seems you would need to access more
; than the current element => you would need to simulate a fold. But map is
; already a specific instance of a fold (some people with thick glasses and big
; bushy beards call fold catamorphism)).

; 2.36
(defn accumulate-n [op init seqs]
  (if (empty? (first seqs))
      nil
      (cons (accumulate op init (map first seqs))
            (accumulate-n op init (map rest seqs)))))
; This one was really easy. The solution just popped into head (nevermind it's midnight already).

(deftest test-accumulate-n
  (is (= (accumulate-n + 0 [[1 2 3] [4 5 6] [7 8 9] [10 11 12]]) [22 26 30])))

; 2.37
; Matrix and matrices.
; A vector is a sequence of numbers [1 2 3] while a matrix is a sequence of vectors, thus
; 1 1 1
; 2 2 2
; 3 3 3
; is represented as [[1 1 1] [2 2 2] [3 3 3]].
;
; At first I made a big mistake of confusing the matrix representation used in this exercise (lists represent rows).
; No wonder I couldn't write (matrix-times-vector) without transposing the argument matrix.
(defn dot-product [v w]
  (accumulate + 0 (map * v w)))

(def *123v* [1 2 3])
(def *123m* [[1 1 1] [2 2 2] [3 3 3]])

; 1 1 1
; 2 2 2 * [1 2 3] = [3 * 1 * 1, 3 * 2 * 2, 3 * 3 * 3] = [3, 12, 27]
; 3 3 3
; We could zip each row with the corresponding vector element
; zip m v
; and fuse them by mapping the result with
; \(row, el) -> foldl (\acc rowel -> acc + rowel * el ) 0 row

(defn matrix-times-vector [m v]
  ; for each row in the matrix (m)
  (map (fn [row]
         (accumulate (fn [el acc]
                       (+ acc (* el (second row)))) 0
                     ; I cheated here by zipping matrix with the vector
                     (first row))) (zip m v)))

(deftest test-times-vector
  (is (= (matrix-times-vector *123m* *123v*) [3 12 27])))

; transpose x = map head x : transpose (map tail x)
; Of course, the above would fail when there were no more heads in the tails of x or when x wasn't a matrix.
(defn transpose [m]
  (accumulate-n cons [] m))

(deftest test-transpose
  (is (= (transpose *123m*) [*123v* *123v* *123v*])))

; I think that this solution is more complex than it ought to be, oh well...
(defn matrix-times-matrix [m n]
  (let [cols (transpose n)]
    (map (fn [row]
           ; for each row element and col element, multiply them together and bind the resulting list of lists to (prods)
           (let [prods (map #(accumulate-n * 1 [row %1]) cols)]
             (map #(accumulate + 0 %1) prods)))
         m)))

(deftest test-times-matrix
  (is (= (matrix-times-matrix *123m* *123m*) [[6 6 6] [12 12 12] [18 18 18]])))

; 2.38
; Here we are told that the accumulate is in fact a foldr.
(defn foldl [f zero xs]
  (letfn [(iter [result remaining]
            (if (empty? remaining) result
                (recur (f result (first remaining))
                       (rest remaining))))]
    (iter zero xs)))

(deftest test-foldl
  (is (= (accumulate + 0 [1 2 3]) (foldl + 0 [1 2 3])))
  (is (not (= (accumulate - 0 [1 2 3]) (foldl - 0 [1 2 3]))))
  ; (/ 1 1) [2 3]
  ; (/ 1 2) [3]
  ; (/ 0.5 3) []
  (is (= (foldl / 1 [1 2 3]) (/ (/ 1 2) 3)))
  ; (/ 1 1) [2 3]
  ; (/ 1 (/ 2) [3]
  ; (/ 1 (/ 2 (/ 3 1)))
  (is (= (accumulate / 1 [1 2 3]) (/ 3 2)))
  (is (= (foldl list nil [1 2 3]) [[[nil 1] 2] 3]))
  (is (= (accumulate list nil [1 2 3]) [1 [2 [3 nil]]])))

; All of the tests passed the first time I wrote them. Gravity cat is amused.
;
; Concerning the second part of the exercise (the question):
; the operation passed into foldl and foldr should be associative in order for them to produce identical results (semigroup).

; 2.39
(defn my-reverse-foldr [xs]
  (accumulate (fn [x y]
                (concat y [x])) nil xs))

; foldl (flip cons) nil
(defn my-reverse-foldl [xs]
  (foldl (fn [x y]
            (cons y x)) nil xs))

(deftest test-my-reverse
  (is (= (my-reverse-foldr [1 2 3]) [3 2 1]))
  (is (= (my-reverse-foldl [1 2 3]) [3 2 1])))
