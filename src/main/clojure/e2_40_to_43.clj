(ns e2-40-to-43
  (:use clojure.test)
  (:use [util.util :only (zip, permutations, exists)])
  (:use [clojure.contrib.generic.math-functions :only (sqrt, round)])
  (:use [clojure.contrib.seq-utils :only (flatten)]))

; 2.40
(defn enum [a b]
  (take b (iterate #(+ 1 %1) a)))

(deftest test-enum
  (is (= (enum 1 6) [1 2 3 4 5 6])))

; So this is an anamorphism (we'll unfold a value into a seq of tuples).
; Unique-pairs should work in two steps: generate and filter. In the first step we generate all possible tuples of
; values. In the second step we filter tuples (i, j) which do not satisfy the condition 1 <= j < i <= n.
(defn unique-pairs [n]
  (let [nums (enum 1 n)]
    (filter (fn [x] (> (first x) (second x)))
    ; this doesn't work - I guess you can't repeat macro arguments several times
    ;(filter #((> (first %1) (second %1)))
            (mapcat #(map (fn [x] (list x %1)) nums) nums))))

(deftest test-unique-pairs
  (is (= (sort-by #(first %1) (unique-pairs 3)) [[2 1] [3 1] [3 2]])))

(defn any? [p xs]
  (not (empty? (filter p xs))))

; Obvious implementation of prime?. Test if n is divisible by any of the values up to (sqrt n).
; I think there was an exercise involving primes in the first part of the book...
(defn prime? [n]
  (empty? (filter #(= (rem n %1) 0)
          (enum 2 (round (sqrt n))))))

(defn prime-sum? [pair]
  (prime? (+ (first pair) (second pair))))

(defn make-pair-sum [pair]
   ; aaargh... the verbosity
   (list (first pair) (second pair) (+ (first pair) (second pair))))

(defn prime-sum-pairs [n]
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(deftest test-prime-sum
  (is (= (prime-sum-pairs 3) [[3 2 5]]))
  (is (= (sort-by #(first %1) (prime-sum-pairs 5)) [[3 2 5] [4 1 5] [4 3 7] [5 2 7]])))

; 2.41
; Reduce = foldl
(defn len [xs]
  (reduce (fn [acc el] (+ acc 1)) 0 xs))

; Breaks on empy input
(defn increasing? [xs]
  (second (reduce (fn [[prev res] el] (if (or (not res) (> prev el))
                         [prev false]
                         [el true]))
                  [(first xs) true] ; initial value
                  xs)))

; Consists of four steps:
; 1. generate all permutations of size n
; 2. truncate permutations to size k (size of our tuple)
; 3. drop non-unique results
; 4. filter non-increasing permutations
(defn unique-ks
  " @param n - upper bound of the range
    @param k - size of the tuple with 1 <= I(1) < I(2) < ... < I(k) <= n "
  [n k]
  (let [nums (enum 1 n)]
    (filter #(increasing? %)
            ; in uniq-pairs we took an [a] and created an [(a, a)] out of it.
            ; in the generic case we need to create a list of n-tuples of type a.
            (set (map #(take k %) (permutations nums))))))

(defn- sum [xs]
  (reduce + 0 xs))

(defn unique-triples [n s]
  (if (< n 3) []
    (filter #(= (sum %) s) (unique-ks n 3))))

(deftest test-unique-triples
  (is (= (unique-triples 3 6) [[1 2 3]]))
  (is (= (unique-triples 4 6) [[1 2 3]]))
  (is (= (unique-triples 5 9) [[2 3 4] [1 3 5]])))

; 2.42
; 8-Queen problem.

(def *board-start* 0)
(def *board-size* 5)
; At first I've made a mistake of producing a 8x8 matrix in empty-board
  ;(take *board-size* (repeat (enum *board-start* *board-size*))))
; It should produce an empty set of positions.
(def empty-board ())

(defn at-x [pos] (first pos))
(defn at-y [pos] (second pos))
(defn pos [x y] [x y])
(defn outside-board? [pos]
  (or (> (at-x pos) *board-size*)
      (< (at-x pos) *board-start*)
      (> (at-y pos) *board-size*)
      (< (at-y pos) *board-start*)))

; -------- safe?
(defn- generate-path [f init]
  (take-while #(not (outside-board? %)) (iterate f init)))

; This probably can be done by a smart macro
(defmulti path-to (fn [dir init] dir))
(defmethod path-to :up-right [_ init]
  (generate-path (fn [init] (pos (inc (at-x init)) (inc (at-y init)))) init))
(defmethod path-to :up-left [_ init]
  (generate-path (fn [init] (pos (dec (at-x init)) (inc (at-y init)))) init))
(defmethod path-to :down-right [_ init]
  (generate-path (fn [init] (pos (inc (at-x init)) (dec (at-y init)))) init))
(defmethod path-to :down-left [_ init]
  (generate-path (fn [init] (pos (dec (at-x init)) (dec (at-y init)))) init))

; At first I was bitten by 'contains?' which returns true if the given index
; (!!) is in range of the indexed sequence (if used on indexed sequences,
; apparently).
(defn- exact-queen-hit [who whom]
  (cond (= who whom) true
        :else (or
            ; this could have been easily checked by checking the slope of the
            ; line formed by two queens (kx + a = y, hits if k = 1).
            (contains? (set (concat (path-to :up-right who)
                                    (path-to :up-left who)
                                    (path-to :down-right who)
                                    (path-to :down-left who)))
                       whom))))

(defn queen-hits
  " @param who - the queen doing the beating
    @param whom - the queen getting beaten "
  [who whom]
  ;{:pre [(assert (> (at-x whom) (at-x who)))]} -- doesn't work
  ; check same horizontal (cannot be the same vertical)
  (cond (= (at-y who) (at-y whom)) true
        (= (at-x who) (at-x whom)) true
        :else (exact-queen-hit who whom)))

(deftest test-hits
  (is (queen-hits (pos 1 1) (pos 2 2)))
  (is (queen-hits (pos 2 2) (pos 1 1)))
  (is (queen-hits (pos 1 1) (pos 2 1)))
  (is (queen-hits (pos 2 1) (pos 1 1)))
  (is (not (queen-hits (pos 1 1) (pos 2 3))))
  (is (not (queen-hits (pos 2 3) (pos 1 1)))))

(defn get-queen-at-column [column positions]
  (let [result (filter #(= (at-y %) column) positions)]
    (if (empty? result) nil (first result))))

(defn safe?
  #^{:doc " @param k - vertical of the current queen
            @param positions - positions of queens in verticals 1..(k-1).
                              Type: List (Int, Int)
            @return true if the position at index k is safe " }
    ;:test (fn [] (assert (and (k >= *board-start*) (>= (count positions) k))))  }
  [column positions]
  (let [at-column (get-queen-at-column column positions)]
    (not (exists #(queen-hits % at-column) (remove #(= % at-column) (vec positions))))))

(deftest test-safe
  (is (not (safe? 1 [(pos 0 0) (pos 1 1)])))
  (is (not (safe? 1 [(pos 0 0) (pos 1 1) (pos 2 3)])))
  (is (safe? 3 [(pos 0 0) (pos 1 3) (pos 2 1)])))

; ------------ queens
(defn add-queen
  " @param new-row - horizontal for the new queen
    @param new-column - vertical for the new queen
    @param queens-already-placed - queens placed before this queen. They are
                            guaranteed to be placed correctly "
  [new-row new-column queens-already-placed]
  (cons (pos new-row new-column) queens-already-placed))

(def queens
 (letfn [(queen-cols [column]
   (if (= column (dec *board-start*))
     (list empty-board)
     (filter #(safe? column %)
             (mapcat (fn [queens-already-placed]
                       (map #(add-queen % column queens-already-placed)
                            (enum *board-start* *board-size*)))
                     (queen-cols (dec column))))))]
   (queen-cols (dec *board-size*))))

(defn print-queens [positions]
  (doseq [y (reverse (enum *board-start* *board-size*))]
    (doseq [x (enum *board-start* *board-size*)]
      (if (contains? (set positions) (pos x y))
        (print "*")
        (print ".")))
    (print "\n")))

; 2.43 - no coding required
; Hugo Doom has screwed up by reordering statements inside of the `queens`
; function. We are challenged with pointing out what exactly went wrong in
; the disfunctional function and to analyze its complexity assuming the correct
; `queens` function produces the result for the board sized N in T.

; In the original function we have (n - row)
; 1. (flatmap (map f [0 .. n]) '([])) => 1 flatmap iteration + 1 * N map iterations
; 2. (flatmap (map f [0 .. n]) '(([0 0]) ([1 0]) .. ([n 0]))) => N flatmap iterations + N * N map iterations
; 3. (flatmap (map f [0 .. n]) '(([0 0] [0 0]) ([1 0] [0 0]) .. ([n 0] [n 0]))) N flatmap iterations + N * N * N map iterations

; Let's only take into account flatmap and map iterations. Then we can see that
; `queen-cols` is O(N^(N-1) + N^N) as each flatmap iteration creates N * (combinations
; created during previous iteration) combinations. However, if we look at the
; `queen-cols` itself, it's only called N+1 times, which is linear.

; In the modified function we have `queen-cols` fully evaluated N times for
; each invocation, so we have O((N^(N-1) + N^N)^N) which is much scarier. The modified
; `queen-cols` spawns a tree recursive process calling `queen-cols` for each N.

(defn failed-queens [_] ; unused parameter so that this would be a valid function definition
 (letfn [(queen-cols [column]
   (if (= column (dec *board-start*))
     (list empty-board)
     (filter #(safe? column %)
             (mapcat (fn [row]
                       (map #(add-queen row column %)
                            (queen-cols (dec column))))
                     (enum *board-start* *board-size*)))))]
   (queen-cols (dec *board-size*))))
