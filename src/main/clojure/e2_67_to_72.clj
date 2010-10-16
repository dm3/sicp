(ns e2-67-to-72
  (:use clojure.test))

; First - Huffman tree machinery from the book

(defn third [x]
  (second (rest x)))

(defn fourth [x]
  (second (rest (rest x))))

(defn make-leaf [s weight]
  ['leaf s weight])

(defn leaf? [o]
  (= (first o) 'leaf))

(defn symbol-leaf [x]
  (second x))

(defn weight-leaf [x]
  (third x))

(defn left-branch [t]
  (first t))

(defn right-branch [t]
  (second t))

; This should probably be done with multimethods (the clojure way?)
(defn symbols [t]
  (if (leaf? t)
      [(symbol-leaf t)]
      (third t)))

(defn weight [t]
  (if (leaf? t)
      (weight-leaf t)
      (fourth t)))

(defn make-code-tree [l r]
  [l r
   (concat (symbols l) (symbols r))
   (+ (weight l) (weight r))])

(defn choose-branch [bit b]
  (cond (= bit 0) (left-branch b)
        (= bit 1) (right-branch b)
        :else nil))

(defn decode [bits t]
  (letfn [(decode-1 [bits current-branch]
            (if (empty? bits) []
                (let [next-branch (choose-branch (first bits) current-branch)]
                  (if (leaf? next-branch)
                      (cons (symbol-leaf next-branch) (decode-1 (rest bits) t))
                      (decode-1 (rest bits) next-branch)))))]
    (decode-1 bits t)))

; 2.67

(def *sample-tree*
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(def *sample-message* [0 1 1 0 0 1 0 1 0 1 1 1 0])

; What will the above produce when decoded?
; My guess is: ADAABBCA

; This is the correct answer. I've actually made a mistake when trying to
; decode the message mentally.
(def *sample-decoded* ['A 'D 'A 'B 'B 'C 'A])

(deftest test-assumption
  (is (= *sample-decoded* (decode *sample-message* *sample-tree*))))

; 2.68

(defn contains-symbol
  "Returns true if tree 't' contains symbol 's'"
  [s t]
  ; Clojure idiom for contains? in lists/vectors?
  (not (nil? (some #{s} (symbols t)))))

(defn encode-symbol [s t]
  (if (contains-symbol s t)
      (if (not (leaf? t))
          (if (contains-symbol s (left-branch t))
              (cons 0 (encode-symbol s (left-branch t)))
              (cons 1 (encode-symbol s (right-branch t))))
          [])
      (assert false)))

(defn encode [msg t]
  (if (empty? msg) []
      (concat (encode-symbol (first msg) t)
              (encode (rest msg) t))))

(deftest test-encode-decode
  (is (= (encode *sample-decoded* *sample-tree*) *sample-message*)))

; 2.69

(defn adjoin-set
  "Inserts an element 'x' into the set 'xs' and keeps the set ordered by weight"
  [x xs]
  (cond (empty? xs) [x]
        (< (weight x) (weight (first xs))) (cons x xs)
        :else (cons (first xs)
                    (adjoin-set x (rest xs)))))

(defn make-leaf-set
  "Creates a set of leaves out of (Symbol, Weight) pairs"
  [pairs]
  (if (empty? pairs) []
      (let [pair (first pairs)]
        (adjoin-set (make-leaf (first pair) (second pair))
                    (make-leaf-set (rest pairs))))))

(defn successive-merge
  "Creates a Huffman tree out of a set of leaves (containing a symbol to be
  encoded and its weight)"
  [xs]
  (cond (empty? xs) []
        (= (count xs) 1) (first xs)
        :else (successive-merge (adjoin-set (make-code-tree (first xs) (second xs))
                                            (rest (rest xs))))))

(defn generate-huffman-tree [pairs]
  (successive-merge (make-leaf-set pairs)))

(deftest test-huffman-tree
  ; My function generates a tree symmetric to the one depicted in the book
  ; (fig. 2.18) as it reverses the set of leaves.
  (is (= (make-code-tree (make-leaf 'A 8)
                         (make-code-tree
                           (make-code-tree (make-code-tree (make-leaf 'H 1) (make-leaf 'G 1))
                                           (make-code-tree (make-leaf 'F 1) (make-leaf 'E 1)))
                           (make-code-tree (make-code-tree (make-leaf 'D 1) (make-leaf 'C 1))
                                           (make-leaf 'B 3))))
         (generate-huffman-tree [['A 8] ['B 3] ['C 1] ['D 1] ['E 1] ['F 1] ['G 1] ['H 1]]))))

; 2.70

(def *rock-70-tree* (generate-huffman-tree [['A 2] ['BOOM 1] ['GET 2] ['JOB 2] ['NA 16] ['SHA 3] ['YIP 9] ['WAH 1]]))

(def *rock-70-message*
  "Get a job
   Sha na na na na na na na na
   Get a job
   Sha na na na na na na na na
   Wah yip yip yip yip yip yip yip yip yip
   Sha boom")

; To encode this message from the cocaine-ridden minds of rockers from the past we'll need to
;
; 1. uppercase it (as our symbols in the huffman tree are uppercased)
; 2. turn it into code (split on spaces and quote)

(defn as-symbol [s]
  (symbol (.toUpperCase (.replace s "\n" ""))))

(defn message-to-code [msg]
  (reverse (reduce #(cons (as-symbol %2) %) []
                   (filter #(seq %) (seq (.split msg " "))))))

; Encoding the given message with the given huffman tree took 84 bytes
(deftest test-70-rock
  (is (= (count (encode (message-to-code *rock-70-message*) *rock-70-tree*)) 84)))

; How many bytes would it take to encode the same message using the same alphabet (8 words) but with a fixed-size code?
; To answer this question we'll need to multiply the length of the message

(defn length [msg]
  (count (message-to-code msg)))

; by the length of the fixed-size encoding which is log2(|Alphabet|) = log2(8) = 3

(deftest test-num-of-bits-fixedsize
  (is (= (* 3 (length *rock-70-message*)) 108)))

; 2.71
; Assume that we have a huffman tree for an alphabet of length 'n' with weights 1, 2, 4, ..., 2^(n-1).
; Let's depict a tree for n=5:

; user=> (generate-huffman-tree [['A 1] ['B 2] ['C 4] ['D 8] ['E 16]])
; [[[[[leaf A 1] [leaf B 2] (A B) 3] [leaf C 4] (A B C) 7] [leaf D 8] (A B C D) 15] [leaf E 16] (A B C D E) 31]
;
;                   {A, B, C, D, E}
;                   /            \
;               {A, B, C, D}    {E}
;               /         \
;           {A, B, C}     {D}
;           /      \
;       {A, B}     {C}
;       /   \
;     {A}   {B}
;
; Even from this one sample we can predict that all of the trees generated for
; the given alphabet and weights will be of the same structure - one branch of
; each subtree will be at most of length 1 and the other branch will go down
; until it reaches the elements with weights 1 and 2.
;
; To encode the most common symbol we will always need just one bit as it will
; always be the first symbol on the left/right (dependening on the
; implementation of the huffman tree generator)
;
; To encode the most uncommon symbol we'll need 'n-1' bits as the most uncommon
; symbol will be at most n-1 steps away from the root of the tree (it will be
; the n-th symbol in the alphabet, so we would need 'n' bits if we stored the
; [symbol, weight] pairs in a list instead of a tree. As we're storing those
; pairs in a binary tree, the second most uncommon symbol will also require
; 'n-1' bits as it will be on the same level as the first most uncommon
; symbol).

; 2.72
; What`s the order of growth for the `encode` procedure from 2.68?
; Here we`ll discard the fact that `append` (or `concat` in clojure) is O(n).
; That aside, if we assume that `encode-symbol` is constant time,  the order of
; growth for `encode` is O(n) as encode is called once for each atom in the
; message. Now, if we assume that the message is constant, the order of growth
; for the `encode-symbol` is O(n) where 'n' is the number of symbols in the
; huffman tree.
; Now we can combine both measures by noting that for each atom in the message
; (of length 'm') we need to encode it by performing a traversal on a tree of
; max 'n' levels (where 'n' is the number of symbols), so the order of growth
; of `encode` for |message|='m' and |tree|='n' is O(n*m).

(run-tests 'e2-67-to-72)
