(ns e2-63-to-66
  (:use clojure.test))

; Tree accessors and a constructor
(defn entry [t]
  (first t))

(defn left-branch [t]
  (second t))

(defn right-branch [t]
  (second (rest t)))

(defn make-tree
  ([e] [e [] []])
  ([e l r] [e l r]))

; 2.63
; a)

(defn tree->list-1 [t]
  (if (empty? t) []
      (concat (tree->list-1 (left-branch t))
              (cons (entry t) (tree->list-1 (right-branch t))))))

(defn tree->list-2 [t]
  (letfn [(copy-to-list [t result]
            (if (empty? t) result
                (copy-to-list (left-branch t)
                              (cons (entry t) (copy-to-list (right-branch t) result)))))]
    (copy-to-list t [])))

; We're given two functions which flatten the tree into a list. After the first
; glance we can see that the first function is a simple in-order traversal of a
; tree which spawns a tree-recursive process. That's exactly how I'd do the
; task. The second function is much harder to understand (and visualize the
; process) as it keeps the state of the list in an additional argument.
; Let's see how these functions flatten actual trees:
;
; 1.           2.               3.
;    7           3                5
;   / \         / \              / \
;  3   9       1   7            3   9
; / \   \         / \          /   / \
;1  5   11       5   9        1   7  11
;                     \
;                     11
;
; Take, for example, the first tree:
; 1.1) tree->list-1 [7
;                      [3                      [9 []
;                         [1 [] []] [5 [] []]]       [11 [] []]]]
;   2.1) tree->list-1 [3 [1 [] []] [5 [] []]]      <--- first left subtree
;     3.1) tree->list-1 [1 [] []]                  <--- left subtree of the first left subtree
;       4.1) tree->list-1 []
;     3.2) append [] 1 [] -> [1]
;     3.3) tree->list-1 [5 [] []]                  <--- right subtree of the first left subtree
;       4.2) tree->list-1 []
;     3.4) append [] 5 [] -> [5]
;   2.2) append [1] 3 [5] -> [1 3 5]
;   2.3) tree->list-1 [9 [] [11 [] []]]            <--- first right subtree
;     3.5) tree->list-1 [] -> []
;     3.6) tree->list-1 [11 [] []] -> [11]         <--- right subtree of the first right subtree
;     3.7) append [] 9 [11] -> [9 11]
;   2.4) append [1 3 5] 7 [9 11] -> [1 3 5 7 9 11] <--- result
;
; 1.1) copy-to-list [7                                           ()
;                      [3                      [9 []
;                         [1 [] []] [5 [] []]]       [11 [] []]]]
;   2.1) copy-to-list [9 [] [11 [] []]] ()
;     3.1) copy-to-list [11 [] []] ()
;       4.1) cons 11 () -> (11)
;     3.2) cons 9 (11) -> (9 11)
;   2.2) cons 7 (9 11) -> (7 9 11)
;     3.4) copy-to-list [3 [1 [] []] [5 [] []]] (7 9 11)
;       4.2) copy-to-list [5 [] []] (7 9 11) -> (5 7 9 11)
;       4.3) cons 3 (5 7 9 11) -> (3 5 7 9 11)
;       4.4) copy-to-list [1 [] []] (3 5 7 9 11) -> (1 3 5 7 9 11)  <--- result
;   2.2) unwind the stack
;
; We can see that both functions produce equal results for all of the trees
; specified in the book, however, the biggest difference between the functions
; is in the way they gather the final results: the second function gathers the
; result sequentially into the resulting list, while the first one divides the
; task by subtrees and joins the results of sub-tasks together.
; In the second function the process first goes as deep as it can into the
; rightmost subtree and then gathers the left subtree. In the first function
; both subtrees are flattened simultaneously (which is a nice property for
; parallelization, we wouldn't be able to parallelize the second function as
; easily as we could the first one).
;
; Note that both functions produce sorted lists, which means that the results
; of applying functions will be equal for all binary trees.
;
(deftest test-equal-results
  (are [a] (= (tree->list-1 a) (tree->list-2 a))
       (make-tree 7 (make-tree 3 (make-tree 1) (make-tree 5)) (make-tree 9 [] (make-tree 11)))
       (make-tree 3 (make-tree 1) (make-tree 7 (make-tree 5) (make-tree 9 [] (make-tree 11))))
       (make-tree 5 (make-tree 3 (make-tree 1) []) (make-tree 9 (make-tree 7) (make-tree 11)))))

; b)
; What's the order of growth for both of the above functions (if the argument
; tree is balanced)?
;
; ## First function
;
; Well, let's analyze the first one. First of all, the height of a balanced
; tree is at most log2(n+1) where n = number of elements in the tree.
; Each recursion level reduces the height by one and spawns another recursive
; process which works with the left or right subtree. The operation taking
; actual time in the first function is `append` (which is O(n), as the first
; argument to append must be traversed to the end in order to append the second
; argument to the last cons cell of the first one). Let's assume that the tree
; is full, then the time taken at different recursion steps would look like
; this:
;                O(n/2-1)
;               /      \
;        O(n/4-1)      O(n/4-1)
;         /     \       /      \
;   O(n/8-2) O(n/8-2) O(n/8-2) O(n/8-2)
;                 ....
;   O(1) ...                  ... O(1)
;
; The tree doesn't start with O(n) because we append to the left subtree in the
; topmost recursion level and only need to iterate over the result of
; flattening the left subtree (which is at most n/2-1 elements).
;
; The height of the tree is at most log2(n+1), then
; T(n) = O(n/2) + 2*O(n/4) + 4*O(n/8) + ... = log2(n+1) * O(n/2) = O(n*log2(n))
;
; ## Second function
;
; This one doesn't do append so the operation performed at each recursion level
; is just O(1). We still visit each node in the tree, so the order of growth is
; n*O(1) = O(n).

; 2.64
; a) `partial-tree` creates a binary tree out of an ordered list by
;
;       1. creating the left subtree out of the first half of the list
;       2. getting the first of the elements which didn't get into the left subtree and making it an entry element
;       3. creating a right subtree out of the remaining elements
;       4. consing the results together
;
;    As this process is performed recursively on both each half of the argument
;    list, the resulting tree should be balanced.
;
;    Running the function on (1 3 5 7 9 11) results in ([5 [1 () [3 () ()]] [9 [7 () ()] [11 () ()]]])
;
; b) The order of growth is O(n) as the below function must visit each element
; of the argument list and perform a cons to make it a node in the resulting tree.

(defn partial-tree [elts n]
  (if (= n 0) (cons '() elts)
      (let [left-size (quot (dec n) 2)]
        (let [left-result (partial-tree elts left-size)]
          (let [left-tree (first left-result)
                non-left-elts (rest left-result)
                right-size (- n (inc left-size))]
            (let [this-entry (first non-left-elts)
                  right-result (partial-tree (rest non-left-elts) right-size)]
              (let [right-tree (first right-result)
                    remaining-elts (rest right-result)]
                (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))

(defn list->tree [xs]
  (first (partial-tree xs (count xs))))

; 2.65
; Here we need to make union/interaction operations on sets represented by
; balanced binary trees work in O(n). It's also noted that we should use the
; result of exercises 2.63 and 2.64. A straightforward solution would be:
;
; 1. take two trees: xs and ys
; 2. apply tree->list to both of them
; 3. append one list to the other
; 4. sort the result
; 5. apply list->tree to the sorted list
;
; However, sorting the list is minimum n*logn so this solution won't cut it.
;
; We must use the fact that the tree->list function produces a sorted list
; (set), so we need to get a union/intersection of 2 sorted sets in O(n) -
; which we already did in exercises 2.61-62.

; placeholders. Do not want to copy or add a dependency on other files.
(defn union-sorted-set [xs ys] nil)
(defn intersection-sorted-set [xs ys] nil)

(defn union-set [xs ys]
  (let [xs-list (tree->list-2 xs)
        ys-list (tree->list-2 ys)]
    (list->tree (union-sorted-set xs-list ys-list))))

(defn intersection-set [xs ys]
  (let [xs-list (tree->list-2 xs)
        ys-list (tree->list-2 ys)]
    (list->tree (intersection-sorted-set xs-list ys-list))))

; 2.66
; Here we need to create a binary tree lookup which should work in log2n time.

(defn lookup [k xs]
  (cond (nil? (entry xs)) nil
        (= k (entry xs)) (entry xs)
        (< k (entry xs)) (lookup k (left-branch xs))
        :else (lookup k (right-branch xs))))

(deftest test-lookup
  (is (= (lookup 2 (list->tree [1 2 5 6 11])) 2))
  (is (= (lookup 9 (list->tree [1 2 5 6 11])) nil)))

(run-tests 'e2-63-to-66)
