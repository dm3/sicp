(ns e2-73-74
  (:use clojure.test))

; 2.73
; a) We can't include the operation which governs the dispatch to
; numbers/variables (by `number?` or `variable?`) because the operations which
; have to be performed if `number?/variable?` conditions hold do not correspond
; to other operations (such as `+/*`). In other words, `number/variable`
; operations do not have an operator, thus requiring a different mechanism of
; abstraction.

; b)
; Let's use some "advanced" clojure to implement global variable with STM.
; `*table*` is a mutable reference which can only be modified inside of the
; transaction.
(def *table* (ref {}))

; 'deriv tag isn't actually used as this table doesn't contain any other
; operation bindings.
(defn get-binding [table sym] (get (deref *table*) sym))
(defn put-binding [table sym f] (dosync (alter *table* assoc sym f)))

(defn same-variable? [exp v] (and (symbol? exp) (= exp v)))
(defn variable? [exp] (symbol? exp))
(defn operator [exp] (first exp))
(defn operands [exp] (rest exp))

(defn deriv [exp v]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp v) 1 0)
        :else ((get-binding 'deriv (operator exp))
                  (operands exp) v)))

(defn rest-or-one [xs] (if (= (count xs) 1) (first xs) xs))

(defn install-deriv-ops []
  (defn make-sum [a & x] (concat (list '+ a) x))
  (defn make-product [a & x] (concat (list '* a) x))
  (put-binding 'deriv '+ (fn [args v] (make-sum (deriv (first args) v)
                                                (deriv (rest-or-one (rest args)) v))))
  (put-binding 'deriv '* (fn [args v] (make-sum (make-product (deriv (rest-or-one (rest args)) v) (first args))
                                                (make-product (deriv (first args) v) (rest-or-one (rest args)))))))

(deftest test-ops
  (install-deriv-ops)
  (is (= (deriv '(+ x 3) 'x) '(+ 1 0)))
  (is (= (deriv '(* x y) 'x) '(+ (* 0 x) (* 1 y)))))

; c)
; Too lazy to actually implement this as I'd need to copy my exponentiation procedure from ex 2.57
; The actual solution would consist of the code for the procedure and an additional binding

(put-binding 'deriv '** (fn [args v] nil))

; d)
; If we were to change the `get` (or `get-binding`) function to
;
;    (defn get-binding [sym op] ...)
;
; instead of
;
;    (defn get-binding [op sym] ...)
;
; Nothing would change in the differentiation system. We would only change the
; order of arguments to the `get-binding` function.  I didn't really understand
; this exercise...

; 2.74
; I just got a new client - Insatiable Enterprises Inc. Completely innocuous name...
; They want to leverage the synergy of SOA in the cloud (REST + NoSQL) and
; we're gonna help them do that.
;
; The requirements for this exercise are quite vague:
;
;   1. Each department has its own format for files
;   2. Files are sets keyed by the persons name
;   3. Keyed records are themselve sets keyed by different types of data (salary/...)
;
; a)
; In order to make a generic `get-record` function which will work with any
; type of file we can use the same principle of data-directed programming with
; additivity. We will have (a) a registry which will be used by different
; departments to register their implementations and (b) a protocol which will
; be implemented and registered by those departments.

; A registry is just a reference to an empty map
(def *file-registry* (ref {}))

; Given a file-type get a record matching the given `person`
(defn get-record [file-type person]
  ((get (get (deref *file-registry*) file-type) 'get-person) person))

(defn file-type-registered? [registry file-type]
  (not (nil? (get (deref registry) file-type))))

(defn register-op [file op]
  (assoc file (first op) (second op)))

; This function accepts a file-type which should be unique through all of the
; departments and any amount of pairs (operation-code, operation) which are
; bound to the given file-type.
(defn register-file [file-type & bindings]
  (do (assert (not (file-type-registered? *file-registry* file-type)))
      (assert (even? (count bindings)))
      (dosync (alter *file-registry* assoc file-type
                     (reduce register-op {} (partition 2 bindings))))))

(defn install-records []
  (defn get-person-a [person] (first (filter #(= (:name %) person) [{:name 'John :salary 1000}])))
  (defn get-person-b [person] (first (filter #(= (:surname %) person) [{:sex 'Male :surname 'Rob :salary 2000}])))
  (register-file 'department-a
    'get-person get-person-a)
  (register-file 'department-b
    'get-person get-person-b))

; b)
; For `get-salary` to be possible, each record must contain an entry tagged
; :salary. If it was scheme, I would have used 'salary as :salary is a clojure
; convention for map keys.

(defn get-in-files
  "Returns a sequence of elements matching the given `item` in the list of
  given `files` selected by the given `op` (operation)"
  [files op item]
  (mapcat #(let [v ((get % op) item)]
                 ; I think there is a clojure idiom which lets to turn nils into
                 ; empty seqs...
                 (if (nil? v) [] [v])) files))

(defn get-salary
  "Returns a salary for the given person. Searches through all of the departments"
  [person]
  (first (map #(get % :salary) (get-in-files (vals (deref *file-registry*)) 'get-person person))))

; c)
; We actually did the largest part of it in b) as the requirements for b)
; didn't even mention we could use the list of files as an input.
(defn find-employee-record
  "Returns one record matching the person in the given files as persons should
  be unique even across departments"
  [person files]
  (first (get-in-files files 'get-person person)))

(deftest test-find-employee
  (install-records)
  (is (= (find-employee-record 'John (vals (deref *file-registry*))) (get-record 'department-a 'John)))
  (is (= (get-salary 'John) (:salary (get-record 'department-a 'John)))))

; d)
; When Insatiable Inc. acquires a new company (or creates a new department)
; with its own personnel record file we only need to add another
;
;   (register-file 'new-department-name
;     'get-person x
;     ... ...)
;
; and make sure that the person entries have a `:salary` tag.

(run-tests 'e2-73-74)
