(ns e2-12-13-14-15-16
  (:use [clojure.contrib.test-is :only (deftest is run-tests)])
  (:use [util.interval :only (make-interval lower-bound upper-bound mul-interval div-interval add-interval)])
  (:use [clojure.contrib.math :only (round)]))

; 2.12
; Lisa got into trouble again by not looking at the specs and not listening to
; the BAs.  Turns out all of the intervals she manufactured were to be
; discarded as their representation didn't suit business needs. The fact struck
; Lisa like a scythe strikes ripe rye.
; After getting out of rehab Lisa got obsessed with various representations of intervals,
; you could often hear her silently weep in her room...
;
; Here's a fragment of code transcribed from the many carvings on the walls of her room:
(defn make-center-width [c w]
  (make-interval (- c w) (+ c w)))

(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(defn width [i]
  (/ (- (upper-bound i) (lower-bound i)) 2))

(defn make-center-percent
  " @param mid - midpoint-interval
    @param p - percent = radius-interval / midpoint-interval
               radius-interval = 1/2 * (upper-bound - lower-bound) => radius is always > 0 "
  [mid p]
  (let [r (if (= mid 0)
              (/ p 100)
              (* mid (/ p 100)))]
    (make-interval (- mid r) (+ mid r))))

(deftest make-center-test
  (is (= (make-center-percent 1 10) (make-interval 0.9 1.1)))
  (is (= (make-center-percent -10 50) (make-interval -5 -15)))
  (is (= (make-center-percent 0 10) (make-interval -0.1 0.1))))

(defn radius-interval [i]
  (/ (- (upper-bound i) (lower-bound i)) 2))

(defn percent-interval [i]
  (let [r (radius-interval i)
        mid (center i)]
    (round (* 100 (/ r (if (= mid 0) 1 mid))))))

(deftest percent-test
  (is (= (percent-interval (make-interval 0.9 1.1)) 10))
  (is (= (percent-interval (make-interval -5 -15)) 50))
  (is (= (percent-interval (make-interval -0.1 0.1)) 10)))

; 2.13
(defn low-percent? [p]
  (< p 10))

; We need to approximate the product of the two intervals given their centers and deviation percentage.
; Assume all numbers are positive, da = deviation a, ca = center a,
; a = [ca - da*ca, ca + da*ca] = [ca*(1 - da), ca*(1 + da)]
; b = [cb - db*cb, cb + db*cb] = [cb*(1 - db), cb*(1 + db)]
; a*b = [ca*cb*(1 - da)(1 - db), ca*cb*(1 + da)(1 + db)] = [ca*cb*(1 - (da + db - da*db)), ca*cb*(1 + (da + db + da*db))]
; We can see that deviation of the product = (da + db - da*db) which is approaching (da + db) if da and db approach 0.
; Conclusion: deviation of the product may be approximated by the sum of deviations of multiplicand and multiplier.
(defn percent-mul-intervals [a b]
  (let [pa (percent-interval a)
        pb (percent-interval b)]
  (if (and (low-percent? pa) (low-percent? pb))
      (+ pa pb)
      (println "Cannot calculate deviation percent of the resulting interval!"))))

(deftest test-deviation
  (is (let [a (make-center-percent 1 0.1)]
          (= (percent-mul-intervals a a) (percent-interval (mul-interval a a)))))
  (is (let [a (make-center-percent 1 5)
            b (make-center-percent 6 9)]
          (= (percent-mul-intervals a b) (percent-interval (mul-interval a b))))))

; 2.14
; Lem E. Tweakit is way too smart, he should be beaten into submission.
; r1*r2 / (r1 + r2)
(defn par1 [r1 r2]
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

; 1 / (1/r1 + 1/r2)
; We can get the latter formula by dividing the former one by r1*r2.
; We can get the former formula by multiplying the denominator of the latter one by r2/r2,
; which doesn't change the result, as r2/r2 = 1. Really? Is it?
(defn par2 [r1 r2]
  (let [one (make-interval 1 1)]
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

; par1 and par2 do indeed return different results. Question is, which one is better? And why are they different
; if the formulas are equivalent?
(deftest par12-test
  (let [a (make-center-percent 1 5)
        b (make-center-percent 5 10)
        c (make-interval -5 20)]
    (is (not (= (par1 a a) (par2 a a))))
    (is (not (= (par1 a b) (par2 a b))))
    (is (not (= (par1 a c) (par2 a c))))))

; The answer is in the result of r2/r2. We're talking about intervals here, so r2/r2 doesn't necessary equal to 1.
; Herein lies the problem (I confess, I lifted the answer to this one from the internets) - the formulas aren't
; equivalent if we're talking intervals.

; 2.15
; Eva Lu Ator suggests that par2 is better than par1.
; par1 performs (assume r1=[lr1, ur1] and r2=[lr2, ur2] are positive intervals)
;   ([lr1, ur1] * [lr2, ur2]) / ([lr1, ur1] + [lr2, ur2]) =>
;   [(lr1 * lr2), (ur1 * ur2)] / [(lr1 + lr2), (ur1 + ur2)] =>
;   [((lr1 * lr2) * (1 / (ur1 + ur2))), ((ur1 * ur2) * (1 / (lr1 + lr2)))] =>
;   [(lr1 * lr2) / (ur1 + ur2), (ur1 * ur2) / (lr1 + lr2)]
; par2
;   [1, 1] / (([1, 1] / [lr1, ur1]) + ([1, 1] / [lr2, ur2])) =>
;   [1, 1] / ([(1 / ur1), (1 / lr1)] + [1 / ur2, 1 / lr2]) =>
;   [1, 1] / [(1 / ur1) + (1 / ur2), (1 / lr1) + (1 / lr2)] =>
;   [1 / ((1 / lr1) + (1 / lr2)), 1 / ((1 / ur1) + (1 / ur2))]
; essentially it's all the same. However, we get less accuracy if we perform more
; operations on the incorrect intermediate results. And we get more intermediate results
; in par1 => par2 gets more accurate results.

; 2.16
; I lack the mathematical background for this one ) After reading a bit on
; intervals I seem to understand that they're lacking the properties of a field
; that the set Z (for example) holds. The zero for addition, the inverse for
; multiplication and the distributivity of multiplication over addition.  All
; the operations for intervals need to check for identity, so that r2/r2 = 1
; iff r2 IS r2.

(run-tests 'e2-12-13-14-15-16)
