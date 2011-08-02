;; ## Tests
;;
;; Runs tests for all of the exercises.
(ns all
  (:use [clojure.contrib.find-namespaces :only (find-namespaces-on-classpath)])
  (:use clojure.test))

;; Our situation is a little bit different from an ordinary project where each
;; separate namespace might get associated with a separate test file. In our
;; case tests are *inside* of the actual sources.

;; Guided by [John Lawrence](http://www.learningclojure.com/2010/02/requiring-all-possible-namespaces.html),
;; we'll dynamically `require` namespaces which interest us to be picked up by
;; whichever test runner runs this file.
(doseq [n (find-namespaces-on-classpath)]
  (when (. (str n) startsWith "e")
    (try
      (require n)
      (catch Exception e (println (str "Could not require: " n))))))

(run-all-tests)
