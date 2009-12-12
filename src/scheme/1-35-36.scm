#lang scheme
(require "sicp-util.scm")

(close-enough? 
 (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
 phi
 tolerance)

; 1.36
; no damping (log1000/logx)
(close-enough?
 (fixed-point (lambda (x) (/ (log 1000) (log x))) 2)
 4.5555
 tolerance)

; damping (log1000/logx + x)/2
(close-enough?
 (fixed-point (lambda (x) (/ (+ (/ (log 1000) (log x)) x) 2)) 2)
 4.5555
 tolerance)