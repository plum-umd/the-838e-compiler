#lang racket
(begin
  (require "exmod1.rkt" "exmod2.rkt" "exmod3.rkt"
           "exmod4.rkt" "exmod5.rkt" "exmod6.rkt")
  (define (f x) x)
  (+ (+ ( + (+ (h4 5) (g2 3)) (h5 5)) (h6 5))
     (+ (g1 5) (+ (h1 5) (+ (h2 5) (h3 5))))))

