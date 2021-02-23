#lang racket
(begin
  (require "exmod1.rkt" "exmod2.rkt" "exmod3.rkt" "exmod4.rkt")
  (define (f x) x)
  (+ (g1 5) (+ (h1 5) (+ (h2 5) (h3 5)))))
