#lang racket
(begin
  (require "./modules-example-1.rkt" "./modules-example-2.rkt"
           "./modules-example-3.rkt" "./modules-example-4.rkt"
           "./modules-example-5.rkt" "./modules-example-6.rkt")
  (define (f x) x)
  (+ (+ ( + (+ (h4 5) (g2 3)) (h5 5)) (h6 5))
     (+ (g1 5) (+ (h1 5) (+ (h2 5) (h3 5))))))

