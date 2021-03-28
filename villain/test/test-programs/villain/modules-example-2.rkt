#lang racket
(begin
  (provide h1)
  (require "./modules-example-3.rkt"
           "./modules-example-4.rkt")
  (define (h1 x) 2))