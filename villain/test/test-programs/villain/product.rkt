#lang racket
(begin
  (define (product xs)
    (match xs
      ['() 1]
      [(cons x xs) (* x (product xs))]))

  (eq? (product (cons 1 (cons 2 (cons 3 '())))) 6))