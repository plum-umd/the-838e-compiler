#lang racket
(begin
  (define (len ls)
    (match ls
      ['() 0]
      [(cons x xs)
       (add1 (len xs))]))
  (len (cons 1 (cons 2 (cons 3 '())))))
