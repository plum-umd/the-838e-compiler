#lang racket
(begin
  (define (f x)
    1 2 3 x)

  (define (g . ys)
    4 5 6 ys)

  (let ()
    7
    (begin)
    (begin 8 9 0)
    9
    (cond [(f #t) 4 5 6 (g #f)])))
