#lang racket
(begin
  (provide length
           append
           reverse)

  (define (length xs)
    (match xs
      ['() 0]
      [(cons x xs) (add1 (length xs))]))

  (define (append xs ys)
    (match xs
      ['() ys]
      [(cons x xs) (cons x (append xs ys))]))

  (define (reverse xs)
    (match xs
      ['() '()]
      [(cons x xs) (append (reverse xs) (cons x '()))]))

  )