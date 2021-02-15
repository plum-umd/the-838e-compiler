#lang racket
(begin
  (provide empty? length concat)

  (define (empty? xs)
    (match xs
      ['() #t]
      [(cons x xs) #f]))

  (define (length xs)
    (match xs
      ['() 0]
      [(cons x xs) (add1 (length xs))]))

  (define (concat xs ys)
    (match xs
      ['() ys]
      [(cons x xs) (cons x (concat xs ys))]))

  )