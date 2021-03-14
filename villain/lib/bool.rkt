#lang racket
(provide boolean? not and)

(define (boolean? x)
  (match x
    [#t #t]
    [#f #t]
    [_  #f]))

(define (not x)
  (match x
    [#f #t]
    [_  #f]))

(define (and x y)
  (match x
    [#f #f]
    [_  y]))
