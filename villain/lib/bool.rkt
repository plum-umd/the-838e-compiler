#lang racket
(provide boolean? not)

(define (boolean? x)
  (match x
    [#t #t]
    [#f #t]
    [_  #f]))

(define (not x)
  (match x
    [#f #t]
    [_  #f]))
