#lang racket
(provide length plusone)
(define (length xs)
  (if (empty? xs)
      0
      (add1 (length (cdr xs)))))
(define (plusone x)
  (+ x 1))
