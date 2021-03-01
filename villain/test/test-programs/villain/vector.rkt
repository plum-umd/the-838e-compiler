#lang racket
(let ((x (make-vector 3 0)))
  (begin (vector-set! x 0 3)
         (vector-ref x 0)))
