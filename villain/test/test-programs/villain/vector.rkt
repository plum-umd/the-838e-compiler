#lang racket
(let ((x (vector 1 2 3))) (begin (vector-set! x 0 3) (vector-ref x 0)))