#lang racket
(begin (define (f x)
         (let ((y #f))
           (add1 #f)))
       (add1 (f 5)))
