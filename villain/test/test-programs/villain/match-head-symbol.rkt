#lang racket
(begin (define (starts-with-Prim1 x)
         (match (car x)
           ['Foo #f]
           ['Prim1 #t]
           [_ #f]))
       (starts-with-Prim1 (list 'Prim1 'add1 (list 'Int 1))))
