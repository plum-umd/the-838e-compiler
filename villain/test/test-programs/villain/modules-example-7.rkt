#lang racket
(begin
  (require "exmod1.rkt")
  (letrec ((f (λ (x y z . t)
                (+ (+ x (apply (λ (a b) (+ a b)) (list y z)))
                   (apply * t))))
           (g (λ (x) (x (x (x 1 2 3 4 5) 2 3 4 5) 3 4 5 6))))
    (g f)))