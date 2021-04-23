#lang racket

(begin
  (provide interp)
  (require "ast.rkt" "interp-prim.rkt")
   
  ;; Expr -> Integer
  (define (interp e)
    (match e
      [(Int i) i]
      [(Bool b) b]
      [(Char c) c]
      [(Eof) eof]
      [(Prim0 p)
       (interp-prim0 p)]
      [(Prim1 p e)
       (interp-prim1 p (interp e))]
      [(If e1 e2 e3)
       (if (interp e1)
           (interp e2)
           (interp e3))]
      [(Begin2 e1 e2)
       (begin
         (interp e1)
         (interp e2))])))