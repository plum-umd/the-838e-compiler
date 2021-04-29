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
      [(Prim1 p e0)
       (match (interp e0)
         ['err 'err]
         [v (interp-prim1 p v)])]
      [(If e1 e2 e3)
       (match (interp e1)
         ['err 'err]
         [v
          (if v
              (interp e2)
              (interp e3))])]
      [(Begin2 e1 e2)
       (match (interp e1)
         ['err 'err]
         [_ (interp e2)])])))