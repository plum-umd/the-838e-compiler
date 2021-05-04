#lang racket

(begin
  (provide interp)
  (require "ast.rkt" "interp-prim.rkt")
   
  (define (interp e)
    (interp-e e '()))

  ;; Env Id Value -> Env
  (define (ext r x val)
    (cons (list x val) r))

  ;; Env Id -> Value
  (define (lookup r x)
    (match r
      [(cons (list y val) r) (if (symbol=? x y) val (lookup r x))]
      [_ 'err]))

  ;; Expr -> Integer
  (define (interp-e e r)
    (match e
      [(Int i) i]
      [(Bool b) b]
      [(Char c) c]
      [(Var x)  (lookup r x)]
      [(Eof) eof]
      [(Prim0 p)
       (interp-prim0 p)]
      [(Prim1 p e0)
       (match (interp-e e0 r)
         ['err 'err]
         [v (interp-prim1 p v)])]
      [(Prim2 p e1 e2)
        (match (interp-e e1 r) 
          ['err 'err]
          [v1 (match (interp-e e2 r)
                  ['err 'err]
                  [v2 (interp-prim2 p v1 v2)])])]
      [(If e1 e2 e3)
       (match (interp-e e1 r)
         ['err 'err]
         [v
          (if v
              (interp-e e2 r)
              (interp-e e3 r))])]
      [(Begin2 e1 e2)
       (match (interp-e e1 r)
         ['err 'err]
         [_ (interp-e e2 r)])]
      [(Let x b e)
        (match (interp-e b r)
          ['err 'err]
          [v (interp-e e (ext r x v))])])))