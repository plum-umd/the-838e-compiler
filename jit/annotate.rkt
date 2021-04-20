#lang racket
(provide Green Red annotate)
(require "ast.rkt")

(struct Green (ast)               #:prefab)
(struct Red (ast)                 #:prefab)

;; Annotation = (Green Expr) | (Red Expr)
;; Color = 'green | 'red

;;Given the ast for a program, annotate the values as static (Green) and dynamic (Red)
;;Expr -> 
(define (annotate ast)
  (annotate-env ast (list)))

;;Annotate an expression under the given environment. Let bindings will need to use the enviroment to figure out the correct annotation for a variable
;;Expr (Listof (Pair Symbol Color))-> Expr
(define (annotate-env ast env)
  (match ast
    [(Int i) (Green (Int i))]
    [(Bool b) (Green (Bool b))]
    [(If e1 e2 e3)
     (match (annotate-env e1 env)
       [(Green e1)
        (Green (If (Green e1) (annotate-env e2 env) (annotate-env e3 env)))]
       [(Red e1)
        (Red (If (Red e1) (annotate-env e2 env) (annotate-env e3 env)))])]
    [(Prim0 p) ;;All our prim0s require interaction with the user hence are red
     (match p
       ['read-byte (Red (Prim0 p))]
       ['write-byte (Red (Prim0 p))]
       ['peek-byte (Red (Prim0 p))])]
    [(Prim1 p e)
     (match (annotate-env e env) ;;All our prim1s are foldable hence green as long as their argument is green
       [(Green e) (Green (Prim1 p (Green e)))]
       [(Red e) (Red (Prim1 p (Red e)))])]))
  