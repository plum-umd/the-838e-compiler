#lang racket
(provide unparse)
(require "ast.rkt")

;;Given a parsed exression, return the original s-expression from which it
;;was parsed.
;;Expr -> S-Expression
(define (unparse e)
  (match e
    [(Int i) i]
    [(Bool b) b]
    [(If e1 e2 e3)
     (list 'if (unparse e1) (unparse e2) (unparse e3))]
    [(Prim1 p e)
     (list p (unparse e))]))