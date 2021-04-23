#lang racket
(provide unparse)
(require "ast.rkt")

;;Given a parsed exression, return the original s-expression from which it
;;was parsed.
;;Expr -> S-Expression
(define (unparse e)
  (match e
    [(Int i)    i]
    [(Bool b)   b]
    [(Char c)   c]
    [(Eof)'eof]
    [(Void) (void)]
    [(If e1 e2 e3)
     (list 'if (unparse e1) (unparse e2) (unparse e3))]
    [(Prim0 p)
     (list p)]
    [(Prim1 p e)
     (list p (unparse e))]
    [(Begin2 e1 e2)
     (list 'begin (unparse e1) (unparse e2))]))