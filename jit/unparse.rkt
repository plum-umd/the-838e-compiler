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
    [(Eof) 'eof]
    [(Err) ''err]
    [(Void) (void)]
    [(Var v) v]
    [(Symbol s) (list 'quote s)]
    [(If e1 e2 e3)
     (list 'if (unparse e1) (unparse e2) (unparse e3))]
    [(Prim0 p)
     (list p)]
    [(Prim1 p e)
     (list p (unparse e))]
    [(App f es)
     (append (list f) (map unparse es))]
    [(Begin2 e1 e2)
     (list 'begin (unparse e1) (unparse e2))]
    [(Match e cls)
     (list 'match (unparse e) (map unparse-clause cls))]))

;;Clause -> S-Expression
(define (unparse-clause c)
  (match c
    [(Clause pat body)
     (list (unparse-pattern pat) (unparse body))]))

;;Pattern  -> S-Expression
(define (unparse-pattern pat)
  (match pat
    [(Int i) i]
    [(Bool b) b]
    [(Char c) c]
    [(Var s) s]
    [(Symbol s) (list 'quote s)]
    [(Wild) '_]
    [(Err) ''err]
    [(Pat (Int s)) (Int s)]
    [(Pat (Bool s)) (Bool s)]
    [(Pat (Char s)) (Char s)]
    [(Pat (Eof)) (Eof)]
    [(Pat (Prim0 p)) (Prim0 p)]
    [(Pat (Begin2 e1 e2)) (Begin2 e1 e2)]
    [(Pat (Prim1 s1 s2)) (Prim1 s1 s2)]
    [(Pat (If s1 s2 s3)) (If s1 s2 s3)]))
  