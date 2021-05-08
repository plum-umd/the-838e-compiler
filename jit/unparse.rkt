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
    [(Empty) '()]
    [(Void) (void)]
    [(Var v) v]
    [(Symbol s) (list 'quote s)]
    [(Box b) (box (unparse b))]
    [(Let x b e)
      (list 'let (list (list x (unparse b))) (unparse e)) ]
    [(If e1 e2 e3)
     (list 'if (unparse e1) (unparse e2) (unparse e3))]
    [(Prim0 p)
     (list p)]
    [(Prim1 p e)
     (list p (unparse e))]
    [(Prim2 p e1 e2)
      (list p (unparse e1) (unparse e2))]
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
    [(Empty) ''()]
    [(Box b) (list 'box b)]
    [(Cons h t) (list 'cons h t)]
    [(Pred p) (list '? p)]
    [(PWV p v) (list '? p v)]
    [(List-S-PWVs s pwvs)
     (cons (list 'quote s)
           (map (λ (pwv)
                  (match pwv
                    [(PWV p v) (list '? p v)]))))]
    [(List-S-Vs s vs)
     (cons (list 'quote s) vs)]
    [(List-S-Ps s ps)
     (cons (list 'quote s)
           (map (λ (p)
                  (match p
                    [(Pred p) (list '? p)]))))]
    [(List-S-LSV s1 (list s2 var))
     (list (list 'quote s1) (list 'quote s2) var)]
    [(Pat (Int s)) (Int s)]
    [(Pat (Bool s)) (Bool s)]
    [(Pat (Char s)) (Char s)]
    [(Pat (Var s)) (Var s)]
    [(Pat (Eof)) (Eof)]
    [(Pat (Prim0 p)) (Prim0 p)]
    [(Pat (Begin2 e1 e2)) (Begin2 e1 e2)]
    [(Pat (Prim1 s1 s2)) (Prim1 s1 s2)]
    [(Pat (Prim2 s1 s2 s3)) (Prim2 s1 s2 s3)]
    [(Pat (If s1 s2 s3)) (If s1 s2 s3)]
    [(Pat (Let s1 s2 s3)) (Let s1 s2 s3)]
    [(Pat (Env-Cons s1 s2 s3)) (Env-Cons s1 s2 s3)]))
  