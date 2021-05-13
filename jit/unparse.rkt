#lang racket
(provide unparse)
(require "interpreter-ast.rkt" "program-ast.rkt")

;;Given a parsed exression, return the original s-expression from which it
;;was parsed.
;;Expr -> S-Expression
(define (unparse e)
  (match e
    [(IInt i)    i]
    [(IBool b)   b]
    [(IChar c)   c]
    [(IEof) 'eof]
    [(IErr) ''err]
    [(IEmpty) ''()]
    [(IVoid) (void)]
    [(IVar v) v]
    [(? program-ast? a) a]
    [(ISymbol s) (list 'quote s)]
    [(ILet x b e)
      (list 'let (list (list x (unparse b))) (unparse e)) ]
    [(IIf e1 e2 e3)
     (list 'if (unparse e1) (unparse e2) (unparse e3))]
    [(IPrim0 p)
     (list p)]
    [(IPrim1 p e)
     (list p (unparse e))]
    [(IPrim2 p e1 e2)
      (list p (unparse e1) (unparse e2))]
    [(IApp f es)
     (append (list f) (map unparse es))]
    [(IBegin2 e1 e2)
     (list 'begin (unparse e1) (unparse e2))]
    [(IMatch e cls)
     (append (list 'match (unparse e)) (map unparse-clause cls))]))

;;Clause -> S-Expression
(define (unparse-clause c)
  (match c
    [(IClause pat body)
     (list (unparse-pattern pat) (unparse body))]))

;;Pattern  -> S-Expression
(define (unparse-pattern pat)
  (match pat
    [(IInt i) i]
    [(IBool b) b]
    [(IChar c) c]
    [(IVar s) s]
    [(ISymbol s) (list 'quote s)]
    [(IWild) '_]
    [(IErr) ''err]
    [(IEmpty) ''()]
    [(IBox b) (list 'box b)]
    [(ICons h t) (list 'cons h t)]
    [(IPred p) (list '? p)]
    [(IPWV p v) (list '? p v)]
    [(IList-S-PWVs s pwvs)
     (cons (list 'quote s)
           (map (λ (pwv)
                  (match pwv
                    [(IPWV p v) (list '? p v)]))))]
    [(IList-S-Vs s vs)
     (cons (list 'quote s) vs)]
    [(IList-S-Ps s ps)
     (cons (list 'quote s) (map (λ (p) (list '? p))))]
    [(IList-S-LSV s1 (list s2 var))
     (list (list 'quote s1) (list 'quote s2) var)]
    [(IStruct (list 'Int s)) (Int s)]
    [(IStruct (list 'Bool s)) (Bool s)]
    [(IStruct (list 'Char s)) (Char s)]
    [(IStruct (list 'Var s)) (Var s)]
    [(IStruct (list 'Eof)) (Eof)]
    [(IStruct (list 'Prim0 p)) (Prim0 p)]
    [(IStruct (list 'Begin2 e1 e2)) (Begin2 e1 e2)]
    [(IStruct (list 'Prim1 s1 s2)) (Prim1 s1 s2)]
    [(IStruct (list 'Prim2 s1 s2 s3)) (Prim2 s1 s2 s3)]
    [(IStruct (list 'If s1 s2 s3)) (If s1 s2 s3)]
    [(IStruct (list 'Let s1 s2 s3)) (Let s1 s2 s3)]))
  