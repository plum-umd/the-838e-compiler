#lang racket
(provide interp interp-env interp-prim1)
(require "ast.rkt"
         "env.rkt"
         "interp-prims.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | String        
;; | Eof
;; | Void
;; | '()
;; | (cons Value Value)
;; | (box Value)

;; type REnv = (Listof (List Id Value))
;; type Defns = (Listof Defn)

;; Prog Defns -> Answer
(define (interp p)
  (match p
    [(Prog ds e)
     (interp-env e '() ds)]))

;; Expr Env Defns -> Answer
(define (interp-env e r ds)
  (match e
    [(Int i)  i]
    [(Bool b) b]
    [(Char c) c]
    [(Float f) f]
    [(String s) s]
    [(Symbol s) s]
    [(Eof)    eof]
    [(Empty)  '()]
    [(Var x)  (lookup r x)]
    [(Prim0 'void) (void)]
    [(Prim0 'read-byte) (read-byte)]
    [(Prim0 'peek-byte) (peek-byte)]
    [(Prim0 'gensym)    (gensym)]
    [(Prim1 p e)
     (match (interp-env e r ds)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r ds)
       ['err 'err]
       [v1 (match (interp-env e2 r ds)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    [(Prim3 p e1 e2 e3)
     (match (interp-env e1 r ds)
       ['err 'err]
       [v1 (match (interp-env e2 r ds)
             ['err 'err]
             [v2 (match (interp-env e3 r ds)
                   ['err 'err]
                   [v3 (interp-prim3 p v1 v2 v3)])])])]
    [(If p e1 e2)
     (match (interp-env p r ds)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r ds)
            (interp-env e2 r ds))])]
    [(Begin e1 e2)
     (match (interp-env e1 r ds)
       ['err 'err]
       [_ (interp-env e2 r ds)])]
    [(Let x e1 e2)
     (match (interp-env e1 r ds)
       ['err 'err]
       [v (interp-env e2 (ext r x v) ds)])]
    [(App f es)
     (match (interp-env* es r ds)
       [(list vs ...)
        (match (defns-lookup ds f)
          [(Defn f xs e)
           ; check arity matches
           (if (= (length xs) (length vs))
               (interp-env e (zip xs vs) ds)
               'err)])])]
    [(Match e0 cs)
     (match (interp-env e0 r ds)
       ['err 'err]
       [v (interp-match v cs r ds)])]))

;; Value (Listof Clause) Env Defs -> Answer
(define (interp-match v cs r ds)
  (match cs
    ['() 'err]
    [(cons c cs)
     (match c
       [(Clause p e)
        (match p
          [(Wild)  (interp-env e r ds)]
          [(Var x) (interp-env e (ext r x v) ds)]
          [(Lit l)
           (if (eq? l v)
               (interp-env e r ds)
               (interp-match v cs r ds))]
          [(Box x)
           (if (box? v)
               (interp-env e (ext r x (unbox v)) ds)
               (interp-match v cs r ds))]
          [(Cons x1 x2)
           (if (cons? v)
               (interp-env e (ext (ext r x2 (cdr v)) x1 (car v)) ds)
               (interp-match v cs r ds))])])]))

;; (Listof Expr) REnv Defns -> (Listof Value) | 'err
(define (interp-env* es r ds)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r ds)
       ['err 'err]
       [v (cons v (interp-env* es r ds))])]))

;; Defns Symbol -> Defn
(define (defns-lookup ds f)
  (findf (match-lambda [(Defn g _ _) (eq? f g)])
         ds))

(define (zip xs ys)
  (match* (xs ys)
    [('() '()) '()]
    [((cons x xs) (cons y ys))
     (cons (list x y)
           (zip xs ys))]))
