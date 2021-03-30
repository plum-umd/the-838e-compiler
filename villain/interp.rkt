#lang racket
(provide interp interp-env interp-prim1)
(require "ast.rkt" "parse.rkt"
         "env.rkt" "externs.rkt"
         "interp-prims.rkt"
         "interp-stdlib.rkt")
(require racket/struct)

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | String
;; | Vector
;; | Eof
;; | Void
;; | '()
;; | (cons Value Value)
;; | #s(Value Value Value)
;; | (box Value)

;; type REnv = (Listof (List Id Value))
;; type Defns = (Listof Defn)

;; (Letrec (Lisof Id) (Listof Lambda) Expr) -> Answer

(define (interp p)
  (match p
    [(Letrec names bodies (Prog sts ds e))
  (let* ((bs (map desugar-def stdlib-defs))
         (new-ds  (interp-structs sts))
         (l (Letrec (append (map car new-ds) names) (append (map cdr new-ds) bodies) e)))
    (interp-aux (Letrec (map car bs) (map cdr bs) l)))]))

(define (interp-aux p)
  (interp-env p '() stdlib))

;; Expr Env Defns -> Answer
(define (interp-env e r ds)
  (match e
    [(Int i)  i]
    [(Bool b) b]
    [(Char c) c]
    [(Flonum f) f]
    [(String s) s]
    [(Symbol s) s]
    [(Eof)    eof]
    [(Empty)  '()]
    [(Vec es) (list->vector (interp-env* es r ds))]
    [(Var x)  (lookup r x)]
    [(Prim0 'void) (void)]
    [(Prim0 'read-byte) (read-byte)]
    [(Prim0 'read-char) (read-char)]
    [(Prim0 'peek-byte) (peek-byte)]
    [(Prim0 'peek-char) (peek-char)]
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
    [(Mps (Prefab-Key (Symbol s) n1 (list n2 v2) muts) rest)
     (apply make-prefab-struct s (map (λ (e) (interp-env e r ds)) rest))]
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
    [(Prog sts '() e)
     (let* ((new-ds (interp-structs sts)))
       (interp-env e r (append new-ds ds)))]
    [(Let xs es e)
     (match (interp-env* es r ds)
       ['err 'err]
       [vs (interp-env e (append (reverse (zip xs vs)) r) ds)])]
    [(Letrec xs es e)
     (letrec ((r* (λ ()
                    (append
                     (zip xs
                          ;; η-expansion to delay evaluating r*
                          ;; relies on RHSs being functions
                          ;; (ref: CMSC430 codes by Dr. Van Horn)
                          (map (λ (l) (λ vs (apply (interp-env l (r*) ds) vs)))
                               es))
                      r))))
       (interp-env e (r*) ds))]
    [(Apply f ex)
     (match (interp-env ex r ds)
       [(list vs ...)
;        (if (or (and (Var? f) (memq (Var-x f) stdlib-ids))
;                (symbol? f))
;            (let ((f (if (symbol? f) f (Var-x f))))
;              (match (defns-lookup ds f)
;                [(Defn f xs e)
;                 ; check arity matches
;                 (if (= (length xs) (length vs))
;                     (interp-env e (zip xs vs) ds)
;                     'err)] 
;                [(Defn* f xs xs* e) 
;                 (if (>= (length vs) (length xs)) 
;                     (interp-env e 
;                        (append (zip xs (take vs (length xs))) 
;                                (list (list xs* (list-tail vs (length xs))))) ds)
;                     'err)]))
            (let ((p (interp-env f r ds)))
              (if (procedure? p)
                  (apply p vs)
                  'err))]
       [_ 'err])]
    
    [(Lam l xs e0)   (λ vs (if (= (length vs) (length xs))
                               (interp-env e0 (append (zip xs vs) r) ds)
                               'err))]
    [(Lam* l xs xs* e0)  (λ vs
                           (if (>= (length vs) (length xs))
                               (interp-env e0 (append
                                     (zip xs (take vs (length xs)))
                                     (list (list xs* (list-tail vs (length xs))))
                                     r) ds)
                             'err))]
    [(LCall e es)
;     (if (and (Var? e) (memq (Var-x e) stdlib-ids))                                          
;                        (interp-env (App (Var-x e) es) r ds)
                        (match (interp-env* (cons e es) r ds)
                          [(list f vs ...)
                           (if (procedure? f)
                               (apply f vs)
                               'err)])]
;    [(App f es)
;     (match (interp-env* es r ds)
;       [(list vs ...)
;        (match (defns-lookup ds f)
;          [(Defn f xs e)
;           ; check arity matches
;           (if (= (length xs) (length vs))
;               (interp-env e (zip xs vs) ds)
;               'err)] 
;          [(Defn* f xs xs* e) 
;           (if (>= (length vs) (length xs)) 
;               (interp-env e 
;                  (append (zip xs (take vs (length xs))) 
;                          (list (list xs* (list-tail vs (length xs))))) ds)
;               'err)])]
;       [_ 'err])]
    [(Match e0 cs)
     (match (interp-env e0 r ds)
       ['err 'err]
       [v (interp-match v cs r ds)])]
    ['err 'err])) 


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
          [(Sym s)
           (if (eq? s v)
               (interp-env e r ds)
               (interp-match v cs r ds))]
          [(Box x)
           (if (box? v)
               (interp-env e (ext r x (unbox v)) ds)
               (interp-match v cs r ds))]
          [(Cons x1 x2)
           (if (cons? v)
               (interp-env e (ext (ext r x2 (cdr v)) x1 (car v)) ds)
               (interp-match v cs r ds))]
          [(Strct s xs)
             (if (and (struct? v) (eq? s (prefab-struct-key v)) (eq? (length xs) (length (struct->list v))))
               (let ((values (struct->list v))
                     (vars xs))
                 (interp-env e (foldr (λ (x a) (ext a (car x) (car (cdr x)))) r (zip vars values)) ds))
               (interp-match v cs r ds))])])]))

;; (Listof Expr) REnv Defns -> (Listof Value) | 'err
(define (interp-env* es r ds)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r ds)
       ['err e]
       [v (cons v (interp-env* es r ds))])]))

;; Defns -> Defns
(define (interp-structs ds)
  (match ds
   ['() '()]
   [(cons (Struct s xs) l) (append (create-struct-bindings s xs) (interp-structs l))]
   [(cons h t) (interp-structs t)]))

;;Symbol (Listof Symbol) -> Defns ;; These only work with symbols as the 's'!  ;; Also, need to do arity checks
(define (create-struct-bindings s xs)
  (let* ((constructor (create-constructor s xs))
         (predicate   (create-predicate s xs))
         (accessors   (create-accessors s xs))
         (result (cons constructor (cons predicate accessors))))
    result))

;;Symbol (Listof Symbol) -> Defn
(define (create-constructor s xs)
  (desugar-def (parse-d `(define ,(cons s xs)
                 (make-prefab-struct (quote ,s) ,xs)))))

;;Symbol (Listof Symbol) -> Defns
(define (create-predicate s xs)
  (let ((f (string-append (symbol->string s) "?")))
    (desugar-def (parse-d `(define ,(list (string->symbol f) 'st)
                        (match st
                          [(struct ,s ,xs) #t]
                          [_ #f]))))))

;;List -> Expr
(define (list->cons xs)
  (match xs
    ['() ''()]
    [(cons h t) `(cons ,h ,(list->cons t))]))

;;(Listof Symbols) -> Expr
(define (var_list->sym_cons xs)
  (match xs
    ['() ''()]
    [(cons h t) `(cons (quote ,h) ,(var_list->sym_cons t))]))

;;Symbol (Listof Symbol) Symbol -> Defn
(define (create-accessor s xs x)
  (let ((f (string-append (symbol->string s) "-" (symbol->string x))))
    (desugar-def (parse-d `(define ,(list (string->symbol (string-append (symbol->string s) "-" (symbol->string x))) 'st)
                (match st
                  [(struct ,s ,xs) ,x]
                  [_ 'err]))))))

;;Symbol (Listof Symbol) -> Defns
(define (create-accessors s xs)
   (map (λ (x) (create-accessor s xs x)) xs))



;; Defns Symbol -> Defn
(define (defns-lookup ds f)

  (findf (match-lambda [(Defn g _ _) (eq? f g)] [(Defn* g _ _ _) (eq? f g)])
         ds))

(define (zip xs ys)
  (match* (xs ys)
    [('() '()) '()]
    [((cons x xs) (cons y ys))
     (cons (list x y)
           (zip xs ys))]))
