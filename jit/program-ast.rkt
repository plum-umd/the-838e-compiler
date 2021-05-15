#lang racket
(provide (all-defined-out))

;; type Prog = (Prog (Listof Defn) Expr)
(struct Prog (ds e) #:prefab)

;; type Defn = (Defn Id (Listof Id) Expr)
(struct Defn (f xs e) #:prefab)

;; type Expr = (Eof)
;;           | (Void)
;;           | (Empty)
;;           | (Int Integer)
;;           | (Bool Boolean)
;;           | (Char Character)
;;           | (Symbol Symbol)
;;           | (Prim0 Op0)
;;           | (Prim1 Op1 Expr)
;;           | (Prim2 Op2 Expr Expr)      
;;           | (If Expr Expr Expr)
;;           | (Begin2 Expr Expr)
;;           | (Or (Listof Expr))
;;           | (And (Listof Expr))
;;           | (Let (Listof Id) (Listof Expr) Expr)
;;           | (Var Id)
;;           | (App Id (Listof Expr))

;; type Id   = Symbol
;; type Op0  = 'read-byte | 'peek-byte | 'void 
;; type Op1  = 'add1 | 'sub1 | 'zero? | 'integer?
;;           | 'char? | 'integer->char | 'char->integer
;;           | 'write-byte | 'eof-object?
;;           | 'box | 'car | 'cdr | 'unbox   
;;           | 'empty?  
;; type Op2  = '+ | '- | 'cons



(struct Eof   ()              #:prefab)
(struct Void  ()              #:prefab)
(struct Empty ()              #:prefab)
(struct Int   (i)             #:prefab)
(struct Bool  (b)             #:prefab)
(struct Char  (c)             #:prefab)  
(struct Symbol (s)            #:prefab)
(struct Prim0 (p)             #:prefab)
(struct Prim1 (p e)           #:prefab)
(struct Prim2 (p e1 e2)       #:prefab)
(struct If    (e1 e2 e3)      #:prefab)
(struct Begin2 (e1 e2)        #:prefab)
(struct Let   (x b e)         #:prefab)
(struct Var   (x)             #:prefab)
(struct App   (f es)          #:prefab)

       

(define (program-ast? v)
  (or (Eof? v) (Void? v) (Empty? v) (Int? v) (Bool? v) (Char? v) (Symbol? v) (Prim0? v) (Prim1? v) (Prim2? v)
      (If? v) (Begin2? v) (Let? v) (Var? v) (App? v) (Prog? v) (Defn? v)))