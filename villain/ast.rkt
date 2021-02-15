#lang racket
(provide (all-defined-out))

;; type Prog = (Prog (Listof Defn) Expr)
(struct Prog (ds e) #:prefab)

;; type Defn = (Defn Id (Listof Id) Expr)
(struct Defn (f xs e) #:prefab) 

;; type Defn* = (Defn* Id (Listof Id) Id Expr)
(struct Defn* (f xs xs* e) #:prefab)

;; type Expr = (Eof)
;;           | (Empty)
;;           | (Int Integer)
;;           | (Bool Boolean)
;;           | (Char Character)
;;           | (String)         
;;           | (Prim0 Op0)
;;           | (Prim1 Op1 Expr)
;;           | (Prim2 Op2 Expr Expr)      
;;           | (Prim3 Op3 Expr Expr Expr)     
;;           | (If Expr Expr Expr)
;;           | (Begin Expr Expr)
;;           | (Let Id Expr Expr)
;;           | (Var Id)
;;           | (App Id (Listof Expr))
;;           | (Match Expr (Listof Pat))
;; type Id   = Symbol
;; type Op0  = 'read-byte | 'void | 'collect-garbage
;; type Op1  = 'add1 | 'sub1 | 'zero?
;;           | 'char? | 'integer->char | 'char->integer
;;           | 'write-byte | 'eof-object?
;;           | 'box | 'car | 'cdr | 'unbox
;;           | 'string-length | 'string? | make-string     
;;           | 'empty?
;; type Op2  = '+ | '- | 'eq?
;;           | 'cons | 'string-ref
;; type Op3  = 'string-set!                    
;; type Pat  = (Wild)
;;           | (Var Id)
;;           | (Lit Literal)
;;           | (Cons Id Id)
;;           | (Box Id)
;; type Litral = Boolean | '() | Char | Integer

(struct Eof   ()           #:prefab)
(struct Empty ()           #:prefab)
(struct Int   (i)          #:prefab)
(struct Bool  (b)          #:prefab)
(struct Char  (c)          #:prefab)
(struct Float (f)          #:prefab)
(struct String (s)         #:prefab)   
(struct Prim0 (p)          #:prefab)
(struct Prim1 (p e)        #:prefab)
(struct Prim2 (p e1 e2)    #:prefab)
(struct Prim3 (p e1 e2 e3) #:prefab)
(struct If    (e1 e2 e3)   #:prefab)
(struct Begin (e1 e2)      #:prefab)
(struct Let   (x e1 e2)    #:prefab)
(struct Var   (x)          #:prefab)
(struct App   (f es)       #:prefab)
(struct Match (e0 cs)      #:prefab)

;; Match clause
(struct Clause (p e)       #:prefab)
;; Pattern constructors
(struct Wild ()            #:prefab)
(struct Lit (l)            #:prefab)
(struct Cons (p1 p2)       #:prefab)
(struct Box (p)            #:prefab)

