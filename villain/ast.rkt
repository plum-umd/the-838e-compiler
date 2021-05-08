#lang racket
(provide (all-defined-out))

;; type Prog = (Prog (Listof Defn) Expr)
;;           | (Lib  (Listof Id) (Listof Defn))
(struct Prog (ds e)  #:prefab)
(struct Lib  (xs ds) #:prefab)
(struct Mod  (pvs rqs ds e) #:prefab)

;; pv-ext: provided by external module
;; pvs:    provided by this module
;; fs:     all functions available
;; ls:     all lambdas
;; dfλs:   definition lambda in this module
;; e:      module result expression
(struct CMod (pv-exts pvs fs ls dfλs e) #:prefab) ; for compiling a module 

;; type Defn = (Defn Id (Listof Id) Expr)
(struct Defn (f xs e) #:prefab) 

;; type Defn* = (Defn* Id (Listof Id) Id Expr)
(struct Defn* (f xs xs* e) #:prefab)

;;(struct Contract (id e) #:prefab)
(struct FnContract (es) #:prefab)
(struct Defn/contract (f xs c e) #:prefab) 

;; type Expr = (Eof)
;;           | (Empty)
;;           | (Int Integer)
;;           | (Bool Boolean)
;;           | (Char Character)
;;           | (Bignum Bignum)
;;           | (Flonum f)
;;           | (String String)
;;           | (Vec (Listof Expr))
;;           | (Symbol Symbol)
;;           | (Prim0 Op0)
;;           | (Prim1 Op1 Expr)
;;           | (Prim2 Op2 Expr Expr)      
;;           | (Prim3 Op3 Expr Expr Expr)     
;;           | (If Expr Expr Expr)
;;           | (Begin Expr Expr)
;;           | (Let (Listof Id) (Listof Expr) Expr)
;;           | (Var Id)
;;           | (App Id (Listof Expr))
;;           | (Match Expr (Listof Pat))
;;           | (Lam Label Formals Expr)
;;           | (Lam* Label Formals Formal Expr)
;;           | (LCall Expr (Listof Expr))
;;           | (Letrec (Lisof Id) (Listof Lambda) Expr)

;; type Id   = Symbol
;; type Label = Symbol
;; type Op0  = 'read-byte | 'void | 'collect-garbage
;; type Op1  = 'add1 | 'sub1 | 'zero? | 'integer?
;;           | 'char? | 'integer->char | 'char->integer
;;           | 'write-byte | 'eof-object?
;;           | 'box | 'car | 'cdr | 'unbox
;;           | 'string-length | 'string? | make-string    
;;           | 'empty?  |'vector? |'vector-length |list?
;; type Op2  = '+ | '- | * | quotient| remainder
;;           | 'eq? | 'cons | 'string-ref | 'make-vector 
;;           | 'vector-ref |'vector-set!
;; type Op3  = 'string-set!
;; type Op4  = 'vector-cas!
;; type Pat  = (Wild)
;;           | (Var Id)
;;           | (Lit Literal)
;;           | (Sym Symbol)
;;           | (Cons Id Id)
;;           | (Box Id)

;; type Litral = Boolean | '() | Char | Integer
;; type Binding = (Binding Id Expr)
;; type Formals = (Listof Id)
;; type Formal = Id
;; type LExpr = Expr    
;; type Lambda = (Lam Label Formals Expr)
;;           | (Lam* Label Formals Formal Expr)
;; type LabelledLambda = Lambda


(struct Pass  ()              #:prefab)
(struct Error ()              #:prefab)
(struct Eof   ()              #:prefab)
(struct Empty ()              #:prefab)
(struct Int   (i)             #:prefab)
(struct Bool  (b)             #:prefab)
(struct Char  (c)             #:prefab)
(struct Flonum (f)            #:prefab)
(struct String (s)            #:prefab)   
(struct Bignum (i)            #:prefab)   
(struct Symbol (s)            #:prefab)
(struct Prim0 (p)             #:prefab)
(struct Prim1 (p e)           #:prefab)
(struct Prim2 (p e1 e2)       #:prefab)
(struct Prim3 (p e1 e2 e3)    #:prefab)
(struct Prim4 (p e1 e2 e3 e4) #:prefab)
(struct If    (e1 e2 e3)      #:prefab)
(struct Begin (e1 e2)         #:prefab)
(struct Let   (xs es e)       #:prefab)
(struct Var   (x)             #:prefab)
(struct App   (f es)          #:prefab)
(struct LCall (e es)          #:prefab)
(struct Apply (f e)           #:prefab)
(struct Match (e0 cs)         #:prefab)
(struct Vec   (es)            #:prefab)
(struct Lam   (l xs e)        #:prefab)
;(struct Lam/contract-l (l xs e)        #:prefab)
(struct Lam/contract
              (l xs c e)      #:prefab)
;(struct Lam-l (l xs e)        #:prefab)
(struct Lam*  (l xs xs* e)    #:prefab)
;(struct Lam*-l (l xs xs* e)   #:prefab)
(struct Letrec (xs es e)      #:prefab)

;; Match clause
(struct Clause (p e)          #:prefab)
;; Pattern constructors
(struct Wild ()               #:prefab)
(struct Lit (l)               #:prefab)
(struct Sym (s)               #:prefab)
(struct Cons (p1 p2)          #:prefab)
(struct Box (p)               #:prefab)
