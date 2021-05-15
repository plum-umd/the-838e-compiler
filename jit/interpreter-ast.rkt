#lang racket
(provide (all-defined-out))

;; type Defn = (Defn Id (Listof Id) Expr)
(struct IDefn (f xs e) #:prefab) 


;; type Expr = (IEof)
;;           | (IVoid)
;;           | (IEmpty)
;;           | (IInt Integer)
;;           | (IBool Boolean)
;;           | (IChar Character)
;;           | (ISymbol Symbol)
;;           | (IPrim0 Op0)
;;           | (IPrim1 Op1 Expr)
;;           | (IPrim2 Op2 Expr Expr)         
;;           | (IIf Expr Expr Expr)
;;           | (IBegin (Listof Expr))
;;           | (IBegin2 Expr Expr)
;;           | (IOr (Listof Expr))
;;           | (IAnd (Listof Expr))
;;           | (ILet (Listof Id) (Listof Expr) Expr)
;;           | (IVar Id)
;;           | (IApp Id (Listof Expr))
;;           | (IMatch Expr (Listof Pat))


;; type Id   = Symbol
;; type Op0  = 'read-byte | 'peek-byte | 'void  
;; type Op1  = 'add1 | 'sub1 | 'zero? | 'integer?
;;           | 'char? | 'integer->char | 'char->integer
;;           | 'write-byte | 'eof-object?
;;           | 'box | 'car | 'cdr | 'unbox  
;;           | 'empty?  
;; type Op2  = '+ | '- | 'cons 
;; type Pat  = (IWild)
;;           | (IVar Id)
;;           | (ILit Literal)
;;           | (ISym Symbol)
;;           | (ICons Id Id)
;;           | (IBox Id)
;;           | (IPat p)
;;           | (IEnv-Cons p1 p2 ec)
;;           | (IErr)
;;           | (IPred p)
;;           | (IPredV p v)
;;           | (IList-S-PWVs Symbol (Listof (Listof Id Id))) ;;stands for List of (Symbol, Predicates with Variables)
;;           | (IList-S-Ps Symbol Id) ;;stands for List of (Symbol, Predicates)
;;           | (IList-S-Vs Symbol Id) ;;stands for List of (Symbol, Variables)
;;           | (IList-S-LSV Symbol (Listof Symbol Id)) ;;stands for List of (Symbol, List  of Symbol and Variable)


;; type Litral = Boolean | '() | Char | Integer
;; type Binding = (Binding Id Expr)


(struct IEof   ()              #:prefab)
(struct IVoid  ()              #:prefab)
(struct IEmpty ()              #:prefab)
(struct IInt   (i)             #:prefab)
(struct IBool  (b)             #:prefab)
(struct IChar  (c)             #:prefab) 
(struct ISymbol (s)            #:prefab)
(struct IPrim0 (p)             #:prefab)
(struct IPrim1 (p e)           #:prefab)
(struct IPrim2 (p e1 e2)       #:prefab)
(struct IIf    (e1 e2 e3)      #:prefab)
(struct IBegin (es)            #:prefab)
(struct IOr (es)               #:prefab)
(struct IAnd (es)              #:prefab)
(struct ILet   (x b e)         #:prefab)
(struct IVar   (x)             #:prefab)
(struct IApp   (f es)          #:prefab)
(struct IMatch (e0 cs)         #:prefab)
(struct IBegin2 (e1 e2)        #:prefab)


;; Match clause
(struct IClause (p e)          #:prefab)
;; More Pattern constructors (Some of the exprs above can also be used in patterns)
(struct IWild ()               #:prefab)
(struct ILit (l)               #:prefab)
(struct ICons (p1 p2)          #:prefab)
(struct IBox (p)               #:prefab)
(struct IErr ()                #:prefab)
(struct IStruct (s)            #:prefab)
(struct IPred (p)              #:prefab)
(struct IPWV (p v)             #:prefab)
(struct IEnv-Cons (p1 p2 ec)   #:prefab) ; Added this pattern for when we are matching in the environment (cons (cons x val) r)
(struct IList-S-PWVs (s pwvs)           #:prefab)
(struct IList-S-Ps (s ps)               #:prefab)
(struct IList-S-LSV (s l)               #:prefab)
(struct IList-S-Vs (s vs)               #:prefab)
(struct IList-Ss-L (vs h)               #:prefab)  

(define (interpreter-ast? v)
  (or (IEof? v) (IVoid? v) (IEmpty? v) (IInt? v) (IBool? v) (IChar? v) (ISymbol? v) (IPrim0? v) (IPrim1? v) (IPrim2? v) 
      (IIf? v) (IBegin? v) (IBegin2? v) (IOr? v) (IAnd? v) (ILet? v) (IVar? v) (IApp? v) (IMatch? v) (IClause? v) (IWild? v) (ILit? v)
      (ICons? v) (IEnv-Cons? v) (IBox? v) (IErr? v) (IStruct? v) (IPred? v) (IPWV? v) (IList-S-PWVs? v) (IList-S-Ps? v) (IList-S-LSV? v) (IList-S-Vs? v) (IList-Ss-L? v)))