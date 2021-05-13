#lang racket
(provide parse-interpreter)
(require "interpreter-ast.rkt")


;; Parse a program in S-Expression form into AST form
;;S-Expression  -> Prog
(define (parse-interpreter p)
  (match p
    [(? integer? i) (IInt i)]
    [(? boolean? b) (IBool b)]
    [(? char? c) (IChar c)]
    ['eof (IEof)]
    [(list 'quote (list))          (IEmpty)]
    [(? symbol? s) (IVar s)]
    [(list 'quote 'err) (IErr)]
    [(list 'quote (? symbol? s)) (ISymbol s)]
    [(list 'let (list (list (? symbol? x) e1)) e2)
     (ILet x (parse-interpreter e1) (parse-interpreter e2))] ; do we want to do n-ary let?
    [(list 'or es ...)
     (IOr (map parse-interpreter es))]
    [(list 'and es ...)
     (IAnd (map parse-interpreter es))]
    [(list 'if e1 e2 e3)
     (IIf (parse-interpreter e1) (parse-interpreter e2) (parse-interpreter e3))]
    [(list (? prim2? prim) e1 e2)
     (IPrim2 prim (parse-interpreter e1) (parse-interpreter e2))]
    [(list (? prim1? prim) e)
     (IPrim1 prim (parse-interpreter e))]
    [(list (? prim0? prim))
     (IPrim0 prim)]
    [(list 'begin e1 e2)
     (IBegin2 (parse-interpreter e1) (parse-interpreter e2))]
    [(list 'begin es ...)
     (IBegin (map parse-interpreter es))]
    [(list 'require files ...)
     (IVoid)]
    [(list 'provide fns ...)
     (IVoid)]
    [(list 'define (list f xs ...) e)
     (IDefn f xs (parse-interpreter e))]
    [(list 'match e cls ...)
     (IMatch (parse-interpreter e)
            (map parse-clause  cls))]
    [(list (? symbol? f) args ...)
     (IApp f (map parse-interpreter args))]))

(define (parse-clause cls)
  (match cls
    [(list pat e)
     (IClause (parse-pat pat) (parse-interpreter e))]))

(define (parse-pat  pat)
  (match pat
    [(? integer? i)               (IInt i)]
    [(? boolean? b)               (IBool b)]
    [(? char? c)                  (IChar c)]
    ['_                           (IWild)]
    [(? symbol? s)                (IVar s)]
    [(list 'quote 'err)           (IErr)]
    [(list 'quote (list))          (IEmpty)]
    [(list 'quote (? symbol? s))  (ISymbol s)]
    [(list 'cons (? symbol? f) (? symbol? s)) (ICons f s)] 
    [(list 'Int s)                (IStruct (list 'Int s))]
    [(list 'Bool s)               (IStruct (list 'Bool s))]
    [(list 'Char s)               (IStruct (list 'Char s))]
    [(list 'Var x)                (IStruct (list 'Var x))]
    [(list 'Eof)                  (IStruct (list 'Eof))]
    [(list 'Empty)                (IStruct (list 'Empty))]
    [(list 'Prim0 p)              (IStruct (list 'Prim0 p))]
    [(list 'Begin2 e1 e2)         (IStruct (list 'Begin2 e1 e2))]
    [(list 'Prim1 s1 s2)          (IStruct (list 'Prim1 s1 s2))]
    [(list 'Prim2 s1 s2 s3)       (IStruct (list 'Prim2 s1 s2 s3))]
    [(list 'If s1 s2 s3)          (IStruct (list 'If s1 s2 s3))]
    [(list 'Let s1 s2 s3)         (IStruct (list 'Let s1 s2 s3))]
    [(list 'cons (list 'list (? symbol? x1) (? symbol? x2)) (? symbol? r))
                                  (IEnv-Cons x1 x2 r)]
    [(list '? (? symbol? p))        (IPred p)]
    [(list 'list (list 'quote (? symbol? s1)) (list '? (? symbol? ps) (? symbol? vs)) ...) ;;To deal with patterns like (list 'add1 (? integer? i))
     (IList-S-PWVs s1 (map (λ (p v) (IPWV p v)) ps vs))]
    [(list 'list (list 'quote (? symbol? s1)) (list '? (? symbol? ps)) ...) ;;To deal with patterns like (list 'integer->char (? integer?))
     (IList-S-Ps s1 ps)]
    [(list 'list (list 'quote (? symbol? s1)) (? symbol? vs) ...) ;;To deal with patterns like (list 'char? v)
     (IList-S-Vs s1 vs)]
    [(list 'list (list 'quote (? symbol? s1)) (list 'list (list 'quote (? symbol? s2)) (? symbol? i))) ;;To deal with patterns like (list 'car (list 'cons v))
     (IList-S-LSV s1 (list s2 i))]))
     

;;Given a symbol, determine if it is a primitive of the language
;;Symbol -> boolean
(define (prim2? s)
  (if (member s (list '+ '- 'cons))
      #t
      #f))

(define (prim1? s)
  (if (member s (list 'write-byte 'eof-object? 'add1 'sub1 'zero? 'char? 'integer->char 'char->integer 'box 'unbox 'car 'cdr 'empty?))
      #t
      #f))

(define (prim0? s)
  (if (member s (list 'read-byte 'peek-byte 'void))
      #t
      #f))