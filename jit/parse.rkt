#lang racket

(provide parse)
(require "ast.rkt")


;; Parse a program in S-Expression form into AST form
;;S-Expression  -> Prog
(define (parse p)
  (match p
    [(? integer? i) (Int i)]
    [(? boolean? b) (Bool b)]
    [(? char? c) (Char c)]
    ['eof (Eof)]
    [(list 'quote (list))          (Empty)]
    [(? symbol? s) (Var s)]
    [(list 'quote 'err) (Err)]
    [(list 'quote (? symbol? s)) (Symbol s)]
    [(list 'let (list (list (? symbol? x) e1)) e2)
     (Let x (parse e1) (parse e2))] ; do we want to do n-ary let?
    [(list 'or es ...)
     (Or (map parse es))]
    [(list 'and es ...)
     (And (map parse es))]
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]
    [(list (? prim2? prim) e1 e2)
     (Prim2 prim (parse e1) (parse e2))]
    [(list (? prim1? prim) e)
     (Prim1 prim (parse e))]
    [(list (? prim0? prim))
     (Prim0 prim)]
    [(list 'begin e1 e2)
     (Begin2 (parse e1) (parse e2))]
    [(list 'begin es ...)
     (Begin (map parse es))]
    [(list 'require files ...)
     (Void)]
    [(list 'provide fns ...)
     (Void)]
    [(list 'define (list f xs ...) e)
     (Defn f xs (parse e))]
    [(list 'match e cls ...)
     (Match (parse e)
            (map parse-clause  cls))]
    [(list (? symbol? f) args ...)
     (App f (map parse args))]))

(define (parse-clause cls)
  (match cls
    [(list pat e)
     (Clause (parse-pat pat) (parse e))]))

(define (parse-pat  pat)
  (match pat
    [(? integer? i)               (Int i)]
    [(? boolean? b)               (Bool b)]
    [(? char? c)                  (Char c)]
    ['_                           (Wild)]
    [(? symbol? s)                (Var s)]
    [(list 'quote 'err)           (Err)]
    [(list 'quote (list))          (Empty)]
    [(list 'quote (? symbol? s))  (Symbol s)]
    [(list 'cons (? symbol? f) (? symbol? s)) (Cons f s)] 
    [(list 'Int s)                (Pat (Int s))]
    [(list 'Bool s)               (Pat (Bool s))]
    [(list 'Char s)               (Pat (Char s))]
    [(list 'Var x)                (Pat (Var x))]
    [(list 'Eof)                  (Pat (Eof))]
    [(list 'Empty)                (Pat (Empty))]
    [(list '? (? symbol? p))        (Pred p)]
    [(list 'Prim0 p)              (Pat (Prim0 p))]
    [(list 'Begin2 e1 e2)         (Pat (Begin2 e1 e2))]
    [(list 'Prim1 s1 s2)          (Pat (Prim1 s1 s2))]
    [(list 'Prim2 s1 s2 s3)       (Pat (Prim2 s1 s2 s3))]
    [(list 'If s1 s2 s3)          (Pat (If s1 s2 s3))]
    [(list 'Let s1 s2 s3)         (Pat (Let s1 s2 s3))]
    [(list 'cons (list 'list (? symbol? x1) (? symbol? x2)) (? symbol? r))
                                  (Env-Cons x1 x2 r)]
    [(list 'list (list 'quote (? symbol? s1)) (list '? (? symbol? ps) (? symbol? vs)) ...) ;;To deal with patterns like (list 'add1 (? integer? i))
     (List-S-PWVs s1 (map (λ (p v) (PWV p v)) ps vs))]
    [(list 'list (list 'quote (? symbol? s1)) (list '? (? symbol? ps)) ...) ;;To deal with patterns like (list 'integer->char (? integer?))
     (List-S-Ps s1 ps)]
    [(list 'list (list 'quote (? symbol? s1)) (? symbol? vs) ...) ;;To deal with patterns like (list 'char? v)
     (List-S-Vs s1 vs)]
    [(list 'list (list 'quote (? symbol? s1)) (list 'list (list 'quote (? symbol? s2)) (? symbol? i))) ;;To deal with patterns like (list 'car (list 'cons v))
     (List-S-LSV s1 (list s2 i))]))
     

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