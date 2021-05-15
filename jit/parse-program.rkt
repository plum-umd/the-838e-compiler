#lang racket

(provide parse-program)
(require "program-ast.rkt")

;; Parse a program in S-Expression form into AST form
;; S-Expr -> Prog
(define (parse-program s)
  (match s
    [(list 'begin (and ds (list 'define _ _)) ... e)
      (Prog (map parse-program-defns ds) (parse-e e))]
    [e (Prog '() (parse-e e))]))

;; S-Expr -> Defn
(define (parse-program-defns s)
  (match s
    [(list 'define (list (? symbol? f) (? symbol? xs) ...) e)
     (Defn f xs (parse-e e))]
    [_ (error "Parse defn error" s)]))

;;S-Expression  -> Prog
(define (parse-e p)
  (match p
    [(? integer? i)                 (Int i)]
    [(? boolean? b)                 (Bool b)]
    [(? char? c)                    (Char c)]
    ['eof                           (Eof)]
    [(list 'quote (list))           (Empty)]
    [(? symbol? s)                  (Var s)]
    [(list 'quote (? symbol? s))    (Symbol s)]
    [(list 'let (list (list (? symbol? x) e1)) e2)
        (Let x (parse-e e1) (parse-e e2))] ; do we want to do n-ary let?
    [(list 'if e1 e2 e3)
     (If (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list (? prim2? prim) e1 e2)
     (Prim2 prim (parse-e e1) (parse-e e2))]
    [(list (? prim1? prim) e)
     (Prim1 prim (parse-e e))]
    [(list (? prim0? prim))
     (Prim0 prim)]
    [(list 'begin e1 e2)
     (Begin2 (parse-e e1) (parse-e e2))]
    [(list (? symbol? f) args ...)
     (App f (map parse-e args))]))
     

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