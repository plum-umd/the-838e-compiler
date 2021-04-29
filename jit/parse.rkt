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
    [(? symbol? s) (Var s)]
    [(list 'quote 'err) (Err)]
    [(list 'quote (? symbol? s)) (Symbol s)]
    [(list 'or es ...)
     (Or (map parse es))]
    [(list 'and es ...)
     (And (map parse es))]
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]
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
    [(? integer? i) (Int i)]
    [(? boolean? b) (Bool b)]
    [(? char? c) (Char c)]
    [(? symbol? s) (Var s)]
    [(list 'quote 'err) (Err)]
    [(list 'quote (? symbol? s)) (Symbol s)]
    ['_ (Wild)]
    [(list 'Int s) (Pat (Int s))]
    [(list 'Bool s) (Pat (Bool s))]
    [(list 'Char s) (Pat (Char s))]
    [(list 'Eof)    (Pat (Eof))]
    [(list 'Prim0 p) (Pat (Prim0 p))]
    [(list 'Begin2 e1 e2) (Pat (Begin2 e1 e2))]
    [(list 'Prim1 s1 s2) (Pat (Prim1 s1 s2))]
    [(list 'If s1 s2 s3) (Pat (If s1 s2 s3))]))

;;Given a symbol, determine if it is a primitive of the language
;;Symbol -> boolean
(define (prim1? s)
  (if (member s (list 'write-byte 'eof-object? 'add1 'sub1 'zero? 'char? 'integer->char 'char->integer))
      #t
      #f))

(define (prim0? s)
  (if (member s (list 'read-byte 'peek-byte 'void))
      #t
      #f))