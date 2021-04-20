#lang racket

(provide parse)
(require "ast.rkt")


;; Parse a program in S-Expression form into AST form
;;S-Expression  -> Prog
(define (parse p)
  (match p
    [(? integer? i) (Int i)]
    [(? boolean? b) (Bool b)]
    [(? symbol? s) (Var s)]
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]
    [(list (? prim1? prim) e)
     (Prim1 prim (parse e))]
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
    [(list 'quote (? symbol? s)) (Symbol s)]
    [(list 'Int s) (Pat (Int s))]
    [(list 'Bool s) (Pat (Bool s))]
    [(list 'Prim1 s1 s2) (Pat (Prim1 s1 s2))]
    [(list 'If s1 s2 s3) (Pat (If s1 s2 s3))]))

;;Given a symbol, determine if it is a primitive of the language
;;Symbol -> boolean
(define (prim1? s)
  (if (member s (list 'add1 'sub1 'zero?))
      #t
      #f))