#lang racket
(require "ast.rkt")
(provide value? extract-literal)

;;Expr -> boolean
;;Given an expression, determine if it is a value
(define (value? v)
  (match v
    [(or (Int _) (Bool _) (Char _) (Empty) (Eof)) #t]
    [_ #f]))

;;Value -> (or Integer Boolean Character '() eof) 
;;Given an Expr that is a Value, extract the data it contains or represents
(define (extract-literal v)
  (match v
    [(Int i) i]
    [(Bool b) b]
    [(Char c) c]
    [(Empty) '()]
    [(Eof) eof]))