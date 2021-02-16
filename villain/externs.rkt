#lang racket
(provide externs char-op->uc)
(require "ast.rkt" a86/ast)

(define (externs p)
  (match p
    [(Prog ds e)
     (remove-duplicates (append (externs-ds ds)
                                (externs-e e)))]))

(define (externs-ds ds)
  (match ds
    ['() '()]
    [(cons d ds)
     (append (externs-d d)
             (externs-ds ds))]))

(define (externs-d d)
  (match d
    [(Defn f xs e) (externs-e e)]
    [(Defn* f xs xs* e) (externs-e e)]))

(define (externs-e e)
  (match e
    [(App f es)
     (externs-es es)]
    [(Symbol _)
     (list (Extern 'str_to_symbol))]
    [(Prim0 p)
     (externs-p p)]
    [(Prim1 p e)
     (append (externs-p p)
             (externs-e e))]
    [(Prim2 p e1 e2)
     (append (externs-p p)
             (externs-e e1)
             (externs-e e2))]
    [(If e1 e2 e3)
     (append (externs-e e1)
             (externs-e e2)
             (externs-e e3))]     
    [(Begin e1 e2)
     (append (externs-e e1)
             (externs-e e2))]
    [(Let x e1 e2)
     (append (externs-e e1)
             (externs-e e2))]
    [_ '()]))

(define (externs-es es)
  (match es
    ['() '()]
    [(cons e es)
     (append (externs-e e)
             (externs-es es))]))
  
(define (externs-p p)
  (let ((r (op->extern p)))
    (if r (list (Extern r)) '())))

(define (op->extern o)
  (match o
    ['peek-byte 'peek_byte]
    ['read-byte 'read_byte]
    ['write-byte 'write_byte]
    ['gensym 'gensym]
    ['string->symbol 'str_to_symbol]
    [_ (char-op->uc o)]))

(define (char-op->uc o)
  (match o
    ['char-alphabetic? 'uc_is_property_alphabetic]
    ['char-whitespace? 'uc_is_property_white_space]
    ['char-upcase 'uc_toupper]
    ['char-downcase 'uc_tolower]
    ['char-titlecase 'uc_totitle]
    [_ #f]))
