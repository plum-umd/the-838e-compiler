#lang racket
(provide externs char-op->uc symbol->label)
(require "ast.rkt" a86/ast)

(define (externs p)
  (match p
    [(Prog ds e)
     (remove-duplicates (append (externs-ds ds)
                                (externs-e e)))]
    [(Lib ps ds)
     ; provided ids aren't external
     (let ((exts (apply set (externs-ds ds)))
           (prvs (apply set ps)))
       (set->list (set-subtract exts prvs)))]))

(define (externs-ds ds)
  (match ds
    ['() '()]
    [(cons d ds)
     (append (externs-d d)
             (externs-ds ds))]))

(define (externs-d d)
  (match d
    [(Defn f xs e) (externs-e e)]))

(define (externs-e e)
  (match e
    [(App f es)
     (append (externs-f f)
             (externs-es es))]
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

(define (externs-f f)
  (if (stdlib-provided? f) (list (Extern (symbol->label f))) '())) ; if it is a call to std library function

(define (externs-p p)
  (let ((r (op->extern p)))
    (if r (list (Extern r)) '())))

(define (op->extern o)
  (match o
    ['peek-byte 'peek_byte]
    ['read-byte 'read_byte]
    ['write-byte 'write_byte]
    [_ (char-op->uc o)]))

(define (char-op->uc o)
  (match o
    ['char-alphabetic? 'uc_is_property_alphabetic]
    ['char-whitespace? 'uc_is_property_white_space]
    ['char-upcase 'uc_toupper]
    ['char-downcase 'uc_tolower]
    ['char-titlecase 'uc_totitle]
    [_ #f]))

;; Symbol -> Boolean
;; Is x provided by a stdlib?
(define (stdlib-provided? x)
  (memq x stdlib-ids))

;; [Listof Symbol]
;; List of all symbols provided by a stdlib
(define stdlib-ids
  (append '(length append sum reverse) ; list
          ; NOTE: add new stdlib ids here
          ))

;; Symbol -> Label
;; Produce a symbol that is a valid Nasm label
(define (symbol->label s)
  (string->symbol
   (string-append
    "label_"
    (list->string
     (map (λ (c)
            (if (or (char<=? #\a c #\z)
                    (char<=? #\A c #\Z)
                    (char<=? #\0 c #\9)
                    (memq c '(#\_ #\$ #\# #\@ #\~ #\. #\?)))
                c
                #\_))
         (string->list (symbol->string s))))
    "_"
    (number->string (eq-hash-code s) 16))))

