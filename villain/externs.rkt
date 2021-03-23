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
           (prvs (apply set (map Extern (map symbol->label ps)))))
       (set->list (set-subtract exts prvs)))]))

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
     (append (externs-f f)
             (externs-es es))]
    ;;; [(Bignum _)                     ; I should probably do this when there are both integers and bignums right (or always?)
    ;;;   (list (Extern 'print_bignum)
    ;;;         (Extern 'bignum_length))]
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
    [(Let xs es e)
     (append (externs-es es)
             (externs-e e))]
    [(Match e cs)
     (append (externs-e e)
             (externs-cs cs))]
    [_ '()]))

;; [Listof Clause] -> [Listof Id]
(define (externs-cs cs)
  (match cs
    ['() '()]
    [(cons (Clause p e) cs)
     (append (externs-e e)
             (externs-cs cs))]))

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
    ['peek-char 'peek_char]
    ['read-byte 'read_byte]
    ['read-char 'read_char]
    ['write-byte 'write_byte]
    ['write-char 'write_char]
    ['gensym 'gensym]
    #;['string->symbol 'str_to_symbol]  ;; always included now
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

;; [Listof Id]
;; List of each Id provided by a stdlib
(define stdlib-ids
  '(; bool
    boolean? 
    not
    ; math
    byte? 
    *
    ; list
    append
    assq
    eighth
    first
    fifth
    fourth
    last
    length
    list
    list?
    list-ref
    list-tail
    memq
    ninth
    null?
    pair?
    remq
    remq*
    rest
    reverse
    second
    seventh
    sixth
    tenth
    third
    ; NOTE: add new stdlib-provided Ids here    
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
