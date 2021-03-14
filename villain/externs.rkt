#lang racket
(provide externs symbol->label)
(require "ast.rkt" "externs-stdlib.rkt" a86/ast)

(define (externs p)
  (match p
    [(Prog ds e)
     (remove-duplicates (append (externs-ds ds)
                                (externs-e e)))]
    [(CMod pv-exts pvs ds e)
     (let ((exts (apply set (append (externs-ds ds)
                                    (externs-e e))))
           (prvs (apply set (map Extern (map symbol->label pv-exts)))))
       (set->list (set-subtract exts prvs)))]
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
    [(App 'ccall 
          (cons (String f) es))
     (list (Extern (string->symbol f)))]
    [(App f es)
     (append (externs-f f)
             (externs-es es))]
    [(Apply f e)
     (append (externs-f f)
             (externs-e e))]
    [(Prim0 p)
     (externs-p p)]
    [(Prim1 p e)
     (append (externs-p p)
             (externs-e e))]
    [(Prim2 p e1 e2)
     (append (externs-p p)
             (externs-e e1)
             (externs-e e2))]
    [(Prim3 p e1 e2 e3)
     (append (externs-p p)
             (externs-e e1)
             (externs-e e2)
             (externs-e e3))]
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
  (if (stdlib-provided? f) 
    (list (Extern (symbol->label f))) 
    '())) ; if it is a call to std library function

(define (externs-p p)
  (let ((r (op->extern p)))
    (match r
      [#f '()]
      [(? list?) (map (λ (e) (Extern e)) r)]
      [_ (list (Extern r))])))

(define (op->extern o)
  (match o
    ['peek-byte '(peek_byte peek_byte_port)]
    ['read-byte '(read_byte read_byte_port)]
    ['peek-char 'peek_char]
    ['read-char 'read_char]
    ['write-byte 'write_byte]
    ['write-char 'write_char]
    ['gensym 'gensym]
    #;['string->symbol 'str_to_symbol]  ;; always included now
    ['open-input-file 'open_input_file]
    ['close-input-port 'close_input_port]
    [_ #f]))

;; Symbol -> Boolean
;; Is x provided by a stdlib?
(define (stdlib-provided? x)
  (memq x stdlib-ids))

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
