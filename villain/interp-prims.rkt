#lang racket
(require "ast.rkt"  racket/flonum)
(provide interp-prim1 interp-prim2 interp-prim3)

;; Op1 Value -> Answer
(define (interp-prim1 p1 v)
  (match (list p1 v)
    [(list 'add1 (? integer?))            (add1 v)]
    [(list 'sub1 (? integer?))            (sub1 v)]
    [(list 'zero? (? integer?))           (zero? v)]
    [(list 'integer? v)                   (integer? v)]
    [(list 'integer-length (? integer?))  (integer-length v)]
    [(list 'char? v)                      (char? v)]
    [(list 'char->integer (? char?))      (char->integer v)]
    [(list 'integer->char (? codepoint?)) (integer->char v)]
    [(list 'eof-object? v)                (eof-object? v)]
    [(list 'write-byte (? byte?))         (write-byte v)]
    [(list 'write-char (? char?))         (write-char v)]
    [(list 'box v)                        (box v)]
    [(list 'unbox (? box?))               (unbox v)]
    [(list 'car (? pair?))                (car v)]
    [(list 'cdr (? pair?))                (cdr v)]
    [(list 'string-length (? string?))    (string-length v)]   
    [(list 'string? v)                    (string? v)]
    [(list 'bytes? v)                     (bytes? v)]
    [(list 'bytes-length (? bytes?))      (bytes-length v)]
    [(list 'empty? v)                     (empty? v)]
    [(list 'char-whitespace? (? char?))   (char-whitespace? v)]
    [(list 'string->symbol (? string?))   (string->symbol v)]
    [(list 'symbol->string (? symbol?))   (symbol->string v)]
    [(list 'symbol? v)                    (symbol? v)]
    [(list 'port? v)                      (port? v)]
    [(list 'open-input-file (? string?))  (with-handlers
                                            ([exn:fail:filesystem:errno? (λ (_) 'err)])
                                            (open-input-file v))]
    [(list 'close-input-port (? port?))   (close-input-port v)]
    [(list 'read-byte
           (and (? port?)
                (not (? port-closed?))))  (read-byte v)]
    [(list 'peek-byte
           (and (? port?)
                (not (? port-closed?))))  (peek-byte v)]
    [(list 'flonum? v)                    (flonum? v)]
    [(list 'vector? v)                    (vector? v)]
    [(list 'vector-length v)              (vector-length v)]
    [_                                    'err]))

;; Op2 Value Value -> Answer
(define (interp-prim2 p v1 v2)
  (match (list p v1 v2)
    [(list '+ (? integer?) (? integer?))  (+ v1 v2)]
    [(list '- (? integer?) (? integer?))  (- v1 v2)]
    [(list '* (? integer?) (? integer?)) (* v1 v2)]
    [(list 'quotient (? integer?) (? integer?)) (if (= v2 0) 'err (quotient v1 v2))]
    [(list 'remainder (? integer?) (? integer?)) (if (= v2 0) 'err (remainder v1 v2))]
    [(list '< (? integer?) (? integer?)) (< v1 v2)]
    [(list '> (? integer?) (? integer?)) (> v1 v2)]
    [(list '<= (? integer?) (? integer?)) (<= v1 v2)]
    [(list '>= (? integer?) (? integer?)) (>= v1 v2)]
    [(list 'eq? v1 v2)                    (eqv? v1 v2)]
    [(list 'cons v1 v2)                   (cons v1 v2)]
    [(list 'string-ref
           (? string?) (? integer?))      (if (<= 0 v2 (sub1 (string-length v1)))
                                              (string-ref v1 v2)
                                              'err)]   
    [(list 'make-string
           (? integer?) (? char?))        (if (< v1 0) 'err (make-string v1 v2))]
    [(list 'fl+ (? flonum?) (? flonum?))  (fl+ v1 v2)]
    [(list 'fl- (? flonum?) (? flonum?))  (fl- v1 v2)]
    [(list 'fl<= (? flonum?) (? flonum?))  (fl<= v1 v2)]
    [(list 'fl= (? flonum?) (? flonum?))  (fl= v1 v2)]
    [(list 'make-vector
           (? integer?) v2 )        (if (< v1 0) 'err (make-vector v1 v2))]
    [(list 'vector-ref
           (? vector?) (? integer?)) (if (<= 0 v2 (sub1 (vector-length v1)))
                                         (vector-ref v1 v2)
                                         'err)]           
                      
    [(list 'bytes-ref
           (? bytes?) (? integer?))       (if (<= 0 v2 (sub1 (bytes-length v1)))
                                              (bytes-ref v1 v2)
                                              'err)]
    [(list 'make-bytes
           (? integer?) (? byte?))        (if (< v1 0) 'err (Bytes (make-bytes v1 v2)))]
    [_                                    'err]))

;; Op3 Value Value Value -> Answer
(define (interp-prim3 p v1 v2 v3)
  (match (list p v1 v2 v3)
    [(list 'string-set! (? string?)
           (? integer?) (? char?))        (if (<= 0 v2 (sub1 (string-length v1)))
                                              (string-set! v1 v2 v3)
                                              'err)]
     [(list 'vector-set! (? vector?)
           (? integer?) v3)        (if (<= 0 v2 (sub1 (vector-length v1)))
                                              (vector-set! v1 v2 v3)
                                              'err)]  
    [(list 'bytes-set! (? bytes?)
           (? integer?) (? byte?))        (if (<= 0 v2 (sub1 (bytes-length v1)))
                                              (bytes-set! v1 v2 v3)
                                              'err)]
    [_                                    'err]))

;; Op3 Value Value Value -> Answer
(define (interp-prim4 p v1 v2 v3 v4)
  (match (list p v1 v2 v3 v4)
    [(list 'vector-cas! (? vector?)
           (? integer?) v3 v4)        (if (<= 0 v2 (sub1 (vector-length v1)))
                                              (vector-cas! v1 v2 v3 v4)
                                              'err)]
    [_                                    'err]))

;; Any -> Boolean
(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))
