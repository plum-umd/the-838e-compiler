#lang racket
(begin
  (provide interp-prim0 interp-prim1)

  ;; Op -> Integer
  (define (interp-prim0 op)
    (match op
      ['read-byte (read-byte)]
      ['peedk-byte (peek-byte)]
      ['void (void)]))
    
  ;; Op Integer -> Integer
  (define (interp-prim1 op v)
    (match op
      ['add1          (if (integer? v) (add1 v) 'err)]
      ['sub1          (if (integer? v) (sub1 v) 'err)]
      ['zero?         (if (integer? v) (zero? v) 'err)]
      ['char?         (char? v)]
      ['char->integer (if (char? v) (char->integer v) 'err)]
      ['integer->char (if (codepoint? v) (integer->char v) 'err)]
      ['eof-object?   (eof-object? v)]
      ['write-byte    (if (byte? v) (write-byte v) 'err)]))

  (define (codepoint? v)
    (and (integer? v)
         (or (<= 0 v 55295)
             (<= 57344 v 1114111)))))