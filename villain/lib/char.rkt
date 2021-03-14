#lang racket
(provide char-alphabetic?
         char-whitespace?
         char-upcase
         char-downcase
         char-titlecase)

(define (char-alphabetic? c)
  (ccall "char_alphabetic" c))

(define (char-whitespace? c)
  (ccall "char_whitespace" c))

(define (char-upcase c)
  (ccall "char_upcase" c))

(define (char-downcase c)
  (ccall "char_downcase" c))

(define (char-titlecase c)
  (ccall "char_titlecase" c))
