#lang racket
(begin
  (provide interp-prim1)
   
  ;; Op Integer -> Integer
  (define (interp-prim1 op v)
    (match op
      ['add1          (add1 v)]
      ['sub1          (sub1 v)]
      ['zero?         (zero? v)]
      ['char?         (char? v)]    
      ['integer->char (integer->char v)]
      ['char->integer (char->integer v)])))