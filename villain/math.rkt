#lang racket
(begin
  (provide * ^)

  ; multiplication
  (define (* x y)
    (if (zero? y)
        0
        (+ x (* x (sub1 y)))))

  ; exponentiation
  (define (^ x y)
    (if (zero? y)
        1
        (* x (^ x (sub1 y)))))

  )