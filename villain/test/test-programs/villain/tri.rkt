#lang racket
(begin
  (define (tri n)
    (if (zero? n)
        0
        (+ n (tri (sub1 n)))))

  (tri (read-byte)))
