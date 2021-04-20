#lang racket
(provide wasm-string)

;; wasm -> String
(define (wasm-string a)
  (begin
    (define op1 (open-output-string))
    (write a op1)
    (get-output-string op1)))

