#lang racket
(provide main)
(require "parse.rkt" "compile-wasm.rkt" "read.rkt" "ast.rkt" 
         "../wasm/printer.rkt")

;; String -> Void
;; Compile contents of given file name,
;; emit asm code on stdout
(define (main fn)
  (let ((p (open-input-file fn)))
    (begin
      (read-line p) ; ignore #lang racket line
      (displayln  (match (parse (read p))
                    [(Letrec fs ls e)
                     (wasm-string (compile (Letrec fs ls e)))]))
      (close-input-port p))))
