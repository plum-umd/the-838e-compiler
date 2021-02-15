#lang racket
(provide main)
(require "parse.rkt" "compile.rkt" "read.rkt" "ast.rkt" a86/printer)

;; String -> Void
;; Compile contents of given file name,
;; emit asm code on stdout
(define (main fn)
  (let ((p (open-input-file fn)))
    (begin
      (read-line p) ; ignore #lang racket line
      (displayln (asm-string (match (parse (read p))
                               [(Prog ds e) (compile (Prog ds e))]
                               [(Lib xs ds) (compile-library (Lib xs ds))])))
      (close-input-port p))))
