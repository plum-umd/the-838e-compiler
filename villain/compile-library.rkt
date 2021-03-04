#lang racket
(provide main)
(require "parse-lib.rkt" "compile.rkt" "read.rkt" "ast.rkt"
         a86/printer (submod a86/printer private))

;; Emit relocatable assembly code
(current-shared? #t)

;; String -> Void
;; Compile contents of library in given file, emit asm code on stdout
(define (main fn)
  (let ((p (open-input-file fn)))
    (begin
      (read-line p) ; ignore #lang racket line
      (displayln (asm-string (compile-library (parse-library (read-all p)))))
      (close-input-port p))))
