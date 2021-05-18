#lang racket
(provide main)
(require "3_parse.rkt" "3_compile.rkt" a86/printer)

;; String -> Void
;; Compile contents of given file name,
;; emit asm code on stdout
(define (main fn)
  (let ((p (open-input-file fn)))
    (begin
      (read-line p) ; ignore #lang racket line
      (displayln (asm-string (compile (parse (read p)))))
      (close-input-port p))))
