#lang racket
(provide main read-sequence)
(require "parse.rkt" "compile.rkt" "read.rkt" a86/printer)

;; Read a list of forms in a file
(define (read-sequence p)
  (let ((x (read p)))
    (if (eof-object? x)
        '()
        (cons x (read-sequence p)))))
  
;; String -> Void
;; Compile contents of given file name,
;; emit asm code on stdout
(define (main fn)
  (let ((p (open-input-file fn)))
    (begin
      (read-line p) ; ignore #lang racket line
      (displayln (asm-string (compile (parse (read-sequence p)))))
      (close-input-port p))))
