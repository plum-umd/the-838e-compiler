#lang racket
(provide compile-file)
(require "parse-program.rkt" "compile.rkt" "a86/printer.rkt")

;; Path Path -> Void
;; Compile contents of given file name,
;; emit asm code to out file
(define (compile-file in out)
  (let ((in (open-input-file in))
        (out (open-output-file out #:mode 'binary #:exists 'replace)))
    (begin
      (displayln (asm-string (compile (parse-program (read in)))) out)
      (close-input-port in)
      (close-output-port out))))