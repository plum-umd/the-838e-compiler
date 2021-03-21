#lang racket
(provide main)
(require "parse.rkt" "compile.rkt" "read.rkt" "ast.rkt" "modules.rkt"
         a86/printer (submod a86/printer private))

;; String -> Void
;; Compile contents of given file name,
;; emit asm code on stdout
(define (main fn)
  (let ((p (open-input-file fn)))
    (begin
      (read-line p) ; ignore #lang racket line
      (displayln  (match (parse (read p))
                    [(Letrec fs ls e)
                     (asm-string (compile (Letrec fs ls e)))]
                    [(Mod pvs rqs fls e)
                     (let ((CModstruct (build-mgraph fn pvs rqs fls e)))
                       (asm-string (compile-module CModstruct #t)))]
                    [(Lib xs ds)
                     (parameterize ((current-shared? #t))
                       (asm-string (compile-library (Lib xs ds))))]))
      (close-input-port p))))
