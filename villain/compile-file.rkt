#lang racket
(provide main)
(require "parse.rkt" "compile.rkt" "ast.rkt" "modules.rkt"
         a86/printer (submod a86/printer private))

;; String -> Void
;; Compile contents of given file name,
;; emit asm code on stdout
(define (main fn)
  (let ((p (open-input-file fn)))
    (begin
      (read-line p) ; ignore #lang racket line
      (displayln  (match (parse (read p))
                    [(Prog ds e) (asm-string (compile (Prog ds e)))]
                    [(Mod pvs rqs ds e)
                     (let ((CModstruct (build-mgraph fn pvs rqs ds e)))
                       ;(let ((b (if (equal? e '#s(Prim0 void)) #f #t)))
                         (asm-string (compile-module CModstruct #t)))]
                    [(Lib xs ds)
                     (parameterize ((current-shared? #t))
                       (asm-string (compile-library (Lib xs ds))))]))
      (close-input-port p))))
