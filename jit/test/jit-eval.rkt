#lang racket
(require "test-runner.rkt"
         "../parse.rkt"
         "../eval.rkt")
         ;; "../unload-bits-asm.rkt"

(current-directory "..")
(test-runner    (λ (e) (evaluate e)))


#;   (match (asm-interp/io (compile (parse e)) s)
                    ['err 'err]
                    [(cons r o) (cons (unload/free r) o)])

(current-directory "test")