#lang racket
(require "test-runner.rkt"
         "../parse.rkt"
         "../eval.rkt")
         ;; "../unload-bits-asm.rkt"

(current-directory "..")
(test-runner    (λ (e) (evaluate e)))

(current-directory "test")