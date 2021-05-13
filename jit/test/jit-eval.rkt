#lang racket
(require "test-runner-pe.rkt"
         "../eval.rkt")

(current-directory "..")
(test-runner    (λ (e) (evaluate e)))

(current-directory "test")