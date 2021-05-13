#lang racket
(require "test-runner-interp.rkt"
         "../parse-program.rkt"
         "../interp-heap.rkt"
         "../interp-io.rkt")

(test-runner    (λ (e) (interp (parse-program e))))
(test-runner-io (λ (e s) (interp/io (parse-program e) s)))
