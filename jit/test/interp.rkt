#lang racket
(require "test-runner-interp.rkt"
         "../parse.rkt"
         "../interp-heap.rkt"
         "../interp-io.rkt")

(test-runner    (λ (e) (interp (parse e))))
(test-runner-io (λ (e s) (interp/io (parse e) s)))
