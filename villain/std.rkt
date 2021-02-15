#lang racket
(provide std-provided?)

(define (std-provided? x)
  (memq x std-ids))

(define std-ids
  '(length)) ; TODO: add more
