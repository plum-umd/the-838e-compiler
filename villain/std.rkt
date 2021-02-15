#lang racket
(provide std-provided?)

;; Symbol -> Boolean
(define (std-provided? x)
  (memq x std-ids))

(define std-ids
  '(length)) ; TODO: add more
