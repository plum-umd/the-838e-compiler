#lang racket
(provide test-runner) ;test-runner-io
(require rackunit)

(define (test-runner run)
  ;; Abscond examples
  (check-equal? (run 7) 7)
  (check-equal? (run -8) -8))