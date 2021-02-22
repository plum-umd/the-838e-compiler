#lang racket
(provide stdlib)
(require "parse.rkt" "ast.rkt" "read.rkt")

(require racket/runtime-path)
(define-runtime-path bool.rkt "lib/bool.rkt")
(define-runtime-path list.rkt "lib/list.rkt")
(define-runtime-path math.rkt "lib/math.rkt")

(define (load-lib-ds f)
  (with-input-from-file f
    (lambda ()
      (read-line) ; ignore #lang
      (match (parse-library (read-all))
        [(Lib ps ds) ds]))))

(define stdlib
  (append (load-lib-ds bool.rkt)
          (load-lib-ds list.rkt)
          (load-lib-ds math.rkt)))

