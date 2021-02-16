#lang racket
(provide stdlib)
(require "parse.rkt" "ast.rkt")

(require racket/runtime-path)
(define-runtime-path list.rkt "list.rkt")
(define-runtime-path math.rkt "math.rkt")

(define (load-lib-ds f)   
  (with-input-from-file f
    (lambda ()
      (read-line) ; ignore #lang
      (match (parse (read))
        [(Lib ps ds) ds]))))

(define stdlib
  (append (load-lib-ds list.rkt)
          (load-lib-ds math.rkt)))

