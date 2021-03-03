#lang racket
(provide stdlib)
(require "parse.rkt" "parse-lib.rkt" "ast.rkt" "read.rkt")

(require racket/runtime-path)
(define-runtime-path lib "lib/")

(define (load-lib-ds f)
  (with-input-from-file (build-path lib f)
    (lambda ()
      (read-line) ; ignore #lang
      (match (parse-library (read-all))
        [(Lib ps ds) ds]))))

(define stdlib
  (apply append
         (map load-lib-ds (filter (lambda (f) (path-has-extension? f ".rkt")) (directory-list lib)))))
