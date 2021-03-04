#lang racket
(provide stdlib-ids)
(require "parse-lib.rkt" "ast.rkt" "read.rkt")

(require racket/runtime-path)
(define-runtime-path lib "lib/")

(define (load-lib-ps f)
  (with-input-from-file (build-path lib f)
    (lambda ()
      (read-line) ; ignore #lang
      (match (parse-library (read-all))
        [(Lib ps ds) ps]))))

;; [Listof Id]
;; List of each Id provided by a stdlib
(define stdlib-ids
  (apply append
         (map load-lib-ps (filter (lambda (f) (path-has-extension? f ".rkt")) (directory-list lib)))))

