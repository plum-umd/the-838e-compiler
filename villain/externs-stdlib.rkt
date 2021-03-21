#lang racket
(provide stdlib-ids stdlib-defs stdlib-fs-ls stdlib-defs-ids)
(require "parse.rkt" "ast.rkt" "read.rkt")

(require racket/runtime-path)
(define-runtime-path lib "lib/")

;(define (load-lib-ps f)
;  (with-input-from-file (build-path lib f)
;    (lambda ()
;      (read-line) ; ignore #lang
;      (match (parse-library (read-all))
;        [(Lib ps ds) ps]))))

(define (load-lib-ps-ds f)
  (with-input-from-file (build-path lib f)
    (lambda ()
      (read-line) ; ignore #lang
      (match (parse-library (read-all))
        [(Lib ps ds) (cons ps ds)]))))

;; [Listof Id]
;; List of each Id provided by a stdlib
(define stdlib-ids
  (apply append
         (map car
           (map load-lib-ps-ds (filter (lambda (f) (path-has-extension? f ".rkt")) (directory-list lib))))))

;(define stdlib-ids
 ; (apply append
  ;       (map load-lib-ps (filter (lambda (f) (path-has-extension? f ".rkt")) (directory-list lib)))))

(define stdlib-defs
  (apply append
         (map cdr
              (map load-lib-ps-ds (filter (lambda (f) (path-has-extension? f ".rkt")) (directory-list lib))))))

(define stdlib-fs-ls
  (map desugar-def-lib stdlib-defs))

(define ordered-provided-stdlib-ls
  (foldr (λ (x acc) (cons (cdr (findf (λ (y) (equal? (car y) x)) stdlib-fs-ls)) acc)) '() stdlib-ids))

(define stdlib-defs-ids
  (map car stdlib-fs-ls))

