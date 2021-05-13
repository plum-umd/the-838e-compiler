#lang racket
(require "eval-file.rkt")

(let* ((files (directory-list "benchmark/original"))
       (input-files (map (λ (f) (string-append "benchmark/original/" (path->string f))) files))
       (output-files (map (λ (f) (string-append "benchmark/simplified/" (path->string f))) files)))
  (for ([f1 input-files] [f2 output-files])
    (eval-file f1 f2)))
  

;;Output the result of evaluating the files into the simplified directory

;;Output the result of compiling the files into the compiled directory

