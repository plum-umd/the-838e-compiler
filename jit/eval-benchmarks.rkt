#lang racket
(require "eval-file.rkt" "parse-program.rkt")

;;Read the original programs from the original directory
;;Output the result of evaluating the files into the simplified directory
;;Output a ready to be interpreted s-expression for each original program to the interp directory
;;Output the result of compiling the files into the compiled directory
(let* ((files (directory-list "benchmark/original"))
       (input-files (map (λ (f) (string-append "benchmark/original/" (path->string f))) files))
       (output-eval-files (map (λ (f) (string-append "benchmark/simplified/" (path->string f))) files))
       (output-interp-files (map (λ (f) (string-append "benchmark/interp/" (path->string f))) files)))
  (for ([f1 input-files] [f2 output-eval-files] [f3 output-interp-files])
    (eval-file f1 f2)
    (let* ((in (open-input-file f1))
          (out (open-output-file f3 #:mode 'binary #:exists 'replace))
          (prog (read in)))
      (displayln "#lang racket" out)
      (writeln
       `(begin
          (require jit/interp-heap)
          (current-input-port (open-input-string "1"))
          (interp ,(parse-program prog)))
       out)
      (close-input-port in)
      (close-output-port out))))
          

