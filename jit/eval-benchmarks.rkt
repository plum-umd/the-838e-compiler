#lang racket
(require "eval-file.rkt" "compile-file.rkt" "parse-program.rkt")

;;Read the original programs from the original directory
;;Output the result of evaluating the files into the simplified directory
;;Output a ready to be interpreted s-expression for each original program to the interp directory
;;Output the result of compiling the files into the compiled directory
(let* ((files (directory-list "benchmark/original"))
       (input-files (map (λ (f) (string-append "benchmark/original/" (path->string f))) files))
       (output-eval-files (map (λ (f) (string-append "benchmark/simplified/" (path->string f))) files))
       (output-interp-files (map (λ (f) (string-append "benchmark/interp/" (path->string f))) files))
       (output-compiled-files (map (λ (f) (string-append "benchmark/compile/" (path->string f) ".s")) files)))
  (for ([f1 input-files] [f2 output-eval-files] [f3 output-interp-files] [f4 output-compiled-files])
    ;;original -> simplified
    (eval-file f1 f2) 
    ;;original -> compile
    (compile-file f1 f4)
     
    (let* ((in (open-input-file f1))
          (out-interp (open-output-file f3 #:mode 'binary #:exists 'replace))
          (prog (read in)))
      ;;original -> interp
      (displayln "#lang racket" out-interp)
      (writeln
       `(begin
          (require jit/interp-heap)
          (current-input-port (open-input-string "1"))
          (interp ,(parse-program prog)))
       out-interp)
      (close-input-port in)
      (close-output-port out-interp))))
          

