#lang racket
(require "eval.rkt")
(provide eval-file)

(define (eval-file input output)
  (let* ((f-in (open-input-file input))
        (f-content (read f-in))
        (f-out (open-output-file output #:mode 'binary #:exists 'replace))
        (res `(begin
                (require jit/interp-heap
                         jit/heap
                         jit/env
                         jit/unload
                         jit/interp-prims-heap
                         jit/program-ast)
                 (current-input-port (open-input-string "1"))
                ,(evaluate f-content))))
    (displayln "#lang racket" f-out)
    (writeln res f-out)
    (close-input-port f-in)
    (close-output-port f-out)))
    