#lang racket
(provide (all-defined-out))
(require "./printer.rkt" racket/runtime-path)
(define-runtime-path dir "..")

;; Asm -> String
;; Interpret (by assemblying, linking, and exec'ing) x86-64 code
;; Assume: starts with entry point run-time expects
(define (wasm-interp a)
   (wasm-interp/io a ""))

;; Wasm String -> String
;; Interpret (by assemblying, linking, and exec'ing) x86-64 code
;; Assume: starts with entry point run-time expects
(define (wasm-interp/io a str)
  (let* ((t.wat (make-temporary-file "wasm~a.wat"))
         (t.wasm (path-replace-extension t.wat #".wasm")))
    (with-output-to-file t.wat
      #:exists 'truncate
      (λ ()
        (displayln (wasm-string a))))
    (system (format "wat2wasm ~a -o ~a" t.wat t.wasm))
    (match 
        (process/ports #f
                       (open-input-string str)
                       (current-error-port)
                       (string-append "node ../jsmain.js "
                                      (path->string t.wasm)))
      [(list in out pid err status)
       (begin
         (status 'wait)
         (delete-file t.wat)
         (read in))])))
 