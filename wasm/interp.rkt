#lang racket
(provide (all-defined-out))
(require "./printer.rkt" "../villain/wtypes.rkt" racket/runtime-path)
(define-runtime-path dir "..")

;; Asm -> String
;; Interpret (by assemblying, linking, and exec'ing) x86-64 code
;; Assume: starts with entry point run-time expects
(define (wasm-interp a)
   (wasm-interp/io a #f))

;; Wasm String -> String
;; Interpret (by assemblying, linking, and exec'ing) x86-64 code
;; Assume: starts with entry point run-time expects
(define (wasm-interp/io a input)
  (let* ((t.wat (make-temporary-file "wasm~a.wat"))
         (t.wasm (path-replace-extension t.wat #".wasm")))
    (with-output-to-file t.wat
      #:exists 'truncate
      (λ ()
        (displayln (wasm-string a))))
    (system (format "wat2wasm ~a -o ~a" t.wat t.wasm))
    (match 
        (process/ports #f
                       (open-input-string (if input input ""))
                       (current-error-port)
                       (if input
                           (string-append "node ../jsmain-io.js "
                                      (path->string t.wasm))
                           (string-append "node ../jsmain.js "
                                      (path->string t.wasm))))
      [(list in out pid err status)
       (begin
         (status 'wait)
         (delete-file t.wat)
         (delete-file t.wasm)
         (if input
             (let ((result-str (read-line in))
                   (output-str (read-line in))
                   (unloaded-result (read-line in)))
               (cons (result-str->value result-str unloaded-result)
                     output-str))
             (read in)))])))

(define (result-str->value str unloaded-result)
  (let ((v (string->number str 10)))
    (match v
      [(? imm-bits?) (bits->imm v)])))
 