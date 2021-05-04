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
                           (string-append "node ../jsmain.js "
                                      (path->string t.wasm) " io")
                           (string-append "node ../jsmain.js "
                                      (path->string t.wasm))))
      [(list in out pid err status)
       (begin
         (status 'wait)
         (delete-file t.wat)
         (delete-file t.wasm)
         (if input
             (let ((result-str (read-line in))
                   (output-str (read-line in)))
               `(,(result-str->value result-str) .
                     ,output-str))
             (let ((s (read in)))
               (match s
                 [`',x  x]
                 [_  s]))))])))

(define (result-str->value str)
  (let ((v (string->number str 10)))
    (match v
      [#f (string->result str)]
      [(? imm-bits?) (bits->imm v)])))

(define (string->result str)
  (let ((s (read (open-input-string str))))
    (match s
      [`',x      (eval x)]
      [`(cons ,x ,y)  (cons->result s)]
      [_              s])))

(define (cons->result s)
  (match s
    [`(cons ,x ,y)
     (if (equal? y '(void))
         (cons (cons->result x) (void))
         (if (equal? x '(void))
             (cons (void) (cons->result y))
             (cons (cons->result x) (cons->result y))))]
    [`',x      x]
    [_   s]))
 