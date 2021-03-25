#lang racket
;(provide main)
(require "parse.rkt" "compile.rkt" "read.rkt" "ast.rkt"
         racket/serialize
         a86/printer a86/ast (submod a86/printer private))


;; Emit relocatable assembly code
;(current-shared? #t)

(let ((fs-ls (libraries-fs-ls)))
  (let ((fs (car fs-ls)) (ls (cdr fs-ls)))
    (begin
      (let ((out (open-output-file "libraries-letrec" #:exists 'truncate)))
        (begin
          (write (serialize (compile-library-letrec fs ls)) out)
          (close-output-port out)))
      ;; Emit relocatable assembly code
      (current-shared? #t)
      (with-output-to-file "libraries-lmdefs.s"
        #:exists 'truncate
        (λ ()
          (displayln (asm-string (compile-library-λdefs ls)))))
      (system "cp ./libraries-letrec ./test")
      (system "cp ./lib-ls-ids ./test")
      (system "cp ./lib-fs ./test")
      (system "cp ./lib-externs ./test")
      (void))))


