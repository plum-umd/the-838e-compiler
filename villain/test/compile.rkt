#lang racket
(require "test-runner.rkt"
         "../parse.rkt"
         "../compile.rkt"
         "../unload-bits-asm.rkt"
         a86/interp)

;; link with runtime for IO operations
(unless (file-exists? "../runtime.o")
  (system "make -C .. runtime.o"))

;(unless (file-exists? "../libraries-lmdefs.o")
;  (system "make -C .. libraries-lmdefs.o"))

(current-objs
 (list (path->string (normalize-path "../runtime.o"))))

(unless (file-exists? "../libraries-letrec")
  (system "make -C .. libraries-letrec"))

(let () (begin (system "cp ../libraries-letrec .")
               (system "cp ../lib-ls-ids .")
               (system "cp ../lib-fs .")
               (system "cp ../lib-externs .")
               (void)))

(test-runner    (λ (e) (unload/free (asm-interp (compile (parse e))))))
(test-runner-io (λ (e s)
                  (match (asm-interp/io (compile (parse e)) s)
                    ['err 'err]
                    [(cons r o) (cons (unload/free r) o)])))
