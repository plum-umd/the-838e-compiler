#lang racket
(provide repl)
(require a86 "parse.rkt" "compile.rkt" "unload-bits-asm.rkt")
(current-objs '("runtime.o"))
(define (repl)
  (displayln "Welcome to Villain v0.0.")
  (let loop ()
    (display "$> ")
    (let ((r (read)))
      (unless (eof-object? r)
        (println (unload/free (asm-interp (compile (parse r)))))
        (loop)))))

(module+ main
  (repl))

