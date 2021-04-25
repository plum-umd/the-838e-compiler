#lang racket
(require "test-runner.rkt" "../parse.rkt"
         "../compile-wasm.rkt" "../../wasm/interp.rkt")

(test-runner    (λ (e) (wasm-interp (compile (parse e)))))
(test-runner-io (λ (e s)
                  (match (wasm-interp/io (compile (parse e)) s)
                    ['err 'err]
                    [(cons r o) (cons r o)])))

