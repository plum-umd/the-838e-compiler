#lang racket
(require "test-runner_2.rkt" "../parse.rkt"
         "../compile-wasm_2.rkt" "../../wasm/interp_2.rkt")

(test-runner    (λ (e) (wasm-interp (compile (parse e)))))


