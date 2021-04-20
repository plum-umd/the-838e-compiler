#lang racket
(provide (all-defined-out))
(require "ast.rkt" "parse.rkt" "types.rkt" "../wasm/printer.rkt")

(define (compile e)
  (match e
    [(Letrec _ _  e) (compile-e e)]))

;; Expr CEnv Boolean -> Asm
(define (compile-e e)
  (match e
    [(Int i)            (compile-int i)]))

(define (compile-int v)
  `(module
       (func (result i32)
             (i32.const ,v)
             )
     (export "sendResult" (func 0))
     )
  )