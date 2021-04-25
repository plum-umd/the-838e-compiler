#lang racket
(provide (all-defined-out))
(require "ast.rkt" "parse.rkt" "wtypes.rkt" "../wasm/printer.rkt")

(define (compile e)
  (match e
    [(Letrec _ _  e)
     (let ((fn `(func $sendResult (result i32) (local $a i32) ,@(compile-e e))))
       `(module
          (import "writeBytejs" "writeByte" (func $writeByte (param i32)))
          (import "readBytejs" "readByte" (func $readByte (result i32)))
          (import "peekBytejs" "peekByte" (func $peekByte (result i32)))
          (import "errorjs" "error" (func $error))
            ,fn
          (export "sendResult" (func $sendResult))
          ))]))

;; Expr CEnv Boolean -> Asm
(define (compile-e e)
  (match e
    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(Eof)              (compile-value eof)]
    [(Prim0 p)          (compile-prim0 p)]
    [(Prim1 p e)        (compile-prim1 p e)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3)]
    [(Begin e1 e2)      (compile-begin e1 e2)]
     ))

(define (compile-value v)
  `(i32.const ,(imm->bits v)))

(define (compile-prim0 p)
  (match p
    ['read-byte
     `(call $readByte)]
    ['peek-byte
     `(call $peekByte)]))  

(define (compile-prim1 p e)
  (append (compile-e e)
  (match p
    ['add1
     (append (assert-integer)
     `(i32.const ,(imm->bits 1)
       i32.add))]
    ['sub1
     (append (assert-integer)
     `(i32.const ,(imm->bits 1)
       i32.sub))]
    ['zero?
     (append (assert-integer)
     `(if 
       i32.const ,val-false
       local.set $a
       else    
       i32.const ,val-true
       local.set $a
       end
       local.get $a))]
    ['integer?
     (append (assert-integer)
     `(i32.const ,mask-int
       i32.and
       i32.const ,type-int
       i32.xor
       if
       i32.const ,val-false
       local.set $a
       else    
       i32.const ,val-true
       local.set $a
       end
       local.get $a))]
    ['integer-length
     (append (assert-integer)
     `(local.set $a
       local.get $a
       i32.const 31
       i32.shr_s
       local.get $a
       i32.xor
       i32.clz
       local.set $a
       i32.const 32
       local.get $a
       i32.sub))]
    ['char?
     `(i32.const ,mask-char
       i32.and
       i32.const ,type-char
       i32.xor
       if
       i32.const ,val-false
       local.set $a
       else    
       i32.const ,val-true
       local.set $a
       end
       local.get $a)]
    ['char->integer
     (append (assert-char)
     `(i32.const ,char-shift
       i32.shr_s
       i32.const ,int-shift
       i32.shl))]
    ['integer->char
     (append assert-codepoint
     `(i32.const ,int-shift
       i32.shr_s
       i32.const ,char-shift
       i32.shl
       i32.const ,type-char
       i32.xor))]
    ['eof-object?
     (eq-imm val-eof)]
    ['write-byte
     (append assert-byte
     `(call $writeByte
       i32.const ,val-void))]
)))

;; Expr Expr Expr CEnv Boolean -> Asm
(define (compile-if e1 e2 e3)
  `(,@(compile-e e1)
    i32.const ,val-false
    i32.eq
    if
    ,@(compile-e e3)
    local.set $a
    else
    ,@(compile-e e2)
    local.set $a
    end
    local.get $a))

(define (compile-begin e1 e2)
  (append (compile-e e1)
          `(drop)
          (compile-e e2)))

(define (eq-imm imm)
  `(i32.const ,imm
    i32.eq
    if
    i32.const ,val-true
    local.set $a
    else    
    i32.const ,val-false
    local.set $a
    end
    local.get $a))

(define (assert-type mask type)
  (λ ()
  `(local.set $a
    local.get $a          
    i32.const ,mask
    i32.and
    i32.const ,type
    i32.ne
    if
    call $error
    end
    local.get $a)))

(define assert-integer
  (assert-type mask-int type-int))
(define assert-char
  (assert-type mask-char type-char))
(define assert-box
  (assert-type ptr-mask type-box))
(define assert-cons
  (assert-type ptr-mask type-cons))

(define assert-byte
  (append (assert-integer)
   `(local.set $a
     local.get $a
     i32.const ,(imm->bits 0)
     i32.lt_s
     if
     call $error
     end
     local.get $a
     i32.const ,(imm->bits 255)
     i32.gt_s
     if
     call $error
     end
     local.get $a)))

(define assert-codepoint
  (append (assert-integer)
   `(local.set $a
     local.get $a
     i32.const ,(imm->bits 0)
     i32.lt_s
     if
     call $error
     end
     local.get $a
     i32.const ,(imm->bits 1114111)
     i32.gt_s
     if
     call $error
     end
     local.get $a
     i32.const ,(imm->bits 55295)
     i32.ge_s
     if
     local.get $a
     i32.const ,(imm->bits 57344)
     i32.le_s
     if
     call $error
     end
     end
     local.get $a)))

     
