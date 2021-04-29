#lang racket
(provide (all-defined-out))
(require "ast.rkt" "parse.rkt" "wtypes.rkt" "../wasm/printer.rkt")

;; Expr -> Wasm
(define (compile e)
  (match e
    [(Letrec _ _  e)
     (let ((fn `(func $sendResult (result i32)
                  (local $a i32)
                  (local $sp i32)        ;; start of stack pointer (top address)     
                  (local $hp i32)                  
                   i32.const 67108860
                   local.set $sp         ;; set the local var for the top address
                                         ;; for the start of stack (fixed for now)
                   i32.const 0
                   local.set $hp         ;; set the local var for start of heap                 
                  ,@(compile-e e '()))))
       `(module
          (import "writeBytejs" "writeByte" (func $writeByte (param i32)))
          (import "readBytejs" "readByte" (func $readByte (result i32)))
          (import "peekBytejs" "peekByte" (func $peekByte (result i32)))
          (import "errorjs" "error" (func $error))
          (memory 1024)  ;; 1024 pages of memory = 2^26 bytes 
          (export "memory" (memory 0))
            ,fn
          (export "sendResult" (func $sendResult))
          ))]))

;; Expr CEnv -> Wasm
(define (compile-e e c)
  (match e
    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(Eof)              (compile-value eof)]
    [(Prim0 p)          (compile-prim0 p)]
    [(Prim1 p e)        (compile-prim1 p e c)]
    [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)      (compile-begin e1 e2 c)]
    [(Let x e1 e2)      (compile-let x e1 e2 c)]
    [(Var x)            (compile-variable x c)]
     ))

;; Value -> Wasm
(define (compile-value v)
  `(i32.const ,(imm->bits v)))

;; Op0 -> Wasm
(define (compile-prim0 p)
  (match p
    ['read-byte
     `(call $readByte)]
    ['peek-byte
     `(call $peekByte)]))  

;; Op1 Expr CEnv -> Wasm
(define (compile-prim1 p e c)
  (append (compile-e e c)
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

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (append
   `(,@(compile-e e1 c)
     local.set $a
     local.get $sp
     i32.const ,(* 4 (length c))
     i32.sub
     local.get $a
     i32.store)       
   (match p
     ['+
      `(,@(compile-e e2 (cons #f c))
        local.set $a
        local.get $sp
        i32.const ,(* 4 (length c))
        i32.sub
        i32.load
        local.get $a       
        i32.add)]
     ['-
      `(,@(compile-e e2 (cons #f c))
        local.set $a
        local.get $sp
        i32.const ,(* 4 (length c))
        i32.sub
        i32.load
        local.get $a       
        i32.sub)])))


;; Expr Expr Expr CEnv -> Wasm
(define (compile-if e1 e2 e3 c)
  `(,@(compile-e e1 c)
    i32.const ,val-false
    i32.eq
    if
    ,@(compile-e e3 c)
    local.set $a
    else
    ,@(compile-e e2 c)
    local.set $a
    end
    local.get $a))

(define (compile-begin e1 e2 c)
  (append (compile-e e1 c)
          `(drop)
          (compile-e e2 c)))

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

;; (Listof Id) (Listof Expr) Expr CEnv -> Wasm
(define (compile-let xs es e c)
  (append (compile-es es c)
          (compile-e e (append (reverse xs) c))))
;;          (local.set $a
;;           i32.const ,(* 4 (length xs))
;;           local.get $sp
;;           i32.add
;;           local.set $sp      ;; consider changing the stack ptr
;;           local.get $a)))
                     

;; [Listof Expr] CEnv -> Wasm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (append (compile-e e c)
     `(local.set $a
       local.get $sp          
       i32.const ,(* 4 (length c))
       i32.sub
;;     i32.set $sp       ;; consider decrementing the stack ptr (akin to push)
;;     i32.get $sp
       local.get $a
       i32.store)
      (compile-es es (cons #f c)))]))

;; Id CEnv -> Integer
(define (lookup x c len-c)
  (match c
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t (* 4 (sub1 len-c))]         ;; consider changing the stack ptr
       [#f (- (lookup x rest len-c) 4)])]))

;; Id CEnv -> Wasm
(define (compile-variable x c)
  (let ((i (lookup x c (length c))))
    `(local.get $sp    ;; the local var for the top address for start of stack
      i32.const ,i
      i32.sub          ;; top address - (4 * ((length c) -  offset))
      i32.load)))

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

     
