;; Expr CEnv Boolean -> Asm
#lang racket
(provide (all-defined-out))
(require a86/ast)

;; Registers used
(define rsp 'rsp) ; stack
(define rbx 'rbx) ; heap
(define rdx 'rdx) ; return, 2
(define rdi 'rdi) ; arg
(define rsi 'rsi) ; arg2

(define word-size 8)

(define (compile-intmd-to-a86 e c)
  (apply seq (map (intmd-to-a86 c) e)))


(define (intmd-to-a86 c)
  (λ (i)
  (match i
    [(ICall x) (if (external-label? x)
                   (seq (pad-stack c)
                    (Call x)
                    (unpad-stack c))
                   (seq (Call x)))]
    [(Je 'err) (seq (Je (error-label c)))]
    [(Jl 'err) (seq (Jl (error-label c)))]
    [(Jg 'err) (seq (Jg (error-label c)))]
    [(Jmp 'err) (seq (Jmp (error-label c)))]
    [(Jne 'err) (seq (Jne (error-label c)))]
    [(Jle 'err) (seq (Jle (error-label c)))]
    [(Jg 'err) (seq (Jge (error-label c)))]
;    [(heap-store n x) (seq (Mov (Offset rbx (* n word-size)) rax))]
    [_      i])))

;; Symbol -> Boolean
(define (external-label? x)
  (memq x (list 'read_byte 'read_char 'peek_byte 'peek_char 'gensym 'add_or_sub1
           'bignum_length 'uc_is_property_alphabetic 'uc_is_property_white_space
           'uc_toupper 'uc_tolower 'uc_totitle 'write_byte 'write_char
           'str_to_symbol 'open_input_file 'close_input_port 'read_byte_port
           'integer_add 'integer_sub 'integer_quotient 'integer_remainder
           'integer_g 'integer_l 'integer_leq 'integer_geq)))

;; CEnv -> Asm
;; Pad the stack to be aligned for a call with stack arguments
(define (pad-stack-call c i)
  (match (odd? (+ (length c) i))
    [#f (seq (Sub rsp 8) (% "padding stack"))]
    [#t (seq)]))

;; CEnv -> Asm
;; Pad the stack to be aligned for a call
(define (pad-stack c)
  (pad-stack-call c 0))

;; CEnv -> Asm
;; Undo the stack alignment after a call
(define (unpad-stack-call c i)
  (match (odd? (+ (length c) i))
    [#f (seq (Add rsp 8) (% "unpadding"))]
    [#t (seq)]))

;; CEnv -> Asm
;; Undo the stack alignment after a call
(define (unpad-stack c)
  (unpad-stack-call c 0))

(define (error-label c)
  (if (even? (length c))
      'raise_error
      'raise_error_align))


