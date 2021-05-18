#lang racket
(provide (all-defined-out))
(require "ast.rkt" "parse.rkt" "wtypes.rkt" "../wasm/printer.rkt")

(define word-size 4)  ;; in bytes

;; This is a version of compile.wasm that uses WebAssembly code and Wasmtime as
;; the runtime rather than Node.js. This is a partial implementation of this runtime
;; for the purpose of delineating the method to write the runtime in WebAssembly.

;; Note about addressing and type tags:

;; Memory addresses 0 to 2^24 - 1 are reserved for writing the data that
;; fd_write uses to print strings

;; Our current word-size for compilation to WebAssembly is 4 bytes (32 bits),
;; and current heap addresses can potentially go from 2^24 to 2^26 - 4. But in order to
;; avoid overwriting the stack which grows downward from the address 2^26 - 4,
;; we should not allow the heap addresses to go beyond 2^25 and also should not
;; allow the stack address to go below 2^25.

;; So, bits 1 and 2 of heap addresses from the right (LSB) are 0, and the
;; component of addresses in bits 3 to 25 can go from 2^22 to 2^23. So, bits 1, 2,
;; and 26 to 31 of addresses can be used as tag bits. We currently use bits 1,
;; 2, 29, 30, and 31 as tag bits (please see wtypes.rkt). Bit 32 is the sign bit.

;; We have a stack in memory that grows downward from the highest address of our
;; memory.  This is different from the implicit operand stack that the stack
;; machine of WebAssembly uses.  The stack that we refer to should be clear from
;; the context. Explicitly, we use stack in memory (or mem.) or user stack to refer
;; to the stack we have in memory, and the operand (or op.) stack to refer to the
;; operand stack of WebAssembly.

;; $b is used as scratch register in compile-string, string-ref, and make string.
;; As such, it should not be used in compile-string-chars and assert-type.

;; Expr -> Wasm
(define (compile e)
  (match e
    [(Letrec fs ls e)
     (let ((prog-fn
               `(func $program (result i32)
                  (local $a i32)         ;; local var (used as virtual register)
                  (local $b i32)         ;; virtual register
                  (local $c i32)         ;; virtual register
                  ,@(compile-e e '())))
           (main-fn
               `(func $main (export "_start") ;; Based on demo.wat in WASI tutorial
                  (local $a i32)
                  i32.const 0            ;; iov.iov_base - This is a pointer to
                                         ;; the start addr of the data
                  i32.const 8
                  i32.store
                  call $program
                  call $process_result   ;; will process the result from the program
                                         ;; and store the output starting at
                                         ;; address 8 in memory 0.  $fd_write
                                         ;; will retrieve the data from this address
                  local.set $a
                  i32.const 4            ;; iov.iov_len - The length of the 
                  local.get $a           ;; the output string = returned nw (number
                  i32.store              ;; of bytes written to memory in data section) 

                  i32.const 1        ;; file_descriptor - 1 for stdout
                  i32.const 0        ;; *iovs - The pointer to the iov array,
                                     ;; which is stored at memory location 0
                  i32.const 1        ;; iovs_len - We're printing 1 string stored
                                     ;; in an iov - so one.
                  i32.const 8
                  local.get $a       ;; 
                  i32.const 4
                  i32.div_s
                  i32.const 4
                  i32.mul
                  i32.const 4
                  i32.add
                  i32.add            ;; this address will hold nwritten - A place
                                     ;; in memory to store the number of bytes
                                     ;; written (address 8 + (4*(floor(nw/4)) + 4))
                                     ;; (should be a multiple of 4)
                  call $fd_write
                  drop
                  i32.const 10          ;; line feed (\n)
                  call $print_codepoint ;; this function was added to Wasmtime's
                                        ;; Rust source code (alternatively can only
                                        ;; use $fd_write which is faster for printing
                                        ;; very long strings (such as 1000000 λs) but
                                        ;; can overflow the data section of memory and
                                        ;; overwrite the heap section of memory [which
                                        ;; we have set to start from memory address
                                        ;; 2^24] for even longer strings [for example
                                        ;; about > 2 ^23 = 8388608 λs])
                  drop
                  ))
           (process-result-fn
               `(func $process_result (param $result i32) (result i32)
                     (local $nw i32)  ;; num of bytes written to memory
                                      ;; starting from memory address of 8
                                      ;; for data for fd_write
                      i32.const 0
                      local.set $nw
                      local.get $result  ;; the parameter 0 of the function
                      i32.const ,ptr-bottom-mask
                      i32.and
                      i32.eqz
                      if                ;; if the result is of an immediate type
                      local.get $result
                      call $process_imm
                      return
                      end
                      local.get $result
                      i32.const ,ptr-mask
                      i32.and
                      i32.const ,type-string
                      i32.xor
                      i32.eqz
                      if                ;; if the result is of type string
                      local.get $result
                      call $process_string
                      ;call $process_string_with_print_codepoint
                      return
                      end
                      local.get $nw))
           (process-imm-fn
               `(func $process_imm (param $result i32) (result i32)
                      local.get $result
                      i32.const ,mask-int
                      i32.and
                      i32.eqz
                      if                    ;; if the result is of type int                     
                      local.get $result                      
                      i32.const ,int-shift
                      i32.shr_s
                      call $print_int
                      i32.const 0           ;; no bytes written to memory for
                                            ;; fd_write, so 0 is returned
                      return
                      end
                      
                      local.get $result
                      i32.const ,mask-char
                      i32.and
                      i32.const ,type-char
                      i32.eq
                      if                     ;; if the result is of type char
                      i32.const 35           ;; the UTF-8 encoding for #
                      call $print_codepoint  ;; can alternatively call $write_utf_to_memory
                                             ;; with param1 = 35 and param2 = 8 
                                             ;; (the address where to write the data)
                      drop
                      i32.const 92           ;; the UTF-8 encoding for \
                      call $print_codepoint  ;; can alternatively call $write_utf_to_memory
                                             ;; with param1 = 92 and param2 = 9
                      drop
                      local.get $result
                      i32.const ,char-shift
                      i32.shr_s
                      call $print_codepoint  ;; can alternatively call $write_utf_to_memory
                                             ;; with param1 = char and param2 = 10
                      drop

                      i32.const 0            ;; no bytes written to memory for
                                             ;; fd_write, so 0 is returned
                      return
                      end

                      local.get $result
                      i32.const ,val-true
                      i32.eq
                      if                    ;; if the resualt is val-true                      
                      i32.const 8           ;; the start address for storing the data
                                            ;; that fd_write uses to print to stdout))
                      i32.const 35          ;; the UTF-8 encoding for #
                      i32.store             
                      i32.const 9           ;; address to the next byte in memory
                      i32.const 116         ;; the UTF-8 encoding for the charachter t
                      i32.store
                      i32.const 2           ;; two bytes written to memory for
                                            ;; fd_write, so 2 is returned
                      return
                      end

                      local.get $result
                      i32.const ,val-false
                      i32.eq
                      if                    ;; if the resualt is val-false                      
                      i32.const 8           
                      i32.const 35          ;; the UTF-8 encoding for #
                      i32.store             
                      i32.const 9       
                      i32.const 102         ;; the UTF-8 encoding for the charachter f
                      i32.store
                      i32.const 2           ;; two bytes written to memory for
                                            ;; fd_write, so 2 is returned
                      return
                      end
                      i32.const 0))
                      
           (process-string-fn
               `(func $process_string (param $str-ptr i32) (result i32)
                     (local $nw i32)  ;; num of bytes written to memory
                                      ;; starting from memory address of 8
                                      ;; for data for fd_write
                      (local $a i32) 
                      (local $cntr i32)
                      (local $c i32)
                      (local $diff i32)
                      i32.const 8     ;; start addr of where to write data
                      i32.const 34    ;; UTF-8 encoding of "
                      i32.store            
                      i32.const 1     
                      local.set $nw   ;; 1 byte (") is already written, so $nw = 1

                      i32.const 9     ;; 1 byte already written to address 8
                      local.set $c     ;; $c is ptr to where to write data in memory 0.
                                       ;; this data will be used by fd_write to write
                                       ;; the string to stdout
                   
                      local.get $str-ptr    ;; parameter 0 (tagged str ptr) 
                      i32.const ,type-string  
                      i32.xor          ;; get the addr from tagged str ptr
                      local.set $a     ;; store the addr of str in $a
                      local.get $a
                      i32.load         ;; load length of string to the op. stack
                      local.set $cntr  ;; length of string as counter in $b                     
                      block
                      loop
                      local.get $a     ;; advance the address by the word-size
                      i32.const ,word-size
                      i32.add
                      local.set $a
                      local.get $cntr ;; the counter starts from the length and counts
                                      ;; down by (imm->bits 1). At the same time, in
                                      ;; each iteration, the address is increased by
                                      ;; word-size (≡ i++, where i is the index of the
                                      ;; word of str on the heap)
                      i32.eqz          ;; check if counter = 0
                      br_if 1          ;; if so, break to the outer block (with index 1)
                      local.get $a
                      i32.load         ;; load the char in str_addr[i] to op. stack
                      i32.const ,char-shift
                      i32.shr_s
;                      call $print_codepoint  ;; print the char. Alternative to using Wasmtime's
                                              ;; fd_write function. Implemented below in
                                              ;; $process_string_with_print_codepoint

                      local.get $c     ;; second arg
                      call $write_utf_to_memory  ;; WASI function fd_write in main will
                                                 ;; use this data to print the output.
                                       ;; the updated data ptr is returned on op. stack
                      local.get $c
                      i32.sub          ;; to get the num bytes written from the difference
                                       ;; between updated data ptr and original ptr
                      local.set $diff  
                      local.get $nw
                      local.get $diff
                      i32.add
                      local.set $nw    ;; $nw += $diff (# bytes written for this char)
                      local.get $c
                      local.get $diff
                      i32.add
                      local.set $c     ;; $c += $diff (updated data ptr, which is the
                                       ;; address of where to write next UTF-8 char)
                      local.get $cntr
                      i32.const ,(imm->bits 1)  ;; decrement the counter
                      i32.sub
                      local.set $cntr
                      br 0             ;; continue with the loop (with index 0)
                      end                     ;; end of the loop (with index 0)
                      end                     ;; end of the block (with index 1)

                      local.get $c     ;; updated addr of where to write data
                      i32.const 34     ;; UTF-8 encoding of "
                      i32.store                        
                      local.get $nw
                      i32.const 1
                      i32.add          ;; return $nw++
                      ))
           (process-string-with-print_codepoint-fn
                ;; Alternative shorter but slower way of printing a string
                ;; using the print_codepoint function added to Wasmtime.
                ;; Its advantage is that it will not overflow the data section memory.
               `(func $process_string_with_print_codepoint (param $str-ptr i32) (result i32)
                      (local $a i32) 
                      (local $cntr i32)
                      i32.const 34    ;; UTF-8 encoding of "
                      call $print_codepoint
                      drop                      
                      local.get $str-ptr    ;; parameter 0 (tagged str ptr) 
                      i32.const ,type-string  
                      i32.xor          ;; get the addr from tagged str ptr
                      local.set $a     ;; store the addr of str in $a
                      local.get $a
                      i32.load         ;; load length of string to the op. stack
                      local.set $cntr  ;; length of string as counter in $b                     
                      block
                      loop
                      local.get $a     ;; advance the address by the word-size
                      i32.const ,word-size
                      i32.add
                      local.set $a
                      local.get $cntr ;; the counter starts from the length and counts
                                      ;; down by (imm->bits 1). At the same time, in
                                      ;; each iteration, the address is increased by
                                      ;; word-size (≡ i++, where i is the index of the
                                      ;; word of str on the heap)
                      i32.eqz          ;; check if counter = 0
                      br_if 1          ;; if so, break to the outer block (with index 1)
                      local.get $a
                      i32.load         ;; load the char in str_addr[i] to op. stack
                      i32.const ,char-shift
                      i32.shr_s
                      call $print_codepoint  ;; print the char. Alternative to using
                                              ;; WASI fd_write function.
                      drop
                      local.get $cntr
                      i32.const ,(imm->bits 1)  ;; decrement the counter
                      i32.sub
                      local.set $cntr
                      br 0             ;; continue with the loop (with index 0)
                      end                     ;; end of the loop (with index 0)
                      end                     ;; end of the block (with index 1)
                      
                      i32.const 34     ;; UTF-8 encoding of "
                      call $print_codepoint
                      drop
                      i32.const 0      ;; return 0, as 0 bytes written to memory
                      ))
           
           (write_utf_to_memory
               `(func $write_utf_to_memory (param $char i32) (param $addr i32)
                      (result i32)
                      ;; Using UTF-8 encoding of codepoint (refs. class code
                      ;; and https://en.wikipedia.org/wiki/UTF-8)
                      local.get $char
                      i32.const 128   ;; case codepoint < 128
                      i32.lt_s
                      if
                      local.get $addr
                      local.get $char
                      i32.store       ;; store Byte 1
                      local.get $addr ;; advance address of where to write data
                      i32.const 1
                      i32.add
                      return          ;; return updated address on op. stack
                      end
                      
                      local.get $char
                      i32.const 2048  ;; case codepoint < 2048
                      i32.lt_s
                      if
                      local.get $addr
                      local.get $char  ;; form first byte from 5 MS bits of char
                      i32.const 6
                      i32.shr_s
                      i32.const 192
                      i32.or
                      i32.store      ;; store 1st byte in smaller address                      

                      local.get $addr  
                      i32.const 1
                      i32.add
                      local.get $char  ;; form second byte from 6 LS bits of char
                      i32.const 63
                      i32.and
                      i32.const 128
                      i32.or         
                      i32.store      ;; store 2nd byte in higher address

                      local.get $addr  ;; advance data address by 2
                      i32.const 2
                      i32.add
                      return         ;; return updated address on op. stack
                      end
                      
                      local.get $char
                      i32.const 65536  ;; case codepoint < 65536
                      i32.lt_s
                      if
                      local.get $addr
                      local.get $char
                      i32.const 12     ;; form first byte from 4 MS bits of char
                      i32.shr_s
                      i32.const 224
                      i32.or
                      i32.store      ;; store 1st byte in smaller address
                      
                      local.get $addr
                      i32.const 1
                      i32.add                      
                      local.get $char  ;; form second byte from bits 7 to 12 of char
                      i32.const 6
                      i32.shr_s
                      i32.const 63
                      i32.and
                      i32.const 128
                      i32.or
                      i32.store      ;; store 2nd byte in the next address
                      
                      local.get $addr
                      i32.const 2
                      i32.add
                      local.get $char  ;; form third byte from 6 LS bits of char
                      i32.const 63
                      i32.and
                      i32.const 128
                      i32.or
                      i32.store      ;; store 3rd byte in higher address
                      
                      local.get $addr  ;; advance data address by 3
                      i32.const 3
                      i32.add
                      return         ;; return updated address on op. stack
                      end
                      
                      local.get $addr  ;; fall-through case (65536 to 1114111)
                      local.get $char
                      i32.const 18
                      i32.shr_s
                      i32.const 240
                      i32.or
                      i32.store      ;; store first byte in smaller address
                      
                      local.get $addr
                      i32.const 1
                      i32.add
                      local.get $char  ;; form 2nd byte
                      i32.const 12
                      i32.shr_s
                      i32.const 63
                      i32.and
                      i32.const 128
                      i32.or
                      i32.store      ;; store 2nd byte in the next address 
                      
                      local.get $addr
                      i32.const 2
                      i32.add
                      local.get $char  ;; form 3rd byte
                      i32.const 6
                      i32.shr_s
                      i32.const 63
                      i32.and
                      i32.const 128
                      i32.or
                      i32.store      ;; store 3rd byte in the next address
                      
                      local.get $addr
                      i32.const 3
                      i32.add
                      local.get $char  ;; form 4th byte
                      i32.const 63
                      i32.and
                      i32.const 128
                      i32.or
                      i32.store      ;; store 4th byte in higher address
                      
                      local.get $addr  ;; advance data address by 4
                      i32.const 4
                      i32.add))
           )
       `(module
          (import "wasi_unstable" "fd_write"
                  (func $fd_write (param i32 i32 i32 i32) (result i32)))  
;          (import "writeBytejs" "writeByte" (func $writeByte (param i32)))
;          (import "readBytejs" "readByte" (func $readByte (result i32)))
;          (import "peekBytejs" "peekByte" (func $peekByte (result i32)))
;          (import "errorjs" "error" (func $error))
          (import "wasi_snapshot_preview1" "print_codepoint" 
                  (func $print_codepoint (param i32) (result i32)))
          (import "wasi_snapshot_preview1" "print_int" 
                  (func $print_int (param i32) (result i32)))
          (import "wasi_snapshot_preview1" "error_exit" 
                  (func $error_exit (result i32)))
          (memory 1024)  ;; 1024 pages of memory = 2^26 bytes 
          (export "memory" (memory 0))
          (global $sp (mut i32) (i32.const 67108860))  ;; global variable $sp is
                ;; the stack ptr initially pointing to the top address in memory 0.
          (global $hp (mut i32) (i32.const 16777216))  ;; global variable $hp is
                           ;; the heap pointer with start addr of 2^24 = 16777216 in
                           ;; memory 0. (this will allow strings of up to length
                           ;; (2^25 - 16777216) / 4 = 2^22 = 4194304 to be stored on
                           ;; the heap, if the upper bound of heap is set to be
                           ;; 2^25, and if a larger heap and a smaller stack is
                           ;; allocated, strings of up to length of about 3 . 2^22 =
                           ;; 12582912 (given that in this WebAssembly implementation
                           ;; we are not using a packed string representation and
                           ;; each character in the string takes one word (4 bytes
                           ;; in this implementation)).
                           ;; But for a string of about > 2^23 λs (with each λ taking
                           ;; 2 bytes in UTF-8 representation), WASI fd_write cannot
                           ;; be used for printing these strings, because we can write
                           ;; the data for fd_write in memory addresses 8 to 2^24,
                           ;; and the data for such strings will overflow this
                           ;; data section into the memory allocated for the heap.
            ,main-fn
            ,prog-fn
            ,process-result-fn
            ,process-imm-fn
            ,process-string-fn
            ,process-string-with-print_codepoint-fn
            ,write_utf_to_memory
            ,@(compile-defines fs ls)
        ;;  (export "sendResult" (func $sendResult))          
          ))]))

;; Expr CEnv -> Wasm
(define (compile-e e c)
  (match e
    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(Eof)              (compile-value eof)]
    [(Empty)            (compile-value '())]
    [(String s)         (compile-string s)]
    [(Prim0 p)          (compile-prim0 p)]
    [(Prim1 p e)        (compile-prim1 p e c)]
    [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)      (compile-begin e1 e2 c)]
    [(Let x e1 e2)      (compile-let x e1 e2 c)]
    [(LCall e es)       (compile-call (symbol->wa-label (Var-x e)) es c)]
    [(Var x)            (compile-variable x c)]
     ))

;; Value -> Wasm
(define (compile-value v)
  `(i32.const ,(imm->bits v)))

;; Op0 -> Wasm
(define (compile-prim0 p)
  (match p
    ['void  `(i32.const ,val-void)]
    ['read-byte
     `(;call $readByte
       )]
    ['peek-byte
     `(;call $peekByte
       )]))  

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
       local.get $a)]
    ['integer-length
     (append (assert-integer)
     `(i32.const ,int-shift
       i32.shr_s          
       local.set $a
       local.get $a
       i32.const 31
       i32.shr_s
       local.get $a
       i32.xor
       i32.clz
       local.set $a
       i32.const 32
       local.get $a
       i32.sub
       i32.const ,int-shift
       i32.shl))]
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
     `(;; call $writeByte
       i32.const ,val-void))]
    ['box
     `(local.set $a          ;; Store result of (compile-e e c) on heap.
       global.get $hp
       local.get $a
       i32.store              
       global.get $hp         ;; tag the heap ptr
       i32.const ,type-box    
       i32.or                 
       local.set $a
       global.get $hp         ;; Advance heap ptr by word size in bytes.
       i32.const ,word-size
       i32.add                
       global.set $hp          
       local.get $a)]        ;; return the tagged ptr
    ['unbox
     (append (assert-box)
     `(i32.const ,type-box
       i32.xor
       i32.load))]
    ['car
     (append (assert-cons)
     `(i32.const ,type-cons  ;; get the address of pair from the tagged ptr
       i32.xor               
       i32.const ,word-size  ;; get the address of car (in higher address)
       i32.add               
       i32.load))]           ;; get the car
    ['cdr
     `(i32.const ,type-cons  ;; get the address of pair from the tagged ptr
       i32.xor                             
       i32.load)]            ;; get the cdr (in lower word of the pair)
    ['empty? (eq-imm val-empty)]
    ['string?
     (type-pred ptr-mask type-string)]
    ['string-length
     (append (assert-string)
     `(i32.const ,type-string
       i32.xor
       i32.load))]
    
)))

;; Op2 Expr Expr CEnv -> Wasm
(define (compile-prim2 p e1 e2 c)
  (append
   `(,@(compile-e e1 c)
     local.set $a
     global.get $sp
     i32.const ,(* word-size (length c))
     i32.sub
     local.get $a
     i32.store)       
   (match p
     ['+
      `(,@(compile-e e2 (cons #f c))
        local.set $a
        global.get $sp
        i32.const ,(* word-size (length c))
        i32.sub
        i32.load
        local.get $a       
        i32.add)]
     ['-
      `(,@(compile-e e2 (cons #f c))
        local.set $a
        global.get $sp
        i32.const ,(* word-size (length c))
        i32.sub
        i32.load
        local.get $a       
        i32.sub)]
     ['eq?
      `(,@(compile-e e2 (cons #f c))
        local.set $a         
        global.get $sp
        i32.const ,(* word-size (length c))
        i32.sub
        i32.load
        local.get $a
        i32.eq
        if
        i32.const ,val-true
        local.set $a
        else
        i32.const ,val-false
        local.set $a
        end
        local.get $a)]
     ['cons
      `(,@(compile-e e2 (cons #f c))
        local.set $a              ;; store cdr in heap
        global.get $hp
        local.get $a
        i32.store
        global.get $hp             ;; tag the heap ptr
        i32.const ,type-cons    
        i32.or                 
        local.set $b              ;; local var $b contains the reutrn result
        global.get $hp             ;; Advance heap ptr by word size in bytes.
        i32.const ,word-size
        i32.add                
        global.set $hp          
        global.get $sp             ;; get the car from operand stack
        i32.const ,(* word-size (length c))
        i32.sub
        i32.load               
        local.set $a
        global.get $hp             ;; store car in the advanced heap ptr
        local.get $a
        i32.store
        global.get $hp             ;; Advance heap ptr by word size in bytes.
        i32.const ,word-size
        i32.add                
        global.set $hp
        local.get $b)]            ;; return the result
     ['string-ref
      `(,@(compile-e e2 (cons #f c))
        ,@(assert-integer)
        local.set $b              ;; $ b = the integer of index
        global.get $sp           ;; get the tagged str ptr from the user stack 
        i32.const ,(* word-size (length c))
        i32.sub
        i32.load
        ,@(assert-string)         ;; $b should not be used in assert-type
        i32.const ,type-string
        i32.xor
        local.set $a              ;; $a = the start address of string on heap
        local.get $a              ;; get the length of string from str_addr[0]
        i32.load        
        local.set $c              ;; $c = the length of the str
        local.get $b              ;; call $error if the int parameter < 0
        i32.const ,(imm->bits 0)
        i32.lt_s
        if
        call $error_exit
        drop
        end
        local.get $b              ;; call $error if the int parameter >= str len
        local.get $c
        i32.ge_s
        if
        call $error_exit
        drop
        end
        local.get $a              ;; put str_addr on the operand stack
        local.get $b              ;; Add 1 to the index, as length is at index 0
        i32.const ,(imm->bits 1)
        i32.add          
        i32.const ,int-shift
        i32.shl                   ;; shift the index to the left by int-shift
        i32.const ,word-size
        i32.shr_s                 ;; then shift it to the right by word-size
        i32.add                   ;; add this to the str_addr
        i32.load)]                ;; get str_addr[index] = char of str at index
     ['make-string
      `(,@(compile-e e2 (cons #f c))
        ,@(assert-char)
        local.set $b              ;; $ b = the char parameter
        global.get $sp       ;; get the first parameter (integer) from the user stack.
        i32.const ,(* word-size (length c))
        i32.sub
        i32.load
        ,@(assert-integer)        ;; $b should not be used in assert-type
        local.set $a              ;; $a = the int parameter
        local.get $a
        i32.const ,(imm->bits 0)  ;; call $error if the int parameter < 0 
        i32.lt_s
        if
        call $error_exit
        drop
        end
        global.get $hp             ;; put the heap ptr on op. stack
        local.get $a
        i32.store               ;; store the length on the heap at str_addr = $hp
        global.get $hp           ;; create the tagged ptr to the str
        i32.const ,type-string
        i32.or
        local.set $c         ;; $ c = the tagged ptr to the start of str on heap
        block               
        loop                      ;; start of the loop
        global.get $hp            ;; advance the heap pointer by the word-size
        i32.const ,word-size
        i32.add
        global.set $hp
        local.get $a            ;; the counter starts from the length and counts
                                ;; down by (imm->bits 1). At the same time, in
                                ;; each iteration, the heap ptr is increased by
                                ;; word-size (≡ i++, where i is the index of the
                                ;; word of str on the heap)
        i32.eqz                 ;; check if counter = 0
        br_if 1                 ;; if so, break to the outer block (with index 1)
        global.get $hp
        local.get $b
        i32.store                 ;; store the char in str_addr[i]
        local.get $a
        i32.const ,(imm->bits 1)  ;; decrement the counter
        i32.sub
        local.set $a
        br 0                      ;; continue with the loop (with index 0)
        end                       ;; end of the loop (with index 0)
        end                       ;; end of the outer block (with index 0)
        local.get $c)]            ;; return the tagged ptr to the start of str
     )))


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

;; Expr Expr CEnv -> Wasm
(define (compile-begin e1 e2 c)
  (append (compile-e e1 c)
          `(drop)
          (compile-e e2 c)))

;; Imm -> Wasm 
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
;;           i32.const ,(* word-size (length xs))
;;           global.get $sp
;;           i32.add
;;           global.set $sp      ;; consider changing the user stack ptr
;;           local.get $a)))
                     

;; [Listof Expr] CEnv -> Wasm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (append (compile-e e c)
     `(local.set $a
       global.get $sp          
       i32.const ,(* word-size (length c))
       i32.sub
;;     i32.set $sp     ;; consider decrementing the user stack ptr (akin to push)
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
       [#t (* word-size (sub1 len-c))]   ;; consider changing the user stack ptr
       [#f (- (lookup x rest len-c) word-size)])]))

;; Id CEnv -> Wasm
(define (compile-variable x c)
  (let ((i (lookup x c (length c))))
    `(global.get $sp    ;; the local var for top address of start of user stack.
      i32.const ,i
      i32.sub          ;; top address - (word-size * ((length c) -  offset))
      i32.load)))

;; String -> Wasm
(define (compile-string s)  
  (let ((len (string-length s)))
    `(global.get $hp                 ;; store length of string on heap 
      i32.const ,(imm->bits len)
      i32.store
      global.get $hp                 ;; store tagged heap pointer in $b
      i32.const ,type-string
      i32.or
      local.set $b        
      global.get $hp                 ;; advance heap pointer
      i32.const ,word-size
      i32.add
      global.set $hp
      ,@(compile-string-chars (string->list s))
      local.get $b)))
   
;; [Listof Char] Nat -> Wasm
(define (compile-string-chars cs)   ;; $b should not be used in this function
  (match cs
    ['() '()]
    [(cons c cs)
     `(global.get $hp               ;; store char on heap
       i32.const ,(imm->bits c)
       i32.store 
       global.get $hp                 ;; advance heap pointer
       i32.const ,word-size
       i32.add
       global.set $hp
       ,@(compile-string-chars cs))]))

;; Integer Integer -> (() -> Wasm)
(define (assert-type mask type)
  (λ ()      
  `(local.set $a           ;; $b should not be used in this function              
    local.get $a           ;; due to use in string-ref 
    i32.const ,mask
    i32.and
    i32.const ,type
    i32.ne
    if
    call $error_exit
    drop
    end
    local.get $a)))

;; Integer Integer -> Wasm
(define (type-pred mask type)
  `(i32.const ,mask
    i32.and
    i32.const ,type
    i32.eq
    if
    i32.const ,val-true
    local.set $a
    else
    i32.const ,val-false
    local.set $a
    end
    local.get $a))

;; Integer Integer -> (() -> Wasm)
(define assert-integer
  (assert-type mask-int type-int))
(define assert-char
  (assert-type mask-char type-char))
(define assert-box
  (assert-type ptr-mask type-box))
(define assert-cons
  (assert-type ptr-mask type-cons))
(define assert-string
  (assert-type ptr-mask type-string))

;; Wasm
(define assert-byte
  (append (assert-integer)
   `(local.set $a
     local.get $a
     i32.const ,(imm->bits 0)
     i32.lt_s
     if
     call $error_exit
     drop
     end
     local.get $a
     i32.const ,(imm->bits 255)
     i32.gt_s
     if
     call $error_exit
     drop
     end
     local.get $a)))

;; Wasm
(define assert-codepoint
  (append (assert-integer)
   `(local.set $a
     local.get $a
     i32.const ,(imm->bits 0)
     i32.lt_s
     if
     call $error_exit
     drop
     end
     local.get $a
     i32.const ,(imm->bits 1114111)
     i32.gt_s
     if
     call $error_exit
     drop
     end
     local.get $a
     i32.const ,(imm->bits 55296)
     i32.ge_s
     if
     local.get $a
     i32.const ,(imm->bits 57343)
     i32.le_s
     if
     call $error_exit
     drop
     end
     end
     local.get $a)))

(define (compile-call f es c)
  (let ((h (* word-size (length c))))
   `(,@(compile-es es c)     ;; put the arguments on the stack
     global.get $sp          ;; change stack ptr to point to top of user stack (to
     i32.const ,h            ;; the addr after env c, where first arg to f is)
     i32.sub
     global.set $sp
     call ,f
     global.get $sp
     i32.const ,h
     i32.add
     global.set $sp)))       ;; return the stack ptr to where it was before the call 

(define (compile-defines fs ls)
  (match* (fs ls)
    [('() '()) '()] 
    [((cons f fs) (cons (Lam l xs e) ls))
     `(,(compile-define f xs e)
       ,@(compile-defines fs ls))]))

(define (compile-define f xs e)
  (let ((wa-f (symbol->wa-label f)))
    `(func ,wa-f (result i32)
       (local $a i32)         ;; local var (used as virtual register)
       (local $b i32)         ;; virtual register
       (local $c i32)         ;; virtual register 
       ,@(compile-e e (reverse xs)))))

;; Symbol -> Wasm Identifier
;; Produce a symbol that is a valid Wasm symbolic identifier. (Based on section
;; 6.3.5 of WebAssembly Specification, Release 1.1. The characters `'| are also
;; valid, but are excluded here because of use in Racket.)
;; 
(define (symbol->wa-label s)
  (string->symbol
   (string-append
    "$label_"
    (list->string
     (map (λ (c)
            (if (or (char<=? #\a c #\z)
                    (char<=? #\A c #\Z)
                    (char<=? #\0 c #\9)
                    (memq c '(#\_ #\$ #\# #\@ #\~ #\. #\? #\! #\% #\& #\^
                              #\* #\+ #\- #\/ #\: #\< #\= #\> #\\)))
                c
                #\_))
         (string->list (symbol->string s))))
    "_"
    (number->string (eq-hash-code s) 16))))       
