#lang racket
(provide unload/free unload-value)
(require "types.rkt"
         ffi/unsafe)

;; Answer* -> Answer
(define (unload/free a)
  (match a
    ['err 'err]
    [(cons h v) (begin0 (unload-value v)
                        (free h))]))

;; Value* -> Value
(define (unload-value v)
  (match v
    [(? imm-bits?) (bits->imm v)]
    [(? box-bits? i)
     (box (unload-value (heap-ref i)))]
    [(? cons-bits? i)
     (cons (unload-value (heap-ref (+ i (arithmetic-shift 1 imm-shift))))
           (unload-value (heap-ref i)))]
    [(? string-bits? i)
        (let ((length (unload-value (heap-ref i))))
          (let ((str-chars (string-loop length i)))
            (list->string (reverse str-chars))))]
    [(? symbol-bits? i)
     (string->symbol
        (let ((length (unload-value (heap-ref i))))
          (let ((str-chars (string-loop length i)))
            (list->string (reverse str-chars)))))]
    [(? bignum-bits? i)
        (let ((length-sign (unload-value (heap-ref-signed i))))
          (* (/ length-sign (abs length-sign)) (bignum-loop (abs length-sign) i)))]))
                     
(define (untag i)
  (arithmetic-shift (arithmetic-shift i (- (integer-length ptr-mask)))
                    (integer-length ptr-mask)))

(define (heap-ref i)
  (ptr-ref (cast (untag i) _int64 _pointer) _uint64))

(define (heap-ref-signed i)
  (ptr-ref (cast (untag i) _int64 _pointer) _sint64))

(define string-loop
  (λ (n i)
    (match n
      [0 '()]
      [n (let ((v1 (bitwise-and (- (expt 2 64) 1) (arithmetic-shift
                    (heap-ref (+ i (* 8 (ceiling (/ n 3)))))
                    (+ 1 (* 21 (- 2 (remainder (- n 1) 3))))))))
           (let ((v2 (arithmetic-shift v1 -43)))
             (cons (unload-value v2) (string-loop (- n 1) i))))])))

(define bignum-loop
  (λ (n i)
    (match n
      [0 0]
      [n (let ((v1 (heap-ref (+ i (* 8 n)))))
             (+ (arithmetic-shift v1 (* 64 (sub1 n))) (bignum-loop (- n 1) i)))])))
       
                      
