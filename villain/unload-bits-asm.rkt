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
        (let ((b (bitwise-xor i type-bignum)))
          (let ((length-sign (unload-value (heap-ref-signed b))))
            (* (/ length-sign (abs length-sign)) (bignum-loop (abs length-sign) b))))]
    [(? vector-bits? i)
        (let ((length (heap-ref i)))
          (let ((elems (vector-loop length i)))
            (list->vector (reverse elems))))]
    [(? flonum-bits? i)
     (bits->flonum (heap-ref i))]))

(define (bits->flonum b)
  (let (; the max decimal places to round to are the first non-zero bits
        ;   (sig (arithmetic-shift b -64))
        ; then is bit representing the sign (0 if positive, 1 if negative)
        (sign (bitwise-and (arithmetic-shift b -63)
                           1))
        ; the next 11 bits are the exponential + 127
        (exp (bitwise-and (- (arithmetic-shift 1 11) 1)
                          (arithmetic-shift b -52)))
        ;; the next 28 bits represent the mantissa
        (mantissa (bitwise-and (- (arithmetic-shift 1 52) 1)
                               b )))

    ;; result is a rounded version of (-1)^sign * 2^(exp - 127) * (1 + .mantissa)
    (let ((result (* (expt -1 sign) (expt 2 (- exp 1023))
                     (+ 1 (binary->decimal mantissa 0 -52)))))
      (exact->inexact result))))

;; converts the binary of the mantissa to decimal with adding
(define (binary->decimal bits acc twoExp)
  (match twoExp
    [0 acc]
    [_  (if (= 1 (bitwise-and bits 1))
            (binary->decimal (arithmetic-shift bits -1)
                             (+ (expt 2 twoExp) acc)
                             (+ twoExp 1))
            (binary->decimal (arithmetic-shift bits -1)
                             acc
                             (+ twoExp 1)))]))

(define (untag i)
  (arithmetic-shift (arithmetic-shift (arithmetic-shift i 4) (- (+ 4 (integer-length ptr-bottom-mask))))
                    (integer-length ptr-bottom-mask)))


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
      [n (let ((v1 (heap-ref (+ i (arithmetic-shift n imm-shift)))))
             (+ (arithmetic-shift v1 (* 64 (sub1 n))) (bignum-loop (- n 1) i)))])))
                             
(define vector-loop
  (λ (n i)
    (match n
      [0 '()]
      [n (cons (unload-value (heap-ref (+ i (arithmetic-shift n imm-shift))))
               (vector-loop (- n 1) i))])))


