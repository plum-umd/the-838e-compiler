#lang racket
(provide = byte?)

(define (byte? b)
  (if (integer? b)
      (if (<= 0 b)
          (if (<= b 255)
              #t
              #f)
          #f)
      #f))

(define (bytes . b)
    (let ([scratch (make-bytes (length b))])
        (define (^bytes ind b)
            (match b
                ['() scratch]
                [(cons byte bytes) 
                    (begin 
                        (bytes-set! scratch ind byte)
                        (^bytes (+ ind 1) bytes))]))
      (^bytes 0 b)))

(define (subbytes bstr start [end (bytes-length bstr)])
  (let ([scratch (make-bytes (- end start))])
    (define (^subbytes! cur)
      (when (< cur end)
        (begin
          (bytes-set! scratch (- cur start) (bytes-ref bstr cur))
          (^subbytes! (+ cur 1)))))
    (begin
      (^subbytes! start)
      scratch)))

(define (bytes-copy bstr) (subbytes bstr 0))

(define (bytes-copy! dest dest-start src [src-start 0] [src-end (bytes-length src)])
  (define (^bytes-copy! src-pos dest-pos)
    (when (< src-pos src-end)
      (begin
        (bytes-set! dest dest-pos (bytes-ref src src-pos))
        (^bytes-copy! (+ src-pos 1) (+ dest-pos 1)))))
  (^bytes-copy! src-start dest-start))

(define (bytes-fill! dest b)
  (let ([dest-length (bytes-length dest)])
    (define (^bytes-fill! cur)
      (when (< cur dest-length)
        (begin
          (bytes-set! dest cur b)
          (^bytes-fill! (+ cur 1)))))
    (^bytes-fill! 0)))

(define (bytes-append . bstr)
  (define (bytelist-length bytelist acc)
    (match bytelist
      ['() acc]
      [(cons bstr others) (bytelist-length others (+ acc (bytes-length bstr)))]))
  
  (let ([scratch (make-bytes (bytelist-length bstr 0))])
    (define (^bytes-append offset bytelist)
      (match bytelist
        ['() scratch]
        [(cons bstr others) (begin
                              (bytes-copy! scratch offset bstr)
                              (^bytes-append (+ offset (bytes-length bstr)) others))]))
    (^bytes-append 0 bstr)))
      