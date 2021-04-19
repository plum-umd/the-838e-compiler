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

(define (bytes->list bstr)
  (let ([bstr-len (bytes-length bstr)])
    (define (^bytes->list cur)
      (if (< cur bstr-len)
          (cons
           (bytes-ref bstr cur)
           (^bytes->list (+ cur 1)))
          '()))

    (^bytes->list 0)))

(define (list->bytes lst)
  (apply bytes lst))

(define (bytes=? . bstrs)
  (match bstrs
    ['() #t]
    [(cons bstr others)
     (define (^bytes=? bstrs)
       (match bstrs
         ['() #t]
         [(cons h others) (and (eqv? h bstr) (^bytes=? others))]))

     (^bytes=? others)]))

(define (bytes<? . bstrs)
  (define (^bytes2<? bstr1 bstr2)
    (let ([len1 (bytes-length bstr1)] [len2 (bytes-length bstr2)])
      (define (^^bytes<? cur)
        (if (< cur len1)
            (if (< cur len2)
                (or
                 (< (bytes-ref bstr1 cur) (bytes-ref bstr2 cur))
                 (^^bytes<? (+ cur 1)))
                #f)
            #f))
      (^^bytes<? 0)))

  (define (^bytes<? bstrs)
    (match bstrs
      [(cons h1 (cons h2 t)) (and (^bytes2<? h1 h2) (^bytes<? t))]
      [_ #t]))

  (^bytes<? bstrs))

(define (bytes>? . bstrs)
  (apply bytes<? (reverse bstrs)))
      