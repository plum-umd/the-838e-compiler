#lang racket

(define (byte? b)
  (if (integer? b)
      (if (<= 0 b)
          (if (<= b 255)
              #t
              #f)
          #f)
      #f))

(define (subbytes b start [end (bytes-length b)])
    
)